from hidc.parser.rules import *
from hidc.lexer.tokens import *
from hidc.parser.ast import *
from hidc.errors import ParserError

import enum
from functools import cached_property


@Parser.routine('variable type')
async def ps_vtype():
    if (tp := await Instance(Type)) and tp.token != Type.VOID:
        if await Exact(BracToken.LSQUARE):
            await expect(Exact(BracToken.RSQUARE))
            return ArrayType(tp.token)
        return DataType(tp.token)


@Parser.routine('identifier')
async def ps_ident(allowed_flavors):
    if ident := await Instance(Ident):
        if ident.token.flavor in allowed_flavors:
            return ident

        raise ParserError(
            f'Improper use of identifier: '
            f'{ident.token.name} is {ident.token.flavor.name}',
            ident.span
        )


@Parser.routine('declaration')
async def ps_decl():
    const = bool(await Exact(StmtToken.CONST))
    if dt := await ps_vtype():
        ident = await expect(ps_ident({Flavor.NONE}), expected='variable name')
        return Variable(const, dt, ident.token.name)
    elif const:
        raise await ParserError.expected('data type after const')


@Parser.routine('variable declaration')
async def ps_vdecl(ctx):
    start = await cursor()
    if var := await ps_decl():
        if await Exact(StmtToken.ASSIGN):
            return Declaration(var, await expect(ps_expr(ctx)), start)
        elif brac := await Exact(BracToken.LSQUARE):
            if var.const:
                raise ParserError('VLAs should not be declared const', start)
            elif isinstance(var.type, ArrayType):
                raise ParserError('Unexpected [', brac.span)
            dt = ArrayType(var.type.token)
            init = ArrayInitializer(dt, await expect(ps_expr(ctx)))
            await expect(Exact(BracToken.RSQUARE))
            return Declaration(Variable(False, dt, var.name), init, start)
        else:
            raise await ParserError.expected('= or [')


# No need for this to be a Parser rule
async def comma_list(rule):
    result = []
    if first := await rule:
        result.append(first)
        while await Exact(SepToken.COMMA):
            result.append(await expect(rule))
    return tuple(result)


# left associative binary op
async def bin_op(expr_rule, ops):
    if not (expr := await expr_rule): return
    while op := await OneOf(ops):
        right = await expect(expr_rule)
        expr = BinaryOp(op.token, op.span, expr, right)
    return expr


@Parser.routine('function call')
async def ps_func_call(ctx):
    if BlockContext.FUNC not in ctx: return
    if ident := await ps_ident(ctx.flavors):
        if await Exact(BracToken.LPAREN):
            args = await comma_list(ps_expr(ctx))
            end = await expect(Exact(BracToken.RPAREN))
            return FuncCall(ident.token, args, ident.span | end.span)


@Parser.routine('expression')
async def ps_expr0(ctx):
    if await Exact(BracToken.LPAREN):
        expr = await expect(ps_expr(ctx))
        await expect(Exact(BracToken.RPAREN))
        return expr
    elif lit := await Instance(IntToken | CharToken):
        return IntLiteral(lit.token.data, lit.span)
    elif lit := await Instance(StringToken):
        return StringLiteral(lit.token.data, lit.span)
    elif lit := await Instance(BoolToken):
        return BoolLiteral(lit.token.data, lit.span)
    elif start := await Exact(BracToken.LSQUARE):
        items = await comma_list(ps_expr(ctx))
        end = await expect(Exact(BracToken.RSQUARE))
        return ArrayLiteral(items, start.span | end.span)
    elif func_call := await ps_func_call(ctx):
        return func_call
    elif ident := await ps_ident({Flavor.NONE}):
        return VariableLookup(ident.token.name, ident.span)

@Parser.routine('expression')
async def ps_expr1(ctx):
    if not (expr := await ps_expr0(ctx)): return
    if await Exact(SepToken.DOT):
        attr = await expect(Exact(Ident('length')))
        return LengthLookup(expr, attr.span.end)
    if await Exact(BracToken.LSQUARE):
        index = await expect(ps_expr(ctx))
        end = await expect(Exact(BracToken.RSQUARE))
        return ArrayLookup(expr, index, end.span.end)
    return expr

@Parser.routine('expression')
async def ps_expr2(ctx):
    if op := await OneOf({Op.ADD, Op.SUB, Op.NOT}):
        return UnaryOp(op.token, op.span, await expect(ps_expr2(ctx)))
    return await ps_expr1(ctx)

@Parser.routine('expression')
async def ps_expr3(ctx):
    return await bin_op(ps_expr2(ctx), {Op.MUL, Op.DIV, Op.MOD})

@Parser.routine('expression')
async def ps_expr4(ctx):
    return await bin_op(ps_expr3(ctx), {Op.ADD, Op.SUB})

@Parser.routine('expression')
async def ps_expr5(ctx):
    return await bin_op(ps_expr4(ctx), {
        Op.LT, Op.LE, Op.GT, Op.GE,
        Op.EQ, Op.NE
    })

@Parser.routine('expression')
async def ps_expr6(ctx):
    return await bin_op(ps_expr5(ctx), {Op.AND})

@Parser.routine('expression')
async def ps_expr(ctx):
    return await bin_op(ps_expr6(ctx), {Op.OR})


@Parser.routine('assignment')
async def ps_assignment(ctx):
    if assignable := await ps_expr(ctx):
        if isinstance(assignable, Assignable):
            if await Exact(StmtToken.ASSIGN):
                return Assignment(assignable, await expect(ps_expr(ctx)))
            elif lxm := await Instance(IncAssignToken):
                return IncAssignment(
                    assignable, await expect(ps_expr(ctx)),
                    lxm.token.operator
                )


@Parser.routine('plain statement')
async def ps_plain_stmt(ctx, *, allow_decl=True):
    if stmt := (await ps_assignment(ctx) or await ps_expr(ctx)):
        return stmt
    elif allow_decl and (stmt := await ps_vdecl(ctx)):
        return stmt


@Parser.routine('statement')
async def ps_stmt(ctx):
    if tok := await Exact(StmtToken.BREAK):
        if BlockContext.LOOP not in ctx:
            raise ParserError('break outside of loop', tok.span)
        return BreakStatement(tok.span)
    elif tok := await Exact(StmtToken.CONTINUE):
        if BlockContext.LOOP not in ctx:
            raise ParserError('continue outside of loop', tok.span)
        return ContinueStatement(tok.span)
    elif tok := await Exact(StmtToken.RETURN):
        return ReturnStatement(tok.span, await ps_expr(ctx))
    return await ps_plain_stmt(ctx)


@Parser.routine('code block')
async def ps_code_block(ctx):
    if start := await Exact(BracToken.LCURLY):
        result = []
        while not (end := await Exact(BracToken.RCURLY)):
            if stmt := await ps_stmt(ctx):
                await expect(Exact(SepToken.SEMICOLON))
                result.append(stmt)
            elif not await Exact(SepToken.SEMICOLON):
                result.append(await expect(
                    ps_block(ctx), expected='statement or block'
                ))

        return CodeBlock(tuple(result), start.span | end.span)


@Parser.routine('block statement')
async def ps_block(ctx):
    start = await cursor()
    if await Exact(BlockToken.IF):
        await expect(Exact(BracToken.LPAREN))
        cond = await expect(ps_expr(ctx))
        await expect(Exact(BracToken.RPAREN))
        body = await expect(ps_block(ctx))
        else_block = await Exact(BlockToken.ELSE) and await ps_block(ctx)
        return IfBlock(start, body, cond, else_block)
    elif await Exact(BlockToken.WHILE):
        await expect(Exact(BracToken.LPAREN))
        cond = await expect(ps_expr(ctx))
        await expect(Exact(BracToken.RPAREN))
        body = await expect(ps_block(ctx | BlockContext.LOOP))
        return LoopBlock(start, body, cond)
    elif await Exact(BlockToken.FOR):
        await expect(Exact(BracToken.LPAREN))
        init = await ps_plain_stmt(ctx, allow_decl=True)
        await expect(Exact(SepToken.SEMICOLON),
                     expected='statement' if not init else ';')
        cond = await ps_expr(ctx)
        await expect(Exact(SepToken.SEMICOLON),
                     expected='expression' if not cond else ';')
        cont = await ps_plain_stmt(ctx, allow_decl=False)
        await expect(Exact(BracToken.RPAREN),
                     expected='assignment or expression' if not cont else ')')
        body = await expect(ps_block(ctx | BlockContext.LOOP))
        return LoopBlock.for_loop(start, body, init, cond, cont)
    elif await Exact(BlockToken.TRY):
        if BlockContext.YOU not in ctx:
            raise ParserError('try outside of you', start)
        body = await expect(ps_block(BlockContext.TRY))
        undo_tok = await expect(Exact(BlockToken.UNDO))
        undo = UndoBlock(undo_tok.span.start, await expect(ps_block(ctx)))
        return TryBlock(start, body, undo)
    elif await Exact(BlockToken.PREEMPT):
        if BlockContext.TRY not in ctx:
            raise ParserError('preempt outside of try', start)
        return PreemptBlock(start, await expect(ps_block(ctx)))
    return await ps_code_block(ctx)


@Parser.routine('function declaration')
async def ps_func():
    if not (tp := await Instance(Type)): return
    if not (name := await Instance(Ident)): return
    if not await Exact(BracToken.LPAREN): return

    ret_type = DataType(tp.token)
    if name.token.flavor == Flavor.YOU:
        ctx = BlockContext.YOU
    elif name.token.flavor == Flavor.DEFEAT:
        ctx = BlockContext.DEFEAT
    else:
        ctx = BlockContext.FUNC

    params = await comma_list(ps_decl())
    end_decl = await expect(Exact(BracToken.RPAREN))
    body = await expect(ps_code_block(ctx))
    return FuncDeclaration(
        tp.span | end_decl.span,
        ret_type, FuncSignature(name.token, params), body
    )


@Parser.routine('program')
async def ps_program():
    var_decls = []
    func_decls = []

    while await CurrentNode():
        if func := await ps_func():
            func_decls.append(func)
        elif not await Exact(SepToken.SEMICOLON):
            var = await expect(
                ps_vdecl(BlockContext.NONE),
                expected='function or variable declaration'
            )

            await expect(Exact(SepToken.SEMICOLON))
            var_decls.append(var)

    return Program(tuple(var_decls), tuple(func_decls))


# Oh cool, more enum breaking changes in Python 3.11, yay!
# https://github.com/python/cpython/issues/103365
#
# I was hoping it would be easy to make it impossible to have TRY
# without DEFEAT.  It would be easy in Python 3.10, but in Python 3.11
# we'd probably need to handle this ourselves in _missing_.  I'm not
# going to bother.
class BlockContext(enum.IntFlag):
    NONE   = 0
    FUNC   = 1
    YOU    = 2 | FUNC
    DEFEAT = 4 | FUNC
    TRY    = 8 | DEFEAT
    LOOP   = 16

    @classmethod
    def _missing_(cls, value):
        bad = cls.YOU.value | cls.DEFEAT.value
        if (bad & value) == bad:
            raise ValueError(f'Invalid block context: {value}')
        return super()._missing_(value)

    @cached_property
    def flavors(self):
        flavors = frozenset({})
        if BlockContext.FUNC in self:
            flavors |= {Flavor.NONE}
        if BlockContext.YOU in self:
            flavors |= {Flavor.YOU}
        if BlockContext.DEFEAT in self:
            flavors |= {Flavor.DEFEAT}
        return flavors