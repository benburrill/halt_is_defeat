from .parser import *
from .tokens import *
from .ast import *
from .errors import ParserError

import enum
from functools import cached_property


@Parser.routine('variable type')
async def ps_vtype():
    if (tp := await Instance(TypeToken)) and tp.token != TypeToken.VOID:
        if await Exact(BracToken.LSQUARE):
            await expect(Exact(BracToken.RSQUARE))
            return ArrayType(tp.token)
        return DataType(tp.token)


@Parser.routine('identifier')
async def ps_ident(allowed_flavors):
    if ident := await Instance(IdentToken):
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
async def ps_vdecl(expr_rule):
    start = await cursor()
    if var := await ps_decl():
        if await Exact(StmtToken.ASSIGN):
            return Declaration(var, await expect(expr_rule), start)
        elif brac := await Exact(BracToken.LSQUARE):
            if var.const:
                raise ParserError('VLAs should not be declared const', start)
            elif isinstance(var.type, ArrayType):
                raise ParserError('Unexpected [', brac.span)
            dt = ArrayType(var.type.token)
            init = ArrayInitializer(dt, await expect(expr_rule))
            await expect(Exact(BracToken.RSQUARE))
            return Declaration(Variable(False, dt, var.name), init, start)
        else:
            raise await ParserError.expected('= or [')


# No need for this to be a Parser rule
async def comma_list(rule):
    if first := await rule:
        yield first
        while await Exact(SepToken.COMMA):
            yield await expect(rule)


@Parser.routine('literal')
async def ps_literal(expr_rule):
    if lit := await Instance(IntToken | CharToken):
        return IntLiteral(lit.token.data, lit.span)
    elif lit := await Instance(StringToken):
        return StringLiteral(lit.token.data, lit.span)
    elif lit := await Instance(BoolToken):
        return BoolLiteral(lit.token.data, lit.span)
    elif start := await Exact(BracToken.LSQUARE):
        items = tuple([expr async for expr in comma_list(expr_rule)])
        end = await expect(Exact(BracToken.RSQUARE))
        return ArrayLiteral(items, Span(start.span.start, end.span.end))


@Parser.routine('expression')
async def ps_expr(ctx):
    return await ps_literal(ps_expr(ctx))
    # TODO


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
    elif allow_decl and (stmt := await ps_vdecl(ps_expr(ctx))):
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
            else:
                result.append(await expect(
                    ps_block(ctx), expected='statement or block'
                ))

        return CodeBlock(tuple(result), Span(start.span.start, end.span.end))


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
        await expect(Exact(SepToken.SEMICOLON))
        cond = await ps_expr(ctx)
        await expect(Exact(SepToken.SEMICOLON))
        cont = await ps_plain_stmt(ctx, allow_decl=False)
        await expect(Exact(BracToken.RPAREN))
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
    if not (tp := await Instance(TypeToken)): return
    if not (name := await Instance(IdentToken)): return
    if not await Exact(BracToken.LPAREN): return

    ret_type = DataType(tp.token)
    if name.token.flavor == Flavor.YOU:
        ctx = BlockContext.YOU
    elif name.token.flavor == Flavor.DEFEAT:
        ctx = BlockContext.DEFEAT
    else:
        ctx = BlockContext.FUNC

    params = tuple([expr async for expr in comma_list(ps_decl())])
    end_decl = await expect(Exact(BracToken.RPAREN))
    body = await expect(ps_code_block(ctx))
    return FuncDeclaration(
        Span(tp.span.start, end_decl.span.end),
        ret_type, name.token, params, body
    )


@Parser.routine('program')
async def ps_program():
    var_decls = []
    func_decls = []

    # Not sure I want to deal with evaluating any expressions in global
    # declarations, so only allow literals (recursively)
    rec_lit = Parser(lambda: ps_literal(rec_lit).consume(),
                     expected='literal value')

    while await CurrentNode():
        if var := await ps_vdecl(rec_lit):
            var_decls.append(var)
            await expect(Exact(SepToken.SEMICOLON))
        else:
            func_decls.append(await expect(
                ps_func(), expected='function or variable declaration'
            ))

    return Program(tuple(var_decls), tuple(func_decls))


# Oh cool, more enum breaking changes in Python 3.11, yay!
# https://github.com/python/cpython/issues/103365
#
# I was hoping it would be easy to make it impossible to have TRY
# without DEFEAT.  It would be easy in Python 3.10, but in Python 3.11
# we'd probably need to handle this ourselves in _missing_.  I'm not
# going to bother.
class BlockContext(enum.IntFlag):
    FUNC   = 0
    YOU    = 1
    DEFEAT = 2
    TRY    = 4 | DEFEAT
    LOOP   = 8

    @classmethod
    def _missing_(cls, value):
        bad = cls.YOU.value | cls.DEFEAT.value
        if (bad & value) == bad:
            raise ValueError(f'Invalid block context: {value}')
        return super()._missing_(value)

    @cached_property
    def flavors(self):
        flavors = frozenset({Flavor.NONE})
        if BlockContext.YOU in self:
            return flavors | {Flavor.YOU}
        if BlockContext.DEFEAT in self:
            return flavors | {Flavor.DEFEAT}
        return flavors
