from .parser import *
from .tokens import *
from .ast import *
from .errors import ParserError

import enum
from functools import cached_property


class BlockContext(enum.Flag):
    FUNC = 0
    YOU = enum.auto()
    DEFEAT = enum.auto()
    LOOP = enum.auto()
    _TRY = enum.auto()
    TRY = _TRY | DEFEAT

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


@Parser.routine('program')
async def ps_program():
    pass


@Parser.routine('scalar variable type')
async def ps_scalar_vtype():
    if tp := await Instance(TypeToken):
        if tp.token != TypeToken.VOID:
            return tp.token


@Parser.routine('variable type')
async def ps_vtype():
    if tp := await ps_scalar_vtype():
        if await Exact(BracToken.LSQUARE):
            await expect(Exact(BracToken.RSQUARE))
            return ArrayType(tp)
        return DataType(tp)


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


@Parser.routine('expression')
async def ps_expr(ctx):
    if lxm := await Instance(IntToken | CharToken):
        return IntLiteral(lxm.token.data, lxm.span)
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
    return await ps_block(ctx) or await ps_plain_stmt(ctx)


@Parser.routine('statements')
async def ps_code_block(ctx):
    if start := await Exact(BracToken.LCURLY):
        result = []
        while stmt := await ps_stmt(ctx):
            result.append(stmt)
            await expect(Exact(SepToken.SEMICOLON))

        end = await expect(Exact(BracToken.RCURLY), expected='statement or }')
        return CodeBlock(tuple(result), Span(start.span.start, end.span.end))


@Parser.routine('block')
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
    return ps_code_block(ctx)
