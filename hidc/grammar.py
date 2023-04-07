from .parser import *
from .tokens import *
from .ast import *
from .errors import ParserError


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
async def ps_vdecl():
    start = await cursor()
    if var := await ps_decl():
        if await Exact(StmtToken.ASSIGN):
            return Declaration(var, await expect(ps_expr()), start)
        elif brac := await Exact(BracToken.LSQUARE):
            if var.const:
                raise ParserError('VLAs should not be declared const', start)
            elif isinstance(var.type, ArrayType):
                raise ParserError('Unexpected [', brac.span)
            dt = ArrayType(var.type.token)
            init = ArrayInitializer(dt, await expect(ps_expr()))
            await expect(Exact(BracToken.RSQUARE))
            return Declaration(Variable(False, dt, var.name), init, start)
        else:
            raise await ParserError.expected('= or [')


@Parser.routine('expression')
async def ps_expr():
    if lxm := await Instance(IntToken | CharToken):
        return IntLiteral(lxm.token.data, lxm.span)
    # TODO


@Parser.routine('assignment')
async def ps_assignment():
    if assignable := await ps_expr():
        if isinstance(assignable, Assignable):
            if await Exact(StmtToken.ASSIGN):
                return Assignment(assignable, await expect(ps_expr()))
            elif lxm := await Instance(IncAssignToken):
                return IncAssignment(
                    assignable, await expect(ps_expr()),
                    lxm.token.operator
                )
