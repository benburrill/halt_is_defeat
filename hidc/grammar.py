from .parser import Syntax, Exact, Instance
from .errors import ParserError
from .lexer import Lexeme
from .tokens import *
from .ast import *

import dataclasses


@Syntax.builder('program')
def syn_program(ps):
    pass


@Syntax.builder('scalar variable type')
def syn_scalar_vtype(ps):
    if tp := ps.parse(Instance(TypeToken)):
        if tp.token != TypeToken.VOID:
            return tp.token


@Syntax.builder('variable type')
def syn_vtype(ps):
    if tp := ps.parse(syn_scalar_vtype()):
        if ps.parse(Exact(BracToken.LSQUARE)):
            ps.expect(Exact(BracToken.RSQUARE))
            return ArrayType(tp)
        return DataType(tp)


@Syntax.builder('identifier')
def syn_ident(ps, allowed_flavors):
    if ident := ps.parse(Instance(IdentToken)):
        if ident.token.flavor in allowed_flavors:
            return ident

        raise ParserError(
            f'Improper use of identifier: '
            f'{ident.token.name} is {ident.token.flavor.name}',
            ident.span
        )


@Syntax.builder('declaration')
def syn_decl(ps):
    const = bool(ps.parse(Exact(StmtToken.CONST)))
    if dt := ps.parse(syn_vtype()):
        ident = ps.expect(syn_ident({Flavor.NONE}), expected='variable name')
        return Variable(const, dt, ident.token.name)
    elif const:
        raise ps.unexpected(expected='data type after const')


@Syntax.builder('variable declaration')
def syn_vdecl(ps):
    start = ps.cursor
    if var := ps.parse(syn_decl()):
        if ps.parse(Exact(StmtToken.ASSIGN)):
            return Declaration(var, ps.expect(syn_expr()), start)
        elif brac := ps.parse(Exact(BracToken.LSQUARE)):
            if var.const:
                raise ParserError('VLAs should not be declared const', start)
            elif isinstance(var.type, ArrayType):
                raise ParserError('Unexpected [', brac.span)
            var = dataclasses.replace(var, type=ArrayType(var.type.token))
            init = ArrayInitializer(var.type, ps.expect(syn_expr()))
            ps.expect(Exact(BracToken.RSQUARE))
            return Declaration(var, init, start)
        else:
            raise ps.unexpected(expected='= or [')


@Syntax.builder('expression')
def syn_expr(ps):
    if lxm := ps.parse(Instance(IntToken | CharToken)):
        return IntLiteral(lxm.token.data, lxm.span)
    # TODO


@Syntax.builder('assignment')
def syn_assignment(ps):
    if assignable := ps.parse(syn_expr()):
        if isinstance(assignable, Assignable):
            if ps.parse(Exact(StmtToken.ASSIGN)):
                return Assignment(assignable, ps.expect(syn_expr()))
            elif lxm := ps.parse(Instance(IncAssignToken)):
                return IncAssignment(
                    assignable, ps.expect(syn_expr()),
                    lxm.token.operator
                )
