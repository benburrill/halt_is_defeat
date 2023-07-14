from .blocks import CodeBlock, ExitMode
from .statements import Declaration, ReturnStatement
from .symbols import Type, DataType, ArrayType, Ident, Flavor, Environment, VarTable
from .expressions import Parameter
from hidc.lexer import Span
from hidc.utils.data_abc import Abstract, DataABC
from hidc.errors import TypeCheckError

import dataclasses as dc
from collections.abc import Sequence


class FuncDefinition(DataABC):
    ret_type: Abstract[DataType]
    name: Abstract[Ident]
    param_types: Abstract[tuple[Type, ...]]

    @property
    def signature(self):
        params = ', '.join(map(str, self.param_types))
        return f'{self.name}({params})'


@dc.dataclass(frozen=True)
class BuiltinStub(FuncDefinition):
    ret_type: DataType
    name: Ident
    param_types: tuple[Type, ...]


@dc.dataclass(frozen=True)
class FuncDeclaration(FuncDefinition):
    span: Span
    ret_type: DataType
    name: Ident
    params: Sequence[Parameter]
    body: CodeBlock

    @property
    def param_types(self):
        return tuple(p.type for p in self.params)

    def evaluate(self, env):
        new_env = env.new_child(self.ret_type)
        for param in self.params:
            Declaration(param.var, param, param.span.start).evaluate(new_env)

        body = self.body.evaluate(new_env)
        exit_modes = body.exit_modes()
        # Should have been handled elsewhere, but assert just in case
        assert ExitMode.BREAK not in exit_modes
        assert ExitMode.DEFEAT not in exit_modes or self.name.flavor == Flavor.DEFEAT
        if ExitMode.NONE in exit_modes:
            if self.ret_type != DataType.VOID:
                raise TypeCheckError('Missing return statement', self.body.span)
            body = CodeBlock(
                body.stmts + (ReturnStatement(body.span.end),),
                body.span, exit_modes.replace(ExitMode.NONE, ExitMode.RETURN)
            )
        return FuncDeclaration(self.span, self.ret_type, self.name, self.params, body)


builtin_stubs = (
    BuiltinStub(DataType.VOID, Ident.defeat('is_defeat'), ()),
    BuiltinStub(DataType.VOID, Ident.defeat('truth_is_defeat'), (DataType.BOOL,)),
    # TODO: I think I want to rename "print" to "write"
    BuiltinStub(DataType.VOID, Ident('print'), (DataType.STRING,)),
    BuiltinStub(DataType.VOID, Ident('print'), (ArrayType(DataType.BYTE, const=True),)),
    BuiltinStub(DataType.VOID, Ident('print'), (DataType.INT,)),
    BuiltinStub(DataType.VOID, Ident('print'), (DataType.BYTE,)),
    BuiltinStub(DataType.VOID, Ident('print'), (DataType.BOOL,)),
    BuiltinStub(DataType.VOID, Ident('println'), (DataType.STRING,)),
    BuiltinStub(DataType.VOID, Ident('println'), (ArrayType(DataType.BYTE, const=True),)),
    BuiltinStub(DataType.VOID, Ident('println'), (DataType.INT,)),
    BuiltinStub(DataType.VOID, Ident('println'), (DataType.BYTE,)),
    BuiltinStub(DataType.VOID, Ident('println'), (DataType.BOOL,)),
    BuiltinStub(DataType.VOID, Ident('println'), ()),
    BuiltinStub(DataType.VOID, Ident('all_is_win'), ()),
    BuiltinStub(DataType.VOID, Ident('all_is_broken'), ())
    # TODO: also sleep and debug?  (level_is_debug?)
)


def typecheck(prog, **options):
    env = Environment(VarTable(), {}, options)
    env.add_funcs(builtin_stubs)
    return prog.evaluate(env)


@dc.dataclass(frozen=True)
class Program:
    var_decls: tuple[Declaration, ...]
    func_decls: tuple[FuncDeclaration, ...]

    def evaluate(self, env):
        env.add_funcs(self.func_decls)
        new_decls = [decl.evaluate(env) for decl in self.var_decls]
        new_funcs = [func.evaluate(env) for func in self.func_decls]
        return Program(tuple(new_decls), tuple(new_funcs))
