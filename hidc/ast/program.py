from .blocks import CodeBlock, ExitMode
from .statements import Declaration, ReturnStatement
from .symbols import Type, DataType, ArrayType, FuncSignature, Ident, Flavor, Environment, VarTable
from .expressions import Parameter
from hidc.lexer import Span
from hidc.utils.data_abc import Abstract, DataABC
from hidc.errors import TypeCheckError

import dataclasses as dc
from collections.abc import Sequence


class FuncDefinition(DataABC):
    ret_type: Abstract[DataType]
    name: Ident
    signature: Abstract[Sequence[Type]]


@dc.dataclass(frozen=True)
class BuiltinStub(FuncDefinition):
    ret_type: DataType
    name: Ident
    param_types: Sequence[Type]

    @property
    def signature(self):
        return FuncSignature(self.name, self.param_types)


@dc.dataclass(frozen=True)
class FuncDeclaration(FuncDefinition):
    span: Span
    ret_type: DataType
    name: Ident
    params: Sequence[Parameter]
    body: CodeBlock

    @property
    def signature(self):
        return FuncSignature(
            self.name,
            tuple(p.type for p in self.params)
        )

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


builtin_funcs = {
    stub.signature: stub
    for stub in [
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
    ]
}

# sig = self.signature
#         if prev_decl := env.vars.get(sig):
#             # Allow re-evaluation if necessary
#             if prev_decl is self:
#                 return self
#
#             raise TypeCheckError(f'Redefinition of function {self.name}', self.span)


@dc.dataclass(frozen=True)
class Program:
    var_decls: Sequence[Declaration]
    func_decls: Sequence[FuncDeclaration]

    def checked(self, options=None):
        if options is None:
            options = {}

        unevaluated_funcs = dict(builtin_funcs)
        for func in self.func_decls:
            sig = func.signature
            if prev_decl := unevaluated_funcs.get(sig):
                raise TypeCheckError(
                    f'Redefinition of function {sig}',
                    (prev_decl.span, func.span)
                )
            unevaluated_funcs[sig] = func

        env = Environment(VarTable(), unevaluated_funcs, options)
        new_decls = [decl.evaluate(env) for decl in self.var_decls]
        new_funcs = [func.evaluate(env) for func in self.func_decls]
        return Program(new_decls, new_funcs)
