from .blocks import CodeBlock
from .statements import Declaration
from .symbols import DataType, FuncSignature, Ident
from .expressions import Parameter
from hidc.lexer import Span

import dataclasses as dc
from collections.abc import Sequence


@dc.dataclass(frozen=True)
class FuncDeclaration:
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


@dc.dataclass(frozen=True)
class Program:
    var_decls: Sequence[Declaration]
    func_decls: Sequence[FuncDeclaration]
