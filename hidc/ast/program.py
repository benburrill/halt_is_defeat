from .blocks import CodeBlock
from .statements import Declaration
from .symbols import DataType, FuncSignature
from hidc.lexer import Span

import dataclasses as dc
from collections.abc import Sequence


@dc.dataclass(frozen=True)
class FuncDeclaration:
    span: Span
    ret_type: DataType
    signature: FuncSignature
    body: CodeBlock


@dc.dataclass(frozen=True)
class Program:
    var_decls: Sequence[Declaration]
    func_decls: Sequence[FuncDeclaration]
