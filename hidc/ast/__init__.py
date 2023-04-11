from .abc import *
from .blocks import *
from .expressions import *
from .statements import *
from .symbols import *

import dataclasses as dc
from hidc.lexer.scanner import Span
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
