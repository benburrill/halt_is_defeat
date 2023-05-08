from hidc.lexer import Span
from hidc.utils.data_abc import DataABC, Abstract
from abc import abstractmethod


class Statement(DataABC):
    span: Abstract[Span]


class Block(Statement):
    pass
