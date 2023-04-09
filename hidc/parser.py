from .tokens import Token
from .utils.lazylist import lazy_list
from .errors import ParserError
from .lexer import lex

import abc
import functools
from dataclasses import dataclass
from collections.abc import Collection


def parse(rule, source, partial=False):
    if isinstance(rule, Parser):
        rule = Parser(rule.consume, backtrack=False)
    elif not isinstance(rule, Rule):
        # coroutine passed directly, eg expect
        rule = Parser((lambda r: lambda: r)(rule), backtrack=False)

    result, remaining = rule.process(lazy_list(lex(source)))

    if remaining and not partial:
        raise ParserError(
            f'Unprocessed token: {remaining.head.token}',
            remaining.head.span
        )

    return result


class Rule(abc.ABC):
    def __str__(self):
        return getattr(self, 'expected', None) or type(self).__name__

    @abc.abstractmethod
    def process(self, start):
        pass

    def __await__(self):
        result = yield self
        return result


class Match(Rule):
    def __init__(self, match, *, expected=None):
        self.match = match
        self.expected = expected

    def process(self, start):
        if start and self.match(start.head.token):
            return start.head, start.tail
        return None, start


@dataclass
class Exact(Match):
    token: Token
    def match(self, token):
        return self.token == token

    def __str__(self):
        return str(self.token)


@dataclass
class Instance(Match):
    type: type
    def match(self, token):
        return isinstance(token, self.type)

    def __str__(self):
        return self.type.__name__


@dataclass
class OneOf(Match):
    tokens: Collection[Token]
    def match(self, token):
        return token in self.tokens

    def __str__(self):
        return f'one of {list(self.tokens)}'


class Parser(Rule):
    def __init__(self, consume, *, expected=None, backtrack=True):
        self.consume = consume
        self.expected = expected
        self.backtrack = backtrack

    @classmethod
    def routine(cls, expected):
        def decorator(func):
            @functools.wraps(func)
            def wrapper(*args, **kwargs):
                return cls(functools.partial(func, *args, **kwargs), expected=expected)
            return wrapper
        return decorator

    def process(self, start):
        coro = self.consume()
        cur = start
        result = None
        try:
            while True:
                result, cur = coro.send(result).process(cur)
                if not self.backtrack:
                    start = cur
        except StopIteration as ret:
            if ret.value is not None:
                return ret.value, cur
            return None, start


class CurrentNode(Rule):
    def process(self, start):
        return start, start


async def cursor():
    if node := await CurrentNode():
        return node.head.span.start
    return node.value


async def expect(rule, *, expected=None):
    if (result := await rule) is not None:
        return result

    raise await ParserError.expected(expected or str(rule))
