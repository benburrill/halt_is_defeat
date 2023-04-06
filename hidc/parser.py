from .tokens import Token
from .utils.lazylist import Cons, Nil
from .utils.scanner import Span, Cursor
from .errors import ParserError

import abc
from dataclasses import dataclass
from functools import wraps


class Rule(abc.ABC):
    def __str__(self):
        return getattr(self, 'expected', None) or type(self).__name__

    @abc.abstractmethod
    def process(self, start):
        pass


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


class Syntax(Rule):
    def __init__(self, func, *, expected=None):
        self.func = func
        self.expected = expected

    @classmethod
    def builder(cls, expected):
        def decorator(func):
            @wraps(func)
            def wrapper(*args, **kwargs):
                bound = lambda ps: func(ps, *args, **kwargs)
                return cls(bound, expected=expected)
            return wrapper
        return decorator

    def process(self, start):
        ps = Parser(start)
        result = self.func(ps)
        if result is not None:
            return result, ps.remaining
        return None, start


@dataclass
class Parser:
    remaining: Cons | Nil

    def expect(self, rule, *, expected=None):
        result, remaining = rule.process(self.remaining)
        if result is not None:
            self.remaining = remaining
            return result

        raise self.unexpected(expected or str(rule))

    def unexpected(self, expected):
        if not self.remaining:
            return ParserError(
                f'Expected {expected}, found end of file',
                self.cursor
            )

        return ParserError(
            f'Expected {expected}, '
            f'found unexpected token: {self.remaining.head.token}',
            self.remaining.head.span
        )

    def parse(self, rule):
        result, self.remaining = rule.process(self.remaining)
        return result

    def fork(self, rule):
        result, remaining = rule.process(self.remaining)
        return result, Parser(remaining)

    def next(self):
        node = self.remaining
        if node:
            self.remaining = node.tail
            return node.head
        return None

    def __bool__(self):
        return bool(self.remaining)

    @property
    def cursor(self):
        if self.remaining:
            return self.remaining.head.span.start

        # TODO: do better than this?
        # Ideally I'd like the end of the last token if there was one
        return Cursor(-1, -1)
