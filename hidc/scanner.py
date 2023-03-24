from dataclasses import dataclass
from collections.abc import Sequence
from functools import wraps
import typing as ty


@dataclass
class SourceCode(Sequence):
    filename: str
    lines: Sequence[str]

    @classmethod
    def from_file(cls, filename):
        with open(filename) as file:
            return cls(filename, [line.removesuffix('\n') for line in file])

    @classmethod
    def from_string(cls, string, filename='<string>'):
        return cls(filename, string.split('\n'))

    def __getitem__(self, item):
        return self.lines[item]

    def __len__(self):
        return len(self.lines)

    def __repr__(self):
        return f'SourceCode.from_file({self.filename!r})'

@dataclass
class Scanner:
    source: SourceCode
    line: int = 0
    col: int = 0

    # TODO: make more things to work with newlines where possible?
    # I don't want newlines to be allowed in any of my tokens, so it
    # doesn't really matter.
    def exact(self, string):
        length = len(string)
        curline = self.source[self.line]
        if curline[self.col:self.col+length] == string:
            self.col += length
            return True
        return False

    def match(self, pat):
        mo = pat.match(self.source[self.line], self.col)
        if mo is not None:
            self.col = mo.end()
            groups = mo.groups('')
            if not groups:
                return mo.group()
            elif len(groups) == 1:
                return groups[0]
            return groups
        return None

    def read(self, count):
        string = self.source[self.line][self.col:self.col+count]
        if len(string) == count:
            self.col += count
            return string
        return None

    def mark(self):
        return Marker(self, self.line, self.col)

    @property
    def eol(self):
        return self.col >= len(self.source[self.line])

    def linebreak(self):
        if self.eol and self:
            self.line += 1
            self.col = 0
            return True
        return False

    def __bool__(self):
        # Is there any more to read?
        return not (self.eol and self.line >= (len(self.source) - 1))

    def __repr__(self):
        return f'<Scanner L{self.line+1} {self.source[self.line][self.col:]!r}>'


@dataclass
class Marker:
    scan: Scanner
    line: int
    col: int

    def update(self):
        self.line = self.scan.line
        self.col = self.scan.col

    def restore(self):
        self.scan.line = self.line
        self.scan.col = self.col

    # I don't think I'm actually going to use this as a context manager,
    # but it seems like a cool idea regardless
    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.restore()

    def collect(self, item):
        old_line, old_col = self.line, self.col
        self.update()
        return Span(
            item, self.scan.source,
            (old_line, old_col),
            (self.line, self.col)
        )


T = ty.TypeVar('T')
@dataclass
class Span(ty.Generic[T]):
    item: T
    source: SourceCode
    start: tuple[int, int]
    end: tuple[int, int]

    def __repr__(self):
        return f'<Span {self.item!r} in {self.source.filename} from {self.start}::{self.end}>'

    # Idea here is some sort of decorator that takes stuff which may or
    # may not wrapped in spans and wraps the return value in span that
    # encompasses all the span-wrapped items.
    # @classmethod
    # def spanned(cls, func):
    #     @wraps(func)
    #     def wrapper(*args):
    #         new_args = [arg.item if isinstance(arg, Span) else arg
    #                     for arg in args]

