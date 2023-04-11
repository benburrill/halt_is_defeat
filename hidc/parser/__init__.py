from . import rules
from .grammar import ps_program

from hidc.utils.lazylist import lazy_list
from hidc.errors import ParserError
from hidc.lexer import lex


def parse(source, rule=ps_program(), partial=False):
    if isinstance(rule, rules.Parser):
        rule = rules.Parser(rule.consume, backtrack=False)
    elif not isinstance(rule, rules.Rule):
        # coroutine passed directly, eg expect
        rule = rules.Parser((lambda r: lambda: r)(rule), backtrack=False)

    result, remaining = rule.process(lazy_list(lex(source)))

    if remaining and not partial:
        raise ParserError(
            f'Unprocessed token: {remaining.head.token}',
            remaining.head.span
        )

    return result
