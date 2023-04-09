from hidc.utils.lazylist import lazy_list
from hidc.errors import ParserError
from hidc.lexer import lex

from hidc.parser import rules
from hidc.parser.grammar import ps_program


def parse(rule, source, partial=False):
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
