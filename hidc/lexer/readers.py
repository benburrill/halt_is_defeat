import re
from hidc.lexer import tokens
from hidc.errors import LexerError


ignore = re.compile(r'\s*//.*|\s+')
def skip_whitespace(scan):
    while True:
        scan.match(ignore)
        if not scan.linebreak():
            break


byte_escape = re.compile(r'\\x([\da-fA-F]{2})')
def read_byte_escape(scan):
    if escape := scan.match(byte_escape):
        return bytes([int(escape, 16)])
    elif scan.exact('\\x'):
        raise LexerError('Invalid byte escape sequence', scan.cursor)


unicode_escape = re.compile(r'\\u\{([\da-fA-F]+)\}')
escape_codes = dict(['a\a', 'b\b', 'f\f', 'n\n', 'r\r', 't\t', '0\0', "''", '""', r'\\'])
def read_char_escape(scan):
    if escape := scan.match(unicode_escape):
        codepoint = int(escape, 16)
        try: return chr(codepoint)
        except ValueError:
            raise LexerError(
                f'Invalid unicode codepoint: {codepoint:X}',
                scan.cursor
            )
    elif scan.exact('\\u'):
        raise LexerError('Invalid unicode escape sequence', scan.cursor)
    elif scan.exact('\\'):
        char = scan.read(1)

        if char is None:
            raise LexerError.unhelpful(scan.cursor)

        if char not in escape_codes:
            raise LexerError(f'Invalid escape sequence: \\{char}', scan.cursor)

        return escape_codes[char]


def read_escape_bytes(scan, encoding):
    if escaped := read_byte_escape(scan):
        return escaped
    elif escaped := read_char_escape(scan):
        try: return escaped.encode(encoding)
        except UnicodeEncodeError as e:
            raise LexerError(str(e), scan.cursor)

def read_char_token(scan):
    if not scan.exact("'"):
        return

    if scan.exact("'"):
        raise LexerError.expected(scan.cursor, need='character')

    if not (byte := read_escape_bytes(scan, 'utf-8')):
        if char := scan.read(1):
            byte = char.encode('utf-8')
        else:
            raise LexerError('Unclosed character literal', scan.cursor)

    if not scan.exact("'"):
        raise LexerError.expected(scan.cursor, need="'")

    if len(byte) != 1:
        # I might consider instead rejecting \x sequences in character
        # literals and ONLY allow unicode, but I can't really have both.
        # However, since strings are actually just UTF-8 byte strings
        # and I don't have a true character data type (only byte),
        # there's not much sense in having unicode char literals.
        raise LexerError(
            'Unicode is not allowed in character literals, unless '
            'encodable as a single UTF-8 byte',
            scan.cursor
        )

    return tokens.CharToken(byte[0])


string_text = re.compile(r'[^\\"]+')
def read_string_token(scan):
    if not scan.exact('"'):
        return

    result = bytearray()
    while True:
        if text := scan.match(string_text):
            result += text.encode('utf-8')
        if escaped := read_escape_bytes(scan, 'utf-8'):
            result += escaped
        elif scan.exact('"'):
            return tokens.StringToken(bytes(result))
        else:
            raise LexerError('Unclosed string literal', scan.cursor)


hex_literal = re.compile(r'0x(?:[\da-fA-F]_?)*[\da-fA-F]')
oct_literal = re.compile(r'0o(?:[0-7]_?)*[0-7]')
bin_literal = re.compile(r'0b(?:[01]_?)*[01]')
dec_literal = re.compile(r'(?:\d_?)*\d')
def read_int_token(scan):
    if lit := scan.match(hex_literal):
        return tokens.IntToken(int(lit, 16))
    elif lit := scan.match(oct_literal):
        return tokens.IntToken(int(lit, 8))
    elif lit := scan.match(bin_literal):
        return tokens.IntToken(int(lit, 2))
    elif lit := scan.match(dec_literal):
        return tokens.IntToken(int(lit, 10))

ident_pattern = re.compile(r'[a-zA-Z_]\w*')
keyword_tokens = {
    str(tok): tok for tok in tokens.enum_tokens
    if ident_pattern.fullmatch(str(tok))
}

def read_ident_or_keyword_token(scan):
    if scan.exact('@'):
        flavor = tokens.Flavor.YOU
    elif scan.exact('!'):
        flavor = tokens.Flavor.DEFEAT
    elif ident := scan.match(ident_pattern):
        if ident in keyword_tokens:
            return keyword_tokens[ident]
        return tokens.Ident(ident)
    else:
        return None

    if ident := scan.match(ident_pattern):
        if ident not in keyword_tokens:
            return tokens.Ident(ident, flavor)

    raise LexerError(f'Invalid {flavor.name} identifier', scan.cursor)


symbol_tokens = sorted(
    (tok for tok in tokens.enum_tokens
     if not ident_pattern.fullmatch(str(tok))),
    key=lambda tok: len(str(tok)), reverse=True
)

def read_symbol_token(scan):
    for symbol in symbol_tokens:
        if scan.exact(str(symbol)):
            return symbol
