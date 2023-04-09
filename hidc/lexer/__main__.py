from hidc.lexer import lex, SourceCode
from hidc.errors import CompilerError
import sys

def main(filename):
    source = SourceCode.from_file(filename)
    try:
        for tok in lex(source):
            print(tok)
    except CompilerError as err:
        print(err.get_info(source), file=sys.stderr)

if __name__ == '__main__':
    main(sys.argv[1])
