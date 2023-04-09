from hidc.lexer import SourceCode
from hidc.parser import parse
from pprint import pprint

def main(filename):
    # not actually very "pretty", but what can you do.
    pprint(parse(SourceCode.from_file(filename)))

if __name__ == '__main__':
    import sys
    main(sys.argv[1])
