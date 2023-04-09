from hidc.lexer import SourceCode
from hidc.parser import parse, ps_program
from pprint import pprint

def main(filename):
    # not actually very "pretty", but what can you do.
    pprint(parse(ps_program(), SourceCode.from_file(filename)))

if __name__ == '__main__':
    import sys
    main(sys.argv[1])
