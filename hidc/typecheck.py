from hidc.lexer import SourceCode
from hidc.parser import parse
from hidc.errors import CompilerError
from pprint import pprint
import sys

def main(filename):
    source = SourceCode.from_file(filename)
    try:
        pprint(parse(source).checked())
    except CompilerError as err:
        print(err.get_info(source), file=sys.stderr)

if __name__ == '__main__':
    main(sys.argv[1])
