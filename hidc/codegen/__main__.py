from hidc.lexer import SourceCode
from hidc.parser import parse
from hidc.codegen import CodeGen
from hidc.errors import CompilerError
from pprint import pprint
import sys

def main(filename):
    source = SourceCode.from_file(filename)
    out = filename + '.s'
    try:
        with open(out, 'wb') as f:
            for line in CodeGen.from_program(parse(source).checked(), 2).generate():
                f.write(line)
                f.write(b'\n')
    except CompilerError as err:
        print(err.get_info(source), file=sys.stderr)

if __name__ == '__main__':
    main(sys.argv[1])
