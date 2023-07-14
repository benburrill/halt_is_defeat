from hidc.lexer import SourceCode
from hidc.parser import parse
from hidc.ast import typecheck
from hidc.codegen import CodeGen
from hidc.errors import CompilerError
from pprint import pprint
import argparse
import sys


hidc = argparse.ArgumentParser(
    description='hidc is compiler, Halt is Defeat',
    prog='hidc'
)

hidc.add_argument(
    'input',
    help='The input Halt is Defeat source file to compile'
)

hidc.add_argument(
    '-o', metavar='FILE', dest='output',
    help='write output to file [default: <input>.s]'
)

hidc.add_argument(
    '-m', dest='word_size',
    help='word size of the target machine in bits [default: 16 bit]',
    type=int, default=16
)

hidc.add_argument(
    '-s', dest='stack_size',
    help='size of stack available to the program [default: 500 words]',
    type=int, default=500
)

hidc.add_argument(
    '--dump-ast', help='output typechecked AST rather than compiling',
    action='store_true'
)

hidc.add_argument(
    '--unchecked', help='disable all runtime checks',
    action='store_true'
)


def main():
    args = hidc.parse_args()
    if args.word_size % 8 != 0 or args.word_size < 0:
        hidc.error('Word size must be divisible by 8')
        return 1

    try:
        source = SourceCode.from_file(args.input)
    except OSError as err:
        hidc.error(str(err))
        return 1

    try:
        ast = typecheck(parse(source))

        if args.dump_ast:
            pprint(ast)
            return 0

        # TODO: actually we'd rather not create the file until the we
        #  have compiled all functions and are ready to generate code.
        #  Otherwise compilation failure still produces a file -_-
        #  But currently generate calls make_funcs
        output = args.output if args.output is not None else args.input + '.s'
        with open(output, 'wb') as f:
            code_gen = CodeGen.from_program(ast, args.word_size // 8, args.stack_size, args.unchecked)
            for line in code_gen.generate():
                f.write(line)
                f.write(b'\n')

        if args.output is None:
            print(f'Sphinx assembly output written to {output}')
    except OSError as err:
        hidc.error(str(err))
        return 1
    except CompilerError as err:
        print(err.get_info(source), file=sys.stderr)
        return 1

if __name__ == '__main__':
    sys.exit(main())
