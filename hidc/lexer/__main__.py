from hidc.lexer import lex, SourceCode

def main(filename):
    for tok in lex(SourceCode.from_file(filename)):
        print(tok)

if __name__ == '__main__':
    import sys
    main(sys.argv[1])
