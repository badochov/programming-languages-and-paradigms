.PHONY : all clean

all : full

full:
	-make parser
	-make interpreter

interpreter: src/zoya.hs src/Interpreter.hs src/Grammar/Abs.hs
	-cd src && ghc -Wall zoya.hs -o ../interpreter

parser: src/grammar.ebnf
	-bnfc -d --functor -o src -m --functor src/grammar.ebnf
	-cd src && make
	-rm src/Grammar/Test.hs

clean:
	-cd src && make clean
	-rm -f src/*.hi src/*.o src/*.log src/*.aux src/*.dvi
