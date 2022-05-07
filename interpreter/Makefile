.PHONY : all clean

all : full

full:
	-make parser
	-make interpreter

interpreter: src/*.hs
	-cd src && ghc zoya.hs -o ../interpreter

parser: src/grammar.ebnf
	-bnfc -d --functor -o src -m --functor src/grammar.ebnf
	-cd src && make

clean:
	-cd src && make clean
	-rm -f src/*.hi src/*.o src/*.log src/*.aux src/*.dvi
