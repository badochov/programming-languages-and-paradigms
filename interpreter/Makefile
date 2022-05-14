.PHONY : all clean

all : full

GHC = ghc
GHC_FLAGS = #-Wall

full:
	-make parser
	-make interpreter

interpreter: src/zoya.hs src/Interpreter.hs src/TypeChecker.hs src/Grammar/Abs.hs
	-cd src && ${GHC} ${GHC_FLAGS} zoya.hs -o ../interpreter

parser: src/grammar.cf
	-bnfc -d --functor -o src -m src/grammar.cf
	-cd src && make
	-rm src/Grammar/Test.hs

clean:
	-cd src && make clean
	-rm -f src/*.hi src/*.o src/*.log src/*.aux src/*.dvi
