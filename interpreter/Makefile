.PHONY : all clean

all : full

GHC = ghc
GHC_FLAGS = #-Wall
BNFC_ENV_CHANGE = env PATH=$(PATH):/home/students/inf/PUBLIC/MRJP/bin/

full:
	-make parser
	-make prelude
	-make interpreter

interpreter: src/zoya.hs src/Interpreter.hs src/TypeChecker.hs src/Grammar/Abs.hs src/Common.hs src/ZoyaPrelude.hs
	-cd src && ${GHC} ${GHC_FLAGS} zoya.hs -o ../interpreter

src/ZoyaPrelude.hs: src/prelude/prelude.zoya build_prelude.py
	-./build_prelude.py

parser: src/grammar.cf
	-${BNFC_ENV_CHANGE} bnfc -d --functor -o src -m src/grammar.cf
	-cd src && make
	-rm src/Grammar/Test.hs

zip:
	-make clean
	-zip Hubert_Badocha.zip -r . -x tabelka-cech-fun2022.txt .vscode/\* interpreter
	-unzip Hubert_Badocha.zip -d Hubert_Badocha
	-rm Hubert_Badocha.zip
	-zip -r Hubert_Badocha.zip Hubert_Badocha
	-rm -rf ./Hubert_Badocha

clean:
	-cd src && make clean
	-rm -f src/*.hi src/*.o src/*.log src/*.aux src/*.dvi
	-rm Hubert_Badocha.zip
