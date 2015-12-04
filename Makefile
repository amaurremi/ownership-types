.PHONY: all

all:
	alex src/Lexer.x
	ghc -outputdir out -isrc --make src/Main.hs -o owni

compile:
	ghc -outputdir out -isrc --make src/Main.hs -o owni

cabal:
	cabal update
	cabal install cabal-install
	cabal install alex
