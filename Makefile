.PHONY: all

all:
	alex src/Lexer.x
	ghc -outputdir out -isrc --make src/Main.hs -o Main

compile:
	ghc -outputdir out -isrc --make src/Main.hs -o Main

cabal:
	cabal update
	cabal install cabal-install
	cabal install alex
