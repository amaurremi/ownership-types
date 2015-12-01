.PHONY: all

compile:
	ghc -outputdir out -isrc --make src/Main.hs -o Main

all:
	alex src/Lexer.x
	ghc -outputdir out -isrc --make src/Main.hs -o Main

cabal:
	cabal install alex happy
