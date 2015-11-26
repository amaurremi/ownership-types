.PHONY: all

all:
	alex src/Lexer.xsc
	ghc -outputdir out -isrc --make src/Main.hs -o Main

cabal:
	cabal install alex happy
