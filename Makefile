.PHONY: all

all:
	alex src/Lexer.x
	happy src/Parser.x
	ghc -outputdir out -isrc --make src/Main.hs -o Main

cabal:
	cabal install alex happy
