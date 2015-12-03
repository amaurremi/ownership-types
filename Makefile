.PHONY: all

all:
	alex src/Lexer.x
	# happy src/Parser.y
	ghc -outputdir out -isrc --make src/Main.hs -o Main

compile:
	ghc -outputdir out -isrc --make src/Main.hs -o Main

cabal:
	cabal install alex happy
