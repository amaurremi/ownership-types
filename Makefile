.PHONY: all

all:
	ghc -outputdir out -isrc --make src/Main.hs -o Main

cabal:
	cabal install alex happy
