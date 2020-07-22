configure:
	cabal configure

build:
	cabal build

clean:
	cabal clean

ghcid:
	ghcid -c "cabal repl"
