default:
	cabal configure --enable-tests
	cabal build

clean:
	cabal clean
