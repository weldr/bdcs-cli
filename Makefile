default: all

all: bdcs-cli

sandbox:
	[ -d .cabal-sandbox ] || cabal sandbox init

bdcs-cli: sandbox
	cabal update
	cabal install --dependencies-only
	cabal configure
	cabal build

clean:
	cabal clean

hlint: sandbox
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	[ -x .cabal-sandbox/bin/hlint ] || cabal install hlint
	cabal exec hlint .

tests: sandbox
	cabal install --dependencies-only --enable-tests
	cabal configure --enable-tests --enable-coverage
	cabal build
	cabal test
	./tests/test_binary.sh

.PHONY: sandbox bdcs-cli clean test hlint
