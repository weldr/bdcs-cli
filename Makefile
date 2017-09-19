ORG_NAME=welder

ifeq ($(API_CONTAINER_RUNNING),skip-check)
else
    API_CONTAINER_RUNNING=$(shell sudo docker ps | grep -c api)
endif

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

test-in-docker: Dockerfile.build
	sudo docker network create welder
	# download metadata and run the API backend which provides depsolving
	[ -f "metadata.db" ] || curl https://s3.amazonaws.com/weldr/metadata.db > metadata.db
	[ "$(API_CONTAINER_RUNNING)" == "1" ] || sudo docker run -d --rm --name api -p 4000:4000 -v `pwd`:/mddb --security-opt label=disable --network welder welder/bdcs-api-rs:latest

	sudo docker build -t $(ORG_NAME)/bdcs-cli:latest -f $< --cache-from $(ORG_NAME)/bdcs-cli:latest .
	sudo docker build -t $(ORG_NAME)/bdcs-cli-integration-test:latest -f Dockerfile.integration-test --network welder .
	sudo docker stop api
	sudo docker network remove welder

.PHONY: sandbox bdcs-cli clean test hlint
