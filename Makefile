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
	sudo docker network inspect welder || sudo docker network create welder

	# building the docker image executes all unit tests
	if [ -n "$$TRAVIS" ]; then \
	    sudo docker build -t $(ORG_NAME)/bdcs-cli:latest -f $< --cache-from $(ORG_NAME)/bdcs-cli:latest . ; \
	else \
	    sudo docker build -t $(ORG_NAME)/bdcs-cli:latest -f $< .; \
	fi;

	# copy metadata.db out of the container so we can pass it to
	# the API container using the volume mount below!
	sudo docker create --name metadata-cont $(ORG_NAME)/bdcs-cli /bin/bash
	sudo docker cp metadata-cont:/bdcs-cli/metadata.db .
	sudo docker rm metadata-cont

	# run the API backend which provides depsolving
	[ "$(API_CONTAINER_RUNNING)" == "1" ] || sudo docker run -d --rm --name api -p 4000:4000 -v `pwd`:/mddb --security-opt label=disable --network welder welder/bdcs-api-rs:latest

	# running the bdcs-cli image executes all integration tests
	sudo docker run --name tests --network welder $(ORG_NAME)/bdcs-cli:latest
	sudo docker stop api
	sudo docker network remove welder

test-images:
	./tests/test_images.sh

.PHONY: sandbox bdcs-cli clean test hlint
