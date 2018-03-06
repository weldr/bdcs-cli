ORG_NAME=welder

ifeq ($(API_CONTAINER_RUNNING),skip-check)
else
    API_CONTAINER_RUNNING=$(shell sudo docker ps | grep -c api)
endif

default: all

all: bdcs-cli

sandbox:
	cabal update
	cabal sandbox init

bdcs-cli: sandbox
	cabal install --dependencies-only
	cabal configure
	cabal build

clean:
	cabal clean

hlint: sandbox
	[ -x .cabal-sandbox/bin/happy ] || cabal install happy
	[ -x .cabal-sandbox/bin/hlint ] || cabal install hlint
	.cabal-sandbox/bin/hlint .

tests: sandbox
	# TODO: shouldn't we install dependencies from RPMs instead ?
	cabal install --dependencies-only --enable-tests
	cabal configure --enable-tests --enable-coverage --ghc-option=-DTEST
	cabal build
	cabal test --show-details=always
	./tests/test_binary.sh

test-in-docker: Dockerfile.build
	sudo docker network inspect welder || sudo docker network create welder

	# run the API backend which provides depsolving
	# metdata.db will be created after all unit tests pass
	[ "$(API_CONTAINER_RUNNING)" == "1" ] || sudo docker run -d --name api -p 4000:4000 -v `pwd`:/mddb --security-opt label=disable --network welder welder/bdcs-api-img:latest

	# building the docker image and execute the tests
	sudo docker build -t $(ORG_NAME)/bdcs-cli:latest -f $< .
	sudo docker run --rm --network welder --security-opt label=disable -v `pwd`:/bdcs-cli/ welder/bdcs-cli
	sudo docker stop api
	sudo docker rm api
	sudo docker network remove welder


ci: test-in-docker

test-images:
	./tests/test_images.sh

ci_after_success:
	sudo docker run --rm --security-opt label=disable -v `pwd`:/bdcs-cli/ \
	        --env "TRAVIS=$$TRAVIS" --env "TRAVIS_JOB_ID=$$TRAVIS_JOB_ID" \
	        --entrypoint /usr/bin/make welder/bdcs-cli coveralls

coveralls: sandbox
	[ -x .cabal-sandbox/bin/hpc-coveralls ] || cabal install hpc-coveralls
	.cabal-sandbox/bin/hpc-coveralls --display-report test-bdcs bdcs

.PHONY: sandbox bdcs-cli clean test hlint
