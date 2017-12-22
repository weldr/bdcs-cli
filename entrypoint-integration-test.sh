#!/bin/bash

set -ex

cd /bdcs-cli/

# tests for bdcs-cli binary
./tests/test_binary.sh

# depsolve integration tests
# NOTE: relies on metadata.db already being built and made available
# to the API container if api is running externally !!!
export START_API_EXTERNALLY=1
./tests/test_depsolve.sh

# collect binary coverage
mkdir ./dist/hpc/vanilla/tix/bdcs-cli/
mv bdcs-cli.tix ./dist/hpc/vanilla/tix/bdcs-cli/
