#!/bin/bash

set -ex

cd /bdcs-cli/

# don't produce XML journal b/c that needs Python
export BEAKERLIB_JOURNAL=0

# tests for bdcs-cli binary
./tests/test_binary.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1


# depsolve integration tests
# NOTE: relies on metadata.db already being built and made available
# to the API container if api is running externally !!!
export START_API_EXTERNALLY=1
./tests/test_depsolve.sh
grep RESULT_STRING /var/tmp/beakerlib-*/TestResults | grep -v PASS && exit 1

# collect binary coverage
mkdir ./dist/hpc/vanilla/tix/bdcs-cli/
mv bdcs-cli.tix ./dist/hpc/vanilla/tix/bdcs-cli/
