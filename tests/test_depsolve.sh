#!/bin/bash
set -x

# Note: execute this file from the project root directory

BDCS_CLI="./dist/build/bdcs-cli/bdcs-cli"

METADATA_DB="metadata.db"

# start the backend API which provides depsolving only if not running already
# later this will be replaced with the depsolver from bdcs library
if [ -z "$START_API_EXTERNALLY" ]; then
    sudo docker ps | grep api
    if [ $? -ne 0 ]; then
        [ -f "$METADATA_DB" ] || curl https://s3.amazonaws.com/weldr/metadata.db > "$METADATA_DB"
        sudo docker run -d --rm --name api -p 4000:4000 -v `pwd`:/mddb --security-opt label=disable welder/bdcs-api-rs:latest
    fi
fi

RUNNER="python"
rpm -q python-nose-parameterized >/dev/null
if [ $? -eq 0 ]; then
    RUNNER="nosetests"
fi
$RUNNER ./tests/test_depsolve.py -v
