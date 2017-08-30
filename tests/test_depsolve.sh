#!/bin/bash
set -x

# Note: execute this file from the project root directory

BDCS_CLI="./dist/build/bdcs-cli/bdcs-cli"

METADATA_DB="metadata.db"
[ -f "$METADATA_DB" ] || curl https://s3.amazonaws.com/weldr/metadata.db > "$METADATA_DB"

# start the backend API which provides depsolving
# later this will be replaces with the depsolver from bdcs library
sudo docker run -d --rm --name api -p 4000:4000 -v `pwd`:/mddb --security-opt label=disable welder/bdcs-api-rs:latest

python ./tests/test_depsolve.py -v
