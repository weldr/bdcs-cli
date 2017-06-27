#!/bin/bash
set -x

# Note: execute this file from the project root directory

bdcscli="./dist/build/bdcs-cli/bdcs-cli"

# When called without parameters returns Unknown Command
if [[ `$bdcscli` != "Unknown Command" ]]; then
    exit 1
fi

# --help option is not recognized
if [[ `$bdcscli --help 2>&1 | head -n 1` != "bdcs-cli: user error (unrecognized option \`--help'" ]]; then
    exit 1
fi

# -? returns version only
if [[ `$bdcscli -? | cut -f1 -d' '` != "bdcs-cli" ]]; then
    exit 1
fi
