#!/bin/bash
set -x

# Note: execute this file from the project root directory

bdcscli="./dist/build/bdcs-cli/bdcs-cli"

# When called without parameters returns Unknown Command
if [[ $($bdcscli 2>&1 | head -n 1) != "Usage: bdcs-cli [OPTIONS...] commands..." ]]; then
    exit 1
fi

# --help option is not recognized
if [[ $($bdcscli --help 2>&1 | head -n 1) != "bdcs-cli: user error (unrecognized option \`--help'" ]]; then
    exit 1
fi

# -? returns version only
if [[ $($bdcscli -? | head -n 1 | cut -f1 -d' ') != "bdcs-cli" ]]; then
    exit 1
fi

# bdcs-cli help returns the help text
if [[ $($bdcscli help 2>&1 | wc -l) != "21" ]]; then
    exit 1
fi
