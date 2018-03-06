#!/bin/bash
# Note: execute this file from the project root directory

. /usr/share/beakerlib/beakerlib.sh

bdcscli="./dist/build/bdcs-cli/bdcs-cli"


rlJournalStart
    rlPhaseStartTest
        rlAssertEquals "When called without parameters prints usage & help" \
            "`$bdcscli 2>&1 | head -n 1`" \
            "Usage: bdcs-cli [OPTIONS...] commands..."

        rlAssertEquals "--help option is not recognized" \
            "`$bdcscli --help 2>&1 | head -n 1`" \
            "bdcs-cli: user error (unrecognized option \`--help'"

        rlAssertEquals "-? returns version only" \
            "`$bdcscli -? | head -n 1 | cut -f1 -d' '`" \
            "bdcs-cli"

        rlAssertEquals "bdcs-cli help returns the help text" \
            "`$bdcscli help 2>&1 | wc -l`" 29
    rlPhaseEnd

rlJournalEnd
rlJournalPrintText
