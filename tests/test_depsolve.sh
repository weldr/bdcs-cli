#!/bin/bash
# Note: execute this file from the project root directory

. /usr/share/beakerlib/beakerlib.sh

METADATA_DB="metadata.db"

rlJournalStart
    rlPhaseStartSetup
        # start the backend API which provides depsolving only if not running already
        # later this will be replaced with the depsolver from bdcs library
        if [ -z "$START_API_EXTERNALLY" ]; then
            sudo docker ps | grep api
            if [ $? -ne 0 ]; then
                [ -f "$METADATA_DB" ] || ./tests/bin/import-metadata
                sudo docker run -d --rm --name api -p 4000:4000 -v `pwd`:/mddb -v `pwd`/examples/recipes:/recipes --security-opt label=disable welder/bdcs-api-img:latest
            fi
        fi
    rlPhaseEnd

    rlPhaseStartTest
        rlRun "python3 ./tests/test_depsolve.py -v"
    rlPhaseEnd

rlJournalEnd
rlJournalPrintText
