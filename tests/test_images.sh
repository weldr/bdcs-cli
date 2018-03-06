#!/bin/bash

### NOTE: should be executed from the project root directory!!!
###
### Produces output images and runs basic functional tests against them

# exits on error but doesn't clean up
set -e

EXPORT="./bdcs-export"
BDCS_CLI="./bdcs-cli"

METADATA_DB="metadata.db"
METADATA_REPO="cs.repo/"


# download precompiled binaries if not available
[ -f "./dist/build/bdcs-cli/bdcs-cli" ] && cp ./dist/build/bdcs-cli/bdcs-cli .
[ -f "$BDCS_CLI" ] || curl -o "$BDCS_CLI" https://s3.amazonaws.com/weldr/bdcs-cli && chmod a+x "$BDCS_CLI"
[ -f "$EXPORT" ] || curl -o "$EXPORT" https://s3.amazonaws.com/weldr/bdcs-export && chmod a+x "$EXPORT"


# create the database if it doesn't exist
[ -f "$METADATA_DB" ] || ./tests/bin/import-metadata $METADATA_REPO


# start the backend API which provides depsolving only if not running already
# later this will be replaced with the depsolver from bdcs library
if [ -z "$START_API_EXTERNALLY" ]; then
    if [ `sudo docker ps | grep -c bdcs_api` -lt 1 ]; then
        sudo docker run -d --rm --name bdcs_api -p 4000:4000 -v `pwd`:/mddb --security-opt label=disable welder/bdcs-api-img:latest
        sleep 5
    fi
else
    BDCS_CLI="$BDCS_CLI --url http://api:4000/"
fi

for RECIPE in http-server; do
    # crate a tar image
    PATH=$PATH:. $BDCS_CLI -m $METADATA_DB -r $METADATA_REPO compose tar $RECIPE
    TAR_FILE=`realpath $RECIPE.tar`

    # work around Trello #413
    # https://trello.com/c/U4BYJ5IV/413-cant-import-exported-tar-images-into-docker
    pushd `mktemp -d`
    sudo tar xf $TAR_FILE
    sudo tar cf $TAR_FILE *
    # don't leave files behind
    sudo rm -rf *
    popd

    # import the tar contents into docker
    cat $TAR_FILE | sudo docker import - $RECIPE

    # Run bash to verify that we can start the container and leave it running
    # in the background so we can actually test something
    sudo docker run -d --rm --name $RECIPE -it --entrypoint /bin/bash $RECIPE

    # install beakerlib
#TODO: update to upstream URL once beakerlib/beakerlib#11 is merged
    curl -k -o- https://raw.githubusercontent.com/atodorov/beakerlib/web-install/install.sh | sudo docker exec -i $RECIPE /bin/bash

    # execute the tests. in case of failure will exit from this script
    for TS in tests/images/$RECIPE.*.test; do
        cat $TS | sudo docker exec -i $RECIPE /bin/bash
    done

    # stop the container after all testing has been completed
    # NOTE: will not cleanup on error b/c we'll exit before that
    sudo docker stop $RECIPE

    sleep 5
    sudo docker rmi $RECIPE
done

if [ -z "$START_API_EXTERNALLY" ]; then
    sudo docker stop bdcs_api
fi
