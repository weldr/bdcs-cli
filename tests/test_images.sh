#!/bin/bash

### NOTE: should be executed from the project root directory!!!
###
### Produces output images and runs basic functional tests against them

# exits on error but doesn't clean up
set -e

EXPORT="./export"
IMPORT="./import"
SCHEMA="./schema.sql"
BDCS_CLI="./bdcs-cli"

METADATA_DB="metadata.db"
CENTOS_REPO="centos.repo/"


# download precompiled binaries if not available
[ -f "./dist/build/bdcs-cli/bdcs-cli" ] && cp ./dist/build/bdcs-cli/bdcs-cli .
[ -f "$BDCS_CLI" ] || curl -o "$BDCS_CLI" https://s3.amazonaws.com/weldr/bdcs-cli && chmod a+x "$BDCS_CLI"
[ -f "$EXPORT" ] || curl -o "$EXPORT" https://s3.amazonaws.com/weldr/export&& chmod a+x "$EXPORT"


# create the database if it doesn't exist
if [ ! -f "$METADATA_DB" ]; then
    [ -f "$IMPORT" ] || curl -o "$IMPORT" https://s3.amazonaws.com/weldr/import && chmod a+x "$IMPORT"
    [ -f "$SCHEMA" ] || curl -o "$SCHEMA" https://raw.githubusercontent.com/weldr/bdcs/master/schema.sql
    sqlite3 "$METADATA_DB" < "$SCHEMA"

    # we need a metadata.db and ostree repository to produce the images
    # build them here because the full CentOS metadata.db and ostree repo
    # are too large to download every time across the network
    ### NOTE: list all dependencies that are required to resolve the recipe
    ### obtained via `bdcs-cli recipes depsolve` **BUT** removed epoch from file names!

    ## for http-server
    for RPM in acl-2.2.51-12.el7.x86_64 apr-1.4.8-3.el7.x86_64 apr-util-1.5.2-6.el7.x86_64 \
            audit-libs-2.7.6-3.el7.x86_64 basesystem-10.0-7.el7.centos.noarch \
            bash-4.2.46-28.el7.x86_64 binutils-2.25.1-31.base.el7.x86_64 \
            bzip2-libs-1.0.6-13.el7.x86_64 ca-certificates-2017.2.14-71.el7.noarch \
            centos-logos-70.0.6-3.el7.centos.noarch centos-release-7-4.1708.el7.centos.x86_64 \
            chkconfig-1.7.4-1.el7.x86_64 coreutils-8.22-18.el7.x86_64 cpio-2.11-24.el7.x86_64 \
            cracklib-2.9.0-11.el7.x86_64 cracklib-dicts-2.9.0-11.el7.x86_64 \
            cryptsetup-libs-1.7.4-3.el7.x86_64 curl-7.29.0-42.el7.x86_64 \
            cyrus-sasl-lib-2.1.26-21.el7.x86_64 dbus-1.6.12-17.el7.x86_64 \
            dbus-libs-1.6.12-17.el7.x86_64 device-mapper-1.02.140-8.el7.x86_64 \
            device-mapper-libs-1.02.140-8.el7.x86_64 diffutils-3.3-4.el7.x86_64 \
            dracut-033-502.el7.x86_64 elfutils-libelf-0.168-8.el7.x86_64 \
            elfutils-libs-0.168-8.el7.x86_64 elfutils-default-yama-scope-0.168-8.el7.noarch \
            expat-2.1.0-10.el7_3.x86_64 \
            filesystem-3.2-21.el7.x86_64 findutils-4.5.11-5.el7.x86_64 \
            fipscheck-1.4.1-6.el7.x86_64 fipscheck-lib-1.4.1-6.el7.x86_64 \
            gawk-4.0.2-4.el7_3.1.x86_64 glib2-2.50.3-3.el7.x86_64 glibc-2.17-196.el7.x86_64 \
            glibc-common-2.17-196.el7.x86_64 gmp-6.0.0-15.el7.x86_64 grep-2.20-3.el7.x86_64 \
            gzip-1.5-9.el7.x86_64 hardlink-1.0-19.el7.x86_64 httpd-2.4.6-67.el7.centos.x86_64 \
            httpd-tools-2.4.6-67.el7.centos.x86_64 info-5.1-4.el7.x86_64 \
            keyutils-libs-1.5.8-3.el7.x86_64 kmod-20-15.el7.x86_64 \
            kmod-libs-20-15.el7.x86_64 kpartx-0.4.9-111.el7.x86_64 \
            krb5-libs-1.15.1-8.el7.x86_64 libacl-2.2.51-12.el7.x86_64 \
            libattr-2.4.46-12.el7.x86_64 libblkid-2.23.2-43.el7.x86_64 \
            libcap-2.22-9.el7.x86_64 libcap-ng-0.7.5-4.el7.x86_64 \
            libcom_err-1.42.9-10.el7.x86_64 libcurl-7.29.0-42.el7.x86_64 \
            libdb-5.3.21-20.el7.x86_64 libdb-utils-5.3.21-20.el7.x86_64 \
            libedit-3.0-12.20121213cvs.el7.x86_64 libevent-2.0.21-4.el7.x86_64 \
            libffi-3.0.13-18.el7.x86_64 libgcc-4.8.5-16.el7.x86_64 \
            libgcrypt-1.5.3-14.el7.x86_64 libgpg-error-1.12-3.el7.x86_64 \
            libidn-1.28-4.el7.x86_64 libmount-2.23.2-43.el7.x86_64 \
            libpwquality-1.2.3-4.el7.x86_64 libselinux-2.5-11.el7.x86_64 \
            libsemanage-2.5-8.el7.x86_64 libsepol-2.5-6.el7.x86_64 \
            libssh2-1.4.3-10.el7_2.1.x86_64 libstdc++-4.8.5-16.el7.x86_64 \
            libtasn1-4.10-1.el7.x86_64 libuser-0.60-7.el7_1.x86_64 \
            libutempter-1.1.6-4.el7.x86_64 libuuid-2.23.2-43.el7.x86_64 \
            libverto-0.2.5-4.el7.x86_64 libxml2-2.9.1-6.el7_2.3.x86_64 \
            libzip-0.10.1-8.el7.x86_64 lua-5.1.4-15.el7.x86_64 mailcap-2.1.41-2.el7.noarch \
            make-3.82-23.el7.x86_64 mariadb-libs-5.5.56-2.el7.x86_64 \
            mod_auth_kerb-5.4-28.el7.x86_64 mod_ssl-2.4.6-67.el7.centos.x86_64 \
            ncurses-5.9-13.20130511.el7.x86_64 ncurses-base-5.9-13.20130511.el7.noarch \
            ncurses-libs-5.9-13.20130511.el7.x86_64 nspr-4.13.1-1.0.el7_3.x86_64 \
            nss-pem-1.0.3-4.el7.x86_64 \
            nss-3.28.4-8.el7.x86_64 nss-softokn-3.28.3-6.el7.x86_64 \
            nss-softokn-freebl-3.28.3-6.el7.x86_64 nss-sysinit-3.28.4-8.el7.x86_64 \
            nss-tools-3.28.4-8.el7.x86_64 nss-util-3.28.4-3.el7.x86_64 \
            openldap-2.4.44-5.el7.x86_64 openssh-7.4p1-11.el7.x86_64 \
            openssh-server-7.4p1-11.el7.x86_64 openssl-1.0.2k-8.el7.x86_64 \
            openssl-libs-1.0.2k-8.el7.x86_64 p11-kit-0.23.5-3.el7.x86_64 \
            p11-kit-trust-0.23.5-3.el7.x86_64 pam-1.1.8-18.el7.x86_64 pcre-8.32-17.el7.x86_64 \
            php-5.4.16-42.el7.x86_64 php-cli-5.4.16-42.el7.x86_64 php-common-5.4.16-42.el7.x86_64 \
            php-mysql-5.4.16-42.el7.x86_64 php-pdo-5.4.16-42.el7.x86_64 pkgconfig-0.27.1-4.el7.x86_64 \
            popt-1.13-16.el7.x86_64 procps-ng-3.3.10-16.el7.x86_64 qrencode-libs-3.4.1-3.el7.x86_64 \
            readline-6.2-10.el7.x86_64 rpm-4.11.3-25.el7.x86_64 rpm-libs-4.11.3-25.el7.x86_64 \
            rsync-3.0.9-18.el7.x86_64 sed-4.2.2-5.el7.x86_64 setup-2.8.71-7.el7.noarch \
            shadow-utils-4.1.5.1-24.el7.x86_64 shared-mime-info-1.8-3.el7.x86_64 \
            sqlite-3.7.17-8.el7.x86_64 systemd-219-42.el7.x86_64 systemd-libs-219-42.el7.x86_64 \
            tar-1.26-32.el7.x86_64 tcp_wrappers-libs-7.6-77.el7.x86_64 tmux-1.8-4.el7.x86_64 \
            tzdata-2017b-1.el7.noarch ustr-1.0.4-16.el7.x86_64 util-linux-2.23.2-43.el7.x86_64 \
            xz-5.2.2-1.el7.x86_64 xz-libs-5.2.2-1.el7.x86_64 zlib-1.2.7-17.el7.x86_64 \
                                        \
            atlas-3.10.1-12.el7.x86_64 \
            basesystem-10.0-7.el7.centos.noarch \
            blas-3.4.2-8.el7.x86_64 \
            bzip2-libs-1.0.6-13.el7.x86_64 \
            filesystem-3.2-21.el7.x86_64 \
            gdbm-1.10-8.el7.x86_64 \
            info-5.1-4.el7.x86_64 \
            keyutils-libs-1.5.8-3.el7.x86_64 \
            lapack-3.4.2-8.el7.x86_64 \
            libacl-2.2.51-12.el7.x86_64 \
            libattr-2.4.46-12.el7.x86_64 \
            libffi-3.0.13-18.el7.x86_64 \
            libgfortran-4.8.5-16.el7.x86_64 \
            libquadmath-4.8.5-16.el7.x86_64 \
            libsepol-2.5-6.el7.x86_64 \
            libverto-0.2.5-4.el7.x86_64 \
            numpy-1.7.1-11.el7.x86_64 \
            popt-1.13-16.el7.x86_64 \
            python-2.7.5-58.el7.x86_64 \
            python-backports-1.0-8.el7.x86_64 \
            python-backports-ssl_match_hostname-3.4.0.2-4.el7.noarch \
            python-libs-2.7.5-58.el7.x86_64 \
            python-nose-1.3.7-1.el7.noarch \
            python-setuptools-0.9.8-7.el7.noarch \
            sed-4.2.2-5.el7.x86_64 \
            setup-2.8.71-7.el7.noarch; do
        "$IMPORT" "$METADATA_DB" "$CENTOS_REPO" "http://mirror.centos.org/centos/7/os/x86_64/Packages/$RPM.rpm"
    done
fi


# start the backend API which provides depsolving only if not running already
# later this will be replaced with the depsolver from bdcs library
if [ -z "$START_API_EXTERNALLY" ]; then
    if [ `sudo docker ps | grep -c bdcs_api` -lt 1 ]; then
        sudo docker run -d --rm --name bdcs_api -p 4000:4000 -v `pwd`:/mddb --security-opt label=disable welder/bdcs-api-rs:latest
        sleep 5
    fi
else
    BDCS_CLI="$BDCS_CLI --url http://api:4000/"
fi

for RECIPE in http-server; do
    # crate a tar image
    PATH=$PATH:. $BDCS_CLI -m $METADATA_DB -r $CENTOS_REPO compose tar $RECIPE
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
