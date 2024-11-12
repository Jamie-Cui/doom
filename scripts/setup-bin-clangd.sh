#!/bin/bash

set -e
set -u

# Get the base directory of
BASE_BIN_DIR="$(dirname $(readlink -f "$0"))/../bin"
echo "Found bin directory at: $BASE_BIN_DIR"

SHA256SUM=2bf1294c426a977613e82c8665f12cb55660a53a5093645c3f5ee902d745509f
FILENAME=clangd
URL=https://github.com/clangd/clangd/releases/download/18.1.3/clangd-mac-18.1.3.zip

# ======================
# Bazel buildifier
# ======================

process() {
    cd $BASE_BIN_DIR
    wget -c $URL -O $FILENAME.zip
    unzip $FILENAME.zip
    mv clangd_18.1.3/bin/clangd .
    chmod +x $FILENAME
    rm $FILENAME.zip
    rm -rf clangd_18.1.3
}

if [[ -e $BASE_BIN_DIR/$FILENAME ]]
then
    if echo "$SHA256SUM $BASE_BIN_DIR/$FILENAME" | sha256sum --check --status
    then
        echo "$FILENAME already exists, exit with success"
    else
        echo "$FILENAME already exists, sha256sum check failed, downloading ... "
        process
    fi
else
    echo "File not file, downloading ... "
    process
fi
