#!/bin/bash

set -e
set -u

# Get the base directory of
BASE_BIN_DIR="$(dirname $(readlink -f "$0"))/../bin"
echo "Found bin directory at: $BASE_BIN_DIR"

SHA256SUM=de53a9aba7b6995d3d90e5ca1c5d264c42d3e8757fe7701065cc7ecafcda98ca
FILENAME=buildifier
URL=https://github.com/bazelbuild/buildtools/releases/download/v6.3.2/buildifier-darwin-arm64

# ======================
# Bazel buildifier
# ======================

process() {
    cd $BASE_BIN_DIR
    wget -c $URL -O $FILENAME
    chmod +x $FILENAME
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

