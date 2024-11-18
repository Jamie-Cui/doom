#!/bin/bash

set -e
set -u

# Get the base directory of
BASE_BIN_DIR="$(dirname $(readlink -f "$0"))/../bin"
echo "Found bin directory at: $BASE_BIN_DIR"

SHA256SUM=2d237a9368e15e39fedbd16f348c33e083e270201a1a5e6060e92576feb3fe14
FILENAME=latexindent
URL=https://github.com/cmhughes/latexindent.pl/releases/download/V3.24.4/latexindent-linux

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
