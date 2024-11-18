#!/bin/bash

set -e
set -u

BASE_BIN_DIR="$(dirname $(readlink -f "$0"))/../bin"

# if exists, re-link
ln -s -f jd.sh $BASE_BIN_DIR/jd
ln -s -f jt.sh $BASE_BIN_DIR/jt

./setup-bin-clangd.sh
./setup-bin-plantuml.sh
./setup-bin-buildifier.sh
./setup-bin-latexindent.sh
