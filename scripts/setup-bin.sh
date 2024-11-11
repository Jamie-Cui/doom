#!/bin/bash

set -e
set -u

# Get the base directory of
BASE_BIN_DIR="$(dirname $(readlink -f "$0"))/../bin"
echo "Found bin directory at: $BASE_BIN_DIR"

# ======================
# Bazel buildifier
# ======================

cd $BASE_BIN_DIR
wget -c https://github.com/bazelbuild/buildtools/releases/download/v6.3.2/buildifier-darwin-arm64 -O buildifier
chmod +x buildifier

# ======================
# Clangd
# ======================

cd $BASE_BIN_DIR
wget -c https://github.com/clangd/clangd/releases/download/18.1.3/clangd-mac-18.1.3.zip -O clangd.zip

# ======================
# plantuml.jar (for uml)
# ======================

cd $BASE_BIN_DIR
wget -c https://github.com/plantuml/plantuml/releases/download/v1.2024.7/plantuml-1.2024.7.jar -O plantuml.jar
