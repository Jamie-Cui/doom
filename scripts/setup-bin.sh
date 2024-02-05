#!/bin/bash

wget -c https://github.com/bazelbuild/buildtools/releases/download/v6.4.0/buildifier-darwin-amd64 || exit
mv buildifier-darwin-amd64 buildifier || exit
