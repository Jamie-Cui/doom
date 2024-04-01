#!/usr/bin/env emacs --script#!/bin/sh
":"; exec emacs --script "$0" "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq private-doom-bin-dir (concat default-directory "../bin/"))

(message "Downloading bazel buildifier ... ")
(url-copy-file "https://github.com/bazelbuild/buildtools/releases/download/v6.4.0/buildifier-darwin-amd64" (concat private-doom-bin-dir "buildifier") t)

(message "Downloading clangd ... ")
(url-copy-file "https://github.com/clangd/clangd/releases/download/17.0.3/clangd-mac-17.0.3.zip" (concat private-doom-bin-dir "clangd.zip") t)

(shell-command "cd ../bin && unzip -o -q clangd.zip && mv clangd_17.0.3/bin/clangd . && rm clangd.zip && rm -rf clangd_17.0.3")
(shell-command "cd ../bin && chmod +x buildifier")
