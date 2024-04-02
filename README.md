# doom.d

This is Jamie's private [doom emacs](https://github.com/doomemacs/doomemacs) config. This config has the following features
- remote cpp developing with tramp, with correctly configured *code-format (clang-format)*, *lsp (eglot + clangd)*, *bazel*
- latex acadamic paper writing, and note taking

**WARNING: This config is only designed for MacOS**.

Thirdparty libraries:
- [wondershaper](https://github.com/magnific0/wondershaper): Command-line utility for limiting an adapter's bandwidth
- [secretflow/devtools](https://github.com/secretflow/devtools): Secretflow's development tools

Recommended MacOS Apps:
- [skim](https://skim-app.sourceforge.io/): open-source PDF reader and note-taker for OS X
- [alacritty](https://alacritty.org/): cross-platform OpenGL terminal emulator
- [zotero](https://www.zotero.org/): free and easy-to-use biblography tool

## Download GNU/Emacs

Of course, you need to download Emacs and doom first. As in [doom's getting started guide](https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#with-homebrew) says,

> emacs-mac offers good integration with macOS, native emojis and better childframe support.

So, do the following steps. First, install presiquites with [Homebrew](https://brew.sh/).

```
# required dependencies
brew install git ripgrep
# optional dependencies
brew install coreutils fd
# Installs clang
xcode-select --install
For Emacs itself, these three formulas are the best options, ordered from most to least recommended for Doom (based on compatibility).
```

Then, install emacs-mac,

```
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules --with-native-comp
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs.app
```

## Other Questions

**Tex preview not working, emacs can not find 'latex' and 'dvipng' binaries.**
```
sudo ln -s /Library/TeX/texbin/latex /usr/local/bin
sudo ln -s /Library/TeX/texbin/dvipng /usr/local/bin
```
