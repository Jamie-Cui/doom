# doom.d

This is Jamie's private doom emacs config. 
- [Doom Emacs's Getting Started Guide](https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org)

Thirdparty libraries:
- [wondershaper](https://github.com/magnific0/wondershaper): Command-line utility for limiting an adapter's bandwidth
- [secretflow/devtools](https://github.com/secretflow/devtools): Secretflow's development tools

## MacOS download GNU/Emacs

With Homebrew
First, Doom’s dependencies:
```
# required dependencies
brew install git ripgrep
# optional dependencies
brew install coreutils fd
# Installs clang
xcode-select --install
For Emacs itself, these three formulas are the best options, ordered from most to least recommended for Doom (based on compatibility).
```

**emacs-mac**

It offers good integration with macOS, native emojis and better childframe support.
```
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules --with-native-comp
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs.app
```

## Repo Layouts

```
.
├── README.md
├── bin
│   ├── buildifier
│   ├── refresh-compile-commands
│   └── setup-bin.sh
├── config.el
├── custom.el
├── data
├── init.el
├── packages.el
├── scripts
├── snippets
├── thirdparty
│   ├── devtools
│   └── wondershaper
└── tweaks
    ├── cppdev.el
    ├── define.el
    ├── editor.el
    ├── org.el
    └── window.el
```
## Q & A

**Tex preview not working, emacs can not find 'latex' and 'dvipng' binaries.**
```
sudo ln -s /Library/TeX/texbin/latex /usr/local/bin
sudo ln -s /Library/TeX/texbin/dvipng /usr/local/bin
```
