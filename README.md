# doom.d

This is Jamie's private [doom emacs](https://github.com/doomemacs/doomemacs) config. Please make sure you are using [zsh shell](https://www.zsh.org/) (recommende to use [omz](https://ohmyz.sh/))! Pesonally, I use this config to do **daily-cpp-dev**, **paper-writing**, and paper related **note-taking**, so as a summary, this config
1. Supports remote cpp developing with tramp, with correctly configured *code-format (clang-format)*, *lsp (eglot + clangd)*, *bazel*
2. Supports latex acadamic paper writing, and note taking

**WARNING: This config is only designed for MacOS**.

Thirdparty libraries:
- [wondershaper](https://github.com/magnific0/wondershaper): Command-line utility for limiting an adapter's bandwidth
- [secretflow/devtools](https://github.com/secretflow/devtools): Secretflow's development tools

Recommended MacOS Apps:
- [skim](https://skim-app.sourceforge.io/): open-source PDF reader and note-taker for OS X
- [alacritty](https://alacritty.org/): cross-platform OpenGL terminal emulator
- [zotero](https://www.zotero.org/): free and easy-to-use biblography tool
- [MacTex](https://tug.org/mactex/): standard distribution of TeX, LaTeX, and related programs produced by TeX Users Groups across the world

## Getting Started

### Step 1: Download GNU/Emacs

Of course, you need to download Emacs and doom first. As in [doom's getting started guide](https://github.com/doomemacs/doomemacs/blob/master/docs/getting_started.org#with-homebrew) says,

> emacs-mac offers good integration with macOS, native emojis and better childframe support.

So, do the following steps. First, install presiquites with [Homebrew](https://brew.sh/).

```sh
xcode-select --install
# for emacs
brew install git ripgrep coreutils fd libtool fontconfig ripgrep pngpaste
# for cpp development (optional)
brew install clang-format bazelisk cmake ninja
```

Then, install emacs-mac,

```sh
brew tap railwaycat/emacsmacport
brew install emacs-mac --with-modules --with-native-comp
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs.app
# open emacs
open -a "emacs" # you can also setup this as quick open script in alfred
```

### Step 2: Install doom emacs

```sh
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

### Step 3: Setup this config

```sh
./${PROJECT_ROOT}/scripts/setup-bin.el
```

### Optional: Install oh-my-zsh

``` sh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

```

### Optional: Install rustlang


``` sh
# rustup
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### Optional: Install dependencies for leetcode

``` sh
cargo install leetcode-cli
sudo ln -s /Library/TeX/texbin/latex /usr/local/bin
```
## Other Questions

**Tex preview not working, emacs can not find 'latex' and 'dvipng' binaries.**
```
sudo ln -s /Library/TeX/texbin/latex /usr/local/bin
sudo ln -s /Library/TeX/texbin/dvipng /usr/local/bin
```

