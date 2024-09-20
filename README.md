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

So, do the following steps. First, install presiquites with [Homebrew](https://brew.sh/).

```sh
xcode-select --install
# for emacs
brew install git ripgrep coreutils fd libtool fontconfig ripgrep pngpaste
# for cpp development (optional)
brew install clang-format bazelisk cmake ninja
```

Then, install emacs-plus,

```sh
brew tap d12frosted/emacs-plus
brew install emacs-plus --with-native-comp
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app
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

**How to increase key repeating delay and key repeating rate on MacOS?**

see: https://apple.stackexchange.com/a/83923, remember to restart your system in order for the configs to take effect.
```
defaults write -g InitialKeyRepeat -int 15 // I think 15 is better, you can set this to 10
defaults write -g KeyRepeat -int 1
# or, config those values though GUI
open ~/Library/Preferences/.GlobalPreferences.plist
```
Remember to log-out and re-log-in for those changes to take effect.

**How to remove unused-org-mode-attachment-files?**

see: https://stackoverflow.com/questions/28213360/how-to-delete-unused-org-mode-attachment-files-from-disc

**Git Fetch RPC failed?**

see: https://stackoverflow.com/questions/46232906/git-clone-error-rpc-failed-curl-56-openssl-ssl-read-ssl-error-syscall-errno

also: https://stackoverflow.com/questions/59282476/error-rpc-failed-curl-92-http-2-stream-0-was-not-closed-cleanly-protocol-erro#comment112866014_59474908

`git config --global http.postBuffer 524288000`

**Emacs struggles on svg images on M1?**

see: https://github.com/railwaycat/homebrew-emacsmacport/issues/312
