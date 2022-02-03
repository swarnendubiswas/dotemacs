#!/bin/bash

# Helper script to install GNU Emacs if not already present. It also sets up packages related to my
# setup.

set -eux

if [[ $EUID -ne 0 ]]; then
    echo "This script must be run as root!"
    exit 1
fi

DISTRO=$(lsb_release -is)
VERSION=$(lsb_release -sr)
DIST_VERSION="${DISTRO}_${VERSION}"

# Add necessary repositories
case "$DIST_VERSION" in
    Ubuntu_18.04)
        add-apt-repository ppa:ubuntu-toolchain-r/test
        ;;
    Ubuntu_20.04) ;;

    *)
        echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
        exit 2
        ;;
esac

# Install important packages

case "$DIST_VERSION" in
    Ubuntu_18.04)
        apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python-pip python3-pip composer imagemagick lua5.3 liblua5.3-dev luarocks cargo pandoc fonts-powerline libncurses5-dev fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo htop x11-utils unifont xfonts-terminus ttf-anonymous-pro libperl-dev libmagickwand-dev cpanminus texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev gnutls-dev libncurses5-dev libxml2-dev libxt-dev aspell libxml2-utils chktex libjansson-dev libyaml-dev libxml2-dev autojump htop x11-utils unifont xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev automake autoconf libgtk2.0-dev librsvg2-dev libmagickwand-dev gcc libtiff5-dev libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev libgccjit-8-dev
        ;;
    Ubuntu_20.04)
        apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python3-pip cppcheck composer imagemagick lua5.3 liblua5.3-dev luarocks cargo pandoc fonts-powerline libncurses5-dev fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo htop x11-utils unifont ttf-ancient-fonts xfonts-terminus ttf-anonymous-pro libperl-dev libmagickwand-dev cpanminus texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev gnutls-dev libncurses5-dev libxml2-dev libxt-dev aspell libxml2-utils chktex libjansson-dev libyaml-dev libxml2-dev autojump htop x11-utils unifont xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev automake autoconf libgtk2.0-dev librsvg2-dev libmagickwand-dev gcc libtiff5-dev libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev
        ;;
    *)
        echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
        exit 2
        ;;
esac

# Install LLVM

LLVM_VERSION="-13"

case "$DIST_VERSION" in
    Ubuntu_18.04) REPO_NAME="deb http://apt.llvm.org/bionic/   llvm-toolchain-bionic$LLVM_VERSION  main" ;;
    Ubuntu_20.04) REPO_NAME="deb http://apt.llvm.org/focal/    llvm-toolchain-focal$LLVM_VERSION   main" ;;
    *)
        echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
        exit 2
        ;;
esac

# REPO_NAME="deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic$LLVM_VERSION  main"
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
add-apt-repository "${REPO_NAME}"

apt install -y clang$LLVM_VERSION clangd$LLVM_VERSION clang-{format,tidy,tools}$LLVM_VERSION clang$LLVM_VERSION-doc clang$LLVM_VERSION-examples llvm$LLVM_VERSION lld$LLVM_VERSION lldb$LLVM_VERSION llvm$LLVM_VERSION-runtime

# Download GNU Emacs source
cd "$HOME"
EMACS_VERSION="28.0.91"
EMACS_NAME="emacs-${EMACS_VERSION}"
wget https://alpha.gnu.org/gnu/emacs/pretest/${EMACS_NAME}.tar.xz
tar xf "${EMACS_NAME}.tar.xz"
EMACS_SOURCE="$HOME/$EMACS_NAME"

# Build the source
cd "$EMACS_NAME"
./autogen.sh
./configure --with-cairo --with-modules --with-x-toolkit=lucid --without-compress-install --with-x-toolkit=no --with-gnutls --without-gconf --without-xwidgets --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation --with-json --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm --with-gif --with-threads --with-included-regex --with-zlib --without-sound --without-pop CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" prefix=/usr/local
make -j2 NATIVE_FULL_AOT=1
# sudo make install

# Checkout configurations

GITHUB="$HOME/github"
DOTEMACS="$GITHUB/dotemacs"
DOTFILES="$GITHUB/dotfiles"
EMACSD="$HOME/.emacs.d"

cd "$HOME"

if [ ! -d "$GITHUB" ]; then
    mkdir -p github
fi

cd "$GITHUB"

if [ -d "$DOTEMACS" ]; then
    cd "$DOTEMACS"
    echo "Pulling dotemacs repository from Github..."
    git pull
else
    echo "Cloning dotemacs repository from Github..."
    git clone https://github.com/swarnendubiswas/dotemacs.git
fi
echo "...Done"

if [ -d "$DOTFILES" ]; then
    cd "$DOTFILES"
    echo "Pulling dotfiles repository from Github..."
    git pull
else
    echo "Cloning dotfiles repository from Github..."
    git clone https://github.com/swarnendubiswas/dotfiles.git
fi
echo "...Done"

cd "$HOME"

if [ -d "$EMACSD" ]; then
    if [ ! -L "$EMACSD" ]; then
        ln -s "$DOTEMACS" "$EMACSD"
    fi
fi

# Update configurations of helper utilities

if [ ! -L ".markdownlint.json" ]; then
    echo "Creating symlink for .markdownlint.json..."
    ln -s "$DOTFILES/markdown/dotmarkdownlint.json" .markdownlint.json
else
    echo "Overwriting symlink for .markdownlint.json..."
    ln -nsf "$DOTFILES/markdown/dotmarkdownlint.json" .markdownlint.json
fi
echo "...Done"

if [ ! -d "$HOME/.config" ]; then
    mkdir -p "$HOME/.config"
fi

cd "$HOME/.config"

if [ ! -L "pylintrc" ]; then
    echo "Creating symlink for pylintrc..."
    ln -s "$DOTFILES/dotconfig/pylintrc" .
else
    echo "Overwriting symlink for pylintrc..."
    ln -nsf "$DOTFILES/dotconfig/pylintrc" .
fi
echo "...Done"

if [ -d "yapf" ]; then
    if [ ! -L "yapf" ]; then
        echo "$HOME/.config/yapf present and is not a symlink!"
    else
        echo "Overwriting symlink for yapf..."
        ln -nsf "$DOTFILES/dotconfig/yapf" .
    fi
else
    echo "Creating symlink for yapf..."
    ln -nsf "$DOTFILES/dotconfig/yapf" .
fi
echo "...Done"

# Install Python packages
python3 -m pip install --upgrade pip pygments isort yapf jedi pylint importmagic pyls-isort pydocstyle setuptools configparser backports-functools_lru_cache yamllint cmake-language-server --user

# Setup Node packages

NPM_HOME="$HOME/tmp"
mkdir -p "$NPM_HOME"
cd "$NPM_HOME"

npm init --yes
npm install --save-dev npm less eslint jsonlint bash-language-server vscode-html-languageserver-bin js-beautify typescript-language-server typescript vscode-css-languageserver-bin intelephense markdownlint-cli markdownlint-cli2 yaml-language-server vscode-json-languageserver intelephense write-good htmlhint javascript-typescript-langserver pyright unofficial-grammarly-language-server-2 @emacs-grammarly/keytar-cli unified-language-server prettier @prettier/plugin-php stylelint

npm install git+https://gitlab.com/matsievskiysv/math-preview --save-dev

# Gem modules
# gem install scss_lint
# gem update

# Composer modules
# composer require jetbrains/phpstorm-stubs:dev-master
# composer require felixfbecker/language-server
# composer update

# Install Texlab
# cargo install --git https://github.com/latex-lsp/texlab.git

snap install shfmt
snap install universal-ctags
snap install ripgrep --classic
snap install shellcheck --edge

# Build CPPCheck
