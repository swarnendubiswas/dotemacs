#!/bin/bash

# Helper script to install GNU Emacs if not already present. It also sets up packages related to my
# setup.

set -eux

if [[ $EUID -ne 0 ]]; then
    echo "This script must be run as root!"
    exit 1
fi

USER="swarnendu"
USER_HOME="/home/$USER"

DISTRO=$(lsb_release -is)
VERSION=$(lsb_release -sr)
DIST_VERSION="${DISTRO}_${VERSION}"

# TODO: Avoid much of the installation exercise if packages and Emacs are already set up.

# Install important packages

case "${DIST_VERSION}" in
Ubuntu_18.04)
    apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python-pip python3-pip composer imagemagick lua5.3 liblua5.3-dev luarocks cargo pandoc fonts-powerline libncurses5-dev fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo htop x11-utils unifont xfonts-terminus ttf-anonymous-pro libperl-dev libmagickwand-dev cpanminus texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev gnutls-dev libncurses5-dev libxml2-dev libxt-dev aspell libxml2-utils chktex libjansson-dev libyaml-dev libxml2-dev autojump htop x11-utils unifont xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev automake autoconf libgtk2.0-dev librsvg2-dev libmagickwand-dev gcc libtiff5-dev libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev libgccjit-8-dev bear
    ;;
Ubuntu_20.04)
    apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python3-pip cppcheck composer imagemagick lua5.3 liblua5.3-dev luarocks cargo pandoc fonts-powerline libncurses5-dev fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo htop x11-utils unifont ttf-ancient-fonts xfonts-terminus ttf-anonymous-pro libperl-dev libmagickwand-dev cpanminus texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev gnutls-dev libncurses5-dev libxml2-dev libxt-dev aspell libxml2-utils chktex libjansson-dev libyaml-dev libxml2-dev autojump htop x11-utils unifont xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev automake autoconf libgtk2.0-dev librsvg2-dev libmagickwand-dev gcc libtiff5-dev libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev bear
    ;;
*)
    echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
    exit 2
    ;;
esac

# Add necessary repositories
case "${DIST_VERSION}" in
Ubuntu_18.04)
    add-apt-repository ppa:ubuntu-toolchain-r/test -y
    ;;
Ubuntu_20.04)
    add-apt-repository ppa:ubuntu-toolchain-r/test -y
    ;;
*)
    echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
    exit 2
    ;;
esac

apt install -y gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev

# Install LLVM

LLVM_VERSION="-13"

case "${DIST_VERSION}" in
Ubuntu_18.04) REPO_NAME="deb http://apt.llvm.org/bionic/   llvm-toolchain-bionic${LLVM_VERSION}  main" ;;
Ubuntu_20.04) REPO_NAME="deb http://apt.llvm.org/focal/    llvm-toolchain-focal${LLVM_VERSION}   main" ;;
*)
    echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
    exit 2
    ;;
esac

wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
add-apt-repository "${REPO_NAME}"

apt install -y clang${LLVM_VERSION} clangd${LLVM_VERSION} clang-{format,tidy,tools}${LLVM_VERSION} clang${LLVM_VERSION}-doc clang${LLVM_VERSION}-examples llvm${LLVM_VERSION} lld${LLVM_VERSION} lldb${LLVM_VERSION} llvm${LLVM_VERSION}-runtime

# Download GNU Emacs source

EMACS_VERSION="28.0.91"

cd "${USER_HOME}"
EMACS_NAME="emacs-${EMACS_VERSION}"
EMACS_FILENAME="${EMACS_NAME}.tar.xz"
if [ ! -f "${EMACS_FILENAME}" ]; then
    wget https://alpha.gnu.org/gnu/emacs/pretest/"${EMACS_FILENAME}"
fi
tar xf "${EMACS_FILENAME}"
rm "${EMACS_FILENAME}*" || true

EMACS_SOURCE="${USER_HOME}/${EMACS_NAME}"
chown -R "$USER:$USER ${EMACS_SOURCE}"

# Build the source

cd "{$EMACS_SOURCE}"
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
# We do not need POP3 support
./configure --with-cairo --with-modules --with-x-toolkit=lucid --without-compress-install --with-x-toolkit=no --with-gnutls --without-gconf --without-xwidgets --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation --with-json --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm --with-gif --with-threads --with-included-regex --with-zlib --without-sound --without-pop --with-dbus CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer"
# prefix=/usr/local
make -j4 NATIVE_FULL_AOT=1

cd "${USER_HOME}"
rm "${EMACS_FILENAME}" || true

# Setup Emacs at the correct path
echo "export EMACS_PATH=${EMACS_SOURCE}/src" >>"$USER_HOME/.bashrc"
echo "PATH=${EMACS_PATH}:$PATH" >>"$USER_HOME/.bashrc"

# Checkout configurations

GITHUB="${USER_HOME}/github"
DOTEMACS="$GITHUB/dotemacs"
DOTFILES="$GITHUB/dotfiles"
EMACSD="${USER_HOME}/.emacs.d"

cd "$USER_HOME"

if [ ! -d "$GITHUB" ]; then
    mkdir -p "$GITHUB"
    chown -R $USER:$USER "$GITHUB"
fi

cd "$GITHUB"

if [ -d "$DOTEMACS" ]; then
    cd "$DOTEMACS"
    echo "Pulling dotemacs repository from GitHub..."
    git pull
else
    echo "Cloning dotemacs repository from GitHub..."
    git clone git@github.com:swarnendubiswas/dotemacs.git
fi
echo "...Done"
chown -R $USER:$USER $DOTEMACS

if [ -d "$DOTFILES" ]; then
    cd "$DOTFILES"
    echo "Pulling dotfiles repository from GitHub..."
    git pull
else
    echo "Cloning dotfiles repository from GitHub..."
    git clone git@github.com:swarnendubiswas/dotfiles.git
fi
echo "...Done"
chown -R $USER:$USER $DOTFILES

# Link .emacs.d

cd "${USER_HOME}"

if [ -d "$EMACSD" ]; then
    if [ ! -L "$EMACSD" ]; then
        ln -s "$DOTEMACS" "$EMACSD"
    fi
fi

# Install Python packages
sudo -H python3 -m pip install --upgrade pip pygments isort yapf jedi pylint importmagic pyls-isort pydocstyle setuptools configparser backports-functools_lru_cache yamllint cmake-language-server --user

# Install Nodejs

NODEJS_VER="17"

# curl -sL https://deb.nodesource.com/setup_{$NODEJS_VER}.x -o nodesource_setup.sh
# bash nodesource_setup.sh
# apt install nodejs

curl -fsSL https://deb.nodesource.com/setup_"${NODEJS_VER}".x | bash -
apt install -y nodejs

# Setup Node packages

TMP_HOME="$USER_HOME/tmp"

NPM_HOME="$TMP_HOME"
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

# Install Texlab. The language server seems feature-incomplete and slow, so I still prefer AuCTeX.

# cargo install --git https://github.com/latex-lsp/texlab.git

# cpanm Perl::LanguageServer

# Update configurations of helper utilities

# HOME Directory
cd "$USER_HOME"

if [ ! -L ".markdownlint.json" ]; then
    echo "Creating symlink for .markdownlint.json..."
    ln -s "$DOTFILES/markdown/dotmarkdownlint.json" .markdownlint.json
else
    echo "Overwriting symlink for .markdownlint.json..."
    ln -nsf "$DOTFILES/markdown/dotmarkdownlint.json" .markdownlint.json
fi
echo "...Done"

# CONFIG Directory

CONFIG_DIR="$USER_HOME/.config"
if [ ! -d "$CONFIG_DIR" ]; then
    mkdir -p "$CONFIG_DIR"
    chown -R $USER:$USER "$CONFIG_DIR"
fi
cd "$CONFIG_DIR"

if [ ! -L "pylintrc" ]; then
    echo "Creating symlink for pylintrc..."
    ln -s "$DOTFILES/python/pylintrc" .
else
    echo "Overwriting symlink for pylintrc..."
    ln -nsf "$DOTFILES/python/pylintrc" .
fi
echo "...Done"

if [ -d "yapf" ]; then
    if [ ! -L "yapf" ]; then
        echo "$CONFIG_DIR/yapf present and is not a symlink!"
    else
        echo "Overwriting symlink for yapf..."
        ln -nsf "$DOTFILES/python/yapf" .
    fi
else
    echo "Creating symlink for yapf..."
    ln -nsf "$DOTFILES/python/yapf" .
fi
echo "...Done"

if [ -d "yamllint" ]; then
    if [ ! -L "yamllint" ]; then
        echo "$CONFIG_DIR/yamllint present and is not a symlink!"
    else
        echo "Overwriting symlink for yamllint..."
        ln -nsf "$DOTFILES/yamllint" .
    fi
else
    echo "Creating symlink for yapf..."
    ln -nsf "$DOTFILES/yamllint" .
fi
echo "...Done"

# Shellcheck
SHELLCHECK_VER="0.8.0"

SHELLCHECK_FILENAME="shellcheck-v${SHELLCHECK_VER}.linux.x86_64"

cd "${USER_HOME}"
wget https://github.com/koalaman/shellcheck/releases/download/v"${SHELLCHECK_VER}/${SHELLCHECK_FILENAME}.tar.xz"
tar xf "${SHELLCHECK_FILENAME}.tar.xz"
cd "shellcheck-v${SHELLCHECK_VER}"
mv shellcheck "${USER_HOME}/.local/bin"
rm -rf "${SHELLCHECK_FILENAME}.tar.xz*"

# shfmt
SHFMT_VER="3.4.3"

cd "${USER_HOME}"
wget https://github.com/mvdan/sh/releases/download/v"${SHFMT_VER}/shfmt_v${SHFMT_VER}"_linux_amd64
mv shfmt_v"${SHFMT_VER}"_linux_amd64 ${USER_HOME}/.local/bin/shfmt

# Ripgrep

RG_VER="13.0.0"

wget https://github.com/BurntSushi/ripgrep/releases/download/"${RG_VER}/ripgrep_${RG_VER}"_amd64.deb
dpkg -i ripgrep_"${RG_VER}"_amd64.deb
rm -rf ripgrep_"${RG_VER}"_amd64.deb*

# Build CPPCheck

apt install libpcre3-dev
git clone git@github.com:danmar/cppcheck.git
cd cppcheck
git checkout 2.7
mkdir -p build
cd build
cmake -DUSE_MATCHCOMPILER=ON -DHAVE_RULES=ON ..
cmake --build .
make install
cd ../..
rm -rf cppcheck

# Build Universal Ctags

# Installing snaps seems to hurt Ubuntu performance.

cd "$GITHUB"

CTAGS_DIR="$GITHUB/ctags"

if [ -d "${CTAGS_DIR}" ]; then
    cd "${CTAGS_DIR}"
    echo "Pulling ctags reposiory from GitHub..."
    git pull
else
    echo "Cloning ctags repository from GitHub..."
    git clone https://github.com/universal-ctags/ctags.git
fi
echo "...Done"
chown -R $USER:$USER "${CTAGS_DIR}"

cd "${CTAGS_DIR}"
./autogen.sh
# "--prefix=/where/you/want" defaults to "/usr/local"
./configure
make
make install

# Alacritty

# ALACRITTY_VER="0.10.0"
# cd "$GITHUB"
# wget https://github.com/alacritty/alacritty/archive/refs/tags/v"{$ALACRITTY_VER}".tar.gz
# tar xz alacritty-"{$ALACRITTY_VER}".tar.gz
# cd alacritty-"{$ALACRITTY_VER}"
# cargo build --release

add-apt-repository ppa:aslatter/ppa -y
apt install alacritty

# Setup 24bit terminal support
/usr/bin/tic -x -o ~/.terminfo "${DOTFILES}/emacs/xterm-24bit.terminfo"

# Remove junk
cd "$USER_HOME"
apt autoremove
apt autoclean

set +eux
