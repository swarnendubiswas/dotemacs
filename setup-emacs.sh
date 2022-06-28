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
        apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python-pip python3-pip composer imagemagick lua5.3 liblua5.3-dev luarocks cargo pandoc fonts-powerline fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo x11-utils ttf-ancient-fonts libmagickwand-dev cpanminus libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev libncurses5-dev libxt-dev htop unifont xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev libgtk2.0-dev librsvg2-dev gcc libtiff5-dev libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev libc6-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libx11-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxrandr-dev libxtst-dev libxv-dev curl libssl-dev
        ;;
    Ubuntu_20.04)
        apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python3-pip composer imagemagick lua5.3 liblua5.3-dev luarocks cargo pandoc fonts-powerline fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo x11-utils ttf-ancient-fonts libmagickwand-dev cpanminus libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev libncurses5-dev libxt-dev htop unifont xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev libgtk2.0-dev librsvg2-dev gcc libtiff5-dev libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev bear libc6-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libx11-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxrandr-dev libxtst-dev libxv-dev curl libssl-dev
        ;;
    *)
        echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
        exit 2
        ;;
esac

# Add necessary repositories
case "${DIST_VERSION}" in
    Ubuntu_18.04 | Ubuntu_20.04)
        add-apt-repository ppa:ubuntu-toolchain-r/test -y
        ;;
    *)
        echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
        exit 2
        ;;
esac

apt install -y gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev gcc-10-multilib g++-10-multilib gcc-9 g++-9

# Install LLVM

LLVM_VERSION="-14"

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

EMACS_VERSION="28.1"

cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
EMACS_NAME="emacs-${EMACS_VERSION}"
EMACS_FILENAME="${EMACS_NAME}.tar.xz"
if [ ! -f "${EMACS_FILENAME}" ]; then
    wget https://ftp.gnu.org/gnu/emacs/"${EMACS_FILENAME}"
fi
tar xf "${EMACS_FILENAME}"
rm "${EMACS_FILENAME}"* || true

EMACS_SOURCE="${USER_HOME}/${EMACS_NAME}"
chown -R $USER:$USER "${EMACS_SOURCE}"

# Build the source

cd "${EMACS_SOURCE}" || echo "Failed: cd ${EMACS_SOURCE}"
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
make distclean
./autogen.sh
# We do not need POP3 support
./configure --with-cairo --with-modules --without-compress-install --with-x-toolkit=no --with-gnutls --without-gconf --without-xwidgets --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation --with-json --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm --with-gif --with-threads --with-included-regex --with-zlib --without-sound --without-pop --with-dbus CFLAGS="-O2 -mtune=native -march=native -fomit-frame-pointer" prefix=/usr/local
# Use NATIVE_FULL_AOT=1 to native compile ahead-of-time all the elisp files included in the Emacs distribution instead of after startup
make -j"$(nproc)" NATIVE_FULL_AOT=1

cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
rm "${EMACS_FILENAME}" || true

# Setup Emacs at the correct path
echo "export EMACS_PATH=${EMACS_SOURCE}/src" >>"$USER_HOME/.bashrc"
echo "PATH=\${EMACS_PATH}:$PATH" >>"$USER_HOME/.bashrc"

# Checkout configurations

GITHUB="${USER_HOME}/github"
DOTEMACS="$GITHUB/dotemacs"
DOTFILES="$GITHUB/dotfiles"
EMACSD="${USER_HOME}/.emacs.d"

cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"

if [ ! -d "$GITHUB" ]; then
    mkdir -p "$GITHUB"
    chown -R $USER:$USER "$GITHUB"
fi

cd "$GITHUB" || echo "Failed: cd ${GITHUB}"

# TODO: There does not seem to be a good way to checkout private repositories

# if [ -d "$DOTEMACS" ]; then
#     cd "$DOTEMACS" || echo "Failed: cd ${DOTEMACS}"
#     echo "Pulling dotemacs repository from GitHub..."
#     sudo -u swarnendu git pull
# else
#     echo "Cloning dotemacs repository from GitHub..."
#     sudo -u swarnendu git clone git@github.com:swarnendubiswas/dotemacs.git
# fi
# echo "...Done"
# chown -R $USER:$USER $DOTEMACS

# if [ -d "$DOTFILES" ]; then
#     cd "$DOTFILES" || echo "Failed: cd ${DOTEMACS}"
#     echo "Pulling dotfiles repository from GitHub..."
#     sudo -u swarnendu git pull
# else
#     echo "Cloning dotfiles repository from GitHub..."
#     sudo -u swarnendu git clone git@github.com:swarnendubiswas/dotfiles.git
# fi
# echo "...Done"
# chown -R $USER:$USER $DOTFILES

# Link .emacs.d

cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"

if [ -d "$EMACSD" ]; then
    if [ ! -L "$EMACSD" ]; then
        ln -s "$DOTEMACS" "$EMACSD"
    fi
fi

# Install Python packages
sudo -u swarnendu python3 -m pip install --upgrade pip pygments isort yapf jedi pylint importmagic pydocstyle setuptools yamllint cmake-language-server pyls-memestra "python-lsp-server[all]" pyls-isort cpplint grip --user

# Install Nodejs

NODEJS_VER="18"

curl -fsSL https://deb.nodesource.com/setup_"${NODEJS_VER}".x | bash -
apt install -y nodejs

# Setup Node packages

TMP_HOME="$USER_HOME/tmp"

NPM_HOME="$TMP_HOME"
mkdir -p "${NPM_HOME}"
cd "${NPM_HOME}" || echo "Failed: cd ${NPM_HOME}"

npm init --yes

# This list matches with "package.json" in $DOTFILES
npm install --save-dev npm less eslint jsonlint bash-language-server vscode-html-languageserver-bin js-beautify typescript-language-server typescript vscode-css-languageserver-bin intelephense markdownlint-cli markdownlint-cli2 yaml-language-server vscode-json-languageserver write-good htmlhint javascript-typescript-langserver pyright @emacs-grammarly/keytar-cli unified-language-server prettier @prettier/plugin-php stylelint remark-language-server

npm install git+https://gitlab.com/matsievskiysv/math-preview --save-dev

# TODO: Add the following to $HOME/.bashrc
# echo "export NODE_PATH=$HOME/tmp/node_modules" >> $HOME/.bashrc

# Gem modules

# gem install scss_lint
# gem update

# Composer modules

# composer require jetbrains/phpstorm-stubs:dev-master
# composer require felixfbecker/language-server
# composer update

# cpanm Perl::LanguageServer

# Install Texlab. The language server seems feature-incomplete and slow, so I still prefer AuCTeX.

TEXLAB_VER="4.1.0"

cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
wget https://github.com/latex-lsp/texlab/releases/download/v"${TEXLAB_VER}"/texlab-x86_64-linux.tar.gz
tar xf texlab-x86_64-linux.tar.gz
mv texlab "${USER_HOME}/.local/bin"
rm texlab-x86_64-linux.tar.gz

# Update configurations of helper utilities

# HOME Directory
cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"

if [ ! -L ".markdownlint.json" ]; then
    echo "Creating symlink for .markdownlint.json..."
    ln -s "$DOTFILES/markdown/dotmarkdownlint.json" .markdownlint.json
else
    echo "Overwriting symlink for .markdownlint.json..."
    ln -nsf "$DOTFILES/markdown/dotmarkdownlint.json" .markdownlint.json
fi
echo "...Done"

if [ -f ".prettierrc" ]; then
    echo "Overwriting symlink for .prettierrc..."
    ln -nsf "$DOTFILES/dotprettierrc" "$HOME/.prettierrc"
else
    echo "Creating symlink for .prettierrc..."
    ln -s "$DOTFILES/dotprettierrc" "$HOME/.prettierrc"
fi
echo "...Done"

# CONFIG Directory

CONFIG_DIR="${USER_HOME}/.config"
if [ ! -d "${CONFIG_DIR}" ]; then
    mkdir -p "${CONFIG_DIR}"
    chown -R $USER:$USER "${CONFIG_DIR}"
fi
cd "${CONFIG_DIR}" || echo "Failed: cd ${CONFIG_DIR}"

if [ ! -L "pylintrc" ]; then
    echo "Creating symlink for pylintrc..."
    ln -s "$DOTFILES/python/pylintrc" .
else
    echo "Overwriting symlink for pylintrc..."
    ln -s "$DOTFILES/python/pylintrc" .
fi
echo "...Done"

if [ -d "yapf" ]; then
    if [ ! -L "yapf" ]; then
        echo "${CONFIG_DIR}/yapf present and is not a symlink!"
    else
        echo "Overwriting symlink for yapf..."
        ln -nsf "$DOTFILES/python/yapf" .
    fi
else
    echo "Creating symlink for yapf..."
    ln -s "$DOTFILES/python/yapf" .
fi
echo "...Done"

if [ -d "yamllint" ]; then
    if [ ! -L "yamllint" ]; then
        echo "${CONFIG_DIR}/yamllint present and is not a symlink!"
    else
        echo "Overwriting symlink for yamllint..."
        ln -nsf "$DOTFILES/yamllint" .
    fi
else
    echo "Creating symlink for yapf..."
    ln -s "$DOTFILES/yamllint" .
fi
echo "...Done"

# Shellcheck
SHELLCHECK_VER="0.8.0"

SHELLCHECK_FILENAME="shellcheck-v${SHELLCHECK_VER}.linux.x86_64"

cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
wget https://github.com/koalaman/shellcheck/releases/download/v"${SHELLCHECK_VER}/${SHELLCHECK_FILENAME}.tar.xz"
tar xf "${SHELLCHECK_FILENAME}.tar.xz"
cd "shellcheck-v${SHELLCHECK_VER}" || echo "Failed: cd shellcheck-v${SHELLCHECK_VER}"
mv shellcheck "${USER_HOME}/.local/bin"
rm -rf "${SHELLCHECK_FILENAME}.tar.xz*"

# shfmt
SHFMT_VER="3.4.3"

cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
wget https://github.com/mvdan/sh/releases/download/v"${SHFMT_VER}/shfmt_v${SHFMT_VER}"_linux_amd64
mv shfmt_v"${SHFMT_VER}"_linux_amd64 ${USER_HOME}/.local/bin/shfmt

# Ripgrep

RG_VER="13.0.0"

wget https://github.com/BurntSushi/ripgrep/releases/download/"${RG_VER}/ripgrep_${RG_VER}"_amd64.deb
dpkg -i ripgrep_"${RG_VER}"_amd64.deb
rm -rf ripgrep_"${RG_VER}"_amd64.deb*

# Build CPPCheck

apt install libpcre3-dev
if [ ! -d cppcheck ]; then
    sudo -u swarnendu git clone git@github.com:danmar/cppcheck.git
else
    cd cppcheck || echo "Failed: cd cppcheck"
    sudo -u swarnendu git pull
    cd ..
fi

cd cppcheck || echo "Failed: cd cppcheck"
git checkout 2.8
mkdir -p build
cd build || echo "Failed: cd build"
cmake -DUSE_MATCHCOMPILER=ON -DHAVE_RULES=ON ..
cmake --build .
make install
cd ../..
rm -rf cppcheck

# Build Universal Ctags, installing snaps seems to hurt Ubuntu performance.

cd "$GITHUB" || echo "Failed: cd $GITHUB"
CTAGS_DIR="$GITHUB/ctags"

if [ -d "${CTAGS_DIR}" ]; then
    cd "${CTAGS_DIR}" || echo "Failed: cd ${CTAGS_DIR}"
    echo "Pulling ctags reposiory from GitHub..."
    sudo -u swarnendu git pull
else
    echo "Cloning ctags repository from GitHub..."
    sudo -u swarnendu git clone https://github.com/universal-ctags/ctags.git
fi
echo "...Done"
chown -R $USER:$USER "${CTAGS_DIR}"

cd "${CTAGS_DIR}" || echo "Failed: cd ${CTAGS_DIR}"
./autogen.sh
./configure
make
make install

# GNU Global
# wget http://tamacom.com/global/global-6.6.8.tar.gz
# tar -xzvf global-6.6.8.tar.gz
# cd global-6.6.8
# ./configure --with-universal-ctags=/usr/local/bin/ctags; make; sudo make install;
# echo "GTAGSCONF=/usr/local/share/gtags/gtags.conf" >> $HOME/.bashrc
# echo "GTAGSLABEL=new-ctags" >> $HOME/.bashrc

# Alacritty

# ALACRITTY_VER="0.10.1"
# cd "$GITHUB"
# wget https://github.com/alacritty/alacritty/archive/refs/tags/v"${ALACRITTY_VER}".tar.gz
# tar xz alacritty-"${ALACRITTY_VER}".tar.gz
# cd alacritty-"${ALACRITTY_VER}"
# cargo build --release

add-apt-repository ppa:aslatter/ppa -y
apt install alacritty

cd "${CONFIG_DIR}" || echo "Failed: cd ${CONFIG_DIR}"

if [ -d "alacritty" ]; then
    if [ ! -L "alacritty" ]; then
        echo "${CONFIG_DIR}/yamllint present and is not a symlink!"
    else
        echo "Overwriting symlink for yamllint..."
        ln -nsf "$DOTFILES/alacritty" .
    fi
else
    echo "Creating symlink for yapf..."
    ln -s "$DOTFILES/alacritty" .
fi
echo "...Done"

# Setup 24bit terminal support
/usr/bin/tic -x -o ~/.terminfo "${DOTFILES}/emacs/xterm-24bit.terminfo"

# Build bear
apt install libssl-dev
if [ ! -d bear ]; then
    sudo -u swarnendu git clone git@github.com:rizsotto/Bear.git bear
else
    cd bear || echo "Failed: cd bear"
    sudo -u swarnendu git pull
    cd ..
fi

cd bear || echo "Failed: cd bear"
mkdir -p build && cd build || echo "Failed: cd bear/build"
cmake -DENABLE_UNIT_TESTS=OFF -DENABLE_FUNC_TESTS=OFF ..
make all
make install
cd ../..
rm -rf bear

# Powerline

cd "$HOME"

# Tmux

# Delta

DELTA_VER="0.13.0"

wget https://github.com/dandavison/delta/releases/download/"$DELTA_VER"/git-delta_"$DELTA_VER"_amd64.deb
dpkg -i git-delta_"$DELTA_VER"_amd64.deb
rm git-delta_"$DELTA_VER"_amd64.deb

# Zoxide

curl -sS https://webinstall.dev/zoxide | bash

# For bash, add this line to ~/.bashrc
echo "eval \"$(zoxide init bash)\"" >>"$USER_HOME/.bashrc"

# For fish, add this line to ~/.config/fish/config.fish
echo "zoxide init fish | source" >>"$CONFIG_DIR/fish/config.fish"

# Bat

BAT_VER="0.21.0"

wget https://github.com/sharkdp/bat/releases/download/v"$BAT_VER"/bat_"$BAT_VER"_amd64.deb
dpkg -i bat_"$BAT_VER"_amd64.deb
rm bat_"$BAT_VER"_amd64.deb

# FZF
cd $GITHUB
if [ ! -d fzf ]; then
    sudo -u swarnendu git clone --depth 1 https://github.com/junegunn/fzf.git
else
    cd fzf || echo "Failed: cd fzf"
    sudo -u swarnendu git pull
    cd ..
fi

cd fzf
bash ./install

# Remove junk
cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
apt autoremove
apt autoclean

set +eux
