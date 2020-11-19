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

case "$DIST_VERSION" in
    Ubuntu_16.04)
        add-apt-repository ppa:ubuntu-toolchain-r/test
        apt-get update
        apt install -y gcc-7 g++-7
        ;;
    *)
        echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
        exit 2
        ;;
esac

# Check if LLVM is installed

LLVM_VERSION="-11"

case "$DIST_VERSION" in
    Ubuntu_16.04) REPO_NAME="deb http://apt.llvm.org/xenial/   llvm-toolchain-xenial$LLVM_VERSION  main" ;;
    Ubuntu_18.04) REPO_NAME="deb http://apt.llvm.org/bionic/   llvm-toolchain-bionic$LLVM_VERSION  main" ;;
    Ubuntu_19.04) REPO_NAME="deb http://apt.llvm.org/disco/    llvm-toolchain-disco$LLVM_VERSION   main" ;;
    Ubuntu_20.04) REPO_NAME="deb http://apt.llvm.org/focal/    llvm-toolchain-focal$LLVM_VERSION    main" ;;
    *)
        echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
        exit 2
        ;;
esac

# REPO_NAME="deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic$LLVM_VERSION  main"
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
add-apt-repository "${REPO_NAME}"
apt-get update

apt install clang$LLVM_VERSION lldb$LLVM_VERSION lld$LLVM_VERSION libllvm$LLVM_VERSION-ocaml-dev libllvm8 llvm$LLVM_VERSION llvm$LLVM_VERSION-dev llvm$LLVM_VERSION-doc llvm$LLVM_VERSION-examples llvm$LLVM_VERSION-runtime clang-tools$LLVM_VERSION clang-tidy$LLVM_VERSION clang$LLVM_VERSION-doc libclang-common$LLVM_VERSION-dev libclang$LLVM_VERSION-dev libclang1$LLVM_VERSION clang-format$LLVM_VERSION python-clang$LLVM_VERSION clangd$LLVM_VERSION libfuzzer$LLVM_VERSION-dev lldb$LLVM_VERSION lld$LLVM_VERSION libc++$LLVM_VERSION-dev libc++abi$LLVM_VERSION-dev libomp$LLVM_VERSION-dev

apt install aspell global exuberant-ctags libxml2-utils chktex shellcheck ruby-dev tidy python-pygments python-pip python3-pip npm cppcheck composer imagemagick lua5.3 liblua5.3-dev luarocks cargo

snap install shfmt
snap install universal-ctags
snap install ripgrep --classic
snap install shellcheck --edge
snap refresh

python -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi==0.15.2 pylint python-language-server importmagic pyls-isort pydocstyle setuptools configparser==3.8.1 backports-functools_lru_cache yamllint --user

python3 -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi==0.15.2 pylint python-language-server importmagic pyls-isort pydocstyle setuptools configparser backports-functools_lru_cache yamllint cmake-language-server --user

npm i -g npm eslint js-yaml less jsonlint bash-language-server vscode-html-languageserver-bin js-beautify typescript-language-server typescript vscode-css-languageserver-bin intelephense markdownlint-cli yaml-language-server vscode-json-languageserver intelephense stylelint
npm update

gem install scss_lint
gem update

composer require jetbrains/phpstorm-stubs:dev-master
composer require felixfbecker/language-server
composer update

cargo install --git https://github.com/latex-lsp/texlab.git
