#!/usr/bin/env bash

# Helper script to install GNU Emacs if not already present. It also sets up packages related to my
# setup.

# set -eux

# We do not use $HOME since it will point to "/root" when run with sudo privileges
USER="swarnendu"
USER_HOME="/home/$USER"
GITHUB="${USER_HOME}/github"
DOTFILES="$GITHUB/dotfiles"
CONFIG_DIR="${USER_HOME}/.config"

DISTRO=$(lsb_release -is)
VERSION=$(lsb_release -sr)
DIST_VERSION="${DISTRO}_${VERSION}"

is_sudo() {
    if [[ $EUID -ne 0 ]]; then
        echo "This script must be run as root!"
        exit 1
    fi
}

command_exists() {
    if command -v "$1" >/dev/null 2>&1; then
        return 0 # true
    fi
    return 1
}

install_emacs() {
    EMACS_VERSION="29.2"

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

    # Use GCC 10 if you are on Ubuntu 20.04, not required for Ubuntu 22.04+
    export CC=/usr/bin/gcc-10 CXX=/usr/bin/g++-10
    make distclean
    ./autogen.sh

    ./configure --with-cairo --with-modules --without-compress-install --with-x-toolkit=no --with-gnutls=ifavailable --without-gconf --without-xwidgets --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation=aot --with-json=ifavailable --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm --with-gif --with-threads --with-included-regex --with-zlib --without-sound --with-dbus --without-pop --with-dbus --with-tree-sitter=ifavailable CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer -funroll-loops -floop-parallelize-all" prefix=/usr/local

    # Use NATIVE_FULL_AOT=1 to native compile ahead-of-time all the elisp files included in the
    # Emacs distribution instead of after startup
    make -j"$(nproc)" NATIVE_FULL_AOT=1
    make install

    cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
    rm "${EMACS_FILENAME}" || true
}

setup_emacs() {
    # private.el, textidote, ltex, languagetool, autofmt
    mkdir -p "$HOME/.emacs.d/etc" && cd "$HOME/.emacs.d/etc" || exit
    ln -s "$DOTFILES"/emacs/private.el .
}

# Install important packages. There is nothing to do if a package is already installed.
install_ubuntu_packages() {
    case "${DIST_VERSION}" in
        Ubuntu_18.04)
            apt install -y aspell hunspell libxml2-utils chktex ruby-dev tidy python-pygments python-pip python3-pip imagemagick libmagick++-dev fonts-powerline pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo x11-utils ttf-ancient-fonts libmagickwand-dev cpanminus libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev libncurses5-dev libxt-dev htop unifont xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev libgtk2.0-dev librsvg2-dev gcc libtiff5-dev gnutls-bin libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev libc6-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxrandr-dev libxtst-dev libxv-dev curl libssl-dev wget gpg
            ;;
        Ubuntu_20.04)
            apt install -y aspell hunspell libxml2-utils chktex ruby-dev tidy python-pygments python3-pip composer imagemagick libmagick++-dev fonts-powerline pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo x11-utils ttf-ancient-fonts libmagickwand-dev cpanminus libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev libncurses5-dev libxt-dev htop unifont xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev libgtk2.0-dev librsvg2-dev gcc libtiff5-dev gnutls-bin libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev libc6-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxrandr-dev libxtst-dev libxv-dev curl libssl-dev wget gpg libenchant-2-dev libwebp-dev webp libxft-dev libxft2 cargo entr
            ;;
        Ubuntu_22.04)
            apt install -y aspell hunspell libxml2-utils chktex ruby-dev tidy python3-pip imagemagick libmagick++-dev fonts-powerline pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo x11-utils ttf-ancient-fonts libmagickwand-dev cpanminus libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev libncurses5-dev libxt-dev htop unifont xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev libgtk2.0-dev librsvg2-dev gcc libtiff5-dev gnutls-bin libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev libc6-dev xaw3dg-dev zlib1g-dev libice-dev libsm-dev libxext-dev libxi-dev libxmu-dev libxmuu-dev libxrandr-dev libxtst-dev libxv-dev curl libssl-dev wget gpg libtree-sitter-dev libenchant-2-dev libwebp-dev webp libxft-dev libxft2 cargo entr
            ;;
        *)
            echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
            exit 2
            ;;
    esac
}

install_gcc() {
    case "${DIST_VERSION}" in
        Ubuntu_18.04 | Ubuntu_20.04)
            add-apt-repository ppa:ubuntu-toolchain-r/test -y
            apt install -y gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev gcc-10-multilib g++-10-multilib gcc-9 g++-9
            ;;
        Ubuntu_22.04) ;;
        *)
            echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
            exit 2
            ;;
    esac
}

install_fish() {
    apt-add-repository -y ppa:fish-shell/release-3
    apt install -y fish
}

install_cmake() {
    wget -O - https://apt.kitware.com/keys/kitware-archive-latest.asc 2>/dev/null | gpg --dearmor - | sudo tee /usr/share/keyrings/kitware-archive-keyring.gpg >/dev/null

    case "${DIST_VERSION}" in
        Ubuntu_18.04) echo 'deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ jammy main' | sudo tee /etc/apt/sources.list.d/kitware.list >/dev/null ;;
        Ubuntu_20.04) echo 'deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ focal main' | sudo tee /etc/apt/sources.list.d/kitware.list >/dev/null ;;
        Ubuntu_22.04) echo 'deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ bionic main' | sudo tee /etc/apt/sources.list.d/kitware.list >/dev/null ;;
        *)
            echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
            exit 2
            ;;
    esac

    rm /usr/share/keyrings/kitware-archive-keyring.gpg
    apt install -y kitware-archive-keyring

    apt install cmake
}

install_llvm() {
    LLVM_VER="16"

    case "${DIST_VERSION}" in
        Ubuntu_18.04) REPO_NAME="deb http://apt.llvm.org/bionic/   llvm-toolchain-bionic-${LLVM_VER}  main" ;;
        Ubuntu_20.04) REPO_NAME="deb http://apt.llvm.org/focal/    llvm-toolchain-focal-${LLVM_VER}   main" ;;
        Ubuntu_22.04) REPO_NAME="deb http://apt.llvm.org/jammy/    llvm-toolchain-jammy-${LLVM_VER}   main" ;;
        *)
            echo "Distribution '$DISTRO' in version '$VERSION' is not supported by this script (${DIST_VERSION})."
            exit 2
            ;;
    esac

    wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
    add-apt-repository -y "${REPO_NAME}"

    apt install -y clang-"${LLVM_VER}" lldb-"${LLVM_VER}" lld-"${LLVM_VER}" libllvm-"${LLVM_VER}"-ocaml-dev libllvm"${LLVM_VER}" llvm-"${LLVM_VER}" llvm-"${LLVM_VER}"-dev llvm-"${LLVM_VER}"-doc llvm-"${LLVM_VER}"-examples llvm-"${LLVM_VER}"-runtime clang-tools-"${LLVM_VER}" clang-"${LLVM_VER}"-doc libclang-common-"${LLVM_VER}"-dev libclang-"${LLVM_VER}"-dev libclang1-"${LLVM_VER}" clang-format-"${LLVM_VER}" python3-clang-"${LLVM_VER}" clangd-"${LLVM_VER}" clang-tidy-"${LLVM_VER}" libfuzzer-"${LLVM_VER}"-dev libc++-"${LLVM_VER}"-dev libc++abi-"${LLVM_VER}"-dev libomp-"${LLVM_VER}"-dev libclc-"${LLVM_VER}"-dev libunwind-"${LLVM_VER}"-dev libmlir-"${LLVM_VER}"-dev mlir-"${LLVM_VER}"-tools
}

cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"

if [ ! -d "$GITHUB" ]; then
    mkdir -p "$GITHUB"
    chown -R $USER:$USER "$GITHUB"
fi

# semgrep, ruff-lsp, python-lsp-ruff, pyright
install_python_packages() {
    #sudo -u swarnendu
    python3 -m pip install --upgrade pip pygments setuptools yamllint cmake-language-server cmake-format "python-lsp-server[all]" python-lsp-isort pylsp-mypy pylsp-rope pyls-memestra yapf jedi pylint importmagic pydocstyle cpplint grip konsave autotools-language-server libtmux argcomplete --user
}

# Ubuntu 18 supports node 16
install_node() {
    NODEJS_VER="18"

    case "${DIST_VERSION}" in
        Ubuntu_18.04)
            NODEJS_VER="16"
            ;;
        Ubuntu_20.04 | Ubuntu_22.04) ;;
    esac

    curl -fsSL https://deb.nodesource.com/setup_"${NODEJS_VER}".x | bash -
    apt install -y nodejs
}

install_npm_packages() {
    TMP_HOME="$USER_HOME/tmp"
    NPM_HOME="$TMP_HOME"
    mkdir -p "${NPM_HOME}"
    cd "${NPM_HOME}" || echo "Failed: cd ${NPM_HOME}"

    npm init --yes
    npm install --save-dev less jsonlint bash-language-server markdownlint-cli markdownlint-cli2 yaml-language-server write-good htmlhint prettier @prettier/plugin-xml vscode-langservers-extracted npm-check-updates dockerfile-language-server-nodejs awk-language-server tree-sitter-cli prettier-plugin-awk emmet-ls @emacs-grammarly/grammarly-languageserver

    # Add the following to "$HOME/.bashrc"
    # echo "export NODE_PATH=$HOME/tmp/node_modules" >>"$HOME/.bashrc"

    # cmdline=$"\n\nexport NODE_PATH=\$HOME/tmp/node_modules\n"
    # printf "%s" "$cmdline" >>"$USER_HOME/.bashrc"
}

install_texlab() {
    TEXLAB_VER="5.13.0"

    cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
    wget https://github.com/latex-lsp/texlab/releases/download/v"${TEXLAB_VER}"/texlab-x86_64-linux.tar.gz
    tar xzf texlab-x86_64-linux.tar.gz
    mv texlab "${USER_HOME}/.local/bin"
    rm texlab-x86_64-linux.tar.gz
}

# Update configurations of helper utilities

# HOME Directory
cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"

# $1 is source, $2 is the destination
create_file_symlink() {
    if [ ! -L "$2" ]; then
        echo "Creating symlink for $2..."
        ln -s "$1" "$2"
    else
        echo "Overwriting symlink for $2..."
        ln -nsf "$1" "$2"
    fi
    echo "...Done"
}

create_symlinks() {
    # HOME directory
    sfname="$DOTFILES/markdown/dotmarkdownlint.json"
    dfname="$USER_HOME/.markdownlint.json"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/dotprettierrc"
    dfname="$USER_HOME/.prettierrc"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/tidyrc"
    dfname="$USER_HOME/.tidyrc"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/dotripgreprc"
    dfname="$USER_HOME/.ripgreprc"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/tmux/tmux.conf"
    dfname="$USER_HOME/.tmux.conf"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/dotgitconfig"
    dfname="$USER_HOME/.gitconfig"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/python/pylintrc"
    dfname="$CONFIG_DIR/pylintrc"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/python/yapf"
    dfname="$CONFIG_DIR/yapf"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/yamllint"
    dfname="$CONFIG_DIR/yamllint"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/alacritty"
    dfname="$CONFIG_DIR/alacritty"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/ctags/dotctags.d"
    dfname="${CONFIG_DIR}/.ctags.d"
    create_file_symlink $sfname $dfname

    sfname="$DOTFILES/bat"
    dfname="${CONFIG_DIR}/bat"
    create_file_symlink $sfname $dfname
}

install_shellcheck() {
    SHELLCHECK_VER="0.10.0"
    SHELLCHECK_FILENAME="shellcheck-v${SHELLCHECK_VER}.linux.x86_64"

    cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
    wget https://github.com/koalaman/shellcheck/releases/download/v"${SHELLCHECK_VER}/${SHELLCHECK_FILENAME}.tar.xz"
    tar xf "${SHELLCHECK_FILENAME}.tar.xz"
    cd "shellcheck-v${SHELLCHECK_VER}" || echo "Failed: cd shellcheck-v${SHELLCHECK_VER}"
    mv shellcheck "${USER_HOME}/.local/bin"
    rm -rf "${SHELLCHECK_FILENAME}.tar.xz*"
}

install_shfmt() {
    SHFMT_VER="3.8.0"

    cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"
    wget https://github.com/mvdan/sh/releases/download/v"${SHFMT_VER}/shfmt_v${SHFMT_VER}"_linux_amd64
    mv shfmt_v"${SHFMT_VER}"_linux_amd64 ${USER_HOME}/.local/bin/shfmt
    chmod a+x ${USER_HOME}/.local/bin/shfmt
}

install_ripgrep() {
    RG_VER="14.1.0"

    wget https://github.com/BurntSushi/ripgrep/releases/download/"${RG_VER}/ripgrep_${RG_VER}"_amd64.deb
    dpkg -i ripgrep_"${RG_VER}"_amd64.deb
    rm -rf ripgrep_"${RG_VER}"_amd64.deb*
}

install_cppcheck() {
    apt install libpcre3-dev
    if [ ! -d cppcheck ]; then
        sudo -u swarnendu git clone git@github.com:danmar/cppcheck.git
    else
        cd cppcheck || echo "Failed: cd cppcheck"
        sudo -u swarnendu git pull
        cd ..
    fi

    cd cppcheck || echo "Failed: cd cppcheck"
    git checkout 2.13.0
    mkdir -p build
    cd build || echo "Failed: cd build"
    cmake -DUSE_MATCHCOMPILER=ON -DHAVE_RULES=ON -DUSE_THREADS=ON ..
    cmake --build .
    make install
    cd ../..
    rm -rf cppcheck
}

# Build Universal Ctags, installing snaps seems to hurt Ubuntu performance.
install_ctags() {
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
}

install_global() {
    GLOBAL_VER="6.6.11"
    wget http://tamacom.com/global/global-${GLOBAL_VER}.tar.gz
    tar -xzvf global-${GLOBAL_VER}.tar.gz
    cd global-${GLOBAL_VER} || exit
    ./configure --with-universal-ctags=/usr/local/bin/ctags
    make
    make install
    echo "GTAGSCONF=/usr/local/share/gtags/gtags.conf" >>"$HOME"/.bashrc
    echo "GTAGSLABEL=new-ctags" >>"$HOME"/.bashrc
}

install_alacritty() {
    add-apt-repository ppa:aslatter/ppa -y
    apt install alacritty
}

install_bear() {
    apt install libssl-dev
    if [ ! -d bear ]; then
        sudo -u swarnendu git clone git@github.com:rizsotto/Bear.git bear
    else
        cd bear || echo "Failed: cd bear"
        sudo -u swarnendu git pull
        cd ..
    fi

    cd bear || echo "Failed: cd bear"
    git checkout 3.1.3
    mkdir -p build && cd build || echo "Failed: cd bear/build"
    cmake -DENABLE_UNIT_TESTS=OFF -DENABLE_FUNC_TESTS=OFF ..
    make all
    make install
}

cd "$HOME" || exit

install_powerline() {
    apt install -y socat

    git clone git@github.com:powerline/powerline.git
    cd powerline || exit
    git checkout 2.8.3
    python3 -m pip install --user --editable=.

    wget https://github.com/powerline/powerline/raw/develop/font/PowerlineSymbols.otf
    mkdir -p ~/.local/share/fonts
    mv PowerlineSymbols.otf ~/.local/share/fonts/
    fc-cache -vf ~/.local/share/fonts/

    wget https://github.com/powerline/powerline/raw/develop/font/10-powerline-symbols.conf
    mkdir -p ~/.config/fontconfig/conf.d
    mv 10-powerline-symbols.conf ~/.config/fontconfig/conf.d/

    git clone https://github.com/powerline/fonts.git --depth=1 powerline-fonts
    cd powerline-fonts || exit
    ./install.sh
}

cd "$GITHUB" || echo "Failed: cd $GITHUB"

install_tmux() {
    apt install libevent-dev entr

    if [ -d tmux ]; then
        cd tmux || echo "Failed: cd tmux"
        echo "Pulling tmux reposiory from GitHub..."
        sudo -u swarnendu git pull
    else
        echo "Cloning tmux repository from GitHub..."
        sudo -u swarnendu git clone https://github.com/tmux/tmux
    fi
    echo "...Done"
    chown -R $USER:$USER tmux

    cd tmux || echo "Failed: cd tmux"
    git checkout 3.4
    ./autogen.sh
    ./configure
    make
    make install
}

install_delta() {
    DELTA_VER="0.17.0"
    # Latest releases do not work with Ubuntu 18/20
    if [[ "${DIST_VERSION}" == Ubuntu_20.04 ]]; then
        DELTA_VER="0.14.0"
    fi

    wget https://github.com/dandavison/delta/releases/download/"$DELTA_VER"/git-delta_"$DELTA_VER"_amd64.deb
    dpkg -i git-delta_"$DELTA_VER"_amd64.deb
    rm git-delta_"$DELTA_VER"_amd64.deb
}

install_difft() {
    DIFFT_VER="0.56.1"

    wget https://github.com/Wilfred/difftastic/releases/download/"$DIFFT_VER"/difft-x86_64-unknown-linux-gnu.tar.gz
    tar xzf difft-x86_64-unknown-linux-gnu.tar.gz
    mv difft "${USER_HOME}/.local/bin/."
    rm difft-x86_64-unknown-linux-gnu.tar.gz
}

install_zoxide() {
    Z_VER="0.9.4"

    curl -sS https://raw.githubusercontent.com/ajeetdsouza/zoxide/main/install.sh | bash

    # For bash, add this line to ~/.bashrc
    echo "eval \"$(zoxide init bash)\"" >>"$USER_HOME/.bashrc"

    # For fish, add this line to ~/.config/fish/config.fish
    echo "zoxide init fish | source" >>"$CONFIG_DIR/fish/config.fish"
}

install_bat() {
    BAT_VER="0.24.0"

    wget https://github.com/sharkdp/bat/releases/download/v"$BAT_VER"/bat_"$BAT_VER"_amd64.deb
    dpkg -i bat_"$BAT_VER"_amd64.deb
    rm bat_"$BAT_VER"_amd64.deb
}

install_marksman() {
    MK_VER="2023-12-09"

    wget https://github.com/artempyanykh/marksman/releases/download/"$MK_VER"/marksman-linux-x64
    mv marksman-linux $USER_HOME/.local/bin/marksman
    chmod a+x $USER_HOME/.local/bin/marksman
}

install_fd() {
    FD_VER="9.0.0"

    wget https://github.com/sharkdp/fd/releases/download/v"$FD_VER"/fd_"$FD_VER"_amd64.deb
    dpkg -i fd_"$FD_VER"_amd64.deb
    rm fd_"$FD_VER"_amd64.deb
}

cd $GITHUB || exit

install_fzf() {
    FZF_VER="0.48.1"

    if [ ! -d fzf ]; then
        sudo -u swarnendu git clone https://github.com/junegunn/fzf.git
    else
        cd fzf || echo "Failed: cd fzf"
        sudo -u swarnendu git pull
        cd ..
    fi

    cd fzf || exit
    git checkout ${FZF_VER}
    bash ./install
}

install_perl_server() {
    apt install libanyevent-perl libclass-refresh-perl libdata-dump-perl libio-aio-perl libjson-perl libmoose-perl libpadwalker-perl libscalar-list-utils-perl libcoro-perl
    cpanm Perl::LanguageServer
}

install_cargo_packages() {
    cargo install asm-lsp
    cargo install taplo-cli --features lsp
}

install_nerd_fonts_helper() {
    echo "$1"
    echo "$2"
    wget https://github.com/ryanoasis/nerd-fonts/releases/download/v"$2"/"$1".zip
    mkdir -p "$1"
    unzip "$1".zip -d "$1"
    mv "$1"/*.ttf "$HOME"/.fonts
    rm -rf "$1"

}

install_font() {
    wget https://github.com/ryanoasis/nerd-fonts/releases/download/v"$2"/"$1.tar.xz"
    tar xf "$1.tar.xz"
    mv "$1"/*.ttf $USER_HOME/.local/fonts/.
    fc-cache -v $USER_HOME/.local/fonts/.
}

install_nerd_fonts() {
    NF_VER="3.1.1"

    declare -a FONT_NAMES=("DejaVuSansMono" "FiraCode" "Hack" "Inconsolata" "Iosevka" "Meslo" "Noto" "RobotoMono" "SourceCodePro" "Ubuntu" "UbuntuMono")

    for i in "${FONT_NAMES[@]}"; do
        install_font "$i" "$NF_VER"
    done
}

# Setup 24bit terminal support
setup_24bit() {
    /usr/bin/tic -x -o "$HOME/.terminfo" "${DOTFILES}/xterm-24bit.terminfo"
}

# echo -e $"export LC_ALL=en_US.utf-8\nexport LANG=en_US.utf-8\nexport LANGUAGE=en_US.utf-8\nexport TERM=xterm-24bit" >>"$USER_HOME/.bashrc"

# cmdline=$"export LC_ALL=en_US.utf-8\nexport LANG=en_US.utf-8\nexport LANGUAGE=en_US.utf-8\nexport TERM=xterm-24bit\n"
# printf "%s" "$cmdline" >>"$USER_HOME/.bashrc"

# Remove junk
cleanup() {
    cd "${USER_HOME}" || echo "Failed: cd ${USER_HOME}"

    apt autoremove
    apt autoclean
}

# is_sudo

# if command_exists emacs; then
#     echo "emacs found"
# else
#     echo "emacs not found"
# fi

# install_ubuntu_packages
# install_gcc
# install_llvm
# install_cmake
# install_fish
# install_emacs
# install_ripgrep
# install_cppcheck
# install_ctags
# install_global
# install_alacritty
# install_bear
# install_powerline
# install_tmux
# install_delta
# install_bat
# install_fd
# install_perl_server
# install_node

setup_emacs
setup_24bit
install_python_packages
install_texlab
install_shellcheck
install_shfmt
install_zoxide
install_fzf
install_marksman
install_nerd_fonts
install_difft
create_symlinks
install_npm_packages

# cleanup

# set +eux
