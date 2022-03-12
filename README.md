# GNU Emacs

This repository lists my preferred configuration for GNU Emacs, my primary editor. The setup should work on a GNU/Linux platform.

Most of the included customizations are from the internet. Suggestions and pull requests are welcome.

## Installation

Back up the contents of your `.emacs.d` directory if it is not empty. Use the following command to check out the source.

```shell
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

### Dependencies

Use the following instructions to install third-party applications. Add recent LLVM sources based on your distribution.

```shell
wget https://github.com/sharkdp/fd/releases/download/v8.3.2/fd_8.3.2_amd64.deb
sudo dpkg -i fd_8.3.2_amd64.deb
```

The following is for Ubuntu 18.04.

```shell
export LLVM_VERSION="-13"
sudo apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python-pip python3-pip cppcheck composer imagemagick lua5.3 liblua5.3-dev luarocks cargo clang$LLVM_VERSION clangd$LLVM_VERSION clang-{format,tidy,tools}$LLVM_VERSION clang$LLVM_VERSION-doc clang$LLVM_VERSION-examples llvm$LLVM_VERSION lld$LLVM_VERSION lldb$LLVM_VERSION llvm$LLVM_VERSION-runtime pandoc fonts-powerline libncurses5-dev fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo htop x11-utils unifont xfonts-terminus ttf-anonymous-pro libperl-dev libmagickwand-dev cpanminus texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev gnutls-dev libncurses5-dev libxml2-dev libxt-dev aspell libxml2-utils chktex libjansson-dev libyaml-dev libxml2-dev autojump htop x11-utils unifont  xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev automake autoconf libgtk2.0-dev librsvg2-dev libmagickwand-dev gcc libtiff5-dev libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev 
```

The following is for Ubuntu 20.04.

```Bash
export LLVM_VERSION="-13"
sudo apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python3-pip cppcheck composer imagemagick lua5.3 liblua5.3-dev luarocks cargo clang$LLVM_VERSION clangd$LLVM_VERSION clang-{format,tidy,tools}$LLVM_VERSION clang$LLVM_VERSION-doc clang$LLVM_VERSION-examples llvm$LLVM_VERSION lld$LLVM_VERSION lldb$LLVM_VERSION llvm$LLVM_VERSION-runtime pandoc fonts-powerline libncurses5-dev fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo htop x11-utils unifont ttf-ancient-fonts xfonts-terminus ttf-anonymous-pro libperl-dev libmagickwand-dev cpanminus texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev gnutls-dev libncurses5-dev libxml2-dev libxt-dev aspell libxml2-utils chktex libjansson-dev libyaml-dev libxml2-dev autojump htop x11-utils unifont  xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev automake autoconf libgtk2.0-dev librsvg2-dev libmagickwand-dev gcc libtiff5-dev libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev
```

```Bash
sudo snap install shfmt
sudo snap install ripgrep --classic
sudo snap install shellcheck --edge

python3 -m pip install --upgrade pip Sphinx pygments yapf jedi pylint importmagic setuptools configparser yamllint cmake-language-server grip data-science-types cpplint --user

sudo gem install scss_lint

composer require jetbrains/phpstorm-stubs:dev-master
composer require felixfbecker/language-server

cargo install --git https://github.com/latex-lsp/texlab.git

sudo cpanm Perl::LanguageServer

cd $HOME/tmp; cd;
npm init --yes;

npm install --save-dev npm less jsonlint bash-language-server vscode-html-languageserver-bin typescript-language-server typescript vscode-css-languageserver-bin intelephense markdownlint-cli markdownlint-cli2 yaml-language-server vscode-json-languageserver intelephense write-good htmlhint javascript-typescript-langserver pyright unofficial-grammarly-language-server-2 @emacs-grammarly/keytar-cli unified-language-server prettier @prettier/plugin-php

npm install git+https://gitlab.com/matsievskiysv/math-preview --save-dev
```

> Update helper packages.

```shell
cd; sudo apt update; sudo snap refresh; sudo gem update; cd $HOME/tmp; composer update; npm update; cd textlint-workspace; npm update; cd;
```

Add the following definitions to `$HOME/.bashrc`.

```shell
echo "export TERM=xterm-256color # Improve Emacs colors in the terminal" >> $HOME/.bashrc
echo "export ALTERNATE_EDITOR=emacs EDITOR=emacs VISUAL=emacs" >> $HOME/.bashrc
echo "export NODE_PATH=$HOME/tmp/node_modules" >> $HOME/.bashrc
```

The setup supports using Universal Ctags. Install using Snap: `sudo snap install universal-ctags`.

```shell
git clone git@github.com:universal-ctags/ctags.git universal-ctags
cd universal-ctags
./autogen.sh; ./configure; make; sudo make install;
```

The setup now does not use GNU Global.

```shell
wget http://tamacom.com/global/global-6.6.7.tar.gz
tar -xzvf global-6.6.7.tar.gz
cd global-6.6.7
./configure --with-universal-ctags=/usr/local/bin/ctags; make; sudo make install;
echo "GTAGSCONF=/usr/local/share/gtags/gtags.conf" >> $HOME/.bashrc
echo "GTAGSLABEL=new-ctags" >> $HOME/.bashrc
```

The setup uses the following configuration files.

```shell
# markdownlint-cli
ln -nsf $HOME/github/dotfiles/markdown/dotmarkdownlint.json $HOME/.markdownlint.json;

# prettier
ln -nsf $HOME/github/dotfiles/dotprettierrc $HOME/.prettierrc

# pylint
ln -nsf $HOME/github/dotfiles/dotconfig/pylintrc $HOME/.config/pylintrc

# yapf
ln -nsf $HOME/github/dotfiles/dotconfig/yapf $HOME/.config/yapf
```

## Build GNU Emacs from source

Add the following PPA for Ubuntu 18.04.

```shell
sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
```

Add the following PPA for Ubuntu 20.04.

```shell
sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
```

Install `gcc-10` packages with required support for `libgccjit`: `sudo apt install -y gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev`.

```shell
# Choose the source folder
tar -xf emacs-28.0.91.tar.xz
# git clone git://git.sv.gnu.org/emacs.git
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
./configure --with-cairo --with-modules --with-x-toolkit=lucid --without-compress-install --with-x-toolkit=no --with-gnutls --without-gconf --without-xwidgets --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation --with-json --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm --with-gif --with-threads --with-included-regex --with-zlib --without-sound  --with-dbus --without-pop CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" prefix=/usr/local
make -j2 NATIVE_FULL_AOT=1
sudo make install
```

Try the [following](https://lists.gnu.org/archive/html/emacs-devel/2021-04/msg01404.html) if the build fails: `make bootstrap` or `rm lisp/loaddefs.el; make;`.

Evaluate the following to test that both fast JSON and native compilation are working.

```emacs-lisp
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))
```

Evaluate the following to test fast JSON is working.

```emacs-lisp
(if (functionp 'json-serialize)
  (message "Native JSON is available")
(message "Native JSON is *not* available"))
```

Run the following to native compile all Elisp files under a directory.

`(native-compile-async "/home/swarnendu/.emacs.d/elpa" 'recursively)`

- [Native compilation and "pure" GTK in Emacs](http://www.cesarolea.com/posts/emacs-native-compile/)
- [My Emacs Flatpak](https://github.com/fejfighter/pgtk-emacs-flatpak)
- <https://emacs.stackexchange.com/questions/59538/compile-emacs-from-feature-native-comp-gccemacs-branch-on-ubuntu>

## Directory structure

- `extras` - third-party packages (may not be available from the package archives)
- `snippets` - custom snippets
- `references` - documentation and help files
- `dir-locals-examples` - examples to show how to use directory-local variables
- `projectile-examples` - projectile configuration files

The following examples of customization options defined in [`init.el`](./init-use-package.el) that you could use to tweak the default setup. Please check [`init.el`](./init-use-package.el) for more options.

- `sb/gui-theme` -- Set the desired GUI theme from a bunch of themes
- `sb/tui-theme` -- Set the desired TUI theme from a bunch of themes
- `sb/modeline-theme` -- Set the desired modeline theme
- `sb/fill-column` -- Column beyond which lines should not extend
- `sb/delete-trailing-whitespace-p` -- Control whether trailing whitespace should be deleted or not

## Browsing Source Code

Support for LSP in GNU Emacs means you will not need to create tags separately, but the following information may still be useful for languages that are not yet supported by the `lsp` mode, or you cannot create a compilation database.

Use GNU Global with `counsel-gtags`: `gtags -cv --gtagslabel=new-ctags`

> **C/C++**

```shell
find -L . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.cc" -o -iname "*.h" -o -iname "*.hpp" -o -iname "*.py" ! -iname "*.cu" | gtags -cv --gtagslabel=new-ctags -f -
```

> **Python**

```shell
find ./src -type f -iname "*.py" ! -iname "__init__.py" | gtags -cv --gtagslabel=new-ctags -f -
```

> **LaTeX**

```shell
find . -type f -iname "*.tex" | gtags -vc --gtagslabel=new-ctags -f -
```

> **TensorFlow**

```shell
find -L . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.cc" -o -iname "*.h" -o -iname "*.hpp" -o -iname "*.proto" | gtags -cv --gtagslabel=new-ctags -f -
```

Use Universal Ctags with `counsel-etags`.

- `-R` -- recursively scan for files
- `-e` -- use Emacs-compatible syntax
- `--list-excludes` -- check which patterns are excluded from processing
- `--list-languages` -- list supported languages

By default, Emacs expects a tag file by the name `TAGS` in the current directory. Once the tag file is built, the following commands exercise the tag indexing feature.

- `M-x visit-tags-table <RET> FILE <RET>` -- Select the tag file `FILE` to use
- `M-. [TAG] <RET>` -- Find the first definition of `TAG`. The default tag is the identifier under the cursor
- `M-*` -- Pop back to where you invoked `M-.`
- `C-u M-.` -- Find the next definition for the last tag

For more commands, see the Tags topic in the Emacs info document.

> **TensorFlow**

```shell
ctags -eR --exclude=*.py --exclude=*.json --exclude=*.js --exclude=bazel-* --exclude=*.sh --exclude=*.xml --exclude=*.java --exclude=*.html --exclude=*.md --exclude=*.pbtxt
```

> **C/C++ projects**

```shell
ctags -eR --exclude=*.py --exclude=*.json --exclude=*.js --exclude=build* --exclude=*.sh --exclude=*.xml --exclude=*.java --exclude=*.html --exclude=*.md --exclude=*.pbtxt --exclude=*.png --exclude=*.css --exclude=*.rst --exclude=doc --exclude=PTRacer-solver
```

> **Ignore directories and files**

```shell
ctags -eR --exclude=node_modules --exclude=.meteor --exclude='packages/*/.build/'
```

> **Use an ignore file**

```shell
ctags -eR --exclude=@.ctagsignore .
```

```shell
$ cat .ctagsignore
dir1
dir2
dir3
```

> **Parse only Python files**

```shell
ctags -eR --languages=Python
```

### Use GNU Global with Universal Ctags support

GNU Global has better database search support while Universal Ctags supports more languages. GNU Global can be built with support for Universal Ctags.

- [Compiling GNU Global with universal-ctags support](https://stackoverflow.com/questions/55073452/compiling-gnu-global-with-universal-ctags-support)
- [Tags for Emacs: Relationship between etags, ebrowse, cscope, GNU Global and exuberant ctags](https://stackoverflow.com/questions/12922526/tags-for-emacs-relationship-between-etags-ebrowse-cscope-gnu-global-and-exub)
- [GTags for Python in Emacs](https://blade6570.github.io/soumyatripathy/blog_gnuglobal/gnu_global.html)

## Server Support

Enable server support either through `init.el` or as a `systemd` service. Create a file `$HOME/.config/systemd/user/emacs.service` with the following content.

```config
[Unit]
Description=GNU Emacs Daemon

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval "(progn (setq kill-emacs-hook 'nil) (kill-emacs))"
Restart=always

[Install]
WantedBy=default.target
```

- Enable the unit to start at login: `systemctl --user enable emacs.service`
- Disable the unit to start at login: `systemctl --user disable emacs.service`
- Start the service for the current session: `systemctl --user start emacs.service`
- Stop the service for the current session: `systemctl --user stop emacs.service`
- Restart the service for the current session: `systemctl --user restart emacs.service`

## Desktop Entry

Create `emacs.desktop` and `emacsclient.desktop` files in `$HOME/.local/share/applications/` with the following content.

```config
[Desktop Entry]
Name=GNU Emacs
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=/home/swarnendu/software/emacs-28.0.91/src/emacs
Icon=/home/swarnendu/software/emacs-28.0.91/etc/images/icons/hicolor/scalable/apps/emacs.svg
Type=Application
Terminal=false
Categories=Development;TextEditor;Utility;
StartupWMClass=Emacs
Keywords=Text;Editor;
```

```config
[Desktop Entry]
Name=GNU Emacsclient
GenericName=Text Editor
Comment=Edit text
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=emacsclient -c -a "" -n -F "'(fullscreen . maximized)" %f
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;Utility;
StartupWMClass=Emacs
Keywords=Text;Editor;
```

- [Running Emacs](https://tychoish.com/post/running-emacs/)

## Emacs in a Terminal

Use the steps mentioned in the link [Spacemacs Terminal](https://github.com/syl20bnr/spacemacs/wiki/Terminal), including enabling support for 24bit colors in the terminal.

```Bash
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export TERM=xterm-24bit
```

This may lead to failures when accessing remote systems. In such cases, we can fall back to "TERM=xterm-256color ssh -X <remote-path>".

## Debugging Emacs

- `kill -s USR2 [pid]`
- `killall -s USR2 emacs`
- `pkill -USR2 emacs`

## Profile startup time

`emacs -Q -l /home/swarnendu/github/dotemacs/extras/profile-dotemacs.el -f profile-dotemacs`

## TODO

- Improve return key in ivy minibuffer map in TUI
- Fix "C-\`" vterm keybinding in TUI
- Format-all is not working automatically with `.md` files
- Company fuzzy is not working with LaTeX files

## Use `conda`

```shell
conda create --prefix /home/hangingpawns/emacs_lsp

conda activate /home/hangingpawns/emacs_lsp

conda install clang clangd libclang bear

export PATH=/home/hangingpawns/emacs_lsp/bin (or wherever the bin is)

Do the same with ld_library_path
```
