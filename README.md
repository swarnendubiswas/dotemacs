# GNU Emacs

This repository lists my preferred customizations for GNU Emacs, which is my primary editor. Most of the included customizations are from the internet. The setup should work on a GNU/Linux platform.

Suggestions and pull requests are welcome.

## Installation

You might want to back up your contents if your `.emacs.d` directory is not empty. Then, use the following command to check out the source.

```shell
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

### Dependencies

Use the following instructions to install third-party applications. Add LLVM 13 sources based on your distribution.

```shell
wget https://github.com/sharkdp/fd/releases/download/v8.2.1/fd_8.2.1_amd64.deb
sudo dpkg -i fd_8.2.1_amd64.deb
```

#### Ubuntu 18.04 Packages

```shell
export LLVM_VERSION="-13"
sudo apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python-pip python3-pip cppcheck composer imagemagick lua5.3 liblua5.3-dev luarocks cargo clang$LLVM_VERSION clangd$LLVM_VERSION clang-{format,tidy,tools}$LLVM_VERSION clang$LLVM_VERSION-doc clang$LLVM_VERSION-examples llvm$LLVM_VERSION lld$LLVM_VERSION lldb$LLVM_VERSION llvm$LLVM_VERSION-runtime pandoc fonts-powerline libncurses5-dev fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo htop x11-utils unifont xfonts-terminus ttf-anonymous-pro libperl-dev cpanminus
wget https://github.com/sharkdp/fd/releases/download/v8.2.1/fd_8.2.1_amd64.deb
sudo dpkg -i fd_8.2.1_amd64.deb
```

#### Ubuntu 20.04 Packages

```Bash
export LLVM_VERSION="-13"
sudo apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python3-pip cppcheck composer imagemagick lua5.3 liblua5.3-dev luarocks cargo clang$LLVM_VERSION clangd$LLVM_VERSION clang-{format,tidy,tools}$LLVM_VERSION clang$LLVM_VERSION-doc clang$LLVM_VERSION-examples llvm$LLVM_VERSION lld$LLVM_VERSION lldb$LLVM_VERSION llvm$LLVM_VERSION-runtime pandoc fonts-powerline libncurses5-dev fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev autojump texinfo htop x11-utils unifont ttf-ancient-fonts xfonts-terminus ttf-anonymous-pro libperl-dev libmagickwand-dev
```

#### Snap Packages

```Bash
sudo snap install shfmt
sudo snap install ripgrep --classic
sudo snap install shellcheck --edge
```

#### Python Packages

```Bash
python3 -m pip install --upgrade pip Sphinx pygments yapf jedi pylint importmagic setuptools configparser yamllint cmake-language-server grip data-science-types cpplint --user
```

#### Other Packages

```shell
sudo gem install scss_lint
composer require jetbrains/phpstorm-stubs:dev-master
composer require felixfbecker/language-server
cargo install --git https://github.com/latex-lsp/texlab.git
sudo cpanm Perl::LanguageServer
```

#### `Node.js` Packages

> Install node packages locally

```shell
cd $HOME/tmp; cd;
npm init --yes;
npm install --save-dev npm less jsonlint bash-language-server vscode-html-languageserver-bin typescript-language-server typescript vscode-css-languageserver-bin intelephense markdownlint-cli markdownlint-cli2 yaml-language-server vscode-json-languageserver intelephense write-good htmlhint javascript-typescript-langserver pyright unofficial-grammarly-language-server-2 @emacs-grammarly/keytar-cli unified-language-server prettier @prettier/plugin-php
npm install git+https://gitlab.com/matsievskiysv/math-preview --save-dev
```

#### Update helper packages

```shell
cd; sudo apt update; sudo snap refresh; sudo gem update; cd $HOME/tmp; composer update; npm update; cd textlint-workspace; npm update; cd;
```

#### Edit Bash files

Add the following definitions to `$HOME/.bashrc`.

```shell
echo "export TERM=xterm-256color # Improve Emacs colors in the terminal" >> $HOME/.bashrc
echo "export ALTERNATE_EDITOR=emacs EDITOR=emacs VISUAL=emacs" >> $HOME/.bashrc
echo "export NODE_PATH=/usr/local/lib/node_modules" >> $HOME/.bashrc
```

#### Universal Ctags

The setup supports using Universal Ctags.

Install using Snap: `sudo snap install universal-ctags`.

```shell
git clone git@github.com:universal-ctags/ctags.git universal-ctags
cd universal-ctags
./autogen.sh; ./configure; make; sudo make install;
```

#### GNU Global

```shell
wget http://tamacom.com/global/global-6.6.7.tar.gz
tar -xzvf global-6.6.7.tar.gz
cd global-6.6.7
./configure --with-universal-ctags=/usr/local/bin/ctags; make; sudo make install;
echo "GTAGSCONF=/usr/local/share/gtags/gtags.conf" >> $HOME/.bashrc
echo "GTAGSLABEL=new-ctags" >> $HOME/.bashrc
```

The setup uses the following configuration files.

> `markdownlint-cli`

```shell
ln -nsf $HOME/github/dotfiles/markdown/dotmarkdownlint.json $HOME/.markdownlint.json;
```

> `prettier`

```shell
ln -nsf $HOME/github/dotfiles/dotprettierrc $HOME/.prettierrc
```

> `pylint`

```shell
ln -nsf $HOME/github/dotfiles/dotconfig/pylintrc $HOME/.config/pylintrc
```

> `yapf`

```shell
ln -nsf $HOME/github/dotfiles/dotconfig/yapf $HOME/.config/yapf
```

I plan to automate the complete setup sometime in the future.

## Directory structure

- `modules` - setup modules split across files
- `extras` - third-party packages (may not be available from the package archives)
- `dir-locals-examples` - examples to show how to use directory-local variables
- `projectile-examples` - projectile configuration files
- `snippets` - custom snippets
- `reference-cards` - documentation and help files

The following are a few customization options defined in `init.el` that you could use to tweak the default setup. Please check `init.el` for more options.

- `dotemacs-gui-theme` -- Set the desired GUI theme from a bunch of themes
- `dotemacs-tui-theme` -- Set the desired TUI theme from a bunch of themes
- `dotemacs-modeline-theme` -- Set the desired modeline theme
- `dotemacs-fill-column` -- Column beyond which lines should not extend
- `dotemacs-delete-trailing-whitespace-p` -- Control whether trailing whitespace should be deleted or not

## Browsing Source Code

Support for LSP in GNU Emacs means you will not need to create tags separately, but the following information may still be useful for languages that are not yet supported by the `lsp` mode, or you cannot create a compilation database.

### GNU Global

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

### Universal Ctags

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

Create a `emacsclient.desktop` file in `$HOME/.local/share/applications/` with the following content.

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

## Build GNU Emacs from source

```shell
sudo apt install -y texinfo libjpeg-dev libtiff-dev libgif-dev libxpm-dev libgtk-3-dev gnutls-dev libncurses5-dev libxml2-dev libxt-dev aspell libxml2-utils chktex libjansson-dev libyaml-dev libxml2-dev autojump htop x11-utils unifont  xfonts-terminus ttf-anonymous-pro libperl-dev libpng-dev libx11-dev automake autoconf libgtk2.0-dev librsvg2-dev libmagickwand-dev gcc libtiff5-dev libgnutls28-dev libharfbuzz-dev libharfbuzz-bin libwebkit2gtk-4.0-dev libxaw7-dev
git clone git://git.sv.gnu.org/emacs.git
./configure --with-cairo --with-modules --with-x-toolkit=lucid --without-compress-install --with-x-toolkit=no --with-gnutls --without-gconf --without-xwidgets --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation --with-json --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm --with-gif --with-threads --with-included-regex --with-zlib --without-sound --without-pop CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" prefix=/usr/local
make -j2
sudo make install
```

## GCCEmacs

```shell
sudo add-apt-repository ppa:ubuntu-toolchain-r/ppa
sudo apt install gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev
git clone git://git.sv.gnu.org/emacs.git gccemacs
git checkout feature/native-comp
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
./configure --with-cairo --with-modules --with-x-toolkit=lucid --without-compress-install --with-x-toolkit=no --with-gnutls --without-gconf --without-xwidgets --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation --with-json --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm --with-gif --with-threads --with-included-regex --with-zlib --without-sound --without-pop CFLAGS="-O3 -mtune=native -march=native -fomit-frame-pointer" prefix=/usr/local
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

## Emacs NG

Test Emacs NG: `(featurep 'emacs-ng)` should return `t`

## Debugging Emacs

- `kill -s USR2 [pid]`
- `killall -s USR2 emacs`
- `pkill -USR2 emacs`

## Profile startup time

`emacs -Q -l /home/swarnendu/github/dotemacs/extras/profile-dotemacs.el -f profile-dotemacs`

## Emacs in a Terminal

Use the steps mentioned in the link [Spacemacs Terminal](https://github.com/syl20bnr/spacemacs/wiki/Terminal), including enabling support for 24bit colors in the terminal.

```Bash
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

# Check whether xterm files are present
ll /lib/terminfo/x/*
ll /usr/share/terminfo/*
mkdir -p $HOME/.terminfo/x
export TERM=xterm-24bit
```

This may lead to failures when accessing remote systems. In such cases, we can fall back to "TERM=xterm-256color ssh -X <remote-path>".

## TODO

- Resolve xml lsp over tramp, not working
- Cursor loses its place after formatting with YAPF
- Integrate `company-yasnippet` and assign a key to `yas-expand-all`
