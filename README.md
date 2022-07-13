# GNU Emacs

This repository lists my preferred configuration for GNU Emacs, my primary editor. The setup should work on a GNU/Linux platform, and is not tested on Windows. Most of the included customizations are from the internet. Suggestions and pull requests are welcome.

## Installation

Back up the contents of your `.emacs.d` directory if it is not empty, and then check out the source.

```shell
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

## Install GNU Emacs from PPA

The Emacs version on Ubuntu is usually outdated. Use the [Emacs stable releases](https://launchpad.net/~kelleyk/+archive/ubuntu/emacs) PPA to install the latest stable version.

```shell
sudo add-apt-repository -y ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs28-nativecomp
```

## Build GNU Emacs from source

```shell
sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
# Install gcc-10 packages with required support for libgccjit
sudo apt install -y gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev

wget https://ftp.gnu.org/gnu/emacs/emacs-28.1.tar.xz
tar -xf emacs-28.1.tar.xz
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
make distclean
./autogen.sh
./configure --with-cairo --with-modules --with-x-toolkit=lucid --without-compress-install --with-x-toolkit=no --with-gnutls --without-gconf --without-xwidgets --without-toolkit-scroll-bars --without-xaw3d --without-gsettings --with-mailutils --with-native-compilation --with-json --with-harfbuzz --with-imagemagick --with-jpeg --with-png --with-rsvg --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm --with-gif --with-threads --with-included-regex --with-zlib --without-sound  --with-dbus --without-pop CFLAGS="-O2 -mtune=native -march=native -fomit-frame-pointer" prefix=/usr/local
make -j4 NATIVE_FULL_AOT=1
sudo make install
```

Try the [following](https://lists.gnu.org/archive/html/emacs-devel/2021-04/msg01404.html) if the build fails: `make bootstrap` or `rm lisp/loaddefs.el; make;`.

Evaluate the following to test that both fast JSON (Emacs 27+) and native compilation (Emacs 28+) are working.

```emacs-lisp
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))

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

- `modules` - Emacs modules divided across features
- `extras` - third-party packages (may not be available from the package archives)
- `snippets` - custom snippets
- `references` - documentation and help files
- `dir-locals-examples` - examples to show how to use directory-local variables
- `projectile-examples` - projectile configuration files

The following are examples of a few customization options defined in [`init-config.el`](./modules/init-config.el). Please check the file for more customization options.

- `sb/gui-theme` -- Set the desired GUI theme from a bunch of themes
- `sb/tui-theme` -- Set the desired TUI theme from a bunch of themes
- `sb/modeline-theme` -- Set the desired modeline theme
- `sb/fill-column` -- Column beyond which lines should not extend
- `sb/delete-trailing-whitespace-p` -- Control whether trailing whitespace should be deleted or not

## LSP

We enable LSP support for all languages supported by `lsp-mode`. You may need to install the necessary language servers to complement `lsp-mode`. Use `M-x lsp-install-server` or check `setup-emacs.sh`.

## Support for Tags

Support for LSP in GNU Emacs means you will not need to create tags separately, but the following information may still be useful for languages that are not yet supported by `lsp-mode'. I do not use GNU Global, and instead prefer Universal Ctags.

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

Use Universal Ctags (`u-ctags`) with `counsel-etags` or `company-ctags`.

- `-R` -- recursively scan for files
- `-e` -- use Emacs-compatible syntax
- `--list-excludes` -- check which patterns are excluded from processing
- `--list-languages` -- list supported languages
- `--languages=Python` -- include Python files

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

> LaTeX project: `find . -name "*.tex" | ctags -e -quiet -L -`

> Python files: `find src -name "*.py" | ctags -e -L -`

> Elisp files: `ctags -e -R --exclude=@.ctagsignore --languages=EmacsLisp .`

> **Ignore directories and files**

```shell
ctags -eR --exclude=node_modules --exclude=.meteor --exclude='packages/*/.build/'
```

> **Use an ignore file**

```shell
ctags -eR -quiet=yes --exclude=@.ctagsignore .
```

```shell
$ cat .ctagsignore
dir1
dir2
dir3
```

`find -L . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.cc" -o -iname "*.h" -o -iname "*.hpp" -o -iname "*.cu" | ctags -e -L -`

> **Parse only Python files**

```shell
ctags -eR --languages=Python
```

### Use GNU Global with Universal Ctags support

GNU Global has better database search support while Universal Ctags supports more languages. GNU Global can be built with support for Universal Ctags.

- [Compiling GNU Global with universal-ctags support](https://stackoverflow.com/questions/55073452/compiling-gnu-global-with-universal-ctags-support)
- [Tags for Emacs: Relationship between etags, ebrowse, cscope, GNU Global and exuberant ctags](https://stackoverflow.com/questions/12922526/tags-for-emacs-relationship-between-etags-ebrowse-cscope-gnu-global-and-exub)
- [GTags for Python in Emacs](https://blade6570.github.io/soumyatripathy/blog_gnuglobal/gnu_global.html)

## Configuring Emacs Daemon

Enable server support either through `init.el` or as a `systemd` service. I prefer the `systemd` approach. Create a file `$HOME/.config/systemd/user/emacs.service` with the following content.

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
Exec=/usr/local/bin/emacs
Icon=emacs
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

I use LSP intensively, and LSP+Tramp is sluggish and fails often. Furthermore, it seems difficult to properly setup many language servers with Tramp support. Instead, I prefer to use Emacs in a terminal. which has much better performance. This requires setting up support for 24-bit colors and proper keybindings in the terminal. I use Alacritty which is easy to customize.

Use the steps mentioned in the link [Spacemacs Terminal](https://github.com/syl20bnr/spacemacs/wiki/Terminal) to enable support for 24bit colors in the terminal.

```Bash
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export TERM=xterm-24bit
```

Using `export TERM=xterm-24bit` may lead to failures when accessing remote systems. In such cases, we can fall back to `TERM=xterm-256color ssh -X <remote-path>`.

## Debugging Emacs

- `kill -s USR2 [pid]`
- `killall -s USR2 emacs`
- `pkill -USR2 emacs`

## Profile startup time

`emacs -Q -l /home/swarnendu/github/dotemacs/extras/profile-dotemacs.el -f profile-dotemacs`

Estimate the best possible startup time: `emacs -q --eval='(message "%s" (emacs-init-time))'`

[Advanced Techniques for Reducing Emacs Startup Time](https://blog.d46.us/advanced-emacs-startup/)
