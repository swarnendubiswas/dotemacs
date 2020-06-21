# GNU Emacs

I use GNU Emacs as my primary editor on a GNU/Linux platform. This repository lists my customizations tailored according to my preferences. This setup should work for a GNU/Linux platform.

Most of the included customizations are from the Internet. Suggestions and pull requests are welcome.

## Installation

You might want to back up your contents if your `.emacs.d` directory is not empty. Then, use the following command to check out the source.

```Bash
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

### Dependencies

You can use the following instructions to install a few third-party applications. These should work on an Ubuntu 18.04 distribution.

```Bash
sudo apt install -y aspell libxml2-utils chktex ruby-dev tidy python-pygments python-pip python3-pip cppcheck composer imagemagick lua5.3 liblua5.3-dev luarocks cargo clang-10 clangd-10 clang-{format,tidy,tools}-10 clang-10-doc clang-10-examples llvm-10 lld-10 lldb-10 llvm-10-runtime pandoc fonts-powerline libncurses5-dev fasd pkg-config autoconf automake python3-docutils libseccomp-dev libjansson-dev libyaml-dev libxml2-dev
wget https://github.com/sharkdp/fd/releases/download/v8.0.0/fd_8.0.0_amd64.deb
sudo dpkg -i fd_8.0.0_amd64.deb
sudo snap install shfmt
sudo snap install ripgrep --classic
sudo snap install shellcheck --edge
sudo snap refresh
python -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi pylint python-language-server importmagic pyls-isort setuptools configparser backports-functools_lru_cache yamllint grip --user
python3 -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi pylint python-language-server importmagic pyls-isort setuptools configparser backports-functools_lru_cache yamllint cmake-language-server grip --user
sudo npm i -g npm eslint js-yaml less jsonlint bash-language-server vscode-html-languageserver-bin js-beautify typescript-language-server typescript vscode-css-languageserver-bin intelephense markdownlint-cli yaml-language-server vscode-json-languageserver intelephense stylelint prettier write-good htmlhint
sudo npm update
sudo gem install scss_lint
sudo gem update
composer require jetbrains/phpstorm-stubs:dev-master
composer require felixfbecker/language-server
composer update
cargo install --git https://github.com/latex-lsp/texlab.git
```

The setup also requires Universal Ctags (for `ctags`) and GNU Global (for `gtags`).

```Bash
git clone git@github.com:universal-ctags/ctags.git universal-ctags
cd universal-ctags
./autogen.sh; ./configure; make; sudo make install;
```

```Bash
wget http://tamacom.com/global/global-6.6.4.tar.gz
tar -xzvf global-6.6.4.tar.gz
cd global-6.6.4
./configure --with-universal-ctags=/usr/local/bin/ctags; make; sudo make install;
echo "GTAGSCONF=/usr/local/share/gtags/gtags.conf" >> $HOME/.bashrc
echo "GTAGSLABEL=new-ctags" >> $HOME/.bashrc
```

The setup uses the following configuration files.

> `markdownlint-cli`

```Bash
ln -nsf $HOME/github/dotfiles/markdown/dotmarkdownlint.json $HOME/.markdownlint.json;
```

> `prettier`

```bash
ln -nsf $HOME/github/dotfiles/dotprettierrc $HOME/.prettierrc
```

> `pylint`

```bash
ln -nsf $HOME/github/dotfiles/dotconfig/pylintrc $HOME/.config/pylintrc;
```

> `yapf`

```bash
ln -nsf $HOME/github/dotfiles/dotconfig/yapf $HOME/.config/yapf;
```

I plan to automate the complete setup sometime in the future.

## Directory structure

| Directory         | Purpose                                                               |
| ----------------- | --------------------------------------------------------------------- |
| `extras`          | third-party packages (may not be available from the package archives) |
| `reference-cards` | documentation and help files                                          |
| `snippets`        | custom snippets                                                       |

## Tweaking the default settings

The following are customization options defined in `init.el` that you could use to tweak the default setup. Check the file for more options.

| Custom variable                         | Documentation                                                                                                                   |
| --------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| `dotemacs-theme`                        | Set the desired theme from a bunch of themes like `leuven`, `professional`, `eclipse`, and `zenburn` or use the `default`.      |
| `dotemacs-modeline-theme`               | Set the desired modeline theme from `powerline`, `smart-mode-line`, `spaceline` or `default`.                                   |
| `dotemacs-window-split`                 | Specify the direction in which the windows should be split. This depends on the orientation of the display.                     |
| `dotemacs-fill-column`                  | Column beyond which lines should not extend.                                                                                    |
| `dotemacs-delete-trailing-whitespace-p` | Control whether trailing whitespace should be deleted or not.                                                                   |
| `dotemacs-tags-scheme`                  | Choose whether to use Gtags or Ctags for C/C++ programming. In general, we use `lsp-mode` for supported languages and projects. |

Please check `init.el` for other options.

## Browsing Source Code

Support for `LSP` protocol in GNU Emacs means you will not need to create tags separately, but the following information may still be useful for languages that are currently not yet supported by the `lsp` mode or you cannot create a compilation database.

### GNU Global

Use GNU Global with `counsel-gtags`: `gtags -cv --gtagslabel=new-ctags`

> **C/C++**

```Bash
find -L . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.cc" -o -iname "*.h" -o -iname "*.hpp" -o -iname "*.py" ! -iname "*.cu" | gtags -cv --gtagslabel=new-ctags -f -
```

> **Python**

```Bash
find ./src -type f -iname "*.py" ! -iname "__init__.py" | gtags -cv --gtagslabel=new-ctags -f -
```

> **LaTeX**

```Bash
find . -type f -iname "*.tex" | gtags -vc --gtagslabel=new-ctags -f -
```

> **TensorFlow**

```Bash
find -L . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.cc" -o -iname "*.h" -o -iname "*.hpp" -o -iname "*.proto" | gtags -cv --gtagslabel=new-ctags -f -
```

### Universal Ctags

Use Universal Ctags with `counsel-etags`.

+ `-R` -- recursively scan for files
+ `-e` -- use Emacs-compatible syntax
+ `--list-excludes` -- check which patterns are excluded from processing
+ `--list-languages` -- list supported languages

By default, Emacs expects a tag file by the name `TAGS` in the current directory. Once the tag file is built, the following commands exercise the tag indexing feature.

+ `M-x visit-tags-table <RET> FILE <RET>` -- Select the tag file `FILE` to use.
+ `M-. [TAG] <RET>` -- Find the first definition of `TAG`. The default tag is the identifier under the cursor.
+ `M-*` -- Pop back to where you previously invoked `M-.`.
+ `C-u M-.` -- Find the next definition for the last tag.

For more commands, see the Tags topic in the Emacs info document.

> **TensorFlow**

```Bash
ctags -eR --exclude=*.py --exclude=*.json --exclude=*.js --exclude=bazel-* --exclude=*.sh --exclude=*.xml --exclude=*.java --exclude=*.html --exclude=*.md --exclude=*.pbtxt
```

> **C/C++ projects**

```Bash
ctags -eR --exclude=*.py --exclude=*.json --exclude=*.js --exclude=build* --exclude=*.sh --exclude=*.xml --exclude=*.java --exclude=*.html --exclude=*.md --exclude=*.pbtxt --exclude=*.png --exclude=*.css --exclude=*.rst --exclude=doc --exclude=PTRacer-solver
```

> **Ignore directories and files**

```Bash
ctags -eR --exclude=node_modules --exclude=.meteor --exclude='packages/*/.build/'
```

> **Use an ignore file**

```Bash
ctags -eR --exclude=@.ctagsignore .
```

```bash
$ cat .ctagsignore
dir1
dir2
dir3
```

> **Parse only Python files**

```Bash
ctags -eR --languages=Python
```

### Use GNU Global with Universal Ctags support

GNU Global has better database search support while Universal Ctags supports more languages. It is possible to build Global with support for Universal Ctags.

+ <https://stackoverflow.com/questions/55073452/compiling-gnu-global-with-universal-ctags-support>
+ <https://stackoverflow.com/questions/12922526/tags-for-emacs-relationship-between-etags-ebrowse-cscope-gnu-global-and-exub/15169556#15169556>

## TODO

+ Use custom major modes for files by names
+ Check if a Markdown formatter other than Prettier is available
