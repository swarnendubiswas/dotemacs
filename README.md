# GNU Emacs

I use GNU Emacs as my primary editor on a GNU/Linux platform. This repository lists my GNU Emacs customizations, tailored according to my preferences and all in one place. This setup should work for a GNU/Linux platform.

Most of the included customizations are collected from the Internet. Suggestions and pull requests are welcome.

## Installation

You might want to backup your contents if your `.emacs.d` directory is not empty. Then, use the following command to check out the source:

```Bash
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

### Dependencies

A few third-party executables and libraries are required to complement the setup. You can use the following instructions to install them on an Ubuntu 18.04 distribution.

``` Bash
sudo apt install -y aspell global libxml2-utils chktex ruby-dev tidy python-pygments python-pip python3-pip npm cppcheck composer imagemagick lua5.3 liblua5.3-dev luarocks cargo clang-9 clangd-9 clang-{format,tidy,tools}-9 clang-9-doc clang-9-examples llvm-9 lld-9 lldb-9 llvm-9-runtime pandoc
sudo snap install shfmt
sudo snap install universal-ctags
sudo snap install ripgrep --classic
sudo snap install shellcheck --edge
sudo snap refresh
python -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi==0.15.2 pylint python-language-server importmagic pyls-isort setuptools configparser==3.8.1 backports-functools_lru_cache yamllint --user
python3 -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi==0.15.2 pylint python-language-server importmagic pyls-isort setuptools configparser backports-functools_lru_cache yamllint cmake-language-server --user
sudo npm i -g npm eslint js-yaml less jsonlint bash-language-server vscode-html-languageserver-bin js-beautify typescript-language-server typescript vscode-css-languageserver-bin intelephense markdownlint-cli yaml-language-server vscode-json-languageserver intelephense stylelint
sudo npm update
sudo gem install scss_lint
sudo gem update
composer require jetbrains/phpstorm-stubs:dev-master
composer require felixfbecker/language-server
composer update
luarocks install --server=http://luarocks.org/dev digestif --local
cargo install --git https://github.com/latex-lsp/texlab.git
```

The setup uses the following configuration files.

+ Markdownlint-cli - `$HOME/.markdownlint.json`
+ Pylint - `$HOME/.config/pylintrc`
+ YAPF - `$HOME/.config/yapf`

I plan to automate the complete setup sometime in the future.

## Directory structure

+ `extras` -- third-party packages (may not be available from the package archives)
+ `modules` -- Elisp modules containing personal customizations
+ `reference-cards` -- documentation and help files
+ `snippets` -- custom snippets

## Tweaking the default settings

The following are customization options defined in `init.el` that you could use to tweak the default setup. Check the file for more options.

+ `dotemacs-theme` - Set the desired theme from a bunch of themes like`leuven`, `professional`, and `eclipse`, or use the `default`.
+ `dotemacs-modeline-theme` - Set the desired modeline theme from `powerline`, `smart-mode-line`, `spaceline` or `default`.
+ `dotemacs-window-split` - Specify the direction in which the windows should be split. This depends on the orientation of the display.
+ `dotemacs-fill-column` - Column beyond which lines should not extend.
+ `dotemacs-delete-trailing-whitespace-p` - Control whether trailing whitespace should be deleted or not.
+ `dotemacs-tags` - Choose whether to use Gtags or CTags for C/C++ programming. In general, we use LSP for supported languages and projects.

## Browsing Source

Support for `LSP` protocol in GNU Emacs means you mostly will not need to create tags separately, but the following information may still be useful for languages that are currently not yet supported by the `lsp` mode or you cannot create a compilation database.

### GTags

Use GNU Global with `counsel-gtags`.

#### Examples

``` Bash
find -L . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.cc" -o -iname "*.h" -o -iname "*.hpp" -o -iname "*.py" ! -iname "*.cu" | gtags -v -f -
```

``` Bash
find ./src -type f -iname "*.py" ! -iname "__init__.py" | gtags -v -f -
```

```Bash
find . -type f -iname "*.tex" | gtags -v -f -
```

+ TensorFlow - `find -L . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.cc" -o -iname "*.h" -o -iname "*.hpp" -o -iname "*.proto" | gtags -cv --gtagslabel=new-ctags -f -`

### Universal CTags

Use Universal CTags with `counsel-etags`. Use `ctags -eR` to recursively scan for files (`R`) and use Emacs-compatible syntax (`e`). You can use `--list-excludes` and `--list-languages` to check which patterns are excluded from processing and which languages are supported.

Emacs will, by default, expect a tag file by the name `TAGS` in the current directory. Once the tag file is built, the following  commands  exercise the tag indexing feature:

+ `M-x visit-tags-table <RET> FILE <RET>` - Select the tag file `FILE` to use.
+ `M-. [TAG] <RET>` - Find the first definition of `TAG`. The default tag is the identifier under the cursor.
+ `M-*` - Pop back to where you previously invoked `M-.`.
+ `C-u M-.` - Find the next definition for the last tag.

For more commands, see the Tags topic in the Emacs info document.

#### Examples

+ TensorFlow - `ctags -eR --exclude=*.py --exclude=*.json --exclude=*.js --exclude=bazel-* --exclude=*.sh --exclude=*.xml --exclude=*.java --exclude=*.html --exclude=*.md --exclude=*.pbtxt`

+ C/C++ projects - `ctags -eR --exclude=*.py --exclude=*.json --exclude=*.js --exclude=build* --exclude=*.sh --exclude=*.xml --exclude=*.java --exclude=*.html --exclude=*.md --exclude=*.pbtxt --exclude=*.png --exclude=*.css --exclude=*.rst --exclude=doc --exclude=PTRacer-solver`

+ `ctags -eR --exclude=node_modules --exclude=.meteor --exclude='packages/*/.build/'`

+ `ctags -eR --exclude=@.ctagsignore .` with the following in `.ctagsignore`

dir1
dir2
dir3

+ `ctags -eR --languages=Python`

### Using GNU Global with Universal CTags support

GNU Global has better database search support while Ctags supports many languages. It is possible to build Global with Ctags support.

+ <https://stackoverflow.com/questions/55073452/compiling-gnu-global-with-universal-ctags-support>
+ <https://stackoverflow.com/questions/12922526/tags-for-emacs-relationship-between-etags-ebrowse-cscope-gnu-global-and-exub/15169556#15169556>

## TODO

+ Use `xref` interface for both `ctags` and `gtags`.
+ Use RE in `find`, it follows Emacs RE.
