# GNU Emacs

I use GNU Emacs as my primary editor on a GNU/Linux platform. This repository lists my GNU Emacs customizations, tailored according to my preferences and all in one place. This setup is tested **for** a GNU/Linux platform.

<!-- Other than text, Markdown, and Elisp editing, I use GNU Emacs for editing C/C++, LaTeX, and Python files. The current setup should work fine for these major modes, but is not well-tuned for programming with Java. I think other IDEs such as Eclipse and Visual Studio Code are way better for working with Java. -->

<!-- * Python - I use Elpy and Gtags (and its variants) for code completion
* C/C++ - I use irony mode and Gtags for code completion. Ctags should also work depending on preference. -->

Most of the included customizations are collected from the Internet. Suggestions and pull requests are welcome.

## Installation

You might want to backup your contents if your `.emacs.d` directory is not empty. Then, use the following command to check out the source:

```Bash
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

### Dependencies

Unfortunately, quite a few third-party executables and libraries need to be installed to complement the setup. You can use the following instructions to install them on a Ubuntu 18.04 distribution.

``` Bash
sudo apt install aspell global exuberant-ctags libxml2-utils chktex ruby-dev tidy python-pygments python-pip python3-pip npm cppcheck composer clang-format imagemagick lua5.3 liblua5.3-dev luarocks
sudo snap install shfmt
sudo snap install universal-ctags
sudo snap install ripgrep --classic
sudo snap install shellcheck --edge
sudo snap refresh
python -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi==0.15.2 pylint rope python-language-server[all] pycodestyle flake8 autopep8 importmagic pyls-isort pydocstyle setuptools configparser==3.8.1 backports-functools_lru_cache yamllint --user
python3 -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi==0.15.2 pylint rope python-language-server[all] pycodestyle flake8 autopep8 importmagic pyls-isort pydocstyle setuptools configparser==3.8.1 backports-functools_lru_cache yamllint cmake-language-server --user
sudo npm i -g npm eslint js-yaml less jsonlint bash-language-server vscode-html-languageserver-bin js-beautify typescript-language-server typescript vscode-css-languageserver-bin intelephense markdownlint-cli yaml-language-server vscode-json-languageserver intelephense
sudo npm i -g --unsafe-perm bash-language-server
sudo npm i -g stylelint --save-dev
sudo npm update
sudo gem install scss_lint
sudo gem update
composer require jetbrains/phpstorm-stubs:dev-master
composer require felixfbecker/language-server
composer update
luarocks install --server=http://luarocks.org/dev digestif --local
cargo install --git https://github.com/latex-lsp/texlab.git
```

I plan to automate the complete setup sometime in the future.

## Directory structure

* `extras` -- third-party packages (may not be available from the package archives)
* `modules` -- Elisp modules containing personal customizations
* `reference-cards` -- documentation and help files
* `snippets` -- custom snippets

## Tweaking the default settings

The following are customization options defined in `init.el` that you could use to tweak the default setup. Check the file for more options.

* `dotemacs-theme` - Set the desired theme from a bunch of themes like`leuven`, `professional`, and `eclipse`, or use the `default`.
* `dotemacs-modeline-theme` - Set the desired modeline theme from `powerline`, `smart-mode-line`, `spaceline` or `default`.
* `dotemacs-window-split` - Specify the direction in which the windows should be split. This depends on the orientation of the display.
* `dotemacs-fill-column` - Column beyond which lines should not extend.
* `dotemacs-delete-trailing-whitespace-p` - Control whether trailing whitespace should be deleted or not.
<!-- * `dotemacs-cc-tags` - Choose whether to use Gtags or RTags for C/C++ programming. -->

## Browsing Source

The `lsp` mode in GNU Emacs means you mostly will not need to create tags separately, but the following information may still be useful for languages that are currently not yet supported by the `lsp` mode.

### GTags

You can use `counsel-gtags`.

``` Bash
find . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.h" -o -iname "*.hpp" -o -iname "*.py" ! -iname "*.cu" | gtags -v -f -
```

``` Bash
find ./src -type f -iname "*.py" ! -iname "__init__.py" | gtags -v -f -
```

``` Bash
find . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.h" -o -iname "*.hpp" | gtags -v -f -
```

### Universal CTags

You can also use `counsel-etags` with Universal CTags. Use `ctags -eR` to recursively scan for files (R) and use Emacs-compatible syntax (-e).

Emacs will, by default, expect a tag file by the name "TAGS" in the current directory. Once the tag file is built, the following  commands  exercise the tag indexing feature:

* `M-x visit-tags-table <RET> FILE <RET>` - Select the tag file `FILE` to use.
* `M-. [TAG] <RET>` - Find the first definition of `TAG`. The default tag is the identifier under the cursor.
* `M-*` - Pop back to where you previously invoked `M-.`.
* `C-u M-.` - Find the next definition for the last tag.

For more commands, see the Tags topic in the Emacs info document.

``` Bash
find -name "*.c" -print -or -name "*.h" -print -or -name "*.hpp" -print -or -name "*.cpp" -print -or -name "*.py" -print | xargs ctags -ea --list-extras
```

## TODO

* Omit sub-directories/files with `counsel-find-file`.
