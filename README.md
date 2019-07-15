# GNU Emacs

My GNU Emacs customizations, all in one place. Some of the included customizations are collected from the Internet, and tailored according to my preferences. This setup is **for** a GNU/Linux platform.

I use GNU Emacs as my primary editor. Other than text, Markdown, and Elisp editing, I use GNU Emacs for editing C/C++, LaTeX, and Python files. The current setup should work fine for these major modes, but is not well-tuned for programming with Java. I think other IDEs such as Eclipse and Visual Studio Code are way better for working with Java.

* Python - I use Elpy and Gtags (and its variants) for code completion
* C/C++ - I use irony mode and Gtags for code completion. Ctags should also work depending on preference.

Suggestions and pull requests are welcome.

## Installation

You might want to backup your contents if your `.emacs.d` directory is not empty. Then, use the following command to checkout the source:

```Bash
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

### Dependencies

A few third-party executables and libraries  to complement the setup. You can use the following instructions to install them on a Ubuntu >=18.04 distribution.

``` Bash
sudo apt install aspell global exuberant-ctags libxml2-utils chktex shellcheck ruby-dev tidy python-pygments python-pip python3-pip npm cppcheck ripgrep the_silver_searcher
python -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi pylint rope python-language-server[all] pycodestyle flake8 autopep8 importmagic pyls-isort pydocstyle setuptools --user
python3 -m pip install --upgrade pip proselint Sphinx pygments isort yapf jedi pylint rope python-language-server[all] pycodestyle flake8 autopep8 importmagic pyls-isort pydocstyle setuptools --user
sudo npm i -g npm eslint js-yaml less jsonlint bash-language-server vscode-html-languageserver-bin js-beautify typescript-language-server typescript vscode-css-languageserver-bin
sudo npm i -g --unsafe-perm bash-language-server
sudo npm i -g stylelint --save-dev
sudo gem install scss_lint mdl
```

## Directory structure

* `extras` -- third-party packages (may not be available from the package archives)
* `modules` -- Elisp modules containing personal customizations
* `reference-cards` -- documentation and help files
* `snippets` -- custom snippets

## Tweaking the default settings

The following are customization options defined in `modules/config-init.el` that you could use to tweak the default setup. Check the module for more options.

* `dotemacs-theme` - Set the desired theme from a bunch of themes like`leuven`, `professional`, and `eclipse`, or use the `default`.
* `dotemacs-modeline-theme` - Set the desired modeline theme from `powerline`, `smart-mode-line`, `spaceline` or `default`.
* `dotemacs-window-split` - Specify the direction in which the windows should be split. This depends on the orientation of the display.
* `dotemacs-fill-column` - Column beyond which lines should not extend.
* `dotemacs-delete-trailing-whitespace-p` - Control whether trailing whitespace should be deleted or not.
* `dotemacs-cc-tags` - Choose whether to use Gtags or RTags for C/C++ programming.

## Browsing Source

* Python

  ``` Bash
  find ./src -type f -iname "*.py" ! -iname "__init__.py" | gtags -v -f -
  ```

* C/C++

  ``` Bash
  find . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.h" -o -iname "*.hpp" | gtags -v -f -
  ```

* Creating tags

  * GTags

    You can use `counsel-gtags`.

    * Selectively tag files

    ``` Bash
    find . -type f -iname "*.cpp" -o -iname "*.c" -o -iname "*.h" -o -iname "*.hpp" -o -iname "*.py" ! -iname "*.cu" | gtags -v -f -
    ```

  * Universal CTags

    You can also use `counsel-etags` with Universal CTags. Use `ctags -eR` to recursively scan for files (R) and use Emacs-compatible syntax (-e).

    ``` Bash
    find -name "*.c" -print -or -name "*.h" -print -or -name "*.hpp" -print -or -name "*.cpp" -print -or -name "*.py" -print | xargs ctags -ea --list-extras
    ```
