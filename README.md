# Emacs

My Emacs customizations, all in one place. Many of the customizations included are collected from the Internet, and tailored according to my preferences.

## Note

I use Emacs as my primary editor. Other than text, Markdown, and Elisp editing, I use Emacs for editing C/C++, LaTeX, and Python files. The current setup should work fine for these major modes, but is not well-tuned for programming with Java. I think other IDEs such as Eclipse are way better for working with Java.

* Python - I use Elpy and Gtags (and its variants) for code completion
* C/C++ - I use irony mode and Gtags for code completion. RTags should also work depending on preference.

## Installation

You might want to backup your contents if your `.emacs.d` directory is not empty. Then, use the following command to checkout the source:

```Bash
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

### Dependencies

A few third-party executables and libraries are required to complement the setup. You can use the following instructions to install them on a Ubuntu >=18.04 distribution.

    sudo apt install aspell global exuberant-ctags libxml2-utils chktex shellcheck ruby-dev tidy
    pip install --update proselint Sphinx pygments isort yapf jedi pylint rope --user
    sudo npm i -g elsint js-yaml less jsonlint
    sudo npm i -g stylelint --save-dev
    sudo gem install scss_lint mdl

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
* `dotemacs-cc-tags` - Choose whether to use Gtags or RTags for C/C++ programming.

Suggestions and pull requests are welcome.
