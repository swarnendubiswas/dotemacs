# Emacs

My Emacs customizations, all in one place. Many of the customizations included are collected from the Internet, and tailored according to my preferences.

## Note

I use Emacs as my primary editor. Other than text and elisp editing, I use Emacs for editing C/C++, LaTeX, and Python files. The current setup should work fine for these major modes, but is not well-tuned for programming with Java. I think other IDEs such as Eclipse are way better for working with Java.

* Python - I use Elpy and gtags (and its variants) for code completion
* C/C++ - I use irony mode and rtags for code completion. Gtags should also work depending on preference.

## Installation

You might want to backup your contents if your `.emacs.d` directory is not empty. Then, use the following command to checkout the source:

```Bash
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

## Directory structure

         * extras -- third-party packages (may not be available from the package archives)
         * modules -- elisp modules containing personal customizations
         * reference-cards -- help files
         * snippets -- custom snippets

## Tweaking the default settings

The following are customization options defined in `modules/config-init.el` that you could use to tweak the default setup. Check the module for more options.

* `dotemacs-completion-in-buffer` - Enable `company` for auto-completion in buffer.
* `dotemacs-selection` - Choose a selection and completion framework between `ido` or `ivy`. The default is `ivy`.
* `dotemacs-ido-view-mode` - Choose how choices are displayed with `ido`. The options are `vertical`, `grid`, and `default`. This option only makes sense if `dotemacs-selection` is set to `ido`.
* `dotemacs-theme` - Set the desired theme from a bunch of themes like`leuven`, `professional`, and `eclipse`, or use the `default`.
* `dotemacs-modeline-theme` - Set the desired modeline theme from `powerline`, `smart-mode-line`, `spaceline` or `default`.
* `dotemacs-window-split` - Specify the direction in which the windows should be split. This depends on the orientation of the display.
* `dotemacs-cc-helper` - Choose whether to use gtags or rtags for C/C++ programming.

## Dependencies

* `aspell`, `gnu global`, `rtags`

Suggestions and pull requests are welcome.
