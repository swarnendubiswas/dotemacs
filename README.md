##Emacs

My Emacs changes, all in one place. Most of the customizations included have been collected from different sources on
the Internet, and tailored according to my preferences.

######Note

The setup is not perfect for programming with Java. I think Eclipse is way better with Java. 

#####Installation

First, if your `.emacs.d` directory is not empty, then you might want to backup the contents. Then, use the following
command to checkout the source:

```
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

#####Directory structure
         <!-- * backup -- older startup files, which I no longer use. I am now
         using [`(use-package)`](https://github.com/jwiegley/use-package). -->
         * modules -- elisp modules containing personal customizations
         * extras -- third-party packages (may not be available from the package archives)
         * reference-cards -- help files 
         * snippets -- custom snippets

####Setup

#####Tweaking the default settings

The following are a few customization options defined in `modules/config-init.el` that you could use to tweak the
default setup. Check the module for more options.

* `dotemacs-completion-in-buffer` - Toggle between `company` or `auto-complete`, with `company` being the default.
* `dotemacs-selection` - Choose a selection and completion framework, between `helm`, `ido`, or `ivy`.
* `dotemacs-ido-view-mode` - Choose how choices are displayed with `ido`. The options are vertical, grid, and default.
* `dotemacs-theme` - Set the desired theme from `leuven`, `professional`, `eclipse`, and default.
* `dotemacs-modeline-theme` - Set the desired modeline theme from `powerline`, `smart-mode-line`, `telephone-line`,
  `spaceline` and default.

Suggestions and pull requests are welcome.

