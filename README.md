##Emacs

My Emacs changes, all in one place. Most of the customizations included have been collected from different sources on
the Internet, and tailored according to my preferences.

#####Installation

Use the following commands to checkout the sources:

```
git clone https://github.com/swarnendubiswas/dotemacs.git .emacs.d
```

If your `.emacs.d` directory is not empty, then you might want to backup the contents.

#####Directory structure
         * backup -- older monolithic startup files, which I no longer use. I am now using [`(use-package)`](https://github.com/jwiegley/use-package).
         * modules -- personal customizations
         * packages -- third-party packages (may not be available from the package archives)
         * reference-cards -- help files 
         * snippets -- custom snippets

####Setup

#####Tweaking the default settings

Here are a few customization options that you could use to tweak the default setup. These options can be modified from `modules/config-init.el`.

* dotemacs-completion - Toggle between `company` and `auto-complete`, with `company` being the default.
* dotemacs-theme - Set the desired theme.
* dotemacs-use-helm-p - Helm often provides functionalities provided by other packages, such as ido, smex, ace-jump-buffer, etc. Use this variable to control enabling/disabling helm.

Suggestions are welcome.

