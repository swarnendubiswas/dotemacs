##emacs

My emacs changes, all in one place. Most of the information here is collected from different sources on the Internet,
dressed with my choices.

#####Directory structure
         * backup -- older changes
         * lisp -- third-party packages (may not be available from the package archives)
         * modules -- personal customizations
         * theme -- custom themes

The backup directory contains older monolithic startup files, which I no longer use. I am now using [`(use-package)`](https://github.com/jwiegley/use-package).

####Setup

#####Tweaking the default settings

Here are a few customization options that you could use to tweak the default setup.

* auto completion - Toggle `use-company` in `init.el`. Default is `company`.
* themes - Set `use-theme` to the desired theme in `appearance-init.el`. Default is `eclipse`.
* helm/ido - Helm provides functionalities that are similar to several other packages, such as ido, smex, recentf, etc.
You can choose which group of features you want by modifying `use-helm` in `init.el`.

Suggestions are welcome.

