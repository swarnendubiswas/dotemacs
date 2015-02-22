;;; packages-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup packages.

;;; Code:

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))
(package-initialize)

;; set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; ensure that a required set of packages are always installed
(require 'ensure-packages)
;; get a list of currently installed packages (excluding built in packages) with '\C-h v package-activated-list'
(setq ensure-packages
      '(async auto-highlight-symbol color-theme  ctags ctags-update dash discover-my-major display-theme duplicate-thing epl es-lib f flex-isearch flx flycheck-tip flyparens ggtags git-rebase-mode git-commit-mode goto-last-change guide-key guide-key-tip pos-tip popwin highlight-numbers icomplete+ idle-highlight idle-highlight-mode jtags let-alist  manage-minor-mode fringe-helper math-symbol-lists mic-paren mode-icons names nav parent-mode pkg-info popup rich-minority s sentence-highlight writegood-mode ibuffer-tramp)
      )
(ensure-packages-install-missing)

(provide 'packages-init)

;;; packages-init.el ends here
