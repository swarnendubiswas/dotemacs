;;; packages-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup packages.

;;; Code:

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))
(package-initialize)

;; set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'packages-init)

;;; packages-init.el ends here
