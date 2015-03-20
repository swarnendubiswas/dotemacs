;;; packages-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup packages.

;;; Code:

(setq package-user-dir (expand-file-name "~/.emacs.d/elpa/")
      package-enable-at-startup nil)
(add-to-list 'package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                                 ("melpa" . "http://melpa.milkbox.net/packages/")
                                 ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(provide 'packages-init)

;;; packages-init.el ends here
