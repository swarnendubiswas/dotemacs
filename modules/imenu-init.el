;;; imenu-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup imenu.

;;; Code:

(use-package imenu+
  :ensure t)

(use-package imenu-anywhere
  :ensure t)

(use-package popup-imenu
  :ensure t)

(provide 'imenu-init)

;;; imenu-init.el ends here
