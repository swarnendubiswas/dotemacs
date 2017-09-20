;;; imenu-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup imenu.

;;; Code:

(defvar dotemacs-selection)

(use-package imenu
  :config (setq imenu-auto-rescan t))

(use-package imenu+
  :ensure t)

(use-package imenu-anywhere
  :ensure t
  :bind* ("C-c C-j" . imenu-anywhere))

(use-package popup-imenu
  :ensure t)

(when (eq dotemacs-selection 'ido)
  (bind-key* "C-c C-j" #'ido-imenu-anywhere))

(use-package imenu-list
  :ensure t)

(provide 'imenu-init)

;;; imenu-init.el ends here
