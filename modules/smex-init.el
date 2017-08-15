;;; smex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup smex.

;;; Code:

(defvar dotemacs-temp-directory)
(defvar dotemacs-selection)

(use-package smex
  :ensure t
  :disabled t
  :if (or (eq dotemacs-selection 'none) (eq dotemacs-selection 'ido))
  :config
  (setq smex-save-file (concat dotemacs-temp-directory "smex-items")
        smex-auto-update t)
  (smex-initialize)
  :bind (("M-x" . smex)
         ("<f1>" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package amx
  :load-path "extras"
  :config (amx-mode 1))

(provide 'smex-init)

;;; smex-init.el ends here
