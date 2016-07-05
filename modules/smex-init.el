;;; smex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup smex.

;;; Code:

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (expand-file-name "smex-items" dotemacs-temp-directory)
        smex-auto-update t)
  (smex-initialize)
  (when (or (eq dotemacs-selection 'none) (eq dotemacs-selection 'ido))
    (bind-key "M-x" #'smex)
    (bind-key "M-X" #'smex-major-mode-commands)))

(provide 'smex-init)

;;; smex-init.el ends here
