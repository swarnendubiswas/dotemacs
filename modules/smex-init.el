;;; smex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup smex.

;;; Code:

(use-package smex
  :ensure t
  :defer t
  :config
  (setq smex-save-file (expand-file-name ".smex-items" dotemacs-temp-directory))
  ;;(smex-initialize)
  (smex-auto-update 60)
  :bind (("<f2>" . smex)
         ("M-X" . smex-major-mode-commands)))

(provide 'smex-init)

;;; smex-init.el ends here
