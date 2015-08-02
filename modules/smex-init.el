;;; smex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup smex.

;;; Code:

(use-package smex
  :ensure t
  :defer t
  :config
  ;;(smex-initialize)
  (setq smex-save-file (expand-file-name ".smex-items" dotemacs-temp-directory))
  ;; (setq smex-completion-method 'ivy) ; this change is yet to be pulled into smex
  :bind (("<f2>" . smex)
         ("M-X" . smex-major-mode-commands)))

(provide 'smex-init)

;;; smex-init.el ends here
