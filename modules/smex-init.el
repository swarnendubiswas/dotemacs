;;; smex-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup smex.

;;; Code:

(use-package smex
  :ensure t
  ;;:commands (smex-initialize) ; this is slow
  ;;:init (autoload 'smex "smex")
  :config
  (setq smex-save-file (expand-file-name ".smex-items" emacs-temp-directory))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(provide 'smex-init)

;;; smex-init.el ends here
