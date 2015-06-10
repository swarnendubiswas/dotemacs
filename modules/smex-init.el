;;; smex-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup smex.

;;; Code:

(use-package smex
  :ensure t
  ;;:init (autoload 'smex "smex")
  :config
  ;;(smex-initialize)
  (setq smex-save-file (expand-file-name ".smex-items" dotemacs-temp-directory)
        smex-completion-method 'ivy)
  :bind* (;;("M-x" . smex)
          ("M-X" . smex-major-mode-commands)))

(provide 'smex-init)

;;; smex-init.el ends here
