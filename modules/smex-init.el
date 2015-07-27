;;; smex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup smex.

;;; Code:

(use-package smex
  :ensure t
  :defer t
  ;;:init (autoload 'smex "smex")
  :config
  ;;(smex-initialize)
  (setq smex-save-file (expand-file-name ".smex-items" dotemacs-temp-directory)
        ;; this change is yet to be pulled into smex
        ;;smex-completion-method 'ivy
        )
  :bind* (("<f2>" . smex)
          ("M-X" . smex-major-mode-commands)))

(provide 'smex-init)

;;; smex-init.el ends here
