;;; smex-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup smex.

;;; Code:

(use-package smex
  :ensure t
  ;;:commands (smex-initialize) ; this is slow
  :init
  (autoload 'smex "smex")
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(provide 'smex-init)

;;; smex-init.el ends here
