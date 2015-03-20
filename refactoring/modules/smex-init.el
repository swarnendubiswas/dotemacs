;;; smex-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup smex.

;;; Code:

(use-package smex
             :ensure t
  :commands (smex-initialize)
  ;; :init
  ;;            ((smex-initialize) ; this is slow
  ;;             ;;(autoload 'smex "smex")
  ;; 	      )
             ;;:config
             ;;((setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))
             )

(provide 'smex-init)

;;; smex-init.el ends here
