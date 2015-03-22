;;; yasnippet-init.el --- Part of emacs initialization

;;; Commentary:
;; Initialize yasnippet 

;;; Code:

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init
  ;;(yas-reload-all 1) ; this slows startup
  (yas-global-mode 1))

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
