;;; yasnippet-init.el --- Part of emacs initialization

;;; Commentary:
;; Initialize yasnippet 

;;; Code:

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  ;;:commands (yas-global-mode yas-minor-mode)
  :init (yas-global-mode 1)
  :config
  ;; (yas-reload-all 1) ; this slows startup
  ;; mostly useful with LaTeX
  ;; (add-hook 'LaTeX-mode-hook
  ;;           '(lambda ()
  ;;              (yas-minor-mode)))
  )

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
