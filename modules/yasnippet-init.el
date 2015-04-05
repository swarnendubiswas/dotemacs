;;; yasnippet-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Initialize yasnippet

;;; Code:

(use-package yasnippet
  :ensure t
  :defer 10
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :config
  (yas-reload-all 1) ; this slows startup
  ;; mostly useful with LaTeX
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (yas-minor-mode)))
  ;;(yas-global-mode 1)
  )

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
