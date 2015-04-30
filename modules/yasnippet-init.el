;;; yasnippet-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Initialize yasnippet.

;;; Code:

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  ;;(yas-global-mode 1)
  ;; I mostly use yasnippet with LaTeX
  (yas-reload-all)
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode)
  (when (bound-and-true-p use-helm)
    (use-package helm-c-yasnippet
      :disabled t ;; I do not use yasnippet for programming modes currently
      :ensure t)))

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
