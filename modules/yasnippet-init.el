;;; yasnippet-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Initialize yasnippet.

;;; Code:

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  ;; this is already the default
  ;;(add-to-list yas-snippet-dirs (concat user-emacs-directory "snippets"))
  ;; SB: I mostly use yasnippet with LaTeX
  (yas-global-mode 1)
  ;;(yas-reload-all)
  ;;(add-hook 'LaTeX-mode-hook #'yas-minor-mode)
  (use-package helm-c-yasnippet
    :disabled t ;; I do not use yasnippet for programming modes currently
    :if (eq dotemacs-helm-or-ido 'helm)
    :ensure t))

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
