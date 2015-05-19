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
  (yas-global-mode 1)
  (use-package helm-c-yasnippet
    :ensure t))

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
