;;; yasnippet-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize yasnippet.

;;; Code:

(use-package yasnippet
  :ensure t
  :defer 2
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
