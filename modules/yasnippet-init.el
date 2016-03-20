;;; yasnippet-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize yasnippet.

;;; Code:

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init (yas-global-mode 1)
  :config
  (setq yas-triggers-in-field t)
  (with-eval-after-load "auto-complete"
    (add-to-list 'ac-sources 'ac-source-yasnippet)))

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
