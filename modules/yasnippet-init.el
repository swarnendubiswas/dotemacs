;;; yasnippet-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize yasnippet.

;;; Code:

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (setq yas-triggers-in-field t)
  (yas-global-mode 1)

  ;; this is already the default
  ;; (add-to-list yas-snippet-dirs (concat user-emacs-directory "snippets"))

  (use-package helm-c-yasnippet
    :ensure t
    :disabled t
    :config
    (setq helm-yas-not-display-dups nil)
    (setq helm-yas-display-key-on-candidate t)))

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
