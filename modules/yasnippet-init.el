;;; yasnippet-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize yasnippet.

;;; Code:

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :init
  (add-hook 'tex-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;;(yas-global-mode 1)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  ;; this is already the default
  ;;(add-to-list yas-snippet-dirs (concat user-emacs-directory "snippets"))

  (use-package helm-c-yasnippet
    :ensure t
    :config
    (setq helm-yas-not-display-dups nil)
    (setq helm-yas-display-key-on-candidate t))

  (setq yas-triggers-in-field t)
  (yas-reload-all))

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
