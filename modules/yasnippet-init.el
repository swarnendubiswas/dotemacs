;;; yasnippet-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize yasnippet.

;;; Code:

(defvar ac-sources)

(use-package yasnippet
  :ensure t
  :commands (yas-expand yas-minor-mode)
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init (yas-global-mode 1)
  :config
  (setq yas-triggers-in-field t
        yas-wrap-around-region t)
  (with-eval-after-load "auto-complete"
    (add-to-list 'ac-sources 'ac-source-yasnippet))
  (unbind-key "<tab>" yas-minor-mode-map))

(provide 'yasnippet-init)

;;; yasnippet-init.el ends here
