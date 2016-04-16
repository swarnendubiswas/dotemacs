;;; whitespace-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure whitespace.

;;; Code:

(use-package whitespace
  :if (bound-and-true-p dotemacs-use-whitespace-p)
  :diminish (global-whitespace-mode whitespace-mode)
  :config
  (setq-default show-trailing-whitespace nil
                whitespace-line-column 'dotemacs-fill-column
                ;; Options: '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab
                ;; space-mark tab-mark newline-mark)
                whitespace-style '(faces trailing empty lines-tail))
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode 1))

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))

(provide 'whitespace-init)

;;; whitespace-init.el ends here
