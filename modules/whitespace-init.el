;;; whitespace-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure whitespace.

;;; Code:

(defvar dotemacs-fill-column)

;; This is different from whitespace-cleanup since this is unconditional
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package whitespace
  :if (bound-and-true-p dotemacs-use-whitespace-p)
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  :commands (whitespace-cleanup whitespace-mode)
  :init (global-whitespace-mode 1)
  :config
  (setq-default show-trailing-whitespace nil
                whitespace-auto-cleanup t
                whitespace-style '(face trailing space-before-tab empty indentation::tab indentation::space)
                whitespace-line-column dotemacs-fill-column))

;; Calls whitespace-cleanup before saving the current buffer, but only if the whitespace in the buffer was initially
;; clean
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init (add-hook 'prog-mode-hook #'global-whitespace-cleanup-mode))

(use-package ws-butler ; Unobtrusively trim extraneous white-space *ONLY* in lines edited
  :ensure t
  :diminish ws-butler-mode
  ;; :init (add-hook 'prog-mode #'ws-butler-global-mode)
  :hook (prog-mode . ws-butler-mode))

(provide 'whitespace-init)

;;; whitespace-init.el ends here
