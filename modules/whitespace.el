;;; whitespace.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure whitespace.

;;; Code:

(defvar dotemacs-fill-column)

(use-package whitespace
  :if (bound-and-true-p dotemacs-use-whitespace-p)
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  :commands (whitespace-cleanup whitespace-mode)
  :config
  (setq-default show-trailing-whitespace nil
                whitespace-auto-cleanup t
                whitespace-line-column dotemacs-fill-column
                whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty
                                        space-after-tab space-mark tab-mark newline-markl)))

;; ;; This is different from whitespace-cleanup since this is unconditional
;; (add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Calls whitespace-cleanup before saving the current buffer, but only if the whitespace in the buffer was initially
;; clean
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init (add-hook 'prog-mode-hook #'global-whitespace-cleanup-mode))

(use-package ws-butler ; Unobtrusively trim extraneous white-space *ONLY* in lines edited
  :ensure t
  :diminish ws-butler-mode
  :init (add-hook 'prog-mode #'ws-butler-global-mode))

(provide 'whitespace)

;;; whitespace.el ends here
