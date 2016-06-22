;;; parens-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup parentheses.  Highlight/track matching/mismatched parentheses and configure auto-pairing.

;;; Code:

(use-package paren
  :disabled t
  :config
  (setq show-paren-delay 0
        show-paren-style 'mixed ; Options: 'expression, 'parenthesis, 'mixed
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode 1)
  (when (fboundp 'show-paren-mode)
    (make-variable-buffer-local 'show-paren-mode)))

(use-package elec-pair
  :disabled t
  :config
  (setq electric-pair-skip-whitespace nil)
  (electric-pair-mode 1))

;; "sp-cheat-sheet" will show you all the commands available, with examples.
(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil ; Don't kill entire symbol on C-k
        sp-navigate-close-if-unbalanced t
        sp-show-pair-from-inside t
        sp-autoinsert-quote-if-followed-by-closing-pair nil
        sp-highlight-pair-overlay t
        sp-highlight-wrap-overlay t
        sp-highlight-wrap-tag-overlay t)
  (smartparens-global-mode)
  (show-smartparens-global-mode 1)

  (sp-with-modes sp-lisp-modes
    ;; Disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil))

  (make-variable-buffer-local 'show-paren-mode)
  :bind (("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp))
  :diminish smartparens-mode)

(provide 'parens-init)

;;; parens-init.el ends here
