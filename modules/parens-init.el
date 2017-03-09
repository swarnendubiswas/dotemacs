;;; parens-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup parentheses.  Highlight/track matching/mismatched parentheses and configure auto-pairing.

;;; Code:

(use-package paren
  :config
  (setq show-paren-delay 0
        show-paren-style 'mixed ; Options: 'expression, 'parenthesis, 'mixed
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode 1)
  (when (fboundp 'show-paren-mode)
    (make-variable-buffer-local 'show-paren-mode)))

;; "sp-cheat-sheet" will show you all the commands available, with examples.
;; https://ebzzry.github.io/emacs-pairs.html
(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :config
  (setq sp-show-pair-from-inside t)
  ;; ;; http://emacs.stackexchange.com/questions/26912/smartparens-do-not-insert-parenthesis-pair-when-point-is-at-the-beginning-of-wo
  ;; (sp-pair "(" nil :unless '(sp-point-before-word-p))
  ;; (sp-pair "[" nil :unless '(sp-point-before-word-p))
  ;; (sp-pair "{" nil :unless '(sp-point-before-word-p))
  ;; (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p))
  :bind (("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         ("C-M-u" . sp-up-sexp)
         ("C-M-w" . sp-down-sexp)
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("C-S-b" . sp-backward-symbol)
         ("C-S-f" . sp-forward-symbol)
         ("C-M-k" . sp-splice-sexp))
  :diminish smartparens-mode)

(use-package elec-pair
  :config (electric-pair-mode 1))

(provide 'parens-init)

;;; parens-init.el ends here
