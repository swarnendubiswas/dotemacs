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
;; https://ebzzry.github.io/emacs-pairs.html
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  ;; (smartparens-global-strict-mode 1)
  (setq sp-autoskip-closing-pair 'always
        ;; Don't kill entire symbol on C-k
        sp-hybrid-kill-entire-symbol nil)

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

(provide 'parens-init)

;;; parens-init.el ends here
