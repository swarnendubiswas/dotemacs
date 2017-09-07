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
  (require 'smartparens-config)
  (setq sp-show-pair-from-inside t)
  ;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">"))
  ;; https://emacs.stackexchange.com/questions/27200/how-to-disable-pairing-in-latex-mode-in-spacemacs
  ;; (sp-local-pair #'latex-mode "$" nil :actions :rem)
  ;; (sp-local-pair 'LaTeX-mode "$" nil :actions :rem)

  (sp-with-modes '(c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                      ("* ||\n[i]" "RET")))

  ;; FIXME: The following seem to be causing problems.
  ;; ;; http://emacs.stackexchange.com/questions/26912/smartparens-do-not-insert-parenthesis-pair-when-point-is-at-the-beginning-of-wo
  ;; (sp-pair "(" nil :unless '(sp-point-before-word-p))
  ;; (sp-pair "[" nil :unless '(sp-point-before-word-p))
  ;; (sp-pair "{" nil :unless '(sp-point-before-word-p))
  ;; (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p))

  :bind (("C-M-a" . sp-beginning-of-sexp)
         ("C-M-e" . sp-end-of-sexp)
         ("C-M-u" . sp-up-sexp)
         ("C-M-w" . sp-down-sexp)
         ;; The following two are the more commonly required use cases.
         ("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("C-S-b" . sp-backward-symbol)
         ("C-S-f" . sp-forward-symbol)
         ("C-M-k" . sp-splice-sexp))
  :diminish smartparens-mode)

(use-package elec-pair
  :disabled t
  :config (electric-pair-mode 1))

(provide 'parens-init)

;;; parens-init.el ends here
