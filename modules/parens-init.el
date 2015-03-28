;;; parens-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup parentheses.

;;; Code:

;; related to pairing of parentheses, brackets, etc.

(use-package paren
  :config
  (setq show-paren-delay 0
        show-paren-style 'parenthesis) ; 'expression, 'parenthesis, 'mixed
  :init 
  (when (fboundp 'show-paren-mode)
    (show-paren-mode 1) ; highlight matching parentheses when the point is on them
    (make-variable-buffer-local 'show-paren-mode)))

;; highlight/track mismatched parentheses

(use-package smartparens
  :ensure t
  :commands (smartparens-mode show-smartparens-mode)
  :diminish smartparens-mode
  :config
  (use-package smartparens-config)
  (smartparens-global-mode))

(use-package mic-paren
  :ensure t
  :defer 5
  :config
  (paren-activate))

;; (flex-autopair-mode 1)

(provide 'parens-init)

;;; parens-init.el ends here
