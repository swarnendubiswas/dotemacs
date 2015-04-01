;;; parens-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup parentheses.

;;; Code:

;; related to pairing of parentheses, brackets, etc.

(use-package paren
  :defer 5
  :init
  (setq show-paren-delay 0
        show-paren-style 'parenthesis) ; 'expression, 'parenthesis, 'mixed
  :config
  (when (fboundp 'show-paren-mode)
    (show-paren-mode 1) ; highlight matching parentheses when the point is on them
    (make-variable-buffer-local 'show-paren-mode)))

;; highlight/track mismatched parentheses

(use-package smartparens
  :ensure t
  :disabled t
  :defer 5
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

(use-package flex-autopair
  :ensure t
  :disabled t
  :config (flex-autopair-mode 1))

(provide 'parens-init)

;;; parens-init.el ends here
