;;; parens-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup parentheses. Highlight/track mismatched parentheses, auto-pairing, etc.

;;; Code:

(or (use-package mic-paren
      :ensure t
      :defer 2
      :config
      (paren-activate))

    (use-package paren
      :disabled t
      :config
      (setq show-paren-delay 0
            show-paren-style 'parenthesis) ; 'expression, 'parenthesis, 'mixed
      (when (fboundp 'show-paren-mode)
        (show-paren-mode 1) ; highlight matching parentheses when the point is on them
        (make-variable-buffer-local 'show-paren-mode))))

(use-package smartparens
  :ensure t
  :disabled t
  :defer 5
  :commands (smartparens-mode show-smartparens-mode)
  :diminish smartparens-mode
  :config
  ;;(smartparens-global-mode)
  (use-package smartparens-config))

(or (use-package elec-pair
      :disabled t
      :config (electric-pair-mode 1))

    (use-package flex-autopair
      :ensure t
      :disabled t
      :config (flex-autopair-mode 1)))

(provide 'parens-init)

;;; parens-init.el ends here
