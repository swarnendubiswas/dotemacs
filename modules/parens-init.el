;;; parens-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup parentheses. Highlight/track mismatched parentheses, auto-pairing, etc.

;;; Code:

(or (use-package mic-paren
      :ensure t
      :config
      (setq paren-highlight-at-point t)
      (paren-activate)
      (make-variable-buffer-local 'show-paren-mode))

    (use-package paren
      :disabled t
      :config
      (setq show-paren-delay 0
            show-paren-style 'parenthesis) ; 'expression, 'parenthesis, 'mixed
      (when (fboundp 'show-paren-mode)
        (show-paren-mode 1) ; highlight matching parentheses when the point is on them
        (make-variable-buffer-local 'show-paren-mode))))

(use-package smartparens
  :disabled t
  :ensure t
  :commands (smartparens-mode show-smartparens-mode)
  :diminish smartparens-mode
  :config
  ;;(smartparens-global-mode)
  (use-package smartparens-config))

(or (use-package elec-pair
      :config (electric-pair-mode 1))

    (use-package autopair
      :disabled t
      :ensure t
      :config (autopair-global-mode 1))
    
    (use-package flex-autopair
      :disabled t
      :ensure t
      :config (flex-autopair-mode 1)))

(provide 'parens-init)

;;; parens-init.el ends here
