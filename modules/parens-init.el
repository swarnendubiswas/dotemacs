;;; parens-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup parentheses.

;;; Code:

;; related to pairing of parentheses, brackets, etc.
(setq show-paren-delay 0
      show-paren-style 'parenthesis) ; 'expression, 'parenthesis, 'mixed
(when (fboundp 'show-paren-mode)
  (show-paren-mode 1) ; highlight matching parentheses when the point is on them
  (make-variable-buffer-local 'show-paren-mode))

; highlight/track mismatched parentheses
(use-package flyparens
  :ensure t
  :defer t
  :config
  (flyparens-mode))

(use-package smartparens
  :ensure t
  :disabled t
  :diminish smartparens
  :commands (smartparens-global-mode show-smartparens-global-mode)
  :config
  (use-package 'smartparens-config))

(use-package mic-paren
  :ensure t
  :defer 5
  :config
  (paren-activate))

;; (flex-autopair-mode 1)

(provide 'parens-init)

;;; parens-init.el ends here
