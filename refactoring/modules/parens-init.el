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
  :init
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
             :config
             (progn
    (use-package 'smartparens-config)
               (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
    (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)))

;; (flex-autopair-mode 1)

(provide 'parens-init)

;;; parens-init.el ends here
