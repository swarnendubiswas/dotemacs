;;; parens-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup parentheses.

;;; Code:

(use-package smartparens
             :ensure t
             :defer t
             :diminish smartparens
             :config
             (progn
               (require 'smartparens-config)
               (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
               (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)
               )
             )

;; related to pairing of parentheses, brackets, etc.
(setq show-paren-delay 0
      show-paren-style 'mixed ; 'expression, 'parenthesis, 'mixed
      )
(when (fboundp 'show-paren-mode)
  (show-paren-mode 1) ; highlight matching parentheses when the point is on them
  (make-variable-buffer-local 'show-paren-mode))
;;(show-paren-mode 1) ; highlight matching parentheses when the point is on them
(setq-default flyparens-mode t) ; highlight/track mismatched parentheses

(provide 'smartparens-init)

;;; parens-init.el ends here
