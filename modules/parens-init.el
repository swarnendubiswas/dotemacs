;;; parens-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup parentheses. Highlight/track mismatched parentheses, auto-pairing, etc.

;;; Code:

(or (use-package mic-paren
      :disabled t
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

;; from https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-miscs.org
(use-package smartparens
  :ensure t
  :commands (smartparens-mode show-smartparens-mode)
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always
        sp-navigate-close-if-unbalanced t)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  ;; pair management
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  ;; tex-mode latex-mode
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">")))

(or (use-package elec-pair
      :disabled t
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
