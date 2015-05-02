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

;; https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-miscs.org
;; https://github.com/Fuco1/smartparens/blob/master/smartparens-config.el
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always
        sp-navigate-close-if-unbalanced t
        sp-show-pair-from-inside t)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (sp-use-smartparens-bindings)
  
  ;; pair management
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  
  ;; do not autoinsert ' pair if the point is preceded by word. This will handle the situation when ' is used as a
  ;; contraction symbol in natural language. Nil for second argument means to keep the original definition of closing
  ;; pair.
  (sp-pair "'" nil :unless '(sp-point-after-word-p))
  
  ;; emacs is lisp hacking enviroment, so we set up some most common lisp modes too
  (sp-with-modes sp--lisp-modes
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil))
  
  ;; tex-mode latex-mode
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">"))
  
  (eval-after-load "html-mode"
    '(require 'smartparens-html))
  (eval-after-load "latex"
    '(require 'smartparens-latex))
  (eval-after-load "tex-mode"
    '(require 'smartparens-latex)))

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
