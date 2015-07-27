;;; parens-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup parentheses. Highlight/track matching/mismatched parentheses, auto-pairing, etc.

;;; Code:

(or (use-package mic-paren
      :ensure t
      :disabled t
      :init
      (setq paren-highlight-at-point t)
      (paren-activate)
      (make-variable-buffer-local 'show-paren-mode))

    (use-package paren
      :init
      (show-paren-mode 1)
      (setq show-paren-delay 0
            show-paren-style 'mixed ; 'expression, 'parenthesis, 'mixed
            show-paren-when-point-inside-paren t
            show-paren-when-point-in-periphery t)
      (when (fboundp 'show-paren-mode)
        (make-variable-buffer-local 'show-paren-mode))))

(or (use-package elec-pair
      :disabled t
      :init (electric-pair-mode 1))

    (use-package autopair
      :ensure t
      :disabled t
      :init (autopair-global-mode 1))

    (use-package flex-autopair
      :ensure t
      :disabled t
      :init (flex-autopair-mode 1)))

;; https://github.com/xiaohanyu/oh-my-emacs/blob/master/core/ome-miscs.org
;; https://github.com/Fuco1/smartparens/blob/master/smartparens-config.el
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
(use-package smartparens
  :ensure t
  :disabled t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  ;;(sp-use-smartparens-bindings)

  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always
        sp-navigate-close-if-unbalanced t
        sp-show-pair-from-inside t
        sp-autoescape-string-quote nil)

  ;; ;; pair management
  ;; (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  ;; ;; do not autoinsert ' pair if the point is preceded by word. This will handle the situation when ' is used as a
  ;; ;; contraction symbol in natural language. Nil for second argument means to keep the original definition of closing
  ;; ;; pair.
  ;; (sp-pair "'" nil :unless '(sp-point-after-word-p))

  ;; Emacs is lisp hacking enviroment, so we set up some most common lisp modes too
  (sp-with-modes sp--lisp-modes
                 ;; disable ', it's the quote character!
                 (sp-local-pair "'" nil :actions nil))

  ;; ;; tex-mode latex-mode
  ;; (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  ;;   (sp-local-tag "i" "\"<" "\">"))

  ;; when you press RET, the curly braces automatically add another newline
  (sp-with-modes '(c-mode c++-mode)
                 (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
                 (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                           ("* ||\n[i]" "RET"))))
  (with-eval-after-load "html-mode"
    (require 'smartparens-html))
  (with-eval-after-load "latex"
    (require 'smartparens-latex))
  (with-eval-after-load "tex-mode"
    (require 'smartparens-latex))

  (make-variable-buffer-local 'show-paren-mode))

(provide 'parens-init)

;;; parens-init.el ends here
