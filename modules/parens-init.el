;;; parens-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup parentheses.  Highlight/track matching/mismatched parentheses, auto-pairing, etc.

;;; Code:

(or (use-package mic-paren
      :ensure t
      :disabled t
      :config
      (setq paren-highlight-at-point t)
      (paren-activate)
      (make-variable-buffer-local 'show-paren-mode))

    (use-package paren
      :config
      (setq show-paren-delay 0
            show-paren-style 'mixed ; Options: 'expression, 'parenthesis, 'mixed
            show-paren-when-point-inside-paren t
            show-paren-when-point-in-periphery t)
      (show-paren-mode 1)
      (when (fboundp 'show-paren-mode)
        (make-variable-buffer-local 'show-paren-mode))))

(or (use-package elec-pair
      :config
      (add-hook 'prog-mode-hook
                (lambda ()
                  (electric-pair-mode 1)))
      (add-hook 'text-mode-hook
                (lambda ()
                  (electric-pair-mode 1))))

    (use-package autopair
      :ensure t
      :disabled t
      :config (autopair-global-mode 1))

    (use-package flex-autopair
      :ensure t
      :disabled t
      :config (flex-autopair-mode 1)))

(provide 'parens-init)

;;; parens-init.el ends here
