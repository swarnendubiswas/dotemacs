;;; prog-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Programming mode specific.

;;; Code:

;; show the name of the function in the modeline
(use-package which-func
  :defer 5
  :config
  ;; (add-hook 'prog-mode-hook 'which-function-mode)
  ;; ;; (add-to-list 'which-func-modes 'java-mode)
  ;; ;; (add-to-list 'which-func-modes 'c-mode)
  ;; ;; (add-to-list 'which-func-modes 'c++-mode)
  ;; ;; (add-to-list 'which-func-modes 'python-mode)
  ;; (eval-after-load "which-func"
  ;;   '(setq which-func-modes '(java-mode c++-mode c-mode python-mode)))
  (setq which-func-modes '(java-mode c++-mode c-mode python-mode))
  (add-hook 'prog-mode-hook 'which-func-mode))

(use-package electric
  :defer 5
  :config (electric-layout-mode 1))

(provide 'prog-init)

;;; prog-init.el ends here
