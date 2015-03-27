;;; prog-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Programming mode specific.

;;; Code:

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'fci-mode)
;;(add-hook 'prog-mode-hook 'idle-highlight-mode) ; highlight all occurrences of word under the point
;;(add-hook 'prog-mode-hook #'auto-highlight-symbol-mode) ; highlight symbol at point

;; ;; show the name of the function in the modeline
;; (add-hook 'prog-mode-hook 'which-function-mode)
;; ;; (add-to-list 'which-func-modes 'java-mode)
;; ;; (add-to-list 'which-func-modes 'c-mode)
;; ;; (add-to-list 'which-func-modes 'c++-mode)
;; ;; (add-to-list 'which-func-modes 'python-mode)
;; (eval-after-load "which-func"
;;   '(setq which-func-modes '(java-mode c++-mode c-mode python-mode)))

(use-package which-func
  :defer t
  :config
  (add-hook 'prog-mode-hook 'which-func-mode))
  

(provide 'prog-init)

;;; prog-init.el ends here
