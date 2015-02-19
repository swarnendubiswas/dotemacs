(add-hook 'prog-mode-hook 'highlight-numbers-mode) ; minor mode to highlight numeric literals
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode) ; enable in programming related-modes (Emacs 24+)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
;;(add-hook 'prog-mode-hook (lambda () (yas-reload-all)))
(add-hook 'prog-mode-hook '(lambda () (yas-minor-mode)))
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook 'idle-highlight-mode) ; highlight all occurrences of word under the point
(add-hook 'prog-mode-hook #'auto-highlight-symbol-mode) ; highlight symbol at point

;; show the name of the function in the modeline
(add-hook 'prog-mode-hook 'which-function-mode)
;; (add-to-list 'which-func-modes 'java-mode)
;; (add-to-list 'which-func-modes 'c-mode)
;; (add-to-list 'which-func-modes 'c++-mode)
;; (add-to-list 'which-func-modes 'python-mode)
(eval-after-load "which-func"
  '(setq which-func-modes '(java-mode c++-mode c-mode python-mode)))
