(use-package rainbow-mode
             :ensure t
             :disabled t
             :config (add-hook 'prog-mode-hook #'rainbow-mode)
             )

(use-package rainbow-identifiers
             :ensure t
             :disabled t
             :config (add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
             )
(use-package rainbow-delimiters
             :ensure t
             :disabled t
             :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
             )
