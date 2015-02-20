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

(provide 'smartparens-init)


