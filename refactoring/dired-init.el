(use-package dired
             :ensure t
             :commands dired-mode
             :config
             (progn
               (setq dired-auto-revert-buffer t ; revert each dired buffer automatically when you visit it
                     dired-recursive-deletes 'always ; single prompt for all n directories
                     dired-recursive-copies 'always
                     )
               (setq-default diredp-hide-details-initially-flag nil)
               )
             )

(use-package direx
             :ensure t
             :defer t
             )

(use-package dired-x
             :commands dired-jump
             :config (setq dired-bind-jump t)
             )

(use-package dired-efap
             :ensure t
             :defer t
             )
