;;; org-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup org mode.

;;; Code:

(use-package org
             :ensure t
             :defer t
             :config
             (progn
               (setq org-completion-use-ido t
                     org-src-fontify-natively t ; code block fontification using the major-mode of the code
                     org-src-preserve-indentation t
                     org-src-window-setup 'current-window
                     )
               )
             )

;; org mode hooks
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'visual-line-mode)
;; turn on soft wrapping mode for org mode
(add-hook 'org-mode-hook 
          (lambda () (setq truncate-lines nil)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode 1)))

(provide 'org-init)

;;; org-init.el ends here
