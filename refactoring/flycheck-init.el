;;; flycheck-init.el --- Part of emacs initialization

;;; Commentary:
;; Configure flycheck.

;;; Code:

(use-package flycheck-color-mode-line-cookie
             :ensure t
             :defer t
             :idle (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
             )

(use-package flycheck
             :ensure t
             :defer t
             :idle (global-flycheck-mode 1)
             )

(provide 'flycheck-init)

;;; flycheck-init.el ends here
