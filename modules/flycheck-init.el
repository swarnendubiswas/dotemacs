;;; flycheck-init.el --- Part of emacs initialization

;;; Commentary:
;; Configure flycheck.

;;; Code:

(use-package flycheck-color-mode-line
  :ensure t
  :defer t
  :config (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode 1)
  :config (use-package flycheck-package))

(use-package flycheck-tip
  :ensure t
  :disabled t)

(provide 'flycheck-init)

;;; flycheck-init.el ends here
