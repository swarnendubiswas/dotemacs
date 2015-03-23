;;; flycheck-init.el --- Part of emacs initialization

;;; Commentary:
;; Configure flycheck.

;;; Code:

(use-package flycheck-color-mode-line
  :ensure t
  :disabled t
  :config (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (use-package flycheck-package)
  (global-flycheck-mode 1))

(use-package flycheck-tip
  :ensure t
  :defer t)

(provide 'flycheck-init)

;;; flycheck-init.el ends here
