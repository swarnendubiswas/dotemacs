;;; flycheck-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (use-package flycheck-tip
    :ensure t)
  (add-hook 'prog-mode-hook #'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :disabled t
  :ensure t
  :config (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; Check package conventions with flycheck
(use-package flycheck-package
  :disabled t
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-package-setup)))

;; Show flycheck messages in popups
(use-package flycheck-pos-tip
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package helm-flycheck
  :ensure t
  :if use-helm)

(provide 'flycheck-init)

;;; flycheck-init.el ends here
