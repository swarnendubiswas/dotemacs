;;; flycheck-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

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
  :init (global-flycheck-mode 1)
  :diminish flycheck-mode)

;; Check package conventions with Flycheck
(use-package flycheck-package
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package flycheck-tip
  :ensure t
  :defer t)

;; Show Flycheck messages in popups
(use-package flycheck-pos-tip           
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck
          (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
