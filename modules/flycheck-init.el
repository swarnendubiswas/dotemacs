;;; flycheck-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config (global-flycheck-mode 1))

(use-package flycheck-color-mode-line
  :ensure t
  :disabled t
  :config (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; Check package conventions with Flycheck
(use-package flycheck-package
  :ensure t
  :disabled t
  :config (with-eval-after-load 'flycheck
            (flycheck-package-setup)))

(use-package flycheck-tip
  :ensure t
  :defer 10)

;; Show Flycheck messages in popups
(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :config (with-eval-after-load 'flycheck
            (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
