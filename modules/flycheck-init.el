;;; flycheck-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

;; flycheck requires gcc 4.8 at least
(use-package flycheck
  :ensure t
  :if (unless (string-equal system-name "rain.cse.ohio-state.edu"))
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (use-package flycheck-tip
    :ensure t)
  ;; Show flycheck messages in popups
  (use-package flycheck-pos-tip
    :ensure t
    :config
    (with-eval-after-load 'flycheck
      (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

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

  (use-package helm-flycheck
    :ensure t
    :if (eq dotemacs-helm-or-ido 'helm)))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
