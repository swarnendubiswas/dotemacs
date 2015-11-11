;;; flycheck-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

;; flycheck requires gcc 4.8 at least
(use-package flycheck
  :if (not (string-equal system-name "XXX"))
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-completion-system 'ido
        flycheck-standard-error-navigation nil
        flycheck-check-syntax-automatically '(save mode-enabled))

  :config
  (use-package flycheck-tip
    :ensure t
    :disabled t)

  (use-package flycheck-pos-tip ; Show flycheck messages in popups
    :ensure t
    :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

  (use-package flycheck-color-mode-line
    :ensure t
    :disabled t
    :config (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

  (use-package flycheck-package ; Check package conventions with flycheck
    :ensure t
    :disabled t
    :init (flycheck-package-setup)))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
