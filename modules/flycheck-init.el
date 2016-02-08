;;; flycheck-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

;; flycheck requires gcc 4.8 at least
(use-package flycheck
  :ensure t
  ;;:if (not (string-equal system-name "rain.cse.ohio-state.edu"))
  :diminish flycheck-mode
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode) ; Enable where possible

  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-standard-error-navigation nil
        flycheck-check-syntax-automatically '(save mode-enabled))

  (if (bound-and-true-p dotemacs-use-helm-p)
      (setq flycheck-completion-system 'helm)
    (setq flycheck-completion-system 'ido))

  :config
  (use-package flycheck-pos-tip ; Show flycheck messages in popups
    :ensure t
    :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

  (use-package flycheck-color-mode-line
    :ensure t
    :disabled t ; FIXME: Messes up the buffer name color.
    :config (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

  (use-package flycheck-status-emoji
    :ensure t
    :config (flycheck-status-emoji-mode)))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
