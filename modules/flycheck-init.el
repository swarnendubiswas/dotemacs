;;; flycheck-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

(defvar dotemacs-selection)

(use-package flycheck
  :ensure t
  :init (add-hook 'prog-mode-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(tex-chktex tex-lacheck)) ; Leave out LaTeX
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        ;; flycheck-display-error-at-point-timer 0.5
        flycheck-display-errors-delay 0.5
        ;;       flycheck-standard-error-navigation nil
        )

  (use-package flycheck-pos-tip ; Show error messages in popups
    :ensure t
    :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
    :config
    ;; Long timeouts hinder visibility
    (setq flycheck-pos-tip-timeout 2))

  (use-package flycheck-color-mode-line
    :ensure t
    :disabled t
    :config (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode)))

(use-package flycheck-irony
  :ensure t
  :commands flycheck-irony-setup)

(use-package avy-flycheck
  :ensure t
  :config
  ;; Binds avy-flycheck-goto-error to C-c ! g
  (avy-flycheck-setup))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
