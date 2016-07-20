;;; flycheck-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

(defvar dotemacs-selection)
(defvar flycheck-completion-system)

;; Flycheck requires gcc 4.8 at least
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (add-hook 'prog-mode-hook #'global-flycheck-mode) ; Enable where possible
  :config
  (setq-default flycheck-disabled-checkers '(tex-chktex tex-lacheck)) ; Leave out LaTeX
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-standard-error-navigation nil
        flycheck-check-syntax-automatically '(save mode-enabled))

  (cond ((eq dotemacs-selection 'helm) (setq flycheck-completion-system 'helm))
        ((eq dotemacs-selection 'ido)  (setq flycheck-completion-system 'ido)))

  (use-package flycheck-pos-tip ; Show flycheck messages in popups
    :ensure t
    :functions flycheck-pos-tip-error-messages
    :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
