;;; flycheck-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-mode-line-theme)

(use-package flycheck
  :ensure t
  :init (add-hook 'prog-mode-hook #'global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(tex-chktex tex-lacheck)) ; Leave out LaTeX
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-display-errors-delay 0.5
        flycheck-clang-language-standard "c++11")
  (when (eq dotemacs-mode-line-theme 'spaceline)
    (setq flycheck-mode-line nil)))

(use-package avy-flycheck
  :ensure t
  :ensure avy
  :after avy flycheck
  :config
  ;; Binds avy-flycheck-goto-error to C-c ! g
  (avy-flycheck-setup))

;; Control appearance of flycheck messages
(or (use-package flycheck-pos-tip ; Show error messages in popups
      :ensure t
      :after flycheck
      :disabled t ; Hinders visibility
      :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
      :config
      ;; Long timeouts hinder visibility
      (setq flycheck-pos-tip-timeout 2))

    (use-package flycheck-title
      :ensure t
      :disabled t
      :after flycheck
      :config (flycheck-title-mode 1))

    (use-package flycheck-popup-tip
      :ensure t
      :after flycheck
      :init (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode)))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
