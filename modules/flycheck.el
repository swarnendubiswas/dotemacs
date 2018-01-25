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
        flycheck-highlighting-mode 'lines ; faster than the default
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

(use-package flycheck-popup-tip ; Show error messages in popups
  :ensure t
  :after flycheck
  :init (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode))

(provide 'flycheck)

;;; flycheck-init.el ends here
