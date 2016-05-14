;;; cedet-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup cedet, ede, and semantic mode.

;;; Code:

(use-package semantic
  :preface
  (defun dotemacs-semantic-functions ()
    (semantic-mode 1)
    (global-semanticdb-minor-mode 1)
    (global-semantic-highlight-func-mode 1)
    (global-semantic-decoration-mode 1)
    (global-semantic-idle-local-symbol-highlight-mode 1)
    ;; (global-semantic-idle-summary-mode 1)
    ;; (global-semantic-idle-completions-mode 1)
    )
  :config
  (setq semanticdb-default-save-directory (concat dotemacs-temp-directory "semanticdb"))
  ;; Ensure semantic can get info from gnu global
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)
  (add-hook 'prog-mode-hook #'dotemacs-semantic-functions))

(use-package cedet
  :disabled t
  :init (require 'cedet-files))

(use-package ede
  :disabled t
  :init
  (global-ede-mode 1)
  (ede-enable-generic-projects))

(use-package idle
  :disabled t
  :preface
  (defun dotemacs-idle-functions ()
    (global-semantic-idle-scheduler-mode 1)
    (global-semantic-idle-completions-mode 1)
    (global-semantic-idle-breadcrumbs-mode 1))
  :config (add-hook 'prog-mode-hook #'dotemacs-idle-functions))

(use-package mode
  :disabled t
  :init (add-hook 'prog-mode-hook #'global-srecode-minor-mode 1))

(use-package ecb
  :ensure t
  :disabled t
  :init
  (setq-default ecb-tip-of-the-day nil)
  (add-hook 'prog-mode-hook #'ecb-minor-mode))

(provide 'cedet-init)

;;; cedet-init.el ends here
