;;; cedet-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup cedet, ede, and semantic mode.

;;; Code:

(use-package cedet
  :disabled t
  :init (require 'cedet-files))

(use-package ede
  :disabled t
  :init
  (global-ede-mode 1)
  (ede-enable-generic-projects))

(use-package semantic
  :preface
  (defun dotemacs-semantic-functions ()
    (semantic-mode 1)
    (global-semanticdb-minor-mode 1)
    (global-semantic-highlight-func-mode 1)
    (global-semantic-decoration-mode 1)
    (global-semantic-idle-local-symbol-highlight-mode 1)
    (global-semantic-idle-summary-mode 1))
  :config
  ;; https://github.com/randomphrase/dotfiles/blob/master/emacs.d/lisp/init/semantic.el
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
  ;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

  (setq semanticdb-default-save-directory (concat dotemacs-temp-directory "semanticdb"))
  ;; Ensure semantic can get info from gnu global
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)
  (add-hook 'prog-mode-hook #'dotemacs-semantic-functions))

(use-package idle
  :preface
  (defun dotemacs-idle-functions ()
    (global-semantic-idle-scheduler-mode 1)
    (global-semantic-idle-completions-mode 1)
    (global-semantic-idle-breadcrumbs-mode 1))
  :config (add-hook 'prog-mode-hook #'dotemacs-idle-functions))

(use-package mode
  :init (add-hook 'prog-mode-hook #'global-srecode-minor-mode 1))

(use-package ecb
  :ensure t
  :disabled t
  :load-path "elpa/ecb-20140215.114"
  :init
  (setq-default ecb-tip-of-the-day nil)
  (add-hook 'prog-mode-hook #'ecb-minor-mode)
  :config
  (defhydra hydra-ecb (:color blue)
    "ecb commands"
    ("g h" ecb-goto-window-history "history")
    ("g m" ecb-goto-window-methods "methods")
    ("g s" ecb-goto-window-sources "sources")
    ("g d" ecb-goto-window-directories "directories")
    ("g y" ecb-goto-window-symboldef "symbol def")
    ("l u" ecb-redraw-layout "redraw layout"))
  (bind-key "C-c ." 'hydra-ecb/body))

(provide 'cedet-init)

;;; cedet-init.el ends here
