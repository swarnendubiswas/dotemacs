;;; cedet-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup cedet, ede, and semantic mode.

;;; Code:

(use-package cedet
  :disabled t
  :load-path "lisp/cedet-1.1/common/"
  :config
  (require 'cedet-files))

(use-package ede
  :disabled t
  :config
  (global-ede-mode 1)
  (ede-enable-generic-projects))

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
;;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator
;;(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberant ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languages only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;;(semantic-load-enable-excessive-code-helpers)
;;(semantic-load-enable-semantic-debugging-helpers)

;; Copied from http://stackoverflow.com/questions/10326001/updating-cedet-1-0-to-1-1
;;(semantic-load-enable-excessive-code-helpers)

(use-package semantic
  :disabled t
  ;;:load-path "lisp/cedet-1.1/semantic/"
  :config
  (require 'semantic-ia)
  (require 'semantic-loaddefs)
  ;; Copied from https://github.com/randomphrase/dotfiles/blob/master/emacs.d/lisp/init/semantic.el
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
  ;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
  ;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  ;; (semantic-mode 1)
  (require 'semanticdb)
  ;;SemanticDB files
  (setq semanticdb-default-save-directory (concat dotemacs-temp-directory "semanticdb"))
  ;; Ensure semantic can get info from gnu global
  ;; (require 'semantic/db-global)
  ;; (semanticdb-enable-gnu-global-databases 'c-mode)
  ;; (semanticdb-enable-gnu-global-databases 'c++-mode))
  
  (global-semanticdb-minor-mode 1)
  (global-semantic-highlight-func-mode 1))

(use-package idle
  :config
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-idle-completions-mode 1)
  (global-semantic-idle-breadcrumbs-mode 1))

(use-package mode
  :config
  (global-semantic-decoration-mode 1)
  ;; Enable SRecode (Template management) minor-mode.
  (global-srecode-minor-mode 1))

(use-package ecb
  :ensure t
  :config
  (setq-default ecb-tip-of-the-day nil)
  (add-hook 'prog-mode-hook 'ecb-auto-activate))

(provide 'cedet-init)

;;; cedet-init.el ends here
