;;; cedet-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup cedet, ede, and semantic mode.

;;; Code:

(use-package cedet
  :load-path "lisp/cedet-1.1/common/"
  :config
  (require 'cedet-files))

(use-package ede
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
(semantic-load-enable-gaudy-code-helpers)

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
  ;;:load-path "lisp/cedet-1.1/semantic/"
  :config
  (require 'semantic-ia)
  (require 'semantic-loaddefs)
  (require 'semanticdb)
  ;;SemanticDB files
  (setq semanticdb-default-save-directory (concat emacs-temp-directory "semanticdb")))

(global-semanticdb-minor-mode 1)
(global-semantic-highlight-func-mode 1)

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
