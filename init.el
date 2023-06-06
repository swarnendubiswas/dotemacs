;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defgroup sb/emacs nil
  "Personal configuration for GNU Emacs."
  :group 'local)

(dolist (dir '("modules" "extras"))
  (push (expand-file-name dir user-emacs-directory) load-path))

;; Splitting the configuration across multiple files is much easier to maintain, and looks less
;; cluttered. The downside is that more files need to be loaded during startup, possibly affecting
;; performance.

(require 'init-config)
(require 'init-packages)
(require 'init-core)
(require 'init-buffer)
(require 'init-dired)

(cond
  ((eq sb/minibuffer-completion 'ivy)
    (require 'init-ivy))
  ((eq sb/minibuffer-completion 'vertico)
    (require 'init-vertico)))

(require 'init-project)
(require 'init-spell)
(require 'init-search)
(require 'init-parens)
(require 'init-misc)
(require 'init-checkers)
(require 'init-tags)

(cond
  ((eq sb/capf 'corfu)
    (require 'init-corfu))
  ((eq sb/capf 'company)
    (require 'init-company)))
;; It is recommended to load `yasnippet' before `eglot'
(require 'init-completion)

(require 'init-languages)

;; I work a lot over SSH, and `lsp-mode' is poor over Tramp. The alternative I used was to use TUI
;; Emacs. Eglot works better than `lsp-mode' over Tramp, which allows me to continue using GUI
;; Emacs. However, Eglot does not support multiple servers for a major-mode. For example, it will be
;; nice to have TexLab and Grammarly with LaTeX files.

(cond
  ((eq sb/lsp-provider 'lsp-mode)
    (require 'init-lsp))
  ((eq sb/lsp-provider 'eglot)
    (require 'init-eglot)))

(require 'init-vcs)
(require 'init-org)
(require 'init-latex)
(require 'init-functions)

;; Configure appearance-related settings at the end
(require 'init-ui)
(require 'init-theme)
(require 'init-keybindings)

(defvar sb/custom-file)
(defvar sb/private-file)

(setq custom-file sb/custom-file)

(let ((gc-cons-threshold most-positive-fixnum))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))
  (when (file-exists-p sb/private-file)
    (load sb/private-file 'noerror 'nomessage)))

;; Mark safe variables

(put 'compilation-read-command 'safe-local-variable #'stringp)

;; (put 'bibtex-completion-bibliography          'safe-local-variable #'listp)
;; (put 'company-bibtex-bibliography             'safe-local-variable #'listp)
;; (put 'company-clang-arguments                 'safe-local-variable #'listp)
;; (put 'counsel-find-file-ignore-regexp         'safe-local-variable #'stringp)
;; (put 'flycheck-checker                        'safe-local-variable #'listp)
;; (put 'flycheck-clang-include-path             'safe-local-variable #'listp)
;; (put 'flycheck-gcc-include-path               'safe-local-variable #'listp)
;; (put 'flycheck-python-pylint-executable       'safe-local-variable #'stringp)
;; (put 'lsp-clients-clangd-args                 'safe-local-variable #'listp)
;; (put 'lsp-latex-root-directory                'safe-local-variable #'stringp)
;; (put 'lsp-pyright-extra-paths                 'safe-local-variable #'listp)
;; (put 'projectile-enable-caching               'safe-local-variable #'stringp)
;; (put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
;; (put 'projectile-project-root                 'safe-local-variable #'stringp)
;; (put 'pyvenv-activate                         'safe-local-variable #'stringp)
;; (put 'reftex-default-bibliography             'safe-local-variable #'listp)
;; (put 'tags-table-list                         'safe-local-variable #'listp)

(when (eq sb/op-mode 'server)
  ;; Start server if not root user
  (unless (string-equal "root" (getenv "USER"))
    (when (and (fboundp 'server-running-p) (not (server-running-p)))
      (server-mode)
      (setq server-client-instructions nil))))

;; https://blog.d46.us/advanced-emacs-startup/
(add-hook
  'emacs-startup-hook
  (lambda ()
    (if (bound-and-true-p sb/disable-package.el)
      (let ((gc-time (float-time gc-elapsed)))
        (message "Emacs ready (init time = %s, gc time = %.2fs, gc count = %d)."
          (emacs-init-time)
          gc-time
          gcs-done))
      (let
        (
          (packages (length package-activated-list))
          (gc-time (float-time gc-elapsed)))
        (message "Emacs ready (init time = %s, packages = %d, gc time = %.2fs, gc count = %d)."
          (emacs-init-time)
          packages
          gc-time
          gcs-done)))))

;;; init.el ends here
