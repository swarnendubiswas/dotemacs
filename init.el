;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defgroup sb/emacs
  nil
  "Personal configuration for dotemacs."
  :group 'local)

;;; Load path
;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.
(dolist (dir '("modules"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(require 'init-config)
(require 'init-packages)
(require 'init-core)
(require 'init-ui)
(require 'init-buffer)
(require 'init-dired)
(require 'init-completion)
(require 'init-project)
(require 'init-spell)
(require 'init-search)
(require 'init-misc)
(require 'init-checkers)
(require 'init-languages)
(require 'init-vcs)
(require 'init-org)
(require 'init-functions)
(require 'init-temp)
(require 'init-keybindings)

;; https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook
          (lambda ()
            (let (;;(packages  (length package-activated-list))
                  (gc-time   (float-time gc-elapsed)))
              ;; (message "Emacs ready (init time = %s, packages = %d, gc time = %.2fs, gc count = %d)."
              ;;          (emacs-init-time) packages gc-time gcs-done)
              (message "Emacs ready (init time = %s, gc time = %.2fs, gc count = %d)."
                       (emacs-init-time) gc-time gcs-done))))

;;; init.el ends here
