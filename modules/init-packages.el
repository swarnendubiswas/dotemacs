;;; init-packages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; Bootstrap `straight.el'

(defvar sb/EMACS27+)
(defvar use-package-enable-imenu-support)
(defvar use-package-hook-name-suffix)

;; To update packages with `straight', run `straight-pull-package' to get the latest version of a
;; given package or `straight-pull-all' to update everything, and then `straight-freeze-versions'
;; to persist the on-disk versions to a lockfile. Run `straight-thaw-versions' to reset on-disk
;; packages to their locked versions, making the config totally reproducible across environments.
(when (bound-and-true-p sb/disable-package.el)
  (defvar bootstrap-version)
  (defvar straight-build-dir)
  (defvar straight-check-for-modifications)
  (defvar straight-profiles)
  (defvar straight-use-package-by-default)
  (defvar straight-disable-native-compile)
  (defvar straight-base-dir)
  (defvar straight-vc-git-default-clone-depth)

  (declare-function straight-freeze-versions "straight")
  (declare-function straight-use-package "straight")

  (setf straight-profiles `((nil . "straight.lockfile.el")))

  (setq straight-build-dir (format "build/%d%s%d"
                                   emacs-major-version
                                   version-separator
                                   emacs-minor-version)
        ;; Do not check packages on startup to reduce load time
        straight-check-for-modifications '(check-on-save find-when-checking)
        straight-use-package-by-default t
        ;; There is no need to download the whole Git history, and a single branch often suffices.
        ;; FIXME: However, that seems to lead to "git revision parsing" errors while using
        ;; `straight-pull-all' and `straight-freeze-versions', which is irritating. The error may
        ;; also be due to renaming of `master' to `main'.
        straight-vc-git-default-clone-depth '(1 single-branch)
        straight-disable-native-compile nil)

  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; These variables need to best before loading `use-package'.
  (setq use-package-enable-imenu-support t
        use-package-hook-name-suffix     nil)

  (straight-use-package 'use-package))

;; (unless (bound-and-true-p sb/disable-package.el)
;;   (with-eval-after-load 'package
;;     (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")        t)
;;     (add-to-list 'package-archives '("celpa" . "https://celpa.conao3.com/packages/") t)
;;     (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/")           t))

;;   ;; Initialise the package management system. Another option is to construct the `load-path'
;;   ;; manually, e.g., "(add-to-list 'load-path (concat package-user-dir "magit-20170715.1731"))".
;;   (package-initialize)

;;   (unless (package-installed-p 'use-package)
;;     (package-refresh-contents)
;;     (package-install 'use-package))

;;   (defvar use-package-always-ensure)

;;   ;; Avoid manual installations whenever I modify package installations
;;   (setq use-package-always-ensure t
;;         ;; These variables need to best before loading `use-package'
;;         use-package-enable-imenu-support t
;;         use-package-hook-name-suffix nil)

;;   (eval-when-compile
;;     (require 'use-package)))

;; If we omit `:defer', `:hook', `:commands', or `:after', then the package is loaded immediately.
;; We do not need `:commands' with `:hook' or `:bind'. The setting `use-package-always-defer'
;; implies always load features lazily unless told otherwise. This implies we should use
;; `after-init' hook or `:init' instead of `:config', since otherwise packages may not be loaded. Be
;; careful about using `:after' and always deferring loading, because then we will need to specifiy
;; alternate ways of loading the package.
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading

;; Hooks in the `:hook' section run in reverse order. Example:
;; (use-package package-name
;;   :hook
;;   ((x-mode-hook . last)
;;    (x-mode-hook . second)
;;    (x-mode-hook . first)))

(defvar use-package-compute-statistics)
(defvar use-package-verbose)
(defvar use-package-expand-minimally)
(defvar use-package-always-defer)
(defvar use-package-minimum-reported-time)

(if (bound-and-true-p sb/debug-init-file)
    (progn
      (setq debug-on-error                 t
            debug-on-event                 'sigusr2
            use-package-compute-statistics t ; Use "M-x use-package-report" to see results
            use-package-verbose            t
            use-package-minimum-reported-time 0 ; Show everything
            use-package-expand-minimally   nil))
  (progn
    (setq use-package-always-defer       t
          ;; Disable error checks during macro expansion because the configuration just works
          use-package-expand-minimally   t
          use-package-compute-statistics nil
          use-package-verbose            nil)))

;; One suggested order of `use-package' keywords:
;; https://github.com/radian-software/radian/blob/develop/emacs/use-package-keywords.md

;; Feature `straight-x' from package `straight' provides experimental/unstable extensions to
;; straight.el which are not yet ready for official inclusion.
(use-package straight-x
  :straight nil
  :commands straight-x-fetch-all)

(use-package diminish
  :demand t)

;; Allow GC to happen after a period of idle time, initialize this package early
(use-package gcmh
  :commands gcmh-idle-garbage-collect
  :hook
  (after-init-hook . gcmh-mode)
  :diminish)

;; Package `bind-key' provides macros `bind-key', `bind-key*', and `unbind-key' which provides a
;; much prettier API for manipulating keymaps than `define-key' and `global-set-key'. "C-h b" lists
;; all the bindings available in a buffer, "C-h m" shows the keybindings for the major and the minor
;; modes.
(use-package bind-key
  :functions bind-key--remove
  :bind
  ("C-c d k" . describe-personal-keybindings))

(use-package no-littering
  :demand t)

;; We can do `package-list-packages', then press `u' and `x'. The only thing missing from "paradox"
;; is `paradox-upgrade-packages' as a single command. Emacs 29 should have a `package-update-all'
;; command.
(with-eval-after-load "no-littering"
  (when (not (bound-and-true-p sb/disable-package.el))
    (when (boundp 'package-quickstart)
      (setq package-quickstart t)))

  (bind-keys :package package
             ("C-c d p" . package-quickstart-refresh)
             ("C-c d l" . package-list-packages)))

(defcustom sb/custom-file
  (no-littering-expand-var-file-name "custom.el")
  "File to write Emacs customizations."
  :type  'string
  :group 'sb/emacs)

;; NOTE: Make a symlink to "private.el" in "$HOME/.emacs.d/etc".
(defcustom sb/private-file
  (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type  'string
  :group 'sb/emacs)

;; Asynchronously byte compile packages installed with `package.el'
(use-package async
  :straight (async :type git :host github :repo "jwiegley/emacs-async")
  :commands async-bytecomp-package-mode
  :init (async-bytecomp-package-mode 1))

;; Get PATH with "(getenv "PATH")".
;; Set PATH with "(setenv "PATH" (concat (getenv "PATH") ":/home/swarnendu/bin"))".

;; These are alternative ways to manipulate the `exec-path'.
;; "(setq exec-path (append exec-path (expand-file-name "node_modules/.bin" sb/user-tmp-directory)))"
;; "(add-to-list 'exec-path (expand-file-name "node_modules/.bin" sb/user-tmp-directory))"
(use-package exec-path-from-shell
  :defines exec-path-from-shell-check-startup-files
  :commands exec-path-from-shell-initialize
  :if (or (daemonp) (not (display-graphic-p)))
  :init
  ;; "-i" is expensive but Tramp is unable to find executables without the option. Furthermore,
  ;; other executables like "prettier" from $PATH are also not found.
  (setq exec-path-from-shell-arguments '("-l" "-i")
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "JAVA_HOME" "LANG" "LC_CTYPE" "LC_ALL" "TERM"))
  (exec-path-from-shell-initialize))

(provide 'init-packages)

;;; init-packages.el ends here
