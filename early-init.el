;;; early-init.el --- Emacs Customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;; This file is supported from Emacs 27+, and is run before package and UI initialization.

;;; Code:

(setq byte-compile-warnings
  '
  (not unresolved
    free-vars
    callargs
    redefine
    obsolete
    noruntime
    lexical
    make-local
    cl-functions
    interactive-only))

(defconst sb/emacs-4MB (* 4 1024 1024))
(defconst sb/emacs-1GB (* 1 1024 1024 1024))

;; Defer GC during startup
(setq
  garbage-collection-messages nil
  gc-cons-percentage 0.3 ; Portion of heap used for allocation
  gc-cons-threshold most-positive-fixnum)

;; GC may happen after this many bytes are allocated since last GC. If you experience freezing,
;; decrease this. If you experience stuttering, increase this.
(defun sb/defer-gc-during-exec ()
  "Defer garbage collection during execution."
  (setq gc-cons-threshold sb/emacs-1GB))

;; Ideally, we should reset `gc-cons-threshold' to its default value otherwise there can be
;; large pause times whenever GC eventually happens. But `lsp-mode' suggests increasing the limit
;; permanently.
;; https://github.com/emacs-lsp/lsp-mode#performance
(defun sb/restore-gc-during-exec ()
  "Restore garbage collection threshold during execution."
  (setq gc-cons-threshold sb/emacs-4MB))

(add-hook 'emacs-startup-hook #'sb/restore-gc-during-exec)
(add-hook 'minibuffer-setup-hook #'sb/defer-gc-during-exec)
(add-hook 'minibuffer-exit-hook #'sb/restore-gc-during-exec)

;; https://github.com/hlissner/doom-emacs/issues/3372#issuecomment-643567913
;; Get a list of loaded packages that depend on `cl' by calling the following.

;; (require 'loadhist)
;; (file-dependents (feature-file 'cl))

;; The run-time load order is: (1) file described by `site-run-file' if non-nil, (2)
;; `user-init-file', and (3) `default.el'.

(setq
  site-run-file nil ; Disable site-wide run-time initialization
  ;; Disable loading of `default.el' at startup
  inhibit-default-init t)

;; Do not resize the frame to preserve the number of columns or lines being displayed when setting
;; font, menu bar, tool bar, tab bar, internal borders, fringes, or scroll bars. Resizing the Emacs
;; frame can be a terribly expensive part of changing the font. By inhibiting this, we easily halve
;; startup times with fonts that are larger than the system default.
(setq
  frame-inhibit-implied-resize t
  frame-resize-pixelwise t
  window-resize-pixelwise t
  inhibit-compacting-font-caches t ; Do not compact font caches during GC
  inhibit-startup-echo-area-message t
  inhibit-startup-screen t ; `inhibit-splash-screen' is an alias
  ;; *scratch* is in `lisp-interaction-mode' by default. `text-mode' is more expensive to start,
  ;; but I use *scratch* for composing emails.
  initial-major-mode 'text-mode
  initial-scratch-message nil
  ;; Prefer new files to avoid loading stale bytecode
  load-prefer-newer t)

;; Disable UI elements early before being initialized. Use `display-graphic-p' since `window-system'
;; is deprecated.
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; The menu bar can be useful to identify different capabilities available and their shortcuts.
(menu-bar-mode -1)

;; Set a hint of transparency, works with GUI frames
(set-frame-parameter (selected-frame) 'alpha '(98 . 98))
(add-to-list 'default-frame-alist '(alpha . (98 . 98)))

;; Maximize Emacs on startup.
;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/

;; Applies only to the initial (startup) Emacs frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Applies to every Emacs frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Append to the hook instead of prepending, this means it will run after other hooks that might
;; fiddle with the frame size.

;; (add-hook 'emacs-startup-hook #'toggle-frame-maximized t)

(let ((file-name-handler-alist-orig file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
    (lambda ()
      (setq file-name-handler-alist file-name-handler-alist-orig)
      (garbage-collect))
    t))

;; Avoid loading packages twice, this is set during `(package-initialize)'. This is also useful if
;; we prefer "straight.el" over "package.el".
(setq package-enable-at-startup nil)

(when (featurep 'native-compile)
  (defvar package-native-compile)
  (defvar native-comp-always-compile)
  (defvar native-comp-async-report-warnings-errors)

  ;; Silence compiler warnings as they can be pretty disruptive
  (setq
    native-comp-async-report-warnings-errors nil
    package-native-compile t ; Enable ahead-of-time compilation when installing a package
    ;; Compile loaded packages asynchronously
    native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache" user-emacs-directory)))

(provide 'early-init)

;;; early-init.el ends here
