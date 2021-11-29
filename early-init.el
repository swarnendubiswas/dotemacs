;;; early-init.el --- Emacs Customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;; Only for use with Emacs 27 or higher. This is run before package and UI initialization.

;;; Code:

(defconst sb/EMACS27+   (> emacs-major-version 26))
(defconst sb/EMACS28+   (> emacs-major-version 27))
(defconst sb/IS-LINUX   (eq system-type 'gnu/linux))
(defconst sb/IS-WINDOWS (eq system-type 'windows-nt))

(defconst sb/emacs-4MB    (*       4 1024 1024))
(defconst sb/emacs-8MB    (*       8 1000 1024))
(defconst sb/emacs-64MB   (*      64 1024 1024))
(defconst sb/emacs-128MB  (*     128 1024 1024))
(defconst sb/emacs-256MB  (*     256 1024 1024))
(defconst sb/emacs-512MB  (*     512 1024 1024))
(defconst sb/emacs-8GB    (*  8 1024 1024 1024))
(defconst sb/emacs-16GB   (* 16 1024 1024 1024))

;; GC may happen after this many bytes are allocated since last GC If you experience freezing,
;; decrease this. If you experience stuttering, increase this.
(defun sb/defer-garbage-collection ()
  "Defer garbage collection."
  (setq gc-cons-percentage 0.1
        gc-cons-threshold sb/emacs-16GB))

;; Ideally, we would have reset `gc-cons-threshold' to its default value otherwise there can be
;; large pause times whenever GC eventually happens. But `lsp-mode' suggests increasing the limit
;; permanently.
(defun sb/restore-garbage-collection ()
  "Restore garbage collection."
  (setq gc-cons-percentage 0.1
        ;; https://github.com/emacs-lsp/lsp-mode#performance
        gc-cons-threshold sb/emacs-8MB))

;; `emacs-startup-hook' runs later than the `after-init-hook', it is the last hook to load
;; customizations.
(add-hook 'emacs-startup-hook    #'sb/restore-garbage-collection)
(add-hook 'minibuffer-setup-hook #'sb/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook  #'sb/restore-garbage-collection)

(require 'package)

;; Avoid loading packages twice, this is set during `(package-initialize)'
(setq package-enable-at-startup nil
      package-user-dir (expand-file-name "elpa" user-emacs-directory))

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")        t)
  (add-to-list 'package-archives '("celpa" . "https://celpa.conao3.com/packages/") t)
  (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/")           t))

(add-to-list 'load-path (concat user-emacs-directory "modules"))

;; Initialise the package management system. Another option is to construct the `load-path'
;; manually.
;; (add-to-list 'load-path sb/extras-directory)
;; (add-to-list 'load-path (concat package-user-dir "magit-20170715.1731"))
(package-initialize)

(unless (package-installed-p 'no-littering)
  (package-refresh-contents nil)
  (package-install 'no-littering))

(require 'no-littering)

;; (customize-set-variable 'package-quickstart t)

(setq package-quickstart t ; Populate one big autoloads file
      package-quickstart-file (no-littering-expand-var-file-name "package-quickstart.el"))

;; Emacs 28+.
;; FIXME: How to prevent Emacs from using `("eln-cache" user-emacs-directory)'?
(when (boundp 'native-comp-eln-load-path)
  ;; (setcar native-comp-eln-load-path
  ;;         (no-littering-expand-var-file-name "eln-cache"))
  (add-to-list 'native-comp-eln-load-path (no-littering-expand-var-file-name "eln-cache")))

(when sb/EMACS28+
  (setq package-native-compile t
        native-comp-always-compile t))

;; https://github.com/kiwanami/emacs-epc/issues/35
;; http://tsengf.blogspot.com/2011/06/disable-byte-compile-warning-in-emacs.html
(setq byte-compile-warnings '(not nresolved free-vars callargs redefine obsolete noruntime
                                  cl-functions interactive-only))

;; https://github.com/hlissner/doom-emacs/issues/3372#issuecomment-643567913
;; Get a list of loaded packages that depend on `cl' by calling the following
;; (require 'loadhist)
;; (file-dependents (feature-file 'cl))

;; Defer GC during startup
(setq garbage-collection-messages nil
      gc-cons-percentage 0.6 ; Portion of heap used for allocation
      gc-cons-threshold most-positive-fixnum)

;; The run-time load order is: (1) file described by `site-run-file', if non-nil, (2)
;; `user-init-file', and (3) `default.el'.
(setq site-run-file nil ; Disable site-wide run-time initialization
      ;; Disable loading of `default.el' at startup
      inhibit-default-init t)

;; Do not resize the frame at this early stage. Resizing the Emacs frame can be a terribly expensive
;; part of changing the font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      window-resize-pixelwise t
      inhibit-compacting-font-caches t ; Do not compact font caches during GC
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t ; `inhibit-splash-screen' is an alias
      load-prefer-newer t ; Prefer new files to avoid loading stable bytecode
      ;; *scratch* is in `lisp-interaction-mode' by default. `text-mode' is more expensive to start,
      ;; but I use *scratch* for composing emails.
      initial-major-mode 'text-mode
      initial-scratch-message nil)

;; Disable UI elements early before being initialized. Use `display-graphic-p', `window-system' is
;; deprecated.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/

;; Applied only to the initial (startup) Emacs frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Applied to every Emacs frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Maximize Emacs on startup, append to the hook instead of prepending, this means it will run after
;; other hooks that might fiddle with the frame size
;; (add-hook 'emacs-startup-hook #'toggle-frame-maximized t)

(let ((file-name-handler-alist-orig file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook (lambda ()
                               (setq file-name-handler-alist file-name-handler-alist-orig))))

(provide 'early-init)

;;; early-init.el ends here
