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

(require 'package)

;; Avoid loading packages twice, this is set during `(package-initialize)'
(setq package-enable-at-startup t
      package-user-dir (expand-file-name "elpa" user-emacs-directory))

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")        t)
  (add-to-list 'package-archives '("celpa" . "https://celpa.conao3.com/packages/") t)
  (add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/")           t))

;; Initialise the package management system. Another option is to construct the `load-path'
;; manually.
;; (add-to-list 'load-path sb/extras-directory)
;; (add-to-list 'load-path (concat package-user-dir "magit-20170715.1731"))

(add-to-list 'load-path (concat user-emacs-directory "modules"))

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

(when (boundp 'package-native-compile)
  (setq package-native-compile t))

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
      ;; Disable loading of `default.el' at startup, inhibits site
      inhibit-default-init t)

;; Do not resize the frame at this early stage. Resizing the Emacs frame can be a terribly expensive
;; part of changing the font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      ;; Do not compact font caches during GC
      inhibit-compacting-font-caches t
      inhibit-startup-echo-area-message t
      ;; `inhibit-splash-screen' is an alias
      inhibit-startup-screen t
      ;; Prefer new files to avoid loading stable bytecode
      load-prefer-newer t
      ;; *scratch* is in `lisp-interaction-mode' by default. `text-mode' is more expensive to start,
      ;; but I use *scratch* for composing emails.
      initial-major-mode 'text-mode
      initial-scratch-message nil)

;; Disable UI elements before being initialized. Use `display-graphic-p', `window-system' is
;; deprecated
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;; (customize-set-variable 'menu-bar-mode nil)

(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/

;; Maximize Emacs on startup, append to the hook instead of prepending, this means it will run after
;; other hooks that might fiddle with the frame size
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Applied to every Emacs frame
;; (add-to-list 'default-frame-alist '(maximized . maximized))
;; Applied only to the initial (startup) Emacs frame
;; (add-to-list 'initial-frame-alist '(maximized . maximized))

(let ((file-name-handler-alist-orig file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook (lambda ()
                               (setq file-name-handler-alist file-name-handler-alist-orig))))

(provide 'early-init)

;;; early-init.el ends here
