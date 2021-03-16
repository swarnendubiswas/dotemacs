;;; early-init.el --- Emacs Customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;; Only for use with Emacs 27 or higher. This is run before package and UI initialization.

;;; Code:

(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name (convert-standard-filename "var/eln-cache/")
                            user-emacs-directory)))

;; https://github.com/kiwanami/emacs-epc/issues/35
(setq byte-compile-warnings '(cl-functions))

;; https://github.com/hlissner/doom-emacs/issues/3372#issuecomment-643567913
;; Get a list of loaded packages that depend on `cl' by calling the following
;; (require 'loadhist)
;; (file-dependents (feature-file 'cl))

;; Defer GC during startup
(setq garbage-collection-messages nil
      gc-cons-percentage 0.6 ; Portion of heap used for allocation
      gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer t
      package-enable-at-startup nil ; Avoid loading packages twice
      package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-native-compile t
      package-quickstart t ; Populate one big autoloads file
      package-quickstart-file (expand-file-name "var/package-quickstart.el"
                                                user-emacs-directory))
;; (customize-set-variable
;;  'package-quickstart-file
;;  (expand-file-name "tmp/package-quickstart.el" user-emacs-directory))
;; (customize-set-variable 'package-quickstart t)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("celpa" . "https://celpa.conao3.com/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Do not resize the frame at this early stage. Resizing the Emacs frame can be a terribly expensive
;; part of changing the font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Disable UI elements before being initialized
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; (customize-set-variable 'menu-bar-mode nil)
;; (customize-set-variable 'tool-bar-mode nil)

;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Maximize all frames
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Maximize all frames

(let (
      (file-name-handler-alist-orig file-name-handler-alist)
      )
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook
            (lambda ()
              (setq file-name-handler-alist file-name-handler-alist-orig)))
  )

(provide 'early-init)

;;; early-init.el ends here
