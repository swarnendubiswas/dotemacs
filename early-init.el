;; early-init.el -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; no-byte-compile: nil;
;; fill-column: 100 -*- Only for use with emacs 27 or higher

(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name (convert-standard-filename "var/eln-cache/")
                            user-emacs-directory)))

(defconst dotemacs-500mb (* 500 1000 1000))

;; Defer GC during startup
(setq gc-cons-percentage 0.6 ; Portion of heap used for allocation
      gc-cons-threshold dotemacs-500mb)

(setq load-prefer-newer t
      package-enable-at-startup nil ; Avoid loading packages twice
      package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-quickstart nil
      package-quickstart-file (expand-file-name "var/package-quickstart.el"
                                                user-emacs-directory))
;; (customize-set-variable
;;  'package-quickstart-file
;;  (expand-file-name "tmp/package-quickstart.el" user-emacs-directory))
;; (customize-set-variable 'package-quickstart t)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Disable tool-bar and menu-bar before being initialized
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)

;; (customize-set-variable 'menu-bar-mode nil)
;; (customize-set-variable 'tool-bar-mode nil)

;; https://emacsredux.com/blog/2020/12/04/maximize-the-emacs-frame-on-startup/
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; Maximize all frames
