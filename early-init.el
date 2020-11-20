;; early-init.el  -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 100 -*-
;; Only for use with emacs 27 or higher

;; ;; Initialize installed packages
;; (setq package-enable-at-startup t)

;; ;; Allow loading from the package cache.
;; (setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; disable tool-bar and menu-bar before being initialized
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
