;;; early-init.el --- Emacs Customization -*- lexical-binding: t; mode:
;;; emacs-lisp; coding: utf-8; no-byte-compile: t; fill-column: 80 -*-

;; Swarnendu Biswas

;;; Commentary:

;; This file is supported from Emacs 27+, and is run before package and UI
;; initialization.

;;; Code:

(defconst sb/emacs-4MB (* 4 1024 1024))
(defconst sb/emacs-1GB (* 1 1024 1024 1024))

;; Defer GC during startup
(setq
 gc-cons-percentage 0.8 ; Portion of heap used for allocation
 gc-cons-threshold sb/emacs-1GB)

;; GC may happen after this many bytes are allocated since last GC. If you
;; experience freezing, decrease this. If you experience stuttering, increase
;; this.
(defun sb/defer-gc ()
  "Defer garbage collection during execution."
  (setq gc-cons-threshold sb/emacs-1GB))

;; There will be large pause times with large `gc-cons-threshold' values
;; whenever GC eventually happens. `lsp-mode' suggests increasing the limit
;; permanently to a reasonable value.
(defun sb/restore-gc ()
  "Restore garbage collection threshold during execution."
  (setq
   gc-cons-threshold sb/emacs-4MB
   gc-cons-percentage 0.3))

(add-hook 'emacs-startup-hook #'sb/restore-gc)
(add-hook 'minibuffer-setup-hook #'sb/defer-gc)
(add-hook 'minibuffer-exit-hook #'sb/restore-gc)

;; https://github.com/hlissner/doom-emacs/issues/3372#issuecomment-643567913
;; Get a list of loaded packages that depend on `cl' by calling the following.

;; (require 'loadhist)
;; (file-dependents (feature-file 'cl))

;; The run-time load order is: (1) file described by `site-run-file' if non-nil,
;; (2) `user-init-file', and (3) `default.el'.
(setq
 site-run-file nil ; Disable site-wide run-time initialization
 ;; Disable loading of `default.el' at startup
 inhibit-default-init t)

;; Do not resize the frame to preserve the number of columns or lines being
;; displayed when setting font, menu bar, tool bar, tab bar, internal borders,
;; fringes, or scroll bars.
(setq
 frame-inhibit-implied-resize t
 frame-resize-pixelwise t
 window-resize-pixelwise t
 inhibit-startup-echo-area-message t
 inhibit-startup-screen t ; `inhibit-splash-screen' is an alias
 ;; *scratch* is in `lisp-interaction-mode' by default. I use *scratch* for
 ;; composing emails, but `text-mode' is more expensive to start. Furthermore,
 ;; lsp support is not enabled for the *scratch* buffer.
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil)

;; Disable UI elements early before being initialized. Use `display-graphic-p'
;; since `window-system' is deprecated.
(scroll-bar-mode -1)
;; The menu bar can be useful to identify different capabilities available and
;; their shortcuts.
(push '(menu-bar-lines . 0) default-frame-alist)
;; This is faster than running "(tool-bar-mode -1)"
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set a hint of transparency, works with GUI frames
(set-frame-parameter (selected-frame) 'alpha '(97 . 97))
(add-to-list 'default-frame-alist '(alpha . (97 . 97)))

;; Maximize Emacs on startup.

;; Applies only to the initial (startup) Emacs frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Applies to every Emacs frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(let ((file-name-handler-alist-orig file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist file-name-handler-alist-orig)
              (garbage-collect))
            t))

;; Avoid loading packages twice, this is set during `(package-initialize)'. This
;; is also useful if we prefer "straight.el" over "package.el".
;; (setq package-enable-at-startup nil)

(when (featurep 'native-compile)
  (setq
   ;; Silence compiler warnings as they can be pretty disruptive
   native-comp-async-report-warnings-errors nil
   package-native-compile t ; Enable ahead-of-time compilation when installing a package
   ;; Compile loaded packages asynchronously
   native-comp-deferred-compilation t))

(setenv "LSP_USE_PLISTS" "true")

;; The value of font height is in 1/10pt, so 100 implies 10pt. Font preferences
;; will be ignored when we use TUI Emacs. Then, the terminal font setting will
;; be used.

;; (add-to-list 'default-frame-alist '(font . "JetBrainsMonoNF-17"))

(defun sb/init-fonts-graphic ()
  (cond
   ((string= (system-name) "inspiron-7572")
    (progn
      (set-face-attribute 'default nil :font "JetBrainsMono NF" :height 220)
      (set-face-attribute 'mode-line nil :height 160)
      (set-face-attribute 'mode-line-inactive nil :height 160)))

   ((string= (system-name) "dell-7506")
    (progn
      (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 150)
      (set-face-attribute 'mode-line nil :height 120)
      (set-face-attribute 'mode-line-inactive nil :height 120)))

   ((string= (system-name) "cseiitk")
    (progn
      ;; Alternate options: "Hack Nerd Font", "MesloLGS Nerd Font"
      ;;   (set-face-attribute 'default nil :font "JetBrainsMono NF" :height 210)
      (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 210)
      (set-face-attribute 'mode-line nil :height 140)
      (set-face-attribute 'mode-line-inactive nil :height 140)))))

(add-hook 'emacs-startup-hook #'sb/init-fonts-graphic)

;; (defun sb/init-fonts-daemon (frame)
;;   (message "getting called")
;;   (cond
;;     ((string= (system-name) "inspiron-7572")
;;       (progn
;;         ;; (add-to-list 'default-frame-alist '(font . "JetBrainsMonoNF-18"))
;;         (add-to-list 'default-frame-alist '(font . "MesloLGSNF-20"))))))
;; FIXME: The hook is not working.
;; (add-hook 'server-after-make-frame-functions #'sb/init-fonts-daemon 'append)

(when (daemonp)
  (cond
   ((string= (system-name) "inspiron-7572")
    (progn
      ;; (add-to-list 'default-frame-alist '(font . "JetBrainsMonoNF-18"))
      (add-to-list 'default-frame-alist '(font . "MesloLGSNF-18"))))

   ((string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
    (progn
      (set-face-attribute 'default nil
                          :font "JetBrainsMono Nerd Font"
                          :height 180)))))

(provide 'early-init)

;;; early-init.el ends here
