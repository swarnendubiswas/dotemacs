;;; early-init.el --- Emacs Customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8; fill-column: 80; -*-

;; Swarnendu Biswas

;;; Commentary:

;; This file is supported from Emacs 27+, and is run before package and UI
;; initialization.

;;; Code:

(defconst sb/emacs-4MB (* 4 1024 1024))
(defconst sb/emacs-64MB (* 64 1024 1024))
(defconst sb/emacs-1GB (* 1 1024 1024 1024))

;; Defer GC during startup
(setopt
 gc-cons-percentage 0.6 ; Portion of heap used for allocation
 ;; Temporarily increase GC threshold during startup
 gc-cons-threshold most-positive-fixnum)

;; GC may happen after this many bytes are allocated since last GC. If you
;; experience freezing, decrease this. If you experience stuttering, increase
;; this.
(defun sb/defer-gc ()
  "Defer garbage collection during execution."
  (setopt gc-cons-threshold sb/emacs-64MB))

;; `lsp-mode' suggests increasing the limit permanently to a reasonable value.
;; There will be large pause times with large `gc-cons-threshold' values
;; whenever GC eventually happens.
(defun sb/restore-gc ()
  "Restore garbage collection threshold during execution."
  (setopt
   gc-cons-threshold sb/emacs-4MB
   gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook #'sb/restore-gc)
(add-hook 'minibuffer-setup-hook #'sb/defer-gc)
(add-hook 'minibuffer-exit-hook #'sb/restore-gc)

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca
    :repo "https://github.com/progfolio/elpaca.git"
    :ref nil
    :depth 1
    :inherit ignore
    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
    :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list
   'load-path
   (if (file-exists-p build)
       build
     repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop
                    (apply #'call-process
                           `("git" nil ,buffer t "clone" ,@
                             (when-let* ((depth (plist-get order :depth)))
                               (list
                                (format "--depth=%d" depth)
                                "--no-single-branch"))
                             ,(plist-get order :repo) ,repo))))
                  ((zerop
                    (call-process "git"
                                  nil
                                  buffer
                                  t
                                  "checkout"
                                  (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop
                    (call-process emacs
                                  nil
                                  buffer
                                  nil
                                  "-Q"
                                  "-L"
                                  "."
                                  "--batch"
                                  "--eval"
                                  "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
          (progn
            (message "%s" (buffer-string))
            (kill-buffer buffer))
          (error "%s"
                 (with-current-buffer buffer
                   (buffer-string))))
      ((error)
       (warn "%s" err)
       (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil))
      (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; The run-time load order is: (1) file described by `site-run-file' if non-nil,
;; (2) `user-init-file', and (3) `default.el'.
(setq
 site-run-file nil ; Disable site-wide run-time initialization
 ;; Disable loading of `default.el' at startup
 inhibit-default-init t)

(setopt
 load-prefer-newer t
 ;; Do not resize the frame to preserve the number of columns or lines being
 ;; displayed when setting font, menu bar, tool bar, tab bar, internal borders,
 ;; fringes, or scroll bars.
 frame-inhibit-implied-resize t
 frame-resize-pixelwise t
 window-resize-pixelwise t
 inhibit-startup-screen t ; `inhibit-splash-screen' is an alias
 inhibit-startup-echo-area-message user-login-name
 initial-scratch-message nil
 ;; *scratch* is in `lisp-interaction-mode' by default.
 initial-major-mode 'fundamental-mode
 use-file-dialog nil
 use-dialog-box nil)

;; Suppress the vanilla startup screen completely. We've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

;; Disable UI elements early before being initialized. Use `display-graphic-p'
;; since `window-system' is deprecated.

;; This is faster than running "(tool-bar-mode -1)"
(push '(tool-bar-lines . 0) default-frame-alist)
;; The menu bar can be useful to identify different capabilities available and
;; their shortcuts.
(push '(menu-bar-lines . 0) default-frame-alist)
;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1))
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; Set a hint of transparency, works with GUI frames
(set-frame-parameter (selected-frame) 'alpha '(97 . 97))
(add-to-list 'default-frame-alist '(alpha . (97 . 97)))

;; Maximize Emacs on startup.

;; Applies only to the initial (startup) Emacs frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Applies to every Emacs frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Avoid loading packages twice, this is set during `(package-initialize)'. This
;; is also useful if we prefer "straight.el" or "Elpaca" over "package.el". We
;; cannot use `setq' for the following variable.
(setq package-enable-at-startup nil)

(setopt
 warning-minimum-level :error
 warning-suppress-types '((lexical-binding))
 warning-suppress-log-types '((files missing-lexbind-cookie)))

;; Move native compilation files to directory used by `no-littering'
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

(when (and (featurep 'native-compile)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setopt
   native-comp-always-compile t
   ;; Silence compiler warnings as they can be pretty disruptive
   native-comp-async-report-warnings-errors nil
   ;; Enable ahead-of-time compilation when installing a package
   package-native-compile t
   ;; Compile loaded packages asynchronously
   native-comp-jit-compilation t
   native-comp-async-query-on-exit t
   native-comp-warning-on-missing-source nil))

(setenv "LSP_USE_PLISTS" "true")

;; The value of font height is in 1/10pt, so 100 implies 10pt. Font preferences
;; will be ignored when we use TUI Emacs, and the terminal font setting will be
;; used.

;; (add-to-list 'default-frame-alist '(font . "JetBrainsMonoNF-17"))

;; Alternate options: "Hack Nerd Font", "MesloLGS Nerd Font"
(defun sb/init-fonts-graphic ()
  (cond
   ((string= (system-name) "inspiron-7572")
    (progn
      (set-face-attribute 'default nil
                          :font "JetBrainsMonoNerdFont"
                          :height 220)
      (set-face-attribute 'mode-line nil :height 160)
      (set-face-attribute 'mode-line-inactive nil :height 160)))

   ((string= (system-name) "dell-7506")
    (progn
      (set-face-attribute 'default nil
                          :font "JetBrainsMonoNerdFont"
                          :height 150)
      (set-face-attribute 'mode-line nil :height 120)
      (set-face-attribute 'mode-line-inactive nil :height 120)))

   ((string= (system-name) "office")
    (progn
      (set-face-attribute 'default nil
                          :font "JetBrainsMonoNerdFont"
                          :height 210)
      (set-face-attribute 'mode-line nil :height 140)
      (set-face-attribute 'mode-line-inactive nil :height 140)))))

(add-hook 'emacs-startup-hook #'sb/init-fonts-graphic)

;; (set-frame-parameter nil 'alpha-background 50)

(provide 'early-init)

;;; early-init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; elisp-autofmt-load-packages-local: ("use-package-core")
;; End:
