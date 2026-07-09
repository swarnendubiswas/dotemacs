;;; early-init.el --- Emacs Customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8; fill-column: 80; -*-

;; Swarnendu Biswas

;;; Commentary:

;; This file is supported from Emacs 27+, and is run before package and UI
;; initialization.

;;; Code:

(defconst sb/emacs-4MB (* 4 1024 1024))
(defconst sb/emacs-64MB (* 64 1024 1024))

;; Defer GC during startup by introducing the hacks early to maximize its
;; influence.
(setopt
 gc-cons-percentage 0.6 ; Wait until the heap has grown by 60%
 ;; Temporarily increase GC threshold during startup to reduce the chances of GC getting triggered
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

;; We are using the `gcmh' package which should be enough.
;; (add-hook 'minibuffer-setup-hook #'sb/defer-gc)
;; (add-hook 'minibuffer-exit-hook #'sb/restore-gc)

;; Defer file-name matching during startup
(defvar sb/emacs-file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore it once startup is complete
(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq file-name-handler-alist
         (append
          sb/emacs-file-name-handler-alist-old file-name-handler-alist))))

;; The run-time load order till Emacs 30 is: (1) early-init.el, (2) file
;; described by `site-run-file' if non-nil, (3) `user-init-file', and (4)
;; `default.el'. The run-time load order on Emacs 31+ is: (1) file described by
;; `site-run-file' if non-nil, (2) early-init.el, (3) `user-init-file', and (4)
;; `default.el'.

;; Disable site-wide run-time initialization. We cannot customize
;; `site-run-file', hence `setopt' does not work.
(setq site-run-file nil)

(setopt
 inhibit-default-init t ; Disable loading of `default.el' at startup
 ;; Avoid loading packages twice, this is set during `(package-initialize)'.
 ;; This is also useful if we prefer "straight.el" or "Elpaca" over
 ;; "package.el".
 package-enable-at-startup nil
 package-quickstart t
 package-archives
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
   ("melpa" . "https://melpa.org/packages/"))
 package-install-upgrade-built-in t
 package-native-compile t)

(setopt
 load-prefer-newer t
 ;; Do not resize the frame to preserve the number of columns or lines being
 ;; displayed when setting font, menu bar, tool bar, tab bar, internal borders,
 ;; fringes, or scroll bars.
 frame-inhibit-implied-resize t
 ;; Prevents tiling window managers from forcing weird fractional sizing. Otherwise, Emacsclient frames may not be maximized.
 frame-resize-pixelwise t
 ;; window-resize-pixelwise t
 inhibit-compacting-font-caches t
 inhibit-startup-screen t ; `inhibit-splash-screen' is an alias
 inhibit-startup-echo-area-message user-login-name
 initial-scratch-message nil
 ;; Avoid overhead of loading more expensive major modes. Plus, I use *scratch*
 ;; as a general-purpose buffer.
 initial-major-mode 'fundamental-mode
 ;; Also disables `use-file-dialog'
 use-dialog-box nil
 ;; warning-minimum-level :error
 warning-suppress-types '((lexical-binding))
 warning-suppress-log-types '((files missing-lexbind-cookie)))

;; We have disabled the startup screen with `inhibit-startup-screen', but it
;; would still initialize anyway. This was a temporary workaround to suppress
;; the vanilla startup screen completely. Hopefully, that problem is resolved.

;; (advice-add #'display-startup-screen :override #'ignore)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setopt
   native-comp-always-compile nil
   ;; Silence compiler warnings as they can be pretty disruptive
   native-comp-async-report-warnings-errors nil
   ;; Compile loaded packages asynchronously
   native-comp-jit-compilation t
   native-comp-async-query-on-exit t
   native-comp-warning-on-missing-source nil
   native-comp-async-on-battery-power nil)

  ;; Move native compilation files to directory used by `no-littering'
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

;; Disable UI elements early before being initialized. Use `display-graphic-p'
;; since `window-system' is deprecated.

;; The following style of manipulating the parameters of `default-frame-alist' is faster than disabling the modes explicitly, i.e., running "(tool-bar-mode -1)".
(push '(tool-bar-lines . 0) default-frame-alist)

;; The menu bar can be useful to identify different capabilities available and
;; their shortcuts but we still turn it off.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

;; Sets the active and inactive frame transparency to 97% for the currently
;; selected frame. Transparency works with GUI frames.
(set-frame-parameter (selected-frame) 'alpha '(97 . 97))
;; Ensures new frames are created with 97% opacity.
(add-to-list 'default-frame-alist '(alpha . (97 . 97)))

;; Maximize Emacs on startup.

;; Applies only to the initial (startup) Emacs frame
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Applies to every Emacs frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ;; Remove title bar on all future frames
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; The value of font height is in 1/10pt, so 100 implies 10pt. Font preferences
;; will be ignored when we use TUI Emacs, and the terminal font setting will be
;; used.

;; I prefer JetBrainsMono and Iosevka. Iosevka is slightly narrower, and so can fit more characters on a line.

;; `after-init-hook' runs after the initial graphical frame has already been
;; created with the system default font. With `set-face-attribute', Emacs is
;; forced to re-calculate all text dimensions and resize the frame. This causes
;; a visible flicker and adds a measurable delay to the startup time. ;; The
;; following page suggests avoiding set-face-attribute for performance reasons.
;; https://github.com/D4lj337/Emacs-performance

;; (defun sb/frame-font ()
;;   (pcase (system-name)
;;     ("inspiron-7572" "IosevkaTerm Nerd Font Mono:size=21")
;;     ("office"         "IosevkaTerm Nerd Font Mono:size=20")
;;     (_                "IosevkaTerm Nerd Font Mono:size=20")))

;; (defun sb/apply-font (&optional frame)
;;   (with-selected-frame (or frame (selected-frame))
;;     (set-frame-font (sb/frame-font) t t)))

;; ;; Apply immediately for non-daemon Emacs
;; (sb/apply-font)

;; ;; Apply for every new frame (daemon or emacsclient)
;; (add-hook 'after-make-frame-functions #'sb/apply-font)

;; (when (daemonp)
;;   (cond
;;    ((string= (system-name) "inspiron-7572")
;;     (add-to-list 'default-frame-alist '(font . "JetBrainsMonoNerdFontMono-21"))
;;     (defun sb/init-fonts-daemon (frame)
;;       (with-selected-frame frame
;;         (set-frame-font "JetBrainsMonoNerdFontMono-21" t t)))
;;     (add-hook 'after-make-frame-functions #'sb/init-fonts-daemon))
;;    ((string= (system-name) "office")
;;     (add-to-list 'default-frame-alist '(font . "JetBrainsMonoNerdFontMono-20"))
;;     (defun sb/init-fonts-daemon (frame)
;;       (with-selected-frame frame
;;         (set-frame-font "JetBrainsMonoNerdFontMono-20" t t)))
;;     (add-hook 'after-make-frame-functions #'sb/init-fonts-daemon))))

;; (unless (daemonp)
;;   (defun sb/init-fonts-graphic ()
;;     (cond
;;      ((string= (system-name) "inspiron-7572")
;;       (progn
;;         (set-face-attribute 'default nil
;;                             :font "JetBrainsMonoNerdFontMono"
;;                             :height 200)
;;         (set-face-attribute 'mode-line nil :height 150)
;;         (set-face-attribute 'mode-line-active nil :height 150)
;;         (set-face-attribute 'mode-line-inactive nil :height 150)))

;;      ((string= (system-name) "dell-7506")
;;       (progn
;;         (set-face-attribute 'default nil
;;                             :font "JetBrainsMonoNerdFontMono"
;;                             :height 150)
;;         (set-face-attribute 'mode-line nil :height 120)
;;         (set-face-attribute 'mode-line-inactive nil :height 120)))

;;      ((string= (system-name) "office")
;;       (progn
;;         (set-face-attribute 'default nil
;;                             :font "JetBrainsMonoNerdFontMono"
;;                             :height 210)
;;         (set-face-attribute 'mode-line nil :height 160)
;;         (set-face-attribute 'mode-line-active nil :height 160)
;;         (set-face-attribute 'mode-line-inactive nil :height 160)))))

;;   (add-hook 'after-init-hook #'sb/init-fonts-graphic))

;; Host-specific font configuration
(defconst sb/font-config
  '(("inspiron-7572"
     :font "Iosevka Nerd Font Mono"
     :gui-height 20
     :daemon-height 20
     :mode-line-height 160)
    ("cseiitk"
     :font "Iosevka Nerd Font Mono"
     :gui-height 19
     :daemon-height 20
     :mode-line-height 160)))

(defun sb/font-config-for-host ()
  (assoc (system-name) sb/font-config))

(defun sb/apply-font-daemon (frame)
  (when-let* ((cfg (sb/font-config-for-host))
              (font (plist-get (cdr cfg) :font))
              (size (plist-get (cdr cfg) :daemon-height)))
    (with-selected-frame frame
      (set-frame-font (format "%s-%d" font size) t t))))

(when (daemonp)
  (when-let* ((cfg (sb/font-config-for-host))
              (font (plist-get (cdr cfg) :font))
              (size (plist-get (cdr cfg) :daemon-height)))
    (add-to-list 'default-frame-alist `(font . ,(format "%s-%d" font size)))
    (add-hook 'after-make-frame-functions #'sb/apply-font-daemon)))

(defun sb/apply-font-gui ()
  (when-let* ((cfg (sb/font-config-for-host))
              (font (plist-get (cdr cfg) :font))
              (size (plist-get (cdr cfg) :gui-height))
              (font-string (format "%s-%d" font size)))
    (add-to-list 'default-frame-alist `(font . ,font-string))
    (add-to-list 'initial-frame-alist `(font . ,font-string))
    ;; (set-face-attribute 'default nil
    ;;                     :font font
    ;;                     :height fh)
    ))

(sb/apply-font-gui)

(defun sb/apply-mode-line-height ()
  (when-let* ((cfg (sb/font-config-for-host))
              (mlh (plist-get (cdr cfg) :mode-line-height)))
    (dolist (face '(mode-line mode-line-active mode-line-inactive))
      (set-face-attribute face nil :height mlh))))

(add-hook 'after-init-hook #'sb/apply-mode-line-height)

;; Recommended by `lsp-mode' for better performance
(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)

;;; early-init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; elisp-autofmt-load-packages-local: ("use-package-core")
;; End:
