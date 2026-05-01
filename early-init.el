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

(add-hook 'minibuffer-setup-hook #'sb/defer-gc)

;; `lsp-mode' suggests increasing the limit permanently to a reasonable value.
;; There will be large pause times with large `gc-cons-threshold' values
;; whenever GC eventually happens.
(defun sb/restore-gc ()
  "Restore garbage collection threshold during execution."
  (setopt
   gc-cons-threshold sb/emacs-4MB
   gc-cons-percentage 0.1))

(add-hook 'elpaca-after-init-hook #'sb/restore-gc)
(add-hook 'minibuffer-exit-hook #'sb/restore-gc)

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; The run-time load order till Emacs 30 is: (1) early-init.el, (2) file
;; described by `site-run-file' if non-nil, (3) `user-init-file', and (4)
;; `default.el'. The run-time load order on Emacs 31+ is: (1) file described by
;; `site-run-file' if non-nil, (2) early-init.el, (3) `user-init-file', and (4)
;; `default.el'.

;; Disable site-wide run-time initialization. We cannot customize
;; `size-run-file', hence `setopt' does not work.
(setq site-run-file nil)

(setopt
 ;; Disable loading of `default.el' at startup
 inhibit-default-init t
 ;; Avoid loading packages twice, this is set during `(package-initialize)'. This
 ;; is also useful if we prefer "straight.el" or "Elpaca" over "package.el".
 package-enable-at-startup nil)

(setopt
 load-prefer-newer t
 ;; Do not resize the frame to preserve the number of columns or lines being
 ;; displayed when setting font, menu bar, tool bar, tab bar, internal borders,
 ;; fringes, or scroll bars.
 frame-inhibit-implied-resize t
 ;; Prevents tiling window managers from forcing weird fractional sizing. Otherwise, Emacsclient frames may not be maximized.
 frame-resize-pixelwise t
 ;; window-resize-pixelwise t
 inhibit-startup-screen t ; `inhibit-splash-screen' is an alias
 inhibit-startup-echo-area-message user-login-name
 initial-scratch-message nil
 ;; Avoid overhead of loading more expensive major modes. Plus, I use *scratch*
 ;; as a general-purpose buffer.
 initial-major-mode 'fundamental-mode
 ;; Also disables `use-file-dialog'
 use-dialog-box nil)

;; Suppress the vanilla startup screen completely. We have disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

;; Disable UI elements early before being initialized. Use `display-graphic-p'
;; since `window-system' is deprecated.

(add-to-list 'default-frame-alist '(undecorated . t))

;; The following is faster than running "(tool-bar-mode -1)"
(push '(tool-bar-lines . 0) default-frame-alist)

;; The menu bar can be useful to identify different capabilities available and
;; their shortcuts but we still turn it off.
(push '(menu-bar-lines . 0) default-frame-alist)

;; The following is faster than running "(scroll-bar-mode -1)"
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
   ;; Compile loaded packages asynchronously
   native-comp-jit-compilation t
   native-comp-async-query-on-exit t
   native-comp-warning-on-missing-source nil))

;; Recommended by `lsp-mode' for better performance
(setenv "LSP_USE_PLISTS" "true")

;; The value of font height is in 1/10pt, so 100 implies 10pt. Font preferences
;; will be ignored when we use TUI Emacs, and the terminal font setting will be
;; used.

;; I prefer JetBrainsMono and Iosevka. Iosevka is slightly narrower, and so can fit more characters on a line.

;; TODO: Review and finalize the best way to customize font preferences.

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

;; ;; The following page suggests avoiding set-face-attribute for performance
;; ;; reasons. https://github.com/D4lj337/Emacs-performance
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

;;   (add-hook 'elpaca-after-init-hook #'sb/init-fonts-graphic))

;; Host-specific font configuration
(defconst sb/font-config
  '(("inspiron-7572" :font "Iosevka Nerd Font Mono"
                     :daemon-size 21
                     :gui-height 200
                     :mode-line 160)
    ("dell-7506"     :font "JetBrainsMonoNerdFontMono"
                     :gui-height 150
                     :mode-line 120)
    ("cseiitk"        :font "Iosevka Nerd Font Mono"
                     :daemon-size 21
                     :gui-height 220
                     :mode-line 160)))

(defun sb/font-config-for-host ()
  (assoc (system-name) sb/font-config))

;; ---------- Daemon (frames created later) ----------

(defun sb/apply-font-daemon (frame)
  (when-let* ((cfg (sb/font-config-for-host))
              (font (plist-get (cdr cfg) :font))
              (size (plist-get (cdr cfg) :daemon-size)))
    (with-selected-frame frame
      (set-frame-font (format "%s-%d" font size) t t))))

(when (daemonp)
  (when-let* ((cfg (sb/font-config-for-host))
              (font (plist-get (cdr cfg) :font))
              (size (plist-get (cdr cfg) :daemon-size)))
    (add-to-list 'default-frame-alist
                 `(font . ,(format "%s-%d" font size)))
    (add-hook 'after-make-frame-functions #'sb/apply-font-daemon)))

;; ---------- Non-daemon (GUI startup) ----------

(defun sb/apply-font-gui ()
  (when-let* ((cfg (sb/font-config-for-host))
              (font (plist-get (cdr cfg) :font))
              (height (plist-get (cdr cfg) :gui-height))
              (mode-line (plist-get (cdr cfg) :mode-line)))
    (set-face-attribute 'default nil
                        :font font
                        :height height)
    (dolist (face '(mode-line mode-line-active mode-line-inactive))
      (set-face-attribute face nil :height mode-line))))

(unless (daemonp)
  (add-hook 'elpaca-after-init-hook #'sb/apply-font-gui))

(provide 'early-init)

;;; early-init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; elisp-autofmt-load-packages-local: ("use-package-core")
;; End:
