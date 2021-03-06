;;; init-autoload.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; Load built-in libraries
(require 'cl-lib)
(require 'subr-x)

(declare-function ht-merge "ht")


(defgroup sb/emacs
  nil
  "Personal configuration for dotemacs."
  :group 'local)


(defcustom sb/extras-directory
  (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type 'string
  :group 'sb/emacs)


(defcustom sb/theme
  'none
  "Specify which Emacs theme to use."
  :type '(radio
          (const :tag "leuven" leuven)
          (const :tag "doom-molokai" doom-molokai)
          (const :tag "doom-one-light" doom-one-light)
          (const :tag "monokai" monokai)
          (const :tag "modus-operandi" modus-operandi)
          (const :tag "modus-vivendi" modus-vivendi)
          (const :tag "customized" sb/default) ; Customizations over the default theme
          (const :tag "none" none))
  :group 'sb/emacs)


(defcustom sb/modeline-theme
  'doom-modeline
  "Specify the mode-line theme to use."
  :type '(radio
          (const :tag "powerline" powerline)
          (const :tag "smart-mode-line" sml)
          (const :tag "spaceline" spaceline)
          (const :tag "doom-modeline" doom-modeline)
          (const :tag "awesome-tray" awesome-tray)
          (const :tag "moody" moody)
          (const :tag "default" default))
  :group 'sb/emacs)


(defcustom sb/window-split
  'horizontal
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type '(radio
          ;; Split into two windows one above the other (`split-window-below')
          (const :tag "vertical" vertical)
          ;; Split into two side-by-side windows (`split-window-right')
          (const :tag "horizontal" horizontal))
  :group 'sb/emacs)


;; Large values make reading difficult when the window is split side-by-side
(defcustom sb/fill-column
  100
  "Column beyond which lines should not extend."
  :type 'number
  :group 'sb/emacs)


(defcustom sb/selection
  'ivy
  "Choose the framework to use for narrowing and selection."
  :type '(radio
          (const :tag "ivy" ivy)
          (const :tag "selectrum" selectrum)
          (const :tag "none" none))
  :group 'dotemacs)


(defcustom sb/delete-trailing-whitespace-p
  nil
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.
Sometimes we do not want to unnecessarily add differences due to
whitespaces."
  :type 'boolean
  :group 'sb/emacs)


;; We use `lsp-mode' and `dumb-jump' for jumping to tags and browsing source code
(defcustom sb/tags-scheme
  'none
  "Choose whether to use gtags or ctags."
  :type '(radio
          (const :tag "ctags" ctags)
          (const :tag "gtags" gtags)
          (const :tag "none" none))
  :group 'sb/emacs)


(defcustom sb/ctags-path
  "/usr/local/bin/ctags"
  "Absolute path to Universal Ctags executable."
  :type 'string
  :group 'sb/emacs)


(defcustom sb/gtags-path
  "/usr/local/bin/gtags"
  "Absolute path to GNU Global executable."
  :type 'string
  :group 'sb/emacs)


;; Keep enabled until the configuration is stable
(defcustom sb/debug-init-file
  nil
  "Enable features to debug errors and performance bottlenecks."
  :type 'boolean
  :group 'sb/emacs)


(when (bound-and-true-p sb/debug-init-file)
  (setq garbage-collection-messages nil
        debug-on-error t
        debug-on-event 'sigusr2)
  (debug-on-entry 'projectile-remove-known-project))


(defconst sb/user-home
  (getenv "HOME")
  "User HOME directory.")


;; We use this location for shared installations like `textlint' and virtualenvs
(defconst sb/user-tmp
  (expand-file-name "tmp" sb/user-home)
  "User temp directory.
This location is used for temporary installations and files.")


(defcustom sb/textlint-home
  (expand-file-name "textlint-workspace" sb/user-tmp)
  "Absolute path to textlint workspace."
  :type 'string
  :group 'sb/emacs)


;; `pyls' and `mspyls' are not actively maintained, and improvements to `py-lsp' is slow
(defcustom sb/python-langserver
  'pyright
  "Choose the Python Language Server implementation."
  :type '(radio
          (const :tag "pyright" pyright)
          (const :tag "jedi" jedi)
          (const :tag "none" none))
  :group 'sb/emacs)


(add-to-list 'load-path sb/extras-directory)


;; Another option is to construct the `load-path' manually
;; (add-to-list 'load-path (concat package-user-dir "magit-20170715.1731"))
(package-initialize)


;; Install required packages with `M-x sb/install-packages', you will need to evalaute the
;; `package-archives' before that.
(load "core-packages")


(defcustom sb/use-no-littering
  t
  "Use the `no-littering' package to keep `.emacs.d' clean."
  :type 'boolean
  :group 'sb/emacs)


(when (bound-and-true-p sb/use-no-littering)
  (require 'no-littering))


(defcustom sb/custom-file
  (no-littering-expand-etc-file-name "custom.el")
  "File to write Emacs customizations."
  :type 'string
  :group 'sb/emacs)


(defcustom sb/private-file
  (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type 'string
  :group 'sb/emacs)


(setq custom-file sb/custom-file)
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(when (file-exists-p sb/private-file)
  (load sb/private-file 'noerror))


(defcustom sb/temp-directory
  (expand-file-name "tmp" user-emacs-directory)
  "Storage location for various configuration files."
  :type 'string
  :group 'sb/emacs)

;; We do not need this with `no-littering'
(unless (or (bound-and-true-p sb/use-no-littering)
            (file-exists-p sb/temp-directory))
  (make-directory sb/temp-directory))


(defconst sb/EMACS27+   (> emacs-major-version 26))
(defconst sb/EMACS28+   (> emacs-major-version 27))
(defconst sb/IS-LINUX   (eq system-type 'gnu/linux))
(defconst sb/IS-WINDOWS (eq system-type 'windows-nt))


(defconst sb/emacs-1MB   (*   1 1000 1000))
(defconst sb/emacs-4MB   (*   4 1000 1000))
(defconst sb/emacs-8MB   (*   8 1000 1000))
(defconst sb/emacs-64MB  (*  64 1000 1000))
(defconst sb/emacs-128MB (* 128 1000 1000))
(defconst sb/emacs-256MB (* 256 1000 1000))
(defconst sb/emacs-512MB (* 512 1000 1000))

;; GC may happen after this many bytes are allocated since last GC If you experience freezing,
;; decrease this. If you experience stuttering, increase this.
(defun sb/defer-garbage-collection ()
  "Defer garbage collection."
  (setq gc-cons-percentage 0.1
        gc-cons-threshold sb/emacs-512MB))

;; Ideally, we would have reset `gc-cons-threshold' to its default value otherwise there can be
;; large pause times whenever GC eventually happens. But lsp suggests increasing the limit
;; permanently.
(defun sb/restore-garbage-collection ()
  "Restore garbage collection."
  (setq gc-cons-percentage 0.1
        ;; https://github.com/emacs-lsp/lsp-mode#performance
        gc-cons-threshold sb/emacs-64MB))

;; `emacs-startup-hook' runs later than the `after-init-hook', it is the last hook to load
;; customizations
(add-hook 'emacs-startup-hook #'sb/restore-garbage-collection)
(add-hook 'minibuffer-setup-hook #'sb/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'sb/restore-garbage-collection)


;; Allow GC to happen after a period of idle time
(progn
  (declare-function gcmh-idle-garbage-collect "gcmh")

  (unless (fboundp 'gcmh-mode)
    (autoload #'gcmh-mode "gcmh" nil t))
  (unless (fboundp 'gcmh-idle-garbage-collect)
    (autoload #'gcmh-idle-garbage-collect "gcmh" nil t))

  (add-hook 'after-init-hook #'gcmh-mode)

  (with-eval-after-load "gcmh"
    (when (bound-and-true-p sb/debug-init-file)
      (defvar gcmh-verbose)

      (setq gcmh-verbose t))

    (diminish 'gcmh-mode)))


(progn
  (unless (fboundp 'paradox-list-packages)
    (autoload #'paradox-list-packages "paradox" nil t))
  (unless (fboundp 'paradox-upgrade-packages)
    (autoload #'paradox-upgrade-packages "paradox" nil t))
  (unless (fboundp 'paradox-enable)
    (autoload #'paradox-enable "paradox" nil t))

  (with-eval-after-load "paradox"
    (defvar paradox-display-star-count)
    (defvar paradox-execute-asynchronously)
    (defvar paradox-github-token)

    (setq paradox-display-star-count nil
          paradox-execute-asynchronously t
          paradox-github-token t)

    (paradox-enable))

  (bind-keys :package paradox
             ("C-c d l" . paradox-list-packages)
             ("C-c d u" . paradox-upgrade-packages)))


(when (or (daemonp) (memq window-system '(x ns)))
  (unless (fboundp 'exec-path-from-shell-initialize)
    (autoload #'exec-path-from-shell-initialize "exec-path-from-shell" nil t))

  (defvar exec-path-from-shell-arguments)
  (defvar exec-path-from-shell-check-startup-files)
  (defvar exec-path-from-shell-variables)

  ;; "-i" is expensive but Tramp is unable to find executables without the option
  (setq exec-path-from-shell-arguments '("-l" "-i")
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "NODE_PATH" "JAVA_HOME" "PYTHONPATH"
                                         "LANG" "LC_CTYPE"))

  (exec-path-from-shell-initialize))

;; LATER: Doing the following to avoid "-i" to `exec-path-from-shell' does not help.
;; (setq exec-path (append exec-path (expand-file-name "node_modules/.bin" sb/user-tmp)))
;; (add-to-list 'exec-path (expand-file-name "node_modules/.bin" sb/user-tmp))


;; Silence "assignment to free variable" warning
(defvar apropos-do-all)
(defvar compilation-always-kill)
(defvar compilation-scroll-output)

(setq ad-redefinition-action 'accept ; Turn off warnings due to redefinitions
      apropos-do-all t ; Make `apropos' search more extensively
      auto-mode-case-fold nil ; Avoid a second pass through `auto-mode-alist'
      ;; Unlike `auto-save-mode', `auto-save-visited-mode' saves the buffer contents to the visiting
      ;; file and runs all save-related hooks
      auto-save-default nil ; Disable `auto-save-mode', prefer `auto-save-visited-mode' instead
      auto-save-no-message t ; Allow for debugging frequent autosave triggers
      auto-save-interval 0 ; Disable autosaving based on number of characters typed
      auto-save-visited-interval 30 ; Default of 5s is too frequent
      backup-inhibited t ; Disable backup for a per-file basis
      blink-matching-paren t ; Distracting
      case-fold-search t ; Searches and matches should ignore case
      comment-auto-fill-only-comments t
      compilation-always-kill t ; Kill a compilation process before starting a new one
      compilation-ask-about-save nil ; Save all modified buffers without asking
      ;; Automatically scroll the *Compilation*  buffer as output appears, but stop at the first error
      compilation-scroll-output 'first-error
      completion-ignore-case t ; Ignore case when completing
      confirm-kill-emacs nil
      confirm-kill-processes nil ; Prevent "Active processes exist" when you quit Emacs
      confirm-nonexistent-file-or-buffer t
      create-lockfiles nil
      cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
      custom-safe-themes t
      delete-by-moving-to-trash t ; Use system trash to deal with mistakes
      echo-keystrokes 0.2 ; Show current key-sequence in minibuffer
      ;; enable-local-variables :all ; Avoid "defvar" warnings
      enable-recursive-minibuffers t
      enable-remote-dir-locals t
      ;; Expand truncated ellipsis:suspension points in the echo area, useful to see more
      ;; information
      eval-expression-print-length 500
      ;; Disable the warning "X and Y are the same file" in case of symlinks
      find-file-suppress-same-file-warnings t
      find-file-visit-truename t ; Show true name, useful in case of symlinks
      ;; Avoid resizing the frame when the font is larger (or smaller) than the system default
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      frame-title-format (list '(buffer-file-name "%f" "%b") " - " invocation-name "@"
                               (system-name))
      help-window-select t ; Makes it easy to close the window
      history-delete-duplicates t
      ;; Doom Emacs: Emacs updates its UI more often than it needs to, so we slow it down slightly
      ;; from 0.5s
      ;; idle-update-delay 1.0
      indicate-buffer-boundaries nil
      inhibit-compacting-font-caches t ; Do not compact font caches during GC
      ;; The run-time load order is: (1) file described by `site-run-file', if non-nil; (2)
      ;; `user-init-file'; (3) default.el. Disable loading of `default.el' at startup, inhibits site
      ;; default settings.
      inhibit-default-init t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t ; `inhibit-splash-screen' is an alias
      ;; *scratch* is in `lisp-interaction-mode' by default. `text-mode' is more expensive to start,
      ;; but I use *scratch* for composing emails.
      initial-major-mode 'text-mode
      initial-scratch-message nil
      kill-do-not-save-duplicates t
      kill-whole-line t
      make-backup-files nil ; Stop making backup `~' files
      mouse-drag-copy-region nil ; Mouse is disabled
      mouse-yank-at-point t ; Yank at point with mouse instead of at click
      pop-up-frames nil ; Avoid making separate frames
      ;; pop-up-windows nil ; Disallow creating new windows
      read-buffer-completion-ignore-case t ; Ignore case when reading a buffer name
      ;; Ignore case when reading a file name completion
      read-file-name-completion-ignore-case t
      read-process-output-max (* 1024 1024) ; 1 MB
      require-final-newline t ; Always end a file with a newline
      ring-bell-function 'ignore ; Disable beeping sound
      save-interprogram-paste-before-kill t
      ;; Enable use of system clipboard across Emacs and other applications
      select-enable-clipboard t
      sentence-end-double-space nil
      ;; set-mark-command-repeat-pop t
      shift-select-mode nil ; Do not use `shift-select' for marking, use it for `windmove'
      standard-indent 2
      suggest-key-bindings t
      switch-to-buffer-preserve-window-point t
      ;; truncate-partial-width-windows nil
      use-dialog-box nil
      use-file-dialog nil
      vc-follow-symlinks t ; No need to ask
      vc-handled-backends '(Git) ; Disabling vc improves performance, alternate option '(Git)
      view-read-only t ; View mode for read-only buffers
      visible-bell nil
      window-resize-pixelwise t
      x-gtk-use-system-tooltips nil ; Do not use system tooltips
      ;; Always trigger an immediate resize of the child frame
      x-gtk-resize-child-frames 'resize-mode
      ;; Underline looks a bit better when drawn lower
      x-underline-at-descent-line t)

(unless (bound-and-true-p sb/use-no-littering)
  (setq auto-save-list-file-prefix (expand-file-name "auto-save" sb/temp-directory)))

;; Changing buffer-local variables will only affect a single buffer. `setq-default' changes the
;; buffer-local variable's default value.
(setq-default electric-indent-inhibit nil
              fill-column sb/fill-column
              indent-tabs-mode nil ; Spaces instead of tabs
              indicate-empty-lines nil
              major-mode 'text-mode ; Major mode to use for files that do no specify a major mode
              ;; TAB first tries to indent the current line, and if the line was already indented,
              ;; then try to complete the thing at point.
              tab-always-indent 'complete
              tab-width 4
              truncate-lines nil)

;; https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
(setq-default bidi-inhibit-bpa t ; Disabling BPA makes redisplay faster
              bidi-paragraph-direction 'left-to-right)

(dolist (exts '(".aux"
                ".class"
                ".dll"
                ".elc"
                ".exe"
                ".fls"
                ".lof"
                ".o"
                ".pyc"
                ".rel"
                ".rip"
                ".so"
                ".toc"))
  (add-to-list 'completion-ignored-extensions exts))

;; Activate utf-8
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Scroll settings from Doom Emacs
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the cursor more than N
      ;; lines past window edges (where N is the setting of `scroll-conservatively'). This is
      ;; especially slow in larger files during large-scale scrolling commands. If kept over 100,
      ;; the window is never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      ;; Do not accelerate scrolling
      mouse-wheel-progressive-speed nil)


(fset 'display-startup-echo-area-message #'ignore)
(fset 'yes-or-no-p 'y-or-n-p) ; Type "y"/"n" instead of "yes"/"no"


(progn
  (unless (fboundp 'global-auto-revert-mode)
    (autoload #'global-auto-revert-mode "autorevert" nil t))

  (run-with-idle-timer 2 nil #'global-auto-revert-mode)

  (with-eval-after-load "autorevert"
    (defvar auto-revert-interval)
    (defvar auto-revert-remote-files)
    (defvar auto-revert-use-notify)
    (defvar auto-revert-verbose)
    (defvar global-auto-revert-non-file-buffers)
    (defvar auto-revert-check-vc-info)

    (setq auto-revert-interval 5 ; Faster (seconds) would mean less likely to use stale data
          auto-revert-remote-files t ; Emacs seems to hang with auto-revert and tramp
          auto-revert-use-notify nil
          auto-revert-verbose nil
          auto-revert-check-vc-info nil ; Should improve performance
          ;; Revert only file-visiting buffers, set to non-nil value to revert dired buffers if the
          ;; contents of the "main" directory changes
          global-auto-revert-non-file-buffers nil)

    (diminish 'auto-revert-mode))

  ;; Revert all (e.g., PDF) files without asking
  (setq revert-without-query '("\\.*")))


;; Remember cursor position in files
(progn
  (unless (fboundp 'save-place-mode)
    (autoload #'save-place-mode "saveplace" nil t))

  ;; We may open a file immediately after starting Emacs, hence we are using a hook instead of a
  ;; timer.
  (add-hook 'after-init-hook #'save-place-mode)

  (with-eval-after-load "saveplace"
    (defvar save-place-file)

    (unless (bound-and-true-p sb/use-no-littering)
      (setq save-place-file (expand-file-name "places" sb/temp-directory)))))


;; Save minibuffer history across sessions
(progn
  (unless (fboundp 'savehist-mode)
    (autoload #'savehist-mode "savehist" nil t))

  (run-with-idle-timer 3 nil #'savehist-mode)

  (with-eval-after-load "savehist"
    (defvar savehist-additional-variables)
    (defvar savehist-file)
    (defvar savehist-save-minibuffer-history)

    (setq savehist-additional-variables '(extended-command-history kill-ring search-ring
                                                                   regexp-search-ring)
          savehist-save-minibuffer-history t)

    (unless (bound-and-true-p sb/use-no-littering)
      (setq savehist-file (expand-file-name "savehist" sb/temp-directory)))))


(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator "/"
      uniquify-strip-common-suffix t)


;; Replace `dabbrev-exp' with `hippie-expand'. Use `C-M-/' for `dabbrev-completion' which finds all
;; expansions in the current buffer and presents suggestions for completion.
(progn
  (defvar hippie-expand-verbose)
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)
        hippie-expand-verbose nil)

  (bind-key "M-/" #'hippie-expand))


(progn
  (unless (fboundp 'subword-mode)
    (autoload #'subword-mode "subword" nil t))

  (add-hook 'prog-mode-hook #'subword-mode)

  (with-eval-after-load "subword"
    (diminish 'subword-mode)))


;; Show dividers on the right of each window, more prominent than the default
(window-divider-mode)


;; horizontal - Split the selected window into two windows (e.g., `split-window-below'), one above
;; the other
(when (eq sb/window-split 'horizontal)
  (setq split-width-threshold nil
        split-height-threshold 0))

;; vertical - Split the selected window into two side-by-side windows (e.g., `split-window-right')
(when (eq sb/window-split 'vertical)
  (setq split-height-threshold nil
        split-width-threshold 0))

;; Make use of wider screens
(when nil
  (when (string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
    (split-window-right)))


;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun sb/auto-save-wrapper (save-fn &rest args)
  "Hide 'Auto-saving...done' messages by calling the method.
SAVE-FN with non-nil ARGS."
  (ignore args)
  (apply save-fn '(t)))

(advice-add 'do-auto-save :around #'sb/auto-save-wrapper)


(progn
  (unless (fboundp 'abbrev-mode)
    (autoload #'abbrev-mode "abbrev" nil t))

  ;; We open the `*scratch*' buffer in `text-mode', so enabling `abbrev-mode' quickly can be useful
  (add-hook 'text-mode-hook #'abbrev-mode)

  (with-eval-after-load "abbrev"
    (setq abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory)
          save-abbrevs 'silently)

    (diminish 'abbrev-mode)))


;; Disable the following modes
(dolist (mode '(blink-cursor-mode ; Blinking cursor is distracting
                ;; desktop-save-mode
                ;; global-prettify-symbols-mode ; Makes it difficult to edit the buffer
                shell-dirtrack-mode
                size-indication-mode
                tooltip-mode))
  (when (fboundp mode)
    (funcall mode -1)))


;; Enable the following modes
(dolist (mode '(;; auto-compression-mode
                auto-save-visited-mode ; Autosave file-visiting buffers at idle time intervals
                column-number-mode
                delete-selection-mode ; Typing with the mark active will overwrite the marked region
                ;; global-hl-line-mode
                ;; global-visual-line-mode ; Wrap lines
                minibuffer-depth-indicate-mode
                ;; outline-minor-mode
                ;; Enable visual feedback on selections, mark follows the point
                ;; transient-mark-mode
                ))
  (when (fboundp mode)
    (funcall mode 1)))

;; Not a library/file, so `eval-after-load' does not work
(diminish 'auto-fill-function)

(with-eval-after-load "simple"
  (diminish 'visual-line-mode))

(with-eval-after-load "outline"
  (diminish 'outline-minor-mode))


;; (fringe-mode '(10 . 10)) ; Default is 8 pixels


;; Native from Emacs 27+
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(add-hook 'prog-mode-hook #'goto-address-prog-mode)


(progn
  ;; This puts the buffer in read-only mode and disables font locking
  (unless (fboundp 'global-so-long-mode)
    (autoload #'global-so-long-mode "so-long" nil t))

  (run-with-idle-timer 3 nil #'global-so-long-mode)

  ;; (with-eval-after-load "so-long"
  ;;   (defvar so-long-threshold)

  ;;   (setq so-long-threshold 500))
  )


(when (display-graphic-p)
  (unless (fboundp 'all-the-icons-install-fonts)
    (autoload #'all-the-icons-install-fonts "all-the-icons" nil t))

  ;; Install fonts with `M-x all-the-icons-install-fonts'
  ;; https://github.com/domtronn/all-the-icons.el/issues/120
  (eval-and-compile
    (defun sb/font-installed-p (font-name)
      "Check if font with FONT-NAME is available."
      (if (find-font (font-spec :name font-name))
          t
        nil)))

  (with-eval-after-load "all-the-icons"
    (unless (sb/font-installed-p "all-the-icons")
      (all-the-icons-install-fonts t))

    (defvar all-the-icons-scale-factor)
    (defvar all-the-icons-color-icons)

    (setq all-the-icons-scale-factor 1.0
          all-the-icons-color-icons nil)))


;; Make the cursor a thin horizontal bar, not a block
;; (set-default 'cursor-type '(bar . 4))


(when t
  (progn
    (require 'solar)

    (declare-function circadian-setup "circadian")

    (unless (fboundp 'circadian-setup)
      (autoload #'circadian-setup "circadian" nil t))

    (defvar calendar-latitude)
    (defvar calendar-longitude)
    (defvar circadian-themes)
    (defvar calendar-location-name)

    ;; Kanpur, UP
    (setq calendar-latitude 26.50
          calendar-longitude 80.23
          calendar-location-name "Kanpur, UP, India"
          circadian-themes '((:sunrise . modus-operandi)
                             (:sunset  . modus-vivendi)))

    (circadian-setup)
    ))


(when nil
  (when (eq sb/theme 'leuven)
    (load-theme 'leuven t)))


(when nil
  (when (eq sb/theme 'doom-molokai)
    (load-theme 'doom-molokai t)
    (set-face-attribute 'font-lock-comment-face nil
                        ;; :foreground "#cccccc"
                        ;; :foreground "#b2b2b2"
                        :foreground "#999999")
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification
    (doom-themes-org-config)))


(when nil
  (when (eq sb/theme 'doom-one-light)
    (load-theme 'doom-one-light t)
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification
    (doom-themes-org-config)))


(when nil
  (when (eq sb/theme 'monokai)
    (load-theme 'monokai t)))


(when (or (eq sb/theme 'modus-operandi)
          (eq sb/theme 'modus-vivendi))
  (defvar modus-themes-completions)
  (defvar modus-themes-fringes)
  (defvar modus-themes-intense-hl-line)
  (defvar modus-themes-mode-line)
  (defvar modus-themes-scale-headings)
  (defvar modus-themes-prompts)
  (defvar modus-themes-variable-pitch-headings)

  ;; (setq modus-themes-completions 'opinionated
  ;;       modus-themes-fringes 'subtle
  ;;       modus-themes-intense-hl-line nil
  ;;       modus-themes-scale-headings nil
  ;;       modus-themes-prompts 'intense-accented
  ;;       modus-themes-variable-pitch-headings nil)

  (when (eq sb/modeline-theme 'default)
    (setq modus-themes-mode-line 'accented-3d))

  (when (eq sb/modeline-theme 'moody)
    (setq modus-themes-mode-line 'borderless-moody)))

(when (eq sb/theme 'modus-operandi)
  (load-theme 'modus-operandi t)

  ;; :custom-face
  ;; (mode-line ((t (:background "#d7d7d7" :foreground "#0a0a0a"
  ;;                             :box (:line-width 1 :color "#505050")
  ;;                             :height 0.8))))
  ;; (mode-line-inactive ((t (:background "#efefef" :foreground "#404148"
  ;;                                      :box (:line-width 1 :color "#bcbcbc")
  ;;                                      :height 0.8))))
  )


(when (eq sb/theme 'modus-vivendi)
  (load-theme 'modus-vivendi t))


(when (and (eq sb/theme 'sb/default) (display-graphic-p))
  (progn
    ;; (setq frame-background-mode 'light)
    ;; (set-background-color "#ffffff")
    (set-foreground-color "#333333")
    (set-face-attribute 'hl-line nil :background "light yellow")
    (set-face-attribute 'region nil :background "gainsboro")))


;; The Python virtualenv information is not shown
(when (eq sb/modeline-theme 'powerline)
  (unless (fboundp 'powerline-default-theme)
    (autoload #'powerline-default-theme "powerline" nil t))

  (defvar powerline-default-separator)
  (defvar powerline-display-buffer-size)
  (defvar powerline-display-hud)
  (defvar powerline-display-mule-info)
  (defvar powerline-gui-use-vcs-glyph)
  (defvar powerline-height)

  (setq powerline-display-hud nil ; Visualization of the position in the buffer is not useful
        powerline-display-mule-info nil ; File encoding information is not useful
        ;; powerline-default-separator 'box
        ;; powerline-display-buffer-size nil
        powerline-height 20)

  (when (eq sb/theme 'leuven)
    (set-face-attribute 'mode-line nil :background "grey88" :foreground "black")
    (set-face-attribute 'mode-line-buffer-id nil :weight 'bold :foreground "black"
                        :background "gray88"))

  (powerline-default-theme))


(when t
  (when (eq sb/modeline-theme 'sml)
    (declare-function sml/setup "smart-mode-line")

    (unless (fboundp 'sml/setup)
      (autoload #'sml/setup "smart-mode-line" nil t))

    (require 'smart-mode-line-powerline-theme)

    (defvar sml/theme)
    (defvar sml/mode-width)
    (defvar sml/no-confirm-load-theme)
    (defvar sml/shorten-modes)
    (defvar sml/shorten-directory)

    (setq sml/theme 'light
          sml/mode-width 'full
          sml/no-confirm-load-theme t
          sml/shorten-modes t
          sml/shorten-directory t)

    (sml/setup)))


(when t
  (when (eq sb/modeline-theme 'spaceline)
    (require 'spaceline-config)
    (require 'spaceline-all-the-icons)

    (defvar spaceline-hud-p)
    (defvar spaceline-input-method-p)
    (defvar spaceline-persp-name-p)
    (defvar spaceline-selection-info-p)
    (defvar spaceline-version-control-p)

    (setq spaceline-hud-p nil
          spaceline-input-method-p nil
          spaceline-persp-name-p nil
          spaceline-selection-info-p nil
          spaceline-version-control-p t)

    (declare-function spaceline-all-the-icons-theme "spaceline-all-the-icons")

    (spaceline-all-the-icons-theme)))


(when t
  (when (eq sb/modeline-theme 'doom-modeline)
    (unless (fboundp 'doom-modeline-mode)
      (autoload #'doom-modeline-mode "doom-modeline" nil t))

    (defvar doom-modeline-buffer-encoding)
    (defvar doom-modeline-checker-simple-format)
    (defvar doom-modeline-indent-info)
    (defvar doom-modeline-lsp)
    (defvar doom-modeline-minor-modes)

    ;; Requires the fonts included with `all-the-icons', run `M-x all-the-icons-install-fonts'
    (setq doom-modeline-buffer-encoding nil
          doom-modeline-checker-simple-format t
          doom-modeline-indent-info nil
          doom-modeline-lsp nil
          doom-modeline-minor-modes t)

    (doom-modeline-mode 1)

    ;; (doom-modeline-bar ((t (:inherit default :height 0.8))))
    ))


(when nil
  (when (eq sb/modeline-theme 'awesome-tray)
    (declare-function awesome-tray-mode "awesome-tray")

    (unless (fboundp 'awesome-tray-mode)
      (autoload #'awesome-tray-mode "awesome-tray" nil t))
    (add-hook 'after-init-hook #'awesome-tray-mode)

    (defvar awesome-tray-active-modules)

    (setq awesome-tray-active-modules '("file-path" "buffer-name" "mode-name" "location" "git"))

    (custom-set-faces
     (backquote
      (awesome-tray-default-face
       ((t
         (:inherit default :height 0.8))))))
    (custom-set-faces
     (backquote
      (awesome-tray-module-awesome-tab-face
       ((t
         (:foreground "#b83059" :weight bold :height 0.8))))))
    (custom-set-faces
     (backquote
      (awesome-tray-module-buffer-name-face
       ((t
         (:foreground "#cc7700" :weight bold :height 0.8))))))
    (custom-set-faces
     (backquote
      (awesome-tray-module-date-face
       ((t
         (:foreground "#717175" :weight bold :height 0.8))))))
    (custom-set-faces
     (backquote
      (awesome-tray-module-file-path-face
       ((t
         (:foreground "#5e8e2e" :weight normal :height 0.8))))))
    (custom-set-faces
     (backquote
      (awesome-tray-module-git-face
       ((t
         (:foreground "#cc2444" :weight normal :height 0.8))))))
    (custom-set-faces
     (backquote
      (awesome-tray-module-last-command-face
       ((t
         (:foreground "#0061cc" :weight bold :height 0.8))))))
    (custom-set-faces
     (backquote
      (awesome-tray-module-location-face
       ((t
         (:foreground "#cc7700" :weight normal :height 0.8))))))
    (custom-set-faces
     (backquote
      (awesome-tray-module-mode-name-face
       ((t
         (:foreground "#00a400" :weight bold :height 0.8))))))
    (custom-set-faces
     (backquote
      (awesome-tray-module-parent-dir-face
       ((t
         (:foreground "#5e8e2e" :weight bold :height 0.8))))))))

(when t
  (when (eq sb/modeline-theme 'moody)
    (unless (fboundp 'moody-replace-vc-mode)
      (autoload #'moody-replace-vc-mode "moody" nil t))
    (unless (fboundp 'moody-replace-mode-line-buffer-identification)
      (autoload #'moody-replace-mode-line-buffer-identification "moody" nil t))

    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)))


;; The icons do not look good.
(when nil
  (progn
    (unless (fboundp 'mode-icons-mode)
      (autoload #'mode-icons-mode "mode-icons" nil t))

    (mode-icons-mode 1)
    ))


(progn
  (declare-function adob--rescan-windows "auto-dim-other-buffers")

  (unless (fboundp 'auto-dim-other-buffers-mode)
    (autoload #'auto-dim-other-buffers-mode "auto-dim-other-buffers" nil t))
  (unless (fboundp 'adob--rescan-windows)
    (autoload #'adob--rescan-windows "auto-dim-other-buffers" nil t))

  (run-with-idle-timer 3 nil #'auto-dim-other-buffers-mode))


(if nil
    (progn
      (declare-function centaur-tabs-group-by-projectile-project "centaur-tabs")

      (unless (fboundp 'centaur-tabs-mode)
        (autoload #'centaur-tabs-mode "centaur-tabs" nil t))
      (unless (fboundp 'centaur-tabs-group-by-projectile-project)
        (autoload #'centaur-tabs-group-by-projectile-project "centaur-tabs" nil t))

      (add-hook 'after-init-hook #'centaur-tabs-mode)

      (with-eval-after-load "centaur-tabs"
        (defvar centaur-tabs-cycle-scope)
        (defvar centaur-tabs-set-close-button)
        (defvar centaur-tabs-set-icons)
        (defvar centaur-tabs-set-modified-marker)
        (defvar centaur-tabs-icon-scale-factor)

        (setq centaur-tabs-cycle-scope 'tabs
              centaur-tabs-icon-scale-factor 0.7
              centaur-tabs-set-close-button nil
              centaur-tabs-set-icons t
              centaur-tabs-set-modified-marker t)

        (centaur-tabs-group-by-projectile-project))))


(when nil
  (unless (fboundp 'hide-mode-line-mode)
    (autoload #'hide-mode-line-mode "hide-mode-line" nil t)))


;; Value is in 1/10pt, so 100 will give you 10pt
;; (set-frame-font "DejaVu Sans Mono" nil t)
;; (set-frame-font "Roboto Mono")

;; https://github.com/wandersoncferreira/dotfiles
;; (when (member "Monaco" (font-family-list))
;;   (set-face-attribute 'default nil :font "Monaco" :height 120)
;;   (setq default-frame-alist '((font . "Monaco-12"))))

;; https://github.com/larstvei/dot-emacs
;; (cond ((member "Inconsolata" (font-family-list))
;;        (set-face-attribute 'default nil :font "Inconsolata-18")))

(when (string= (system-name) "swarnendu-Inspiron-7572")
  (set-face-attribute 'default nil :height 140))

(when (string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
  (set-face-attribute 'default nil :height 140))

(set-face-attribute 'mode-line nil :height 110)
(set-face-attribute 'mode-line-inactive nil :height 110)


;; Decrease minibuffer font
;; https://stackoverflow.com/questions/7869429/altering-the-font-size-for-the-emacs-minibuffer-separately-from-default-emacs
(progn
  (defun sb/minibuffer-font-setup ()
    "Customize minibuffer font."
    (set (make-local-variable 'face-remapping-alist)
         '((default :height 0.90))))

  (add-hook 'minibuffer-setup-hook #'sb/minibuffer-font-setup))


(when nil
  (progn
    ;; Changing height of the echo area is jarring, but limiting the height makes it difficult to
    ;; see useful information
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq resize-mini-windows nil)))
    ))


;; LATER: Check the impact on performance, do I really need it?
(when nil
  (progn
    (unless (fboundp 'beacon-mode)
      (autoload #'beacon-mode "beacon") nil t)

    (run-with-idle-timer 3 nil #'beacon-mode)

    (with-eval-after-load "beacon"
      (diminish 'beacon-mode))))


(progn
  (unless (fboundp 'ibuffer)
    (autoload #'ibuffer "ibuffer" nil t))

  (bind-keys :package ibuffer
             ("C-x C-b" . ibuffer))


  (declare-function ibuffer-auto-mode "ibuffer-ext")
  (unless (fboundp 'ibuffer-auto-mode)
    (autoload #'ibuffer-auto-mode "ibuf-ext" nil t))

  (add-hook 'ibuffer-hook #'ibuffer-auto-mode)

  (with-eval-after-load "ibuf-ext"
    ;; Do not show filter groups if there are no buffers in that group
    (defvar ibuffer-show-empty-filter-groups)

    (setq ibuffer-show-empty-filter-groups nil))


  ;; Group buffers by projectile project
  (unless (fboundp 'ibuffer-projectile-set-filter-groups)
    (autoload #'ibuffer-projectile-set-filter-groups "ibuffer-projectile" nil t))

  (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))


(when (display-graphic-p)
  (unless (fboundp 'all-the-icons-ibuffer-mode)
    (autoload #'all-the-icons-ibuffer-mode "all-the-icons-ibuffer" nil t))

  (add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)

  (with-eval-after-load "all-the-icons-ibuffer"
    (defvar all-the-icons-ibuffer-human-readable-size)
    (defvar all-the-icons-ibuffer-icon-size)

    (setq all-the-icons-ibuffer-human-readable-size t
          all-the-icons-ibuffer-icon-size 0.9)))


(when nil
  ;; IBuffer works well enough for me
  (unless (fboundp 'bufler)
    (autoload #'bufler "bufler" nil t))
  (unless (fboundp 'bufler-mode)
    (autoload #'bufler-mode "bufler" nil t))

  (with-eval-after-load "bufler"
    (bufler-mode 1)

    (diminish 'bufler-workspace-mode)))


(progn
  (declare-function dired-next-line "dired")
  (declare-function dired-jump "dired")

  (unless (fboundp 'sb/dired-go-home)
    (autoload #'sb/dired-go-home "init-autoload" nil t))
  (unless (fboundp 'find-file)
    (autoload #'find-file "dired" nil t))
  (unless (fboundp 'sb/dired-jump-to-top)
    (autoload #'sb/dired-jump-to-top "init-autoload" nil t))
  (unless (fboundp 'sb/dired-jump-to-bottom)
    (autoload #'sb/dired-jump-to-bottom "init-autoload" nil t))
  (unless (fboundp 'auto-revert-mode)
    (autoload #'auto-revert-mode "dired" nil t))
  (unless (fboundp 'dired-next-line)
    (autoload #'dired-next-line "dired" nil t))
  (unless (fboundp 'dired-jump)
    (autoload #'dired-jump "dired" nil t))

  (eval-and-compile
    (defun sb/dired-go-home nil
      (interactive)
      (dired sb/user-home))

    (defun sb/dired-jump-to-top nil
      (interactive)
      (goto-char
       (point-min))
      (dired-next-line 2))

    (defun sb/dired-jump-to-bottom nil
      (interactive)
      (goto-char
       (point-max))
      (dired-next-line -1)))

  (with-eval-after-load "dired"
    (defvar dired-auto-revert-buffer)
    (defvar dired-dwim-target)
    (defvar dired-ls-F-marks-symlinks)
    (defvar dired-recursive-copies)
    (defvar dired-recursive-deletes)
    (defvar dired-omit-verbose)

    (setq dired-auto-revert-buffer t
          dired-dwim-target t ; Guess a default target directory
          ;; Check `ls' for additional options
          dired-listing-switches "-ABhl --si --group-directories-first"
          dired-ls-F-marks-symlinks t ; -F marks links with @
          dired-recursive-copies 'always ; Single prompt for all n directories
          ;; Single prompt for all n directories
          dired-recursive-deletes 'always)

    (add-hook 'dired-mode-hook #'auto-revert-mode)
    (add-hook 'dired-mode-hook #'dired-omit-mode)

    (defvar dired-mode-map)

    (bind-keys :package dired
               ("C-x C-j" . dired-jump))

    (bind-keys :package dired :map dired-mode-map
               ("M-<home>" . sb/dired-go-home)
               ("i"        . find-file)
               ("M-<up>"   . sb/dired-jump-to-top)
               ("M-<down>" . sb/dired-jump-to-bottom))

    (when nil
      ;; More detailed colors, but can be jarring with certain themes
      (unless (fboundp #'diredfl-mode)
        (autoload #'diredfl-mode "diredfl" nil t))

      (add-hook 'dired-mode-hook #'diredfl-mode))

    ;; Narrow dired to match filter
    (unless (fboundp 'dired-narrow)
      (autoload #'dired-narrow "dired-narrow" nil t))

    (defvar dired-mode-map)

    (bind-keys :package dired-narrow :map dired-mode-map
               ("/" . dired-narrow))))


(when t
  (progn
    (declare-function dired-omit-mode "dired-x")

    (unless (fboundp 'dired-omit-mode)
      (autoload #'dired-omit-mode "dired-x" nil t))

    (with-eval-after-load "dired-x"
      ;; Do not show messages when omitting files
      (defvar dired-omit-verbose)

      (setq dired-omit-verbose nil)

      ;; (setq dired-omit-files
      ;;       (concat dired-omit-files
      ;;               "\\|^.DS_Store\\'"
      ;;               "\\|^.project\\(?:ile\\)?\\'"
      ;;               "\\|^.\\(svn\\|git\\)\\'"
      ;;               "\\|^.ccls-cache\\'"
      ;;               ;; FIXME: Fix the regexp
      ;;               ;; "\\|__pycache__"
      ;;               "\\|\\(?:\\.js\\)?\\.meta\\'"
      ;;               "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))


      ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
      (defadvice dired-omit-startup (after diminish-dired-omit activate)
        "Remove 'Omit' from the modeline."
        (diminish 'dired-omit-mode)
        dired-mode-map))
    ))


;; Do not create multiple dired buffers
(when nil
  (progn
    (declare-function diredp-toggle-find-file-reuse-dir "dired+")

    (unless (fboundp 'diredp-toggle-find-file-reuse-dir)
      (autoload #'diredp-toggle-find-file-reuse-dir "dired+" nil t))

    (add-hook 'dired-mode-hook (lambda ()
                                 (diredp-toggle-find-file-reuse-dir 1)))

    (with-eval-after-load "dired+"
      (defvar diredp-hide-details-initially-flag)
      (defvar diredp-hide-details-propagate-flag)

      (setq diredp-hide-details-initially-flag nil
            diredp-hide-details-propagate-flag nil)

      )))


(progn
  (unless (fboundp 'dired-efap)
    (autoload #'dired-efap "dired-efap" nil t))

  (defvar dired-efap-initial-filename-selection)

  (setq dired-efap-initial-filename-selection nil)

  ;; Bound to `diredp-rename-this-file', prefer `dired-efap'. This binding only works if we load
  ;; after `dired+' and not `dired', even with `bind-keys*'.
  (defvar dired-mode-map)

  (bind-keys* :package dired-efap :map dired-mode-map
              ("r" . dired-efap)))


(progn
  (unless (fboundp 'async-bytecomp-package-mode)
    (autoload #'async-bytecomp-package-mode "async" nil t))

  (async-bytecomp-package-mode 1)

  (unless (fboundp 'dired-async-mode)
    (autoload #'dired-async-mode "dired-async" nil t))

  (add-hook 'dired-mode-hook #'dired-async-mode)

  (with-eval-after-load "dired-async"
    (diminish 'dired-async-mode)))


(when (display-graphic-p)
  (unless (fboundp 'all-the-icons-dired-mode)
    (autoload #'all-the-icons-dired-mode "all-the-icons-dired" nil t))

  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

  (with-eval-after-load "all-the-icons-dired"
    (diminish 'all-the-icons-dired-mode)))


(when t
  (progn
    (when (display-graphic-p)
      (declare-function treemacs-git-mode "treemacs")
      (declare-function treemacs-follow-mode "treemacs")
      (declare-function treemacs-current-workspace "treemacs")
      (declare-function treemacs-fringe-indicator-mode "treemacs")
      (declare-function treemacs-goto-file-node "treemacs")
      (declare-function treemacs-find-file-node "treemacs")
      (declare-function treemacs-filewatch-mode "treemacs")
      (declare-function treemacs-create-theme "treemacs")
      (declare-function treemacs-load-theme "treemacs")
      (declare-function treemacs-canonical-path "treemacs")
      (declare-function treemacs-add-project-to-workspace "treemacs")
      (declare-function treemacs-do-add-project-to-workspace "treemacs")
      (declare-function treemacs-scope->current-scope "treemacs")
      (declare-function treemacs--restore-eldoc-after-log "treemacs")
      (declare-function treemacs--filename "treemacs")
      (declare-function treemacs--find-workspace "treemacs")
      (declare-function treemacs-workspace->is-empty "treemacs")
      (declare-function treemacs-pulse-on-success "treemacs")
      (declare-function treemacs-is-path "treemacs")
      (declare-function treemacs-theme->gui-icons "treemacs")
      (declare-function treemacs--find-current-user-project "treemacs")
      (declare-function treemacs--propagate-new-icons "treemacs")
      (declare-function treemacs-workspace->is-empty? "treemacs")

      (unless (fboundp 'treemacs)
        (autoload #'treemacs "treemacs" nil t))
      (unless (fboundp 'treemacs-current-workspace)
        (autoload #'treemacs-current-workspace "treemacs" nil t))
      (unless (fboundp 'treemacs--find-current-user-project)
        (autoload #'treemacs--find-current-user-project "treemacs" nil t))
      (unless (fboundp 'treemacs-do-add-project-to-workspace)
        (autoload #'treemacs-do-add-project-to-workspace "treemacs" nil t))
      (unless (fboundp 'treemacs-add-project-to-workspace)
        (autoload #'treemacs-add-project-to-workspace "treemacs" nil t))
      (unless (fboundp 'treemacs-git-mode)
        (autoload #'treemacs-git-mode "treemacs" nil t))
      (unless (fboundp 'treemacs-follow-mode)
        (autoload #'treemacs-follow-mode "treemacs" nil t))
      (unless (fboundp 'treemacs-fringe-indicator-mode)
        (autoload #'treemacs-fringe-indicator-mode "treemacs" nil t))
      (unless (fboundp 'treemacs-filewatch-mode)
        (autoload #'treemacs-filewatch-mode "treemacs" nil t))
      (unless (fboundp 'treemacs-goto-file-node)
        (autoload #'treemacs-goto-file-node "treemacs" nil t))
      (unless (fboundp 'treemacs--propagate-new-icons)
        (autoload #'treemacs--propagate-new-icons "treemacs" nil t))
      (unless (fboundp 'treemacs-scope->current-scope)
        (autoload #'treemacs-scope->current-scope "treemacs" nil t))
      (unless (fboundp 'treemacs--restore-eldoc-after-log)
        (autoload #'treemacs--restore-eldoc-after-log "treemacs" nil t))
      (unless (fboundp 'treemacs-load-theme)
        (autoload #'treemacs-load-theme "treemacs" nil t))
      (unless (fboundp 'treemacs-find-file-node)
        (autoload #'treemacs-find-file-node "treemacs" nil t))
      (unless (fboundp 'treemacs-resize-icons)
        (autoload #'treemacs-resize-icons "treemacs" nil t))
      (unless (fboundp 'treemacs-select-window)
        (autoload #'treemacs-select-window "treemacs" nil t))
      (unless (fboundp 'treemacs-add-and-display-current-project)
        (autoload #'treemacs-add-and-display-current-project "treemacs" nil t))
      (unless (fboundp 'treemacs-display-current-project-exclusively)
        (autoload #'treemacs-display-current-project-exclusively "treemacs" nil t))

      (eval-and-compile
        (defun sb/setup-treemacs-quick nil
          "Setup treemacs."
          (interactive)
          (when (projectile-project-p)
            (treemacs-add-and-display-current-project)
            (other-window 1)))

        (defun sb/setup-treemacs-detailed (args)
          "Setup treemacs."
          (let* ((root (treemacs--find-current-user-project))
                 (path (treemacs-canonical-path root))
                 (name (treemacs--filename path)))
            (unless (treemacs-current-workspace)
              (treemacs--find-workspace))
            (if (treemacs-workspace->is-empty\?)
                (progn
                  (treemacs-do-add-project-to-workspace path name)
                  (treemacs-select-window)
                  (treemacs-pulse-on-success)
                  (other-window 1)
                  (when (featurep 'auto-dim-other-buffers)
                    (adob--rescan-windows)))
              (treemacs-select-window)
              (if (treemacs-is-path path :in-workspace)
                  (treemacs-goto-file-node path)
                (treemacs-add-project-to-workspace path name))
              (other-window 1)
              (when (featurep 'auto-dim-other-buffers)
                (adob--rescan-windows))))))

      (with-eval-after-load "treemacs"
        (defvar treemacs-collapse-dirs)
        (defvar treemacs-follow-after-init)
        (defvar treemacs-indentation)
        (defvar treemacs-is-never-other-window)
        (defvar treemacs-no-png-images)
        (defvar treemacs-position)
        (defvar treemacs-project-follow-cleanup)
        (defvar treemacs-recenter-after-file-follow)
        (defvar treemacs-recenter-after-tag-follow)
        (defvar treemacs-show-hidden-files)
        (defvar treemacs-silent-filewatch)
        (defvar treemacs-silent-refresh)
        (defvar treemacs-width)
        (defvar treemacs-persist-file)
        (defvar treemacs-indentation-string)
        (defvar treemacs-user-mode-line-format)

        (setq treemacs-collapse-dirs 2
              treemacs-follow-after-init t
              treemacs-indentation 1
              treemacs-indentation-string (propertize " ⫶ " 'face 'font-lock-comment-face)
              ;; Prevents treemacs from being selected with `other-window'
              treemacs-is-never-other-window nil
              ;; treemacs-no-png-images nil
              ;; treemacs-position 'right
              treemacs-project-follow-cleanup t
              treemacs-recenter-after-file-follow 'on-distance
              treemacs-recenter-after-tag-follow 'on-distance
              treemacs-show-hidden-files nil
              treemacs-silent-filewatch t
              treemacs-silent-refresh t
              treemacs-width 18
              ;; Hide the mode-line in the Treemacs buffer
              treemacs-user-mode-line-format 'none)

        (unless (bound-and-true-p sb/use-no-littering)
          (setq treemacs-persist-file (expand-file-name "treemacs-persist" sb/temp-directory)))

        (treemacs-filewatch-mode 1)
        (treemacs-follow-mode 1) ; Following tags is noisy
        (treemacs-git-mode 'deferred)

        ;; Always show the file indicator
        (treemacs-fringe-indicator-mode 'always)

        (require 'treemacs-all-the-icons)

        ;; https://github.com/Alexander-Miller/treemacs/issues/735
        (treemacs-create-theme "Default-Tighter"
          :extends "Default"
          :config
          (let ((icons (treemacs-theme->gui-icons theme)))
            (maphash (lambda
                       (ext icon)
                       (puthash ext
                                (concat
                                 (substring icon 0 1)
                                 (propertize " " 'display
                                             '(space . (:width 0.5))))
                                icons))
                     icons)))

        (treemacs-create-theme "all-the-icons-tighter"
          :extends "all-the-icons"
          :config
          (let ((icons (treemacs-theme->gui-icons theme)))
            (maphash (lambda
                       (ext icon)
                       (puthash ext
                                (concat
                                 (substring icon 0 1)
                                 (propertize " " 'display
                                             '(space . (:width 0.5))))
                                icons))
                     icons)))

        (treemacs-load-theme "all-the-icons")

        (set-face-attribute 'treemacs-root-face nil :height 0.8)
        (set-face-attribute 'treemacs-directory-collapsed-face nil :height 0.7)
        (set-face-attribute 'treemacs-directory-face nil :height 0.7)
        (set-face-attribute 'treemacs-file-face nil :height 0.7)
        (set-face-attribute 'treemacs-tags-face nil :height 0.7)
        (set-face-attribute 'treemacs-git-ignored-face nil :height 0.7)
        (set-face-attribute 'treemacs-git-untracked-face nil :height 0.7)

        (when (or (eq sb/theme 'modus-operandi) (eq sb/theme 'modus-vivendi))
          (set-face-attribute 'treemacs-git-modified-face nil :height 0.7)
          (set-face-attribute 'treemacs-git-unmodified-face nil :height 0.7))

        (when (eq sb/theme 'sb/default)
          (set-face-attribute 'treemacs-git-modified-face nil :height 0.8)
          (set-face-attribute 'treemacs-git-unmodified-face nil :height 1.0)))

      ;; Ignore files

      ;; (defun sb/treemacs-ignore-files (filename absolute-path)
      ;;   (or (string-equal filename "foo")
      ;;       (string-prefix-p "/x/y/z/" absolute-path)))

      ;; (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-files)


      ;; Allows to quickly add projectile projects to the treemacs workspace
      (unless (fboundp 'treemacs-projectile)
        (autoload #'treemacs-projectile "treemacs-projectile" nil t))

      (require 'treemacs-magit)

      (bind-keys* :package treemacs
                  ;; ("C-j" . treemacs) ; Interferes with `dired-jump'
                  ("M-0" . treemacs-select-window)))

    ;; Starts Treemacs automatically with Emacsclient
    ;; https://github.com/Alexander-Miller/treemacs/issues/624

    ;; (add-hook 'after-make-frame-functions
    ;;           (lambda (frame)
    ;;             (run-with-timer
    ;;              1 nil
    ;;              (lambda ()
    ;;                (with-selected-frame frame
    ;;                  (save-selected-window
    ;;                    (treemacs-select-window)))))))

    ;; (add-hook 'emacs-startup-hook (lambda()
    ;;                                 (treemacs)
    ;;                                 (other-window 1)))
    ))


(with-eval-after-load "org"
  (defvar org-fontify-done-headline)
  (defvar org-fontify-whole-heading-line)
  (defvar org-hide-emphasis-markers)
  (defvar org-hide-leading-stars)
  (defvar org-hide-leading-stars-before-indent-mode)
  (defvar org-src-fontify-natively)
  (defvar org-src-preserve-indentation)
  (defvar org-src-tabs-acts-natively)
  (defvar org-src-window-setup)
  (defvar org-startup-indented)
  (defvar org-startup-truncated)
  (defvar org-startup-folded)
  (defvar org-startup-with-inline-images)
  (defvar org-support-shift-select)
  (defvar org-use-speed-commands)
  (defvar org-src-strip-leading-and-trailing-blank-lines)
  (defvar org-pretty-entities)
  (defvar org-pretty-entities-include-sub-superscripts)
  (defvar org-footnote-auto-adjust)

  (setq org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        ;; Code block fontification using the major-mode of the code
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tabs-acts-natively t
        org-src-window-setup 'current-window
        org-startup-indented t
        org-startup-truncated nil
        org-startup-folded 'showeverything
        org-startup-with-inline-images t
        org-support-shift-select t
        ;; See `org-speed-commands-default' for a list of the keys and commands enabled at the
        ;; beginning of headlines. `org-babel-describe-bindings' will display a list of the code
        ;; blocks commands and their related keys.
        org-use-speed-commands t
        org-src-strip-leading-and-trailing-blank-lines t
        ;; Display entities like `\tilde' and `\alpha' in UTF-8 characters
        org-pretty-entities t
        ;; Render subscripts and superscripts in org buffers
        org-pretty-entities-include-sub-superscripts t
        ;; Automatically sorted and renumbered whenever I insert a new one
        org-footnote-auto-adjust t
        )

  (defvar org-mode-map)
  ;; (unbind-key "M-<left>" org-mode-map)
  ;; (unbind-key "M-<right>" org-mode-map)

  (bind-keys :package org :map org-mode-map
             ("M-<left>")
             ("M-<right>")
             ("C-'")
             ("<tab>"      . org-indent-item)
             ("<backtab>"  . org-outdent-item))

  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode 1)
                             (prettify-symbols-mode 1))))


(progn
  (declare-function org-indent-mode "org-indent")
  (declare-function org-indent-item "org-indent")
  (declare-function org-outdent-item "org-indent")

  (unless (fboundp 'org-indent-mode)
    (autoload #'org-indent-mode "org-indent"))

  (add-hook 'org-mode-hook #'org-indent-mode)

  (with-eval-after-load "org-indent"
    (diminish 'org-indent-mode)))


(progn
  (unless (fboundp 'org-bullets-mode)
    (autoload #'org-bullets-mode "org-bullets" nil t))

  (add-hook 'org-mode-hook #'org-bullets-mode))


;; TODO: Use `C-c o' as the binding for `org-mode-map'


;; Use `C-'' in `isearch-mode-map' to use `avy-isearch' to select one of the currently visible
;; isearch candidates
(progn
  (unless (fboundp 'isearch-forward-regexp)
    (autoload #'isearch-forward-regexp "isearch" nil t))
  (unless (fboundp 'isearch-repeat-forward)
    (autoload #'isearch-repeat-forward "isearch" nil t))
  (unless (fboundp 'isearch-occur)
    (autoload #'isearch-occur "isearch" nil t))

  (with-eval-after-load "isearch"
    (setq search-highlight t) ; Highlight incremental search

    (unless (fboundp 'isearch-symbol-at-point)
      (autoload #'isearch-symbol-at-point "isearch-symbol-at-point" nil t))
    (unless (fboundp 'isearch-backward-symbol-at-point)
      (autoload #'isearch-backward-symbol-at-point "isearch-symbol-at-point" nil t))

    (unless (fboundp 'isearch-dabbrev-expand)
      (autoload #'isearch-dabbrev-expand "isearch-dabbrev" nil t))

    (bind-keys :package isearch-dabbrev :map isearch-mode-map
               ("<tab>" . isearch-dabbrev-expand)))

  ;; Change the binding for `isearch-forward-regexp' and `isearch-repeat-forward'
  (bind-keys :package isearch
             ("C-s")
             ("C-f"     . isearch-forward-regexp)
             :map isearch-mode-map
             ("C-s")
             ("C-f"     . isearch-repeat-forward)
             ("C-c C-o" . isearch-occur)))


(progn
  (unless (fboundp 'global-anzu-mode)
    (autoload #'global-anzu-mode "anzu" nil t))

  (global-anzu-mode 1)

  (defvar anzu-search-threshold)
  (defvar anzu-minimum-input-length)

  (setq anzu-search-threshold 10000
        anzu-minimum-input-length 2)

  ;; (when (eq sb/modeline-theme 'spaceline)
  ;;   (setq anzu-cons-mode-line-p nil))

  ;; (unless (eq sb/theme 'leuven)
  ;;   (set-face-attribute 'anzu-mode-line nil :foreground "blue" :weight 'light))

  (diminish 'anzu-mode))


(progn
  (unless (fboundp 'swiper)
    (autoload #'swiper "swiper" nil t))
  (unless (fboundp 'swiper-isearch)
    (autoload #'swiper-isearch "swiper" nil t))

  (with-eval-after-load "swiper"
    (defvar swiper-action-recenter)

    (setq swiper-action-recenter t)))


(progn
  (with-eval-after-load "grep"
    (defvar grep-highlight-matches)
    (defvar grep-scroll-output)
    (defvar grep-find-ignored-directories)

    (setq grep-command "grep -irHn "
          grep-highlight-matches t
          grep-scroll-output t)

    (dolist (dirs '(".cache" "node_modules" "vendor*"))
      (add-to-list 'grep-find-ignored-directories dirs))))


;; When the *grep* buffer is huge, `wgrep-change-to-wgrep-mode' might freeze Emacs for several
;; minutes.
(progn
  (declare-function wgrep-finish-edit "wgrep")
  (declare-function wgrep-exit "wgrep")
  (declare-function wgrep-abort-changes "wgrep")
  (declare-function wgrep-change-to-wgrep-mode "wgrep")

  (unless (fboundp 'wgrep-finish-edit)
    (autoload #'wgrep-finish-edit "wgrep" nil t))
  (unless (fboundp 'wgrep-exit)
    (autoload #'wgrep-exit "wgrep" nil t))
  (unless (fboundp 'wgrep-abort-changes)
    (autoload #'wgrep-abort-changes "wgrep" nil t))
  (unless (fboundp 'wgrep-change-to-wgrep-mode)
    (autoload #'wgrep-change-to-wgrep-mode "wgrep" nil t))

  (with-eval-after-load "wgrep"
    (defvar wgrep-auto-save-buffer)

    (setq wgrep-auto-save-buffer t))

  (defvar grep-mode-map)
  (bind-keys :package wgrep :map grep-mode-map
             ("C-c C-c" . wgrep-finish-edit)
             ("C-x C-s" . wgrep-finish-edit)
             ("C-x C-q" . wgrep-exit)
             ("C-c C-k" . wgrep-abort-changes)
             ("C-x C-p" . wgrep-change-to-wgrep-mode)))


(progn
  (unless (fboundp 'deadgrep)
    (autoload #'deadgrep "deadgrep" nil t))

  (bind-keys :package deadgrep
             ("C-c s d" . deadgrep)))


(unless (fboundp 'ripgrep)
  (autoload #'ripgrep-regexp "ripgrep" nil t))


(when nil
  (unless (fboundp 'ctrlf-mode)
    (autoload #'ctrlf-mode "ctrlf" nil t))
  (unless (fboundp 'ctrlf-local-mode)
    (autoload #'ctrlf-local-mode "ctrlf" nil t))
  (unless (fboundp 'ctrlf-forward-literal)
    (autoload #'ctrlf-forward-literal "ctrlf" nil t))
  (unless (fboundp 'ctrlf-backward-literal)
    (autoload #'ctrlf-backward-literal "ctrlf" nil t))
  (unless (fboundp 'ctrlf-forward-regexp)
    (autoload #'ctrlf-forward-regexp "ctrlf" nil t))
  (unless (fboundp 'ctrlf-backward-regexp)
    (autoload #'ctrlf-backward-regexp "ctrlf" nil t))

  (with-eval-after-load "ctrlf"
    (ctrlf-mode 1)

    (add-hook 'pdf-isearch-minor-mode-hook
              (lambda ()
                (ctrlf-local-mode -1))))

  (bind-keys :package ctrlf
             ("C-f"   . ctrlf-forward-literal)
             ("C-r"   . ctrlf-backward-literal)
             ("C-M-s" . ctrlf-forward-regexp)
             ("C-M-r" . ctrlf-backward-regexp)))


(progn
  (defvar reb-re-syntax)

  (setq reb-re-syntax 'string))

(progn
  (unless (fboundp 'vr/query-replace)
    (autoload #'vr/query-replace "visual-regexp" nil t))

  (bind-keys :package visual-regexp
             ([remap query-replace] . vr/query-replace)))


(progn
  (declare-function recentf-save-file "recentf")
  (declare-function recentf-save-list "recentf")
  (declare-function recentf-cleanup "recentf")

  (unless (fboundp 'recentf-mode)
    (autoload #'recentf-mode "recentf" nil t))
  (unless (fboundp 'recentf-save-file)
    (autoload #'recentf-save-file "recentf" nil t))
  (unless (fboundp 'recentf-cleanup)
    (autoload #'recentf-cleanup "recentf" nil t))
  (unless (fboundp 'recentf-save-list)
    (autoload #'recentf-save-list "recentf" nil t))

  ;; Load immediately after start since I use it often
  (add-hook 'after-init-hook #'recentf-mode)

  (with-eval-after-load "recentf"
    (defvar recentf-auto-cleanup)
    (defvar recentf-exclude)
    (defvar recentf-max-saved-items)
    (defvar recentf-menu-filter)
    (defvar recentf-save-file)
    (defvar recentf-keep)
    (defvar recentf-filename-handlers)

    (setq recentf-auto-cleanup 'never ; Do not stat remote files
          ;; Check regex with `re-builder', use `recentf-cleanup' to update the list
          recentf-exclude '("[/\\]elpa/"
                            "[/\\]\\.git/"
                            ".*\\.gz\\'"
                            ".*\\.xz\\'"
                            ".*\\.zip\\'"
                            "[/\\]archive-contents\\'"
                            "[/\\]\\.loaddefs\\.el\\'"
                            "[/\\]tmp/.*"
                            ".*/recentf\\'"
                            ".*/recentf-save.el\\'"
                            ".*/init.el\\'"
                            "~$"
                            "/.autosaves/"
                            ".*/TAGS\\'"
                            "*.cache")
          ;; https://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
          ;; Keep remote file without testing if they still exist
          recentf-keep '(file-remote-p file-readable-p)
          recentf-max-saved-items 250 ; Larger values help in lookup
          ;; recentf-menu-filter 'recentf-sort-descending
          recentf-filename-handlers (append '(abbreviate-file-name)
                                            recentf-filename-handlers))

    (unless (bound-and-true-p sb/use-no-littering)
      (setq recentf-save-file (expand-file-name "recentf" sb/temp-directory)))

    (when (bound-and-true-p sb/use-no-littering)
      (defvar no-littering-etc-directory)
      (defvar no-littering-var-directory)

      (add-to-list 'recentf-exclude (file-truename no-littering-etc-directory))
      (add-to-list 'recentf-exclude (file-truename no-littering-var-directory)))

    ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
    ;; after idling for 30 seconds.
    (run-with-idle-timer 30 t #'recentf-save-list)

    ;; Adding many functions to `kill-emacs-hook' slows down Emacs exit, hence we are only using
    ;; idle timers.
    (run-with-idle-timer 60 t #'recentf-cleanup)))


(defun sb/inhibit-message-call-orig-fun (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN, forward ARGS."
  (let ((inhibit-message t))
    (apply orig-fun args)))

;; Hide the "Wrote to recentf" message which is irritating
(advice-add 'recentf-save-list :around #'sb/inhibit-message-call-orig-fun)


;; TODO: Is this causing tramp to fail? I have disabled it to test.
;; Hide the "Wrote ..." message which is irritating
;; (advice-add 'write-region :around #'sb/inhibit-message-call-orig-fun)


;; Use `M-x company-diag' or the modeline status to see the backend used. Try `M-x
;; company-complete-common' when there are no completions. Use `C-M-i' to `complete-symbol' with
;; regex search.
(progn
  (declare-function company-select-next "company")
  (declare-function company-select-previous "company")
  (declare-function company-complete-common-or-cycle "company")
  (declare-function company-abort "company")
  (declare-function company-clang-set-prefix "company-clang")

  (unless (fboundp 'global-company-mode)
    (autoload #'global-company-mode "company" nil t))
  (unless (fboundp 'company-select-next)
    (autoload #'company-select-next "company" nil t))
  (unless (fboundp 'company-select-previous)
    (autoload #'company-select-previous "company" nil t))
  (unless (fboundp 'company-complete-common-or-cycle)
    (autoload #'company-complete-common-or-cycle "company" nil t))
  (unless (fboundp 'sb/quit-company-save-buffer)
    (autoload #'sb/quit-company-save-buffer "init-autoload" nil t))
  (unless (fboundp 'company-abort)
    (autoload #'company-abort "company" nil t))

  (add-hook 'after-init-hook #'global-company-mode)

  (eval-and-compile
    (defun sb/quit-company-save-buffer ()
      "Quit company popup and save the buffer."
      (interactive)
      (company-abort)
      (save-buffer)))

  (with-eval-after-load "company"
    (defvar company-dabbrev-other-buffers)
    (defvar company-idle-delay)
    (defvar company-ispell-available)
    (defvar company-ispell-dictionary)
    (defvar company-minimum-prefix-length)
    (defvar company-require-match)
    (defvar company-selection-wrap-around)
    (defvar company-show-numbers)
    (defvar company-tooltip-align-annotations)
    (defvar company-transformers)
    (defvar company-dabbrev-downcase)
    (defvar company-dabbrev-ignore-case)
    (defvar company-tooltip-limit)

    (setq company-dabbrev-downcase nil ; Do not downcase returned candidates
          company-dabbrev-ignore-case nil ; Do not ignore case when collecting completion candidates
          company-dabbrev-other-buffers t ; Search in other buffers with the same major mode
          company-ispell-available t
          company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory)
          company-minimum-prefix-length 3 ; Small words are faster to type
          company-require-match nil ; Allow input string that do not match candidates
          company-selection-wrap-around t
          company-show-numbers t ; Speed up completion
          ;; Align additional metadata, like type signatures, to the right-hand side
          company-tooltip-align-annotations t)

    ;; Ignore matches that consist solely of numbers from `company-dabbrev'
    ;; https://github.com/company-mode/company-mode/issues/358
    (push (apply-partially #'cl-remove-if (lambda (c)
                                            (string-match-p "\\`[0-9]+\\'" c)))
          company-transformers)

    ;; We set `company-backends' as a local variable
    ;; (dolist (backends '(company-semantic company-bbdb company-oddmuse company-cmake))
    ;;   (delq backends company-backends))

    (remove-hook 'kill-emacs-hook #'company-clang-set-prefix)

    (diminish 'company-mode)

    ;; Posframes do not have unaligned rendering issues with variable `:height' unlike an overlay.
    ;; https://github.com/company-mode/company-mode/issues/1010
    (unless (fboundp 'company-posframe-mode)
      (autoload #'company-posframe-mode "company-posframe" nil t))

    ;; The width of the frame popup is often not enough and the right side gets cut off.
    (company-posframe-mode 1)

    (defvar company-posframe-show-metadata)
    (defvar company-posframe-show-indicator)
    (defvar company-posframe-quickhelp-delay)

    (setq company-posframe-show-metadata nil ; Difficult to distinguish and distracting
          company-posframe-show-indicator nil
          company-posframe-quickhelp-delay nil)

    (diminish 'company-posframe-mode)


    (unless (fboundp 'company-quickhelp-mode)
      (autoload #'company-quickhelp-mode "company-quickhelp" nil t))

    (run-at-time 3 nil #'company-quickhelp-mode)


    ;; Company now has in-built support for completion icons
    (when nil
      (progn
        (when (display-graphic-p)
          (unless (fboundp 'company-box-mode)
            (autoload #'company-box-mode "company-box" nil t))

          ;; LATER: This is not working.
          (company-box-mode 1)

          (with-eval-after-load "company-box"
            (defvar company-box-icons-alist)
            (defvar company-box-show-single-candidate)
            (defvar company-frontends)

            (setq company-box-icons-alist 'company-box-icons-all-the-icons
                  company-box-show-single-candidate t
                  company-frontends '(company-box-frontend)))

          (diminish 'company-box-mode)

          ;; (set-face-background 'company-box-background "cornsilk")
          ;; (set-face-background 'company-box-selection "light blue")
          )))


    ;; Typing `TabNine::config' in any buffer should open the extension settings, deep local mode is
    ;; computationally expensive. Completions seem to be laggy with TabNine enabled.


    ;; Nice but slows completions
    (when nil
      (unless (fboundp 'company-fuzzy-mode)
        (autoload #'company-fuzzy-mode "company-fuzzy" nil t))
      (unless (fboundp 'global-company-fuzzy-mode)
        (autoload #'global-company-fuzzy-mode "company-fuzzy" nil t))

      (global-company-fuzzy-mode 1)

      (diminish 'company-fuzzy-mode)

      (setq company-fuzzy-show-annotation t
            company-fuzzy-sorting-backend 'flx))


    ;; Company statistics, we are currently trying out `company-prescient'
    (when nil
      (progn
        (unless (fboundp 'company-statistics-mode)
          (autoload #'company-statistics-mode "company-statistics" nil t))

        (company-statistics-mode 1))
      ))

  (defvar company-active-map)
  (bind-keys :package company :map company-active-map
             ("C-n"      . company-select-next)
             ("C-p"      . company-select-previous)
             ;; Insert the common part of all candidates, or select the next one.
             ("<tab>"    . company-complete-common-or-cycle)
             ("C-s"      . sb/quit-company-save-buffer)
             ("<escape>" . company-abort)))


;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)
;; Hide the "Starting new Ispell process" message
(advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)
(advice-add 'ispell-lookup-words :around #'sb/inhibit-message-call-orig-fun)


(progn
  (unless (fboundp 'snippet-mode)
    (autoload #'snippet-mode "yasnippet" nil t))
  (unless (fboundp 'yas-global-mode)
    (autoload #'yas-global-mode "yasnippet" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("/\\.emacs\\.d/snippets/" . snippet-mode))

  (add-hook 'text-mode-hook #'yas-global-mode)
  (add-hook 'prog-mode-hook #'yas-global-mode)

  (with-eval-after-load "yasnippet"
    (defvar yas-snippet-dirs)
    (defvar yas-verbosity)

    (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))
          yas-verbosity 1)

    (diminish 'yas-minor-mode)

    (unless (fboundp 'yasnippet-snippets-initialize)
      (autoload #'yasnippet-snippets-initialize "yasnippet-snippets" nil t))

    (yasnippet-snippets-initialize)))


(when (eq sb/selection 'ivy)
  (with-eval-after-load "ivy"
    (progn
      (declare-function ivy-yasnippet "ivy-yasnippet")

      (unless (fboundp 'ivy-yasnippet)
        (autoload #'ivy-yasnippet "ivy-yasnippet" nil t))

      (bind-keys :package ivy-yasnippet
                 ("C-M-y" . ivy-yasnippet)))))


;; `amx-major-mode-commands' limits to commands that are relevant to the current major mode
;; `amx-show-unbound-commands' shows frequently used commands that have no key bindings
(progn
  (unless (fboundp 'amx-mode)
    (autoload #'amx-mode "amx" nil t))

  (add-hook 'after-init-hook #'amx-mode)

  (with-eval-after-load "amx"
    (defvar amx-save-file)
    (defvar amx-auto-update-interval)

    (setq amx-auto-update-interval 5) ; Update the command list every n minutes

    (unless (bound-and-true-p sb/use-no-littering)
      (setq amx-save-file (expand-file-name "amx-items" sb/temp-directory)))))


(when (eq sb/selection 'ivy)
  (progn
    (declare-function ivy-alt-done "ivy")
    (declare-function ivy-previous-line "ivy")
    (declare-function ivy-next-line "ivy")
    (declare-function ivy-completion-in-region "ivy")
    (declare-function ivy-format-function-line "ivy")

    (unless (fboundp 'ivy-mode)
      (autoload #'ivy-mode "ivy" nil t))
    (unless (fboundp 'ivy-resume)
      (autoload #'ivy-resume "ivy" nil t))
    (unless (fboundp 'ivy-switch-buffer)
      (autoload #'ivy-switch-buffer "ivy" nil t))
    (unless (fboundp 'ivy-alt-done)
      (autoload #'ivy-alt-done "ivy" nil t))
    (unless (fboundp 'ivy-previous-line)
      (autoload #'ivy-previous-line "ivy" nil t))
    (unless (fboundp 'ivy-next-line)
      (autoload #'ivy-next-line "ivy" nil t))
    (unless (fboundp 'ivy-read)
      (autoload #'ivy-read "ivy" nil t))

    (add-hook 'after-init-hook #'ivy-mode)

    (eval-and-compile
      ;; https://github.com/abo-abo/swiper/wiki/Hiding-dired-buffers
      (defun sb/ignore-dired-buffers (str)
        "Return non-nil if STR names a Dired buffer.
 This function is intended for use with `ivy-ignore-buffers'."
        (let ((buf (get-buffer str)))
          (and buf (eq (buffer-local-value 'major-mode buf)
                       'dired-mode)))))

    (with-eval-after-load "ivy"
      (defvar ivy-initial-inputs-alist)
      (defvar ivy-case-fold-search)
      (defvar ivy-count-format)
      (defvar ivy-extra-directories)
      (defvar ivy-fixed-height-minibuffer)
      (defvar ivy-ignore-buffers)
      (defvar ivy-truncate-lines)
      (defvar ivy-wrap)

      (setq ivy-case-fold-search 'always ; Always ignore case while searching
            ;; ivy-initial-inputs-alist nil ; Do not start completion with `^'
            ivy-count-format "(%d/%d) " ; Helps identify wrap around
            ivy-extra-directories nil ; Hide . and ..
            ivy-fixed-height-minibuffer t ; Distracting if the height keeps changing
            ;; Make the height of the minibuffer proportionate to the screen, but it requires more
            ;; processing
            ;; ivy-height-alist '((t
            ;;                      lambda (_caller)
            ;;                      (/ (frame-height) 2)))
            ;; Set this after loading `orderless'
            ;; ivy-re-builders-alist '((counsel-M-x       . ivy--regex-fuzzy)
            ;;                         (counsel-find-file . ivy--regex-fuzzy)
            ;;                         (t                 . ivy--regex-ignore-order))
            ivy-truncate-lines nil ; `counsel-flycheck' output gets truncated
            ivy-wrap t)

      (dolist (buffer
               '("TAGS" "magit-process" "*emacs*" "*xref*"
                 ;; "*eldoc for use-package*" "^\\*Help\\*$" "^\\*Ibuffer\\*$" "*Warnings*"
                 ;; "^\\*Compile-Log\\*$" "^\\*.+Completions\\*$" "^\\*Backtrace\\*$"
                 ;; "*flycheck-posframe-buffer*" "^\\*prettier" "^\\*json*" "^\\*texlab*"
                 ;; "^\\*clangd*" "^\\*shfmt*" "*company-documentation*"
                 ))
        (add-to-list 'ivy-ignore-buffers buffer))

      ;; Ignore `dired' buffers from `ivy-switch-buffer'
      ;; (add-to-list 'ivy-ignore-buffers #'sb/ignore-dired-buffers)

      (diminish 'ivy-mode))

    (defvar ivy-minibuffer-map)
    (bind-keys :package ivy
               ("C-c r"    . ivy-resume)
               ("<f3>"     . ivy-switch-buffer)
               :map ivy-minibuffer-map
               ("<return>" . ivy-alt-done)
               ("<left>"   . ivy-previous-line)
               ("<right>"  . ivy-next-line))
    ))


(when (eq sb/selection 'ivy)
  (with-eval-after-load "ivy"
    (progn
      (unless (fboundp 'counsel-mode)
        (autoload #'counsel-mode "counsel" nil t))
      (unless (fboundp 'counsel-M-x)
        (autoload #'counsel-M-x "counsel" nil t))
      (unless (fboundp 'counsel-company)
        (autoload #'counsel-company "counsel" nil t))
      (unless (fboundp 'counsel-descbinds)
        (autoload #'counsel-descbinds "counsel" nil t))
      (unless (fboundp 'counsel-dired)
        (autoload #'counsel-dired "counsel" nil t))
      (unless (fboundp 'counsel-find-file)
        (autoload #'counsel-find-file "counsel" nil t))
      (unless (fboundp 'counsel-git-grep)
        (autoload #'counsel-git-grep "counsel" nil t))
      (unless (fboundp 'sb/counsel-goto-recent-directory)
        (autoload #'sb/counsel-goto-recent-directory "init-autoload" nil t))
      (unless (fboundp 'counsel-info-lookup-symbol)
        (autoload #'counsel-info-lookup-symbol "counsel" nil t))
      (unless (fboundp 'counsel-load-library)
        (autoload #'counsel-load-library "counsel" nil t))
      (unless (fboundp 'counsel-load-theme)
        (autoload #'counsel-load-theme "counsel" nil t))
      (unless (fboundp 'counsel-minor)
        (autoload #'counsel-minor "counsel" nil t))
      (unless (fboundp 'counsel-recentf)
        (autoload #'counsel-recentf "counsel" nil t))
      (unless (fboundp 'counsel-rg)
        (autoload #'counsel-rg "counsel" nil t))
      (unless (fboundp 'counsel-mark-ring)
        (autoload #'counsel-mark-ring "counsel" nil t))
      (unless (fboundp 'counsel-yank-pop)
        (autoload #'counsel-yank-pop "counsel" nil t))
      (unless (fboundp 'counsel-imenu)
        (autoload #'counsel-imenu "counsel" nil t))
      (unless (fboundp 'counsel-grep-or-swiper)
        (autoload #'counsel-grep-or-swiper "counsel" nil t))

      (add-hook 'ivy-mode-hook #'counsel-mode)

      (eval-and-compile
        ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
        (defun sb/counsel-goto-recent-directory ()
          "Open recent directories with `dired'."
          (interactive)
          (defvar recentf-list)
          (unless recentf-mode
            (recentf-mode 1))
          (let
              ((collection
                (delete-dups
                 (append
                  (mapcar 'file-name-directory recentf-list)
                  (if
                      (executable-find "fasd")
                      (split-string
                       (shell-command-to-string "fasd -ld")
                       "\n" t))))))
            (ivy-read "Directories:" collection :action 'dired))))

      (with-eval-after-load "counsel"
        (defvar counsel-describe-function-function)
        (defvar counsel-describe-variable-function)
        (defvar counsel-find-file-at-point)
        (defvar counsel-find-file-ignore-regexp)
        (defvar counsel-mode-override-describe-bindings)
        (defvar counsel-preselect-current-file)
        (defvar counsel-switch-buffer-preview-virtual-buffers)
        (defvar counsel-yank-pop-preselect-last)
        (defvar counsel-yank-pop-separator)

        (setq counsel-describe-function-function #'helpful-callable
              counsel-describe-variable-function #'helpful-variable
              counsel-find-file-at-point t
              counsel-find-file-ignore-regexp (concat
                                               "\\(?:\\`[#.]\\)"
                                               "\\|\\(?:\\`.+?[#~]\\'\\)"
                                               "\\|__pycache__"
                                               "\\|.cb$"
                                               "\\|.cb2$"
                                               "\\|.class$"
                                               "\\|.djvu$"
                                               "\\|.doc$"
                                               "\\|.docx$"
                                               "\\|.elc$"
                                               "\\|.fdb_latexmk$"
                                               "\\|.fls$"
                                               "\\|.lof$"
                                               "\\|.lot$"
                                               "\\|.o$"
                                               "\\|.ppt$"
                                               "\\|.pptx$"
                                               "\\|.pyc$"
                                               "\\|.rel$"
                                               "\\|.rip$"
                                               "\\|.so$"
                                               "\\|.synctex$"
                                               "\\|.synctex.gz$"
                                               "\\|.toc$"
                                               "\\|.xls$"
                                               "\\|.xlsx$"
                                               "\\|tags"
                                               "\\|TAGS"
                                               "\\|GPATH"
                                               "\\|GRTAGS"
                                               "\\|GTAGS"
                                               "\\|tramp"
                                               "\\|.clangd"
                                               "\\|.metadata"
                                               "\\|.recommenders"
                                               "\\|typings")
              counsel-mode-override-describe-bindings t
              counsel-preselect-current-file t
              counsel-switch-buffer-preview-virtual-buffers nil
              counsel-yank-pop-preselect-last t
              counsel-yank-pop-separator "\n------------------------------------------\n")

        (diminish 'counsel-mode))

      (bind-keys* :package counsel
                  ("C-c C-j" . counsel-imenu))

      (bind-keys :package counsel
                 ([remap execute-extended-command] . counsel-M-x)
                 ([remap completion-at-point]      . counsel-company)
                 ([remap find-file]                . counsel-find-file)
                 ;; `counsel-flycheck' shows less information than `flycheck-list-errors'
                 ;; ([remap flycheck-list-errors]  . counsel-flycheck)
                 ("<f1>"                           . counsel-M-x)
                 ("<f2>"                           . counsel-find-file)
                 ("C-c s g"                        . counsel-git-grep)
                 ("C-<f9>"                         . sb/counsel-goto-recent-directory)
                 ("C-c d m"                        . counsel-minor)
                 ("<f9>"                           . counsel-recentf)
                 ("C-c s r"                        . counsel-rg)
                 ("C-c RET"                        . counsel-mark-ring)
                 ;; Enabling preview can make switching over remote buffers slow
                 ;; ("<f3>"                        . counsel-switch-buffer)
                 ("<f4>"                           . counsel-grep-or-swiper))
      )))


(progn
  (declare-function prescient-persist-mode "prescient")

  (unless (fboundp 'prescient-persist-mode)
    (autoload #'prescient-persist-mode "prescient" nil t))

  (add-hook 'after-init-hook #'prescient-persist-mode)

  (with-eval-after-load "prescient"
    (defvar prescient-history-length)
    (defvar prescient-save-file)

    (unless (bound-and-true-p sb/use-no-littering)
      (setq prescient-save-file (expand-file-name "prescient-save.el" sb/temp-directory)))))


;; https://www.reddit.com/r/emacs/comments/9o6inu/sort_ivys_counselrecentf_results_by_timestamp/e7ze1c8/
;; LATER: This is expensive, we can possibly reduce the size of the list. But we can also search
;; easily with `ivy', so maybe sorting is not very important given the overhead.
;; (with-eval-after-load "ivy"
;;   (add-to-list 'ivy-sort-functions-alist '(counsel-recentf . file-newer-than-file-p)))

;; (setq ivy-sort-matches-functions-alist '((t . ivy--prefix-sort)))

;; (add-to-list 'ivy-sort-matches-functions-alist
;;              '(read-file-name-internal . ivy--sort-files-by-date))


(with-eval-after-load "company"
  (unless (fboundp 'company-prescient-mode)
    (autoload #'company-prescient-mode "company-prescient" nil t))

  ;; We want `capf' sort for programming modes, not with recency. This breaks the support for the
  ;; `:separate' keyword in `company'
  (company-prescient-mode 1)

  ;; (add-hook 'prog-mode-hook (lambda ()
  ;;                             (company-prescient-mode -1)))
  )


(when (eq sb/selection 'ivy)
  (with-eval-after-load "ivy"
    (progn
      (unless (fboundp 'all-the-icons-ivy-setup)
        (autoload #'all-the-icons-ivy-setup "all-the-icons-ivy" nil t))

      (all-the-icons-ivy-setup)
      )))


(progn
  (require 'orderless)

  (defvar orderless-component-separator)

  (setq completion-styles '(orderless))

  ;; (declare-function sb/just-one-face "init-autoload")

  ;; (defun sb/just-one-face (fn &rest args)
  ;;   (let ((orderless-match-faces
  ;;          [completions-common-part]))
  ;;     (apply fn args)))

  ;; (advice-add 'company-capf--candidates :around #'sb/just-one-face)

  (when (eq sb/selection 'ivy)
    (with-eval-after-load "ivy"
      (defvar ivy-re-builders-alist)
      (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))))
  )


(when (symbol-value 'sb/IS-LINUX)
  (with-eval-after-load "ispell"
    (defvar ispell-dictionary)
    (defvar ispell-extra-args)
    (defvar ispell-local-dictionary)
    (defvar ispell-silently-savep)

    (setq ispell-dictionary "en_US"
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90")
          ispell-local-dictionary "en_US"
          ispell-personal-dictionary (expand-file-name "spell" sb/extras-directory)
          ;; Save a new word to personal dictionary without asking
          ispell-silently-savep t)

    ;; Skip regions in Org-mode
    (add-to-list 'ispell-skip-region-alist '("#\\+begin_src"     . "#\\+end_src"))
    (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    ))


(when (symbol-value 'sb/IS-LINUX)
  (declare-function flyspell-overlay-p "flyspell")

  (unless (fboundp 'flyspell-prog-mode)
    (autoload #'flyspell-prog-mode "flyspell" nil t))
  (unless (fboundp 'flyspell-mode)
    (autoload #'flyspell-mode "flyspell" nil t))
  (unless (fboundp 'flyspell-buffer)
    (autoload #'flyspell-buffer "flyspell" nil t))
  (unless (fboundp 'sb/flyspell-goto-previous-error)
    (autoload #'sb/flyspell-goto-previous-error "init-autoload" nil t))
  (unless (fboundp 'flyspell-overlay-p)
    (autoload #'flyspell-overlay-p "flyspell" nil t))
  (unless (fboundp 'flyspell-correct-previous)
    (autoload #'flyspell-correct-previous "flyspell" nil t))
  (unless (fboundp 'flyspell-correct-next)
    (autoload #'flyspell-correct-next "flyspell" nil t))

  ;; Enabling `flyspell-prog-mode' does not seem to be very useful and highlights links and
  ;; language-specific words
  (add-hook 'text-mode-hook #'flyspell-mode)

  ;; (add-hook 'before-save-hook #'flyspell-buffer) ; Saving files will be slow

  ;; `find-file-hook' will not work for buffers with no associated files
  (add-hook 'after-init-hook
            (lambda nil
              (when (string= (buffer-name) "*scratch*")
                (flyspell-mode 1))))

  (eval-and-compile
    ;; Move point to previous error
    ;; http://emacs.stackexchange.com/a/14912/2017
    ;; http://pragmaticemacs.com/emacs/jump-back-to-previous-typo/
    (defun sb/flyspell-goto-previous-error (arg)
      "Go to arg previous spelling error."
      (interactive "p")
      (defvar flyspell-old-pos-error)
      (defvar flyspell-old-buffer-error)

      (while (not (= 0 arg))
        (let ((pos (point))
              (min (point-min)))
          (if (and (eq (current-buffer) flyspell-old-buffer-error)
                   (eq pos flyspell-old-pos-error))
              (progn
                (if (= flyspell-old-pos-error min)
                    (progn
                      (message "Restarting from end of buffer")
                      (goto-char (point-max)))
                  (backward-word 1))
                (setq pos (point))))
          (while (and (> pos min)
                      (let ((ovs (overlays-at pos))
                            (r 'nil))
                        (while (and (not r)
                                    (consp ovs))
                          (if (flyspell-overlay-p
                               (car ovs))
                              (setq r t)
                            (setq ovs (cdr ovs))))
                        (not r)))
            (backward-word 1)
            (setq pos (point)))
          (setq arg (1- arg))
          (setq flyspell-old-pos-error pos)
          (setq flyspell-old-buffer-error
                (current-buffer))
          (goto-char pos)
          (if
              (= pos min)
              (progn
                (message "No more misspelled word!")
                (setq arg 0))
            (forward-word))))))

  (with-eval-after-load "flyspell"
    (defvar flyspell-abbrev-p)
    (defvar flyspell-issue-message-flag)
    (defvar flyspell-issue-welcome-flag)

    (setq flyspell-abbrev-p t
          flyspell-issue-message-flag nil
          flyspell-issue-welcome-flag nil)

    (diminish 'flyspell-mode)

    ;; Flyspell popup is more efficient. Ivy completion does not show the "Save" option in a few
    ;; cases.
    (unless (fboundp 'flyspell-popup-correct)
      (autoload #'flyspell-popup-correct "flyspell-popup" nil t))

    (defvar flyspell-popup-correct-delay)

    (setq flyspell-popup-correct-delay 0.2)

    (bind-keys :package flyspell-popup
               ("C-;" . flyspell-popup-correct)))

  (defvar flyspell-mode-map)
  (bind-keys :package flyspell
             ("C-c f f" . flyspell-mode)
             ("C-c f b" . flyspell-buffer)
             :map flyspell-mode-map
             ("C-;")
             ("C-,"     . sb/flyspell-goto-previous-error)))


;; The package does not add any extra functionality.
(when (eq sb/selection 'ivy)
  (progn
    (unless (fboundp 'flyspell-correct-wrapper)
      (autoload #'flyspell-correct-wrapper "flyspell-correct-ivy" nil t))

    (bind-keys :package flyspell-correct-ivy
               ("C-;" . flyspell-correct-wrapper))
    ))


;; As of Emacs 28, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.
(progn
  (declare-function spell-fu-goto-next-error "spell-fu")
  (declare-function spell-fu-goto-previous-error "spell-fu")
  (declare-function spell-fu-word-add "spell-fu")

  (unless (fboundp 'spell-fu-mode)
    (autoload #'spell-fu-mode "spell-fu" nil t))
  (unless (fboundp 'spell-fu-goto-next-error)
    (autoload #'spell-fu-goto-next-error "spell-fu" nil t))
  (unless (fboundp 'spell-fu-goto-previous-error)
    (autoload #'spell-fu-goto-previous-error "spell-fu" nil t))
  (unless (fboundp 'spell-fu-word-add)
    (autoload #'spell-fu-word-add "spell-fu" nil t))

  (defvar spell-fu-faces-exclude)
  (defvar spell-fu-directory)
  (defvar no-littering-var-directory)

  (if (bound-and-true-p sb/use-no-littering)
      (setq spell-fu-directory (expand-file-name "spell-fu" no-littering-var-directory))
    (setq spell-fu-directory (expand-file-name "spell-fu" sb/temp-directory)))

  (add-hook 'text-mode-hook
            (lambda ()
              ;; `nxml-mode' is derived from `text-mode'
              (setq spell-fu-faces-exclude '(hl-line
                                             nxml-attribute-local-name))
              (spell-fu-mode)))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(org-block
                                             org-block-begin-line
                                             org-block-end-line
                                             org-code
                                             org-date
                                             org-formula
                                             org-latex-and-related
                                             org-link
                                             org-meta-line
                                             org-property-value
                                             org-ref-cite-face
                                             org-special-keyword
                                             org-tag
                                             org-todo
                                             org-todo-keyword-done
                                             org-todo-keyword-habt
                                             org-todo-keyword-kill
                                             org-todo-keyword-outd
                                             org-todo-keyword-todo
                                             org-todo-keyword-wait
                                             org-verbatim
                                             hl-line))
              (spell-fu-mode)))


  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(markdown-blockquote-face
                                             markdown-code-face
                                             markdown-html-attr-name-face
                                             markdown-html-attr-value-face
                                             markdown-html-tag-name-face
                                             markdown-inline-code-face
                                             markdown-link-face
                                             markdown-markup-face
                                             markdown-plain-url-face
                                             markdown-reference-face
                                             markdown-url-face
                                             hl-line))
              (spell-fu-mode)))

  (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
    (add-hook hook (lambda ()
                     (setq spell-fu-faces-exclude '(font-latex-math-face
                                                    font-latex-sedate-face
                                                    hl-line))
                     (spell-fu-mode))))

  (bind-keys :package spell-fu
             ("C-c f n" . spell-fu-goto-next-error)
             ("C-c f p" . spell-fu-goto-previous-error)
             ("C-c f a" . spell-fu-word-add)))


(progn
  (unless (fboundp 'highlight-indentation-mode)
    (autoload #'highlight-indentation-mode "highlight-indentation" nil t))

  (dolist (hook '(yaml-mode-hook python-mode-hook))
    (add-hook hook #'highlight-indentation-mode))

  (with-eval-after-load "highlight-indentation"
    (diminish 'highlight-indentation-mode)
    (diminish 'highlight-indentation-current-column-mode)))


;; Claims to be better than `electric-indent-mode'
(progn
  (unless (fboundp 'aggressive-indent-mode)
    (autoload #'aggressive-indent-mode "aggressive-indent" nil t))

  (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
    (add-hook hook #'aggressive-indent-mode))

  (with-eval-after-load "aggressive-indent"
    (defvar aggressive-indent-comments-too)
    (defvar aggressive-indent-dont-electric-modes)

    (setq aggressive-indent-comments-too t
          ;; Never use `electric-indent-mode'
          aggressive-indent-dont-electric-modes t)

    (diminish 'aggressive-indent-mode)))


(progn
  (unless (fboundp 'show-paren-mode)
    (autoload #'show-paren-mode "paren" nil t))

  (run-at-time 3 nil #'show-paren-mode)

  (with-eval-after-load "paren"
    (defvar show-paren-style)
    (defvar show-paren-when-point-inside-paren)
    (defvar show-paren-when-point-in-periphery)

    (setq show-paren-style 'parenthesis ; `mixed' may lead to performance problems
          show-paren-when-point-inside-paren t
          show-paren-when-point-in-periphery t)))


(progn
  (unless (fboundp 'electric-pair-mode)
    (autoload #'electric-pair-mode "elec-pair" nil t))

  ;; Enable autopairing, `smartparens' seems slow
  (run-at-time 2 nil #'electric-pair-mode)

  (with-eval-after-load "elec-pair"
    ;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
    (defvar sb/markdown-pairs '((?` . ?`)) "Electric pairs for `markdown-mode'.")
    (defvar electric-pair-pairs)
    (defvar electric-pair-text-pairs)
    (defvar electric-pair-preserve-balance)

    (declare-function sb/add-markdown-pairs "init-autoload")

    (defun sb/add-markdown-pairs ()
      "Add custom pairs to `markdown-mode'."
      (setq-local electric-pair-pairs (append electric-pair-pairs sb/markdown-pairs))
      (setq-local electric-pair-text-pairs electric-pair-pairs))

    (add-hook 'markdown-mode-hook #'sb/add-markdown-pairs)

    (setq electric-pair-preserve-balance nil) ; Avoid balancing parentheses

    ;; Disable pairs when entering minibuffer
    (add-hook 'minibuffer-setup-hook (lambda ()
                                       (electric-pair-mode -1)))
    ;; Re-enable pairs when existing minibuffer
    (add-hook 'minibuffer-exit-hook (lambda ()
                                      (electric-pair-mode 1)))))


;; https://web.archive.org/web/20201109035847/http://ebzzry.io/en/emacs-pairs/

;; Seems to have performance issue with `latex-mode', `markdown-mode', and large JSON files.
;; `sp-cheat-sheet' will show you all the commands available, with examples.

;; (unless (fboundp 'sp-beginning-of-sexp)
;;   (autoload #'sp-beginning-of-sexp "smartparens-config" nil t))
;; (unless (fboundp 'sp-end-of-sexp)
;;   (autoload #'sp-end-of-sexp "smartparens-config" nil t))
;; (unless (fboundp 'sp-up-sexp)
;;   (autoload #'sp-up-sexp "smartparens-config" nil t))
;; (unless (fboundp 'sp-down-sexp)
;;   (autoload #'sp-down-sexp "smartparens-config" nil t))
;; (unless (fboundp 'sp-forward-sexp)
;;   (autoload #'sp-forward-sexp "smartparens-config" nil t))
;; (unless (fboundp 'sp-backward-sexp)
;;   (autoload #'sp-backward-sexp "smartparens-config" nil t))
;; (unless (fboundp 'sp-next-sexp)
;;   (autoload #'sp-next-sexp "smartparens-config" nil t))
;; (unless (fboundp 'sp-previous-sexp)
;;   (autoload #'sp-previous-sexp "smartparens-config" nil t))
;; (unless (fboundp 'sp-backward-symbol)
;;   (autoload #'sp-backward-symbol "smartparens-config" nil t))
;; (unless (fboundp 'sp-forward-symbol)
;;   (autoload #'sp-forward-symbol "smartparens-config" nil t))
;; (unless (fboundp 'sp-splice-sexp)
;;   (autoload #'sp-splice-sexp "smartparens-config" nil t))

;; (add-hook 'latex-mode-hook #'(lambda nil
;;                                (require 'smartparens-latex)))
;; (add-hook 'LaTeX-mode-hook #'(lambda nil
;;                                (require 'smartparens-latex)))
;; (add-hook 'after-init-hook #'(lambda nil
;;                                (smartparens-global-mode 1)
;;                                (show-smartparens-global-mode 1)))

;; (eval-after-load 'smartparens-config
;;   '(progn
;;      (setq sp-show-pair-from-inside t
;;            sp-autoskip-closing-pair 'always)

;;      ;; Stop pairing single quotes in elisp
;;      (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
;;      (sp-local-pair 'markdown-mode "<" ">")

;;   ;; Do not insert a parenthesis pair when the point is at the beginning of a word
;;   ;; (sp-pair "(" nil :unless '(sp-point-before-word-p))
;;   ;; (sp-pair "[" nil :unless '(sp-point-before-word-p))
;;   ;; (sp-pair "{" nil :unless '(sp-point-before-word-p))
;;   ;; (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p))

;;      t))

;; (bind-keys :package smartparens-config
;;            ("C-M-a" . sp-beginning-of-sexp) ; "foo ba_r" -> "_foo bar"
;;            ("C-M-e" . sp-end-of-sexp) ; "f_oo bar" -> "foo bar_"
;;            ("C-M-u" . sp-up-sexp) ; "f_oo bar" -> "foo bar"_
;;            ("C-M-w" . sp-down-sexp) ; "foo ba_r" -> "_foo bar"
;;            ("C-M-f" . sp-forward-sexp) ; "foo ba_r" -> "foo bar"_
;;            ("C-M-b" . sp-backward-sexp) ; "foo ba_r" -> "_foo bar"
;;            ("C-M-n" . sp-next-sexp) ; ))" -> ((foo) (bar))"
;;            ("C-M-p" . sp-previous-sexp) ; "(foo (b|ar baz))" -> "(foo| (bar baz))"
;;            ("C-S-b" . sp-backward-symbol) ; "foo bar| baz" -> "foo |bar baz"
;;            ("C-S-f" . sp-forward-symbol) ; "|foo bar baz" -> "foo| bar baz"
;;            ;; "(foo bar)" -> "foo bar"
;;            ("C-M-k" . sp-splice-sexp))


(declare-function projectile-project-root "projectile")
(declare-function projectile-expand-root "projectile")
(declare-function projectile-project-name "projectile")
(declare-function projectile-project-p "projectile")

(unless (fboundp 'projectile-find-file)
  (autoload #'projectile-find-file "projectile" nil t))
(unless (fboundp 'projectile-switch-project)
  (autoload #'projectile-switch-project "projectile" nil t))
(unless (fboundp 'projectile-add-known-project)
  (autoload #'projectile-add-known-project "projectile" nil t))
(unless (fboundp 'projectile-project-p)
  (autoload #'projectile-project-p "projectile" nil t))
(unless (fboundp 'projectile-project-name)
  (autoload #'projectile-project-name "projectile" nil t))
(unless (fboundp 'projectile-expand-root)
  (autoload #'projectile-expand-root "projectile" nil t))
(unless (fboundp 'projectile-project-root)
  (autoload #'projectile-project-root "projectile" nil t))
(unless (fboundp 'projectile-mode)
  (autoload #'projectile-mode "projectile" nil t))

(with-eval-after-load "projectile"
  (defvar projectile-auto-discover)
  (defvar projectile-enable-caching)
  (defvar projectile-file-exists-remote-cache-expire)
  (defvar projectile-require-project-root)
  (defvar projectile-sort-order)
  (defvar projectile-verbose)
  (defvar projectile-cache-file)
  (defvar projectile-known-projects-file)
  (defvar projectile-dynamic-mode-line)
  (defvar projectile-indexing-method)
  (defvar projectile-project-root-files)
  (defvar projectile-globally-ignored-directories)
  (defvar projectile-globally-ignored-file-suffixes)
  (defvar projectile-ignored-projects)
  (defvar projectile-globally-ignored-files)
  (defvar projectile-mode-line-prefix)

  (setq projectile-auto-discover nil ; Do not discover projects
        projectile-enable-caching t ; Caching will not watch for new files automatically
        projectile-file-exists-remote-cache-expire nil
        projectile-indexing-method 'alien
        projectile-mode-line-prefix ""
        ;; Use only in desired directories, too much noise otherwise
        projectile-require-project-root t
        projectile-sort-order 'recently-active ; No sorting should be faster
        projectile-verbose nil
        ;; This can help reduce the overhead with large projects, but then recent files information
        ;; may be incomplete
        ;; projectile-switch-project-action 'projectile-recentf
        )

  (unless (bound-and-true-p sb/use-no-littering)
    (setq  projectile-cache-file (expand-file-name "projectile.cache" sb/temp-directory)
           projectile-known-projects-file (expand-file-name "projectile-known-projects.eld"
                                                            sb/temp-directory)))

  ;; https://github.com/MatthewZMD/.emacs.d
  (when (and sb/IS-WINDOWS (executable-find "tr"))
    (setq projectile-indexing-method 'alien))

  ;; Disable computing the project type that is shown on the modeline
  (defun projectile-default-mode-line nil
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      (format " %s [%s] " projectile-mode-line-prefix
              (or project-name "-"))))

  (setq projectile-project-root-files '("build.gradle"
                                        "setup.py"
                                        "requirements.txt"
                                        "package.json"
                                        "composer.json"
                                        "CMakeLists.txt"
                                        "Makefile"
                                        "WORKSPACE"
                                        "meson.build"
                                        "SConstruct"
                                        "configure.ac"
                                        "configure.in"))

  ;; Avoid search when `projectile-mode' is enabled for faster startup
  ;; (setq projectile-project-search-path (list
  ;;                                       (concat `,(getenv "HOME") "/bitbucket")
  ;;                                       (expand-file-name "github" sb/user-home)
  ;;                                       (expand-file-name "iitk-workspace" sb/user-home)
  ;;                                       (expand-file-name "iitkgp-workspace" sb/user-home)
  ;;                                       (expand-file-name "iss-workspace" sb/user-home)
  ;;                                       (expand-file-name "plass-workspace" sb/user-home)
  ;;                                       (expand-file-name "prospar-workspace" sb/user-home)
  ;;                                       (expand-file-name "research" sb/user-home)
  ;;                                       ))

  (dolist (prjs (list (expand-file-name sb/user-home) ; Do not consider $HOME as a project
                      "~/" ; Do not consider $HOME as a project
                      (expand-file-name "/tmp")
                      (expand-file-name "bitbucket/.metadata" sb/user-home)
                      (expand-file-name "github/.metadata" sb/user-home)
                      (expand-file-name "iitk-workspace/.metadata" sb/user-home)
                      (expand-file-name "plass-workspace/.metadata" sb/user-home)))
    (add-to-list 'projectile-ignored-projects prjs))

  ;; Filtering works with `alien' indexing
  (dolist (dirs '(".cache" ".clangd" ".dropbox" ".git" ".hg" ".metadata" ".nx" ".recommenders"
                  ".svn" ".vscode" "__pycache__" "auto" "elpa" "node_modules"))
    (add-to-list 'projectile-globally-ignored-directories dirs))

  (dolist (items '("GPATH" "GRTAGS" "GTAGS" "GSYMS" "TAGS" "tags" ".tags" "__init__.py"))
    (add-to-list 'projectile-globally-ignored-files items))

  (dolist (exts '(".a" ".aux" ".bak" ".blg" ".class" ".deb" ".doc" ".docx" ".elc" ".o" ".odt"
                  ".ppt" ".pptx" ".pt" ".pyc" ".rel" ".rip" ".rpm" ".so" ".xls" ".xlsx" "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes exts))

  (projectile-mode 1)

  (defvar projectile-mode-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(when nil
  (eval-and-compile
    (defun sb/projectile-kill ()
      (interactive)
      (projectile-kill-buffers)
      (treemacs))))

;; Set these in case `counsel-projectile' is disabled
(defvar projectile-command-map)
(bind-keys :package projectile
           ("<f6>" . projectile-find-file)
           ("<f5>" . projectile-switch-project)
           :map projectile-command-map
           ("A"    . projectile-add-known-project)
           ;; ("k"    . sb/projectile-kill)
           )

;; Use idle timer in case we open a project file without enabling projectile via bind-keys
(run-with-idle-timer 3 nil #'projectile-mode)

;; https://github.com/Alexander-Miller/treemacs/issues/660
;; (add-hook 'projectile-after-switch-project-hook
;;           (lambda ()
;;             (interactive)
;;             (treemacs-add-and-display-current-project)
;;             (treemacs-display-current-project-exclusively)
;;             (other-window 1)))

;; (add-hook 'projectile-after-switch-project-hook
;;           (lambda ()
;;             (treemacs-display-current-project-exclusively)
;;             (other-window 1)))


(when nil
  (declare-function counsel-projectile-switch-project-by-name "counsel-projectile")

  (unless (fboundp 'counsel-projectile-find-file)
    (autoload #'counsel-projectile-find-file "counsel-projectile" nil t))
  (unless (fboundp 'counsel-projectile-switch-project)
    (autoload #'counsel-projectile-switch-project "counsel-projectile" nil t))
  (unless (fboundp 'counsel-projectile-switch-project-by-name)
    (autoload #'counsel-projectile-switch-project-by-name "counsel-projectile" nil t))
  (unless (fboundp 'counsel-projectile-mode)
    (autoload #'counsel-projectile-mode "counsel-projectile" nil t))

  (eval-and-compile
    (defun sb/counsel-projectile-switch-project-magit (project)
      "Open Magit for the PROJECT."
      (let
          ((projectile-switch-project-action 'magit-status))
        (counsel-projectile-switch-project-by-name project)))

    (defun sb/counsel-projectile-open-default-file nil
      "Open the current project's default file.
This file is specified in `counsel-projectile-default-file'."
      (interactive)
      (defvar counsel-projectile-default-file)
      (let
          ((file counsel-projectile-default-file))
        (if
            (and file
                 (setq file
                       (projectile-expand-root file))
                 (file-exists-p file))
            (find-file file)
          (message "File %s doesn't exist." file))))

    (defun sb/counsel-projectile-switch-project-action-default-file (project)
      "Open PROJECT's default file.
This file is specified in `counsel-projectile-default-file'."
      (let
          ((projectile-switch-project-action #'sb/counsel-projectile-open-default-file))
        (counsel-projectile-switch-project-by-name project)))

    ;; Set `counsel-projectile-switch-project-action' to the following action
    ;;   (defun sb/counsel-projectile-switch-project-action-default-file (project)
    ;;     "Open PROJECT's default file.
    ;; This file is specified in `counsel-projectile-default-file'."
    ;;     (let ((projectile-switch-project-action #'sb/counsel-projectile-open-default-file))
    ;;       (counsel-projectile-switch-project-by-name project)))

    )

  (with-eval-after-load "counsel-projectile"
    (defvar counsel-projectile-remove-current-buffer)
    (defvar counsel-projectile-sort-directories)
    (defvar counsel-projectile-sort-files)
    (defvar counsel-projectile-find-file-more-chars)
    (defvar counsel-projectile-sort-buffers)
    (defvar counsel-projectile-sort-projects)

    ;; Setting these to `t' can be slow for large projects
    (setq counsel-projectile-remove-current-buffer t
          counsel-projectile-sort-directories nil
          counsel-projectile-find-file-more-chars 0
          counsel-projectile-sort-buffers nil
          counsel-projectile-sort-projects nil
          counsel-projectile-sort-files nil)

    (counsel-projectile-mode 1)

    ;; (counsel-projectile-modify-action
    ;;  'counsel-projectile-switch-project-action
    ;;  '((default sb/counsel-projectile-switch-project-action-default-file)))

    )

  ;; The `counsel' actions seem to be slower than base `projectile'
  (bind-keys :package counsel-projectile
             ("<f5>" . counsel-projectile-switch-project)
             ("<f6>" . counsel-projectile-find-file)
             ;; ("<f7>" . counsel-projectile-rg)
             ;; ([remap projectile-switch-project]   . counsel-projectile-switch-project)
             ;; ([remap projectile-find-file]        . counsel-projectile-find-file)
             ;; ([remap projectile-find-dir]         . counsel-projectile-find-dir)
             ;; ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
             )
  )


(when (eq sb/selection 'ivy)
  ;; Enable before `ivy-rich-mode' for better performance
  ;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el
  ;; The new transformers (file permissions) seem more of an overkill and buggy, and it hides the
  ;; file names
  (when nil
    (progn
      (when (display-graphic-p)
        (unless (fboundp 'all-the-icons-ivy-rich-mode)
          (autoload #'all-the-icons-ivy-rich-mode "all-the-icons-ivy-rich" nil t))

        (add-hook 'ivy-mode-hook #'all-the-icons-ivy-rich-mode)

        (with-eval-after-load "all-the-icons-ivy-rich"
          (defvar all-the-icons-ivy-rich-icon-size)

          (setq all-the-icons-ivy-rich-icon-size 1.0)))))


  (when t
    (progn
      (declare-function ivy-rich-modify-column "ivy-rich")

      (unless (fboundp 'ivy-rich-mode)
        (autoload #'ivy-rich-mode "ivy-rich" nil t))
      (unless (fboundp 'ivy-rich-modify-column)
        (autoload #'ivy-rich-modify-column "ivy-rich" nil t))

      (add-hook 'ivy-mode-hook #'ivy-rich-mode)

      (with-eval-after-load "ivy-rich"
        (defvar ivy-rich-parse-remote-buffer)
        (defvar ivy-format-functions-alist)

        (setq ivy-rich-parse-remote-buffer nil)
        (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

        ;; Increase the width to see the major mode clearly
        ;; FIXME: `ivy-rich-modify-column' is not taking effect.
        (ivy-rich-modify-column 'ivy-switch-buffer 'ivy-rich-switch-buffer-major-mode
                                '(:width 24 :face warning)))
      ))
  )


(when (and (eq sb/selection 'ivy)
           (executable-find "fd"))
  (with-eval-after-load "counsel"
    (progn
      (unless (fboundp 'counsel-fd-file-jump)
        (autoload #'counsel-fd-file-jump "counsel-fd" nil t))
      (unless (fboundp 'counsel-fd-dired-jump)
        (autoload #'counsel-fd-dired-jump "counsel-fd" nil t))

      (bind-keys :package counsel-fd
                 ;; Jump to a directory below the current directory
                 ("C-x d" . counsel-fd-dired-jump)
                 ("C-x f" . counsel-fd-file-jump))
      ))
  )


(progn
  (declare-function flycheck-buffer "flycheck")
  (declare-function flycheck-add-next-checker "flycheck")
  (declare-function flycheck-describe-checker "flycheck")
  (declare-function flycheck-select-checker "flycheck")
  (declare-function flycheck-verify-setup "flycheck")
  (declare-function flycheck-next-error "flycheck")
  (declare-function flycheck-previous-error "flycheck")
  (declare-function flycheck-next-checker "flycheck")
  (declare-function flycheck-list-errors "flycheck")
  (declare-function flycheck-disable-checker "flycheck")
  (declare-function flycheck-add-mode "flycheck")
  (declare-function flycheck-display-error-messages-unless-error-list "flycheck")

  (unless (fboundp 'global-flycheck-mode)
    (autoload #'global-flycheck-mode "flycheck" nil t))
  (unless (fboundp 'flycheck-mode)
    (autoload #'flycheck-mode "flycheck" nil t))
  (unless (fboundp 'flycheck-add-next-checker)
    (autoload #'flycheck-add-next-checker "flycheck" nil t))
  (unless (fboundp 'flycheck-next-checker)
    (autoload #'flycheck-next-checker "flycheck" nil t))
  (unless (fboundp 'flycheck-previous-error)
    (autoload #'flycheck-previous-error "flycheck" nil t))
  (unless (fboundp 'flycheck-describe-checker)
    (autoload #'flycheck-describe-checker "flycheck" nil t))
  (unless (fboundp 'flycheck-buffer)
    (autoload #'flycheck-buffer "flycheck" nil t))
  (unless (fboundp 'flycheck-list-errors)
    (autoload #'flycheck-list-errors "flycheck" nil t))
  (unless (fboundp 'flycheck-select-checker)
    (autoload #'flycheck-select-checker "flycheck" nil t))
  (unless (fboundp 'flycheck-verify-setup)
    (autoload #'flycheck-verify-setup "flycheck" nil t))
  (unless (fboundp 'flycheck-next-error)
    (autoload #'flycheck-next-error "flycheck" nil t))
  (unless (fboundp 'flycheck-disable-checker)
    (autoload #'flycheck-disable-checker "flycheck" nil t))
  (unless (fboundp 'flycheck-add-mode)
    (autoload #'flycheck-add-mode "flycheck" nil t))
  (unless (fboundp 'flycheck-manual)
    (autoload #'flycheck-manual "flycheck" nil t))

  (run-at-time 2 nil #'global-flycheck-mode)

  (with-eval-after-load "flycheck"
    (defvar flycheck-check-syntax-automatically)
    (defvar flycheck-checker-error-threshold)
    (defvar flycheck-idle-buffer-switch-delay)
    (defvar flycheck-idle-change-delay)
    (defvar flycheck-emacs-lisp-load-path)
    (defvar flycheck-mode-line)
    (defvar flycheck-textlint-plugin-alist)
    (defvar flycheck-hooks-alist)
    (defvar flycheck-display-errors-function)
    (defvar flycheck-global-modes)

    ;; Remove newline checks, since they would trigger an immediate check when we want the
    ;; `flycheck-idle-change-delay' to be in effect while editing.
    (setq flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change)
          flycheck-checker-error-threshold 500
          flycheck-idle-buffer-switch-delay 5 ; Increase the time to allow for quick transitions
          flycheck-idle-change-delay 5 ; Increase the time (s) to allow for edits
          flycheck-emacs-lisp-load-path 'inherit
          ;; Show error messages only if the error list is not already visible
          flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
          ;; `chktex' errors are often not very helpful, and `csv-mode' does not have a checker yet.
          flycheck-global-modes '(not csv-mode))

    ;; TODO: Is this the reason why `flycheck' and `doom-modeline' does not work well?
    (when (or (eq sb/modeline-theme 'spaceline)
              (eq sb/modeline-theme 'doom-modeline))
      (setq flycheck-mode-line nil))

    (setq-default flycheck-markdown-markdownlint-cli-config (expand-file-name ".markdownlint.json"
                                                                              sb/user-home)
                  flycheck-pylintrc (expand-file-name ".config/pylintrc" sb/user-home)
                  flycheck-python-pylint-executable "python3"
                  ;; Shellcheck is a resource hog for `$HOME/.bash*' files
                  flycheck-shellcheck-follow-sources nil
                  flycheck-textlint-config (expand-file-name "textlintrc.json" sb/textlint-home)
                  flycheck-textlint-executable (expand-file-name "node_modules/.bin/textlint"
                                                                 sb/textlint-home))

    (add-to-list 'flycheck-textlint-plugin-alist '(tex-mode . "latex"))
    (add-to-list 'flycheck-textlint-plugin-alist '(rst-mode . "rst"))

    ;; https://github.com/flycheck/flycheck/issues/1833
    (add-to-list 'flycheck-hooks-alist '(after-revert-hook . flycheck-buffer))

    ;; Exclude directories and files from being checked
    ;; https://github.com/flycheck/flycheck/issues/1745

    (declare-function sb/flycheck-may-check-automatically "init-autoload.el")

    (defvar sb/excluded-directory-regexps
      '(".git/" "elpa/"))

    (defun sb/flycheck-may-check-automatically (&rest _conditions)
      (or (null buffer-file-name)
          (let ((bufname (file-truename buffer-file-name)))
            (not (seq-some (lambda (re) (string-match-p re bufname))
                           sb/excluded-directory-regexps)))))

    (advice-add 'flycheck-may-check-automatically
                :after-while #'sb/flycheck-may-check-automatically)

    ;; The advantage with `flycheck-grammarly' over `lsp-grammarly' is that you need not set up lsp
    ;; support, so you can use it anywhere.
    (require 'flycheck-grammarly)

    (defvar flycheck-grammarly-check-time)
    (defvar flycheck-checkers)

    (setq flycheck-grammarly-check-time 3
          ;; LATER: Can we combine the delete operations?
          flycheck-checkers (delete 'proselint flycheck-checkers)
          ;; Remove from the beginning of the list `flycheck-checkers' and append to the end
          flycheck-checkers (delete 'grammarly flycheck-checkers))

    (add-to-list 'flycheck-checkers 'grammarly t)

    (when nil
      (progn
        (require 'flycheck-languagetool)

        (defvar flycheck-languagetool-commandline-jar)

        (setq flycheck-languagetool-commandline-jar (no-littering-expand-etc-file-name
                                                     "languagetool-5.3-commandline.jar"))))

    ;; We prefer to use `textlint' and `grammarly', `proselint' is not maintained. Add `textlint',
    ;; then `grammarly'.
    (add-hook 'text-mode-hook
              (lambda ()
                (flycheck-add-next-checker 'textlint 'grammarly)
                ;; (flycheck-add-next-checker 'grammarly 'languagetool)
                ))

    ;; `markdown-mode' is derived from `text-mode'
    (add-hook 'markdown-mode-hook
              (lambda()
                ;; (make-local-variable 'flycheck-error-list-minimum-level)
                ;; (setq flycheck-error-list-minimum-level 'warning
                ;;       flycheck-navigation-minimum-level 'warning)
                (flycheck-add-next-checker 'markdown-markdownlint-cli 'textlint)))

    (dolist (hook '(LaTex-mode-hook latex-mode-hook))
      (add-hook hook (lambda ()
                       (flycheck-add-next-checker 'tex-chktex 'textlint))))))


;; Does not display popup under TTY, check possible workarounds at
;; https://github.com/flycheck/flycheck-popup-tip
(when nil
  (progn
    (when (display-graphic-p)
      (unless (fboundp 'flycheck-pos-tip-mode)
        (autoload #'flycheck-pos-tip-mode "flycheck-pos-tip" nil t))

      (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode))
    ))


;; Showing errors/warnings in a posframe seems more intrusive than showing errors in the minibuffer
(when t
  (progn
    (when (display-graphic-p)
      (unless (fboundp 'flycheck-posframe-mode)
        (autoload #'flycheck-posframe-mode "flycheck-posframe" nil t))
      (unless (fboundp 'flycheck-posframe-configure-pretty-defaults)
        (autoload #'flycheck-posframe-configure-pretty-defaults "flycheck-posframe" nil t))

      (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)

      (with-eval-after-load "flycheck-posframe"
        (defvar flycheck-posframe-position)
        (defvar flycheck-posframe-border-width)

        (setq flycheck-posframe-position 'point-bottom-left-corner
              flycheck-posframe-border-width 1)

        (flycheck-posframe-configure-pretty-defaults)))
    ))


(when nil
  (progn
    (declare-function whitespace-buffer "whitespace")
    (declare-function whitespace-turn-off "whitespace")

    (unless (fboundp 'whitespace-mode)
      (autoload #'whitespace-mode "whitespace" nil t))
    (unless (fboundp 'global-whitespace-mode)
      (autoload #'global-whitespace-mode "whitespace" nil t))
    (unless (fboundp 'whitespace-buffer)
      (autoload #'whitespace-buffer "whitespace" nil t))
    (unless (fboundp 'whitespace-cleanup)
      (autoload #'whitespace-cleanup "whitespace" nil t))
    (unless (fboundp 'whitespace-turn-off)
      (autoload #'whitespace-turn-off "whitespace" nil t))

    (add-hook 'markdown-mode-hook #'whitespace-mode)

    (with-eval-after-load "whitespace"
      (defvar whitespace-line-column)
      (defvar whitespace-style)

      (setq show-trailing-whitespace t
            whitespace-line-column sb/fill-column
            whitespace-style '(face lines-tail trailing))

      (diminish 'global-whitespace-mode)
      (diminish 'whitespace-mode)
      (diminish 'whitespace-newline-mode))
    ))


;; This is different from `whitespace-cleanup-mode' since this is unconditional
(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (setq delete-trailing-lines t) ; `M-x delete-trailing-whitespace' deletes trailing lines
  (add-hook 'before-save-hook #'delete-trailing-whitespace))


;; Call `whitespace-cleanup' only if the initial buffer was clean
(when nil
  (progn
    (unless (fboundp 'global-whitespace-cleanup-mode)
      (autoload #'global-whitespace-cleanup-mode "whitespace-cleanup-mode" nil t))
    (unless (fboundp 'whitespace-cleanup-mode)
      (autoload #'whitespace-cleanup-mode "whitespace-cleanup-mode" nil t))

    ;; To enable it for an entire project, set `whitespace-cleanup-mode' to `t' in your
    ;; `.dir-locals.el' file.
    ;; (add-hook 'after-init-hook #'global-whitespace-cleanup-mode)

    (with-eval-after-load "whitespace-cleanup-mode"
      (defvar whitespace-cleanup-mode-preserve-point)
      (defvar whitespace-cleanup-mode-ignore-modes)

      (setq whitespace-cleanup-mode-preserve-point t)
      (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)

      (diminish 'whitespace-cleanup-mode))
    ))


;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(progn
  (unless (fboundp 'ws-butler-mode)
    (autoload #'ws-butler-mode "ws-butler" nil t))

  (add-hook 'prog-mode-hook #'ws-butler-mode)

  (with-eval-after-load "ws-butler"
    (diminish 'ws-butler-mode)))


;; Highlight symbol under point
(progn
  (unless (fboundp 'symbol-overlay-mode)
    (autoload #'symbol-overlay-mode "symbol-overlay" nil t))
  (unless (fboundp 'symbol-overlay-jump-prev)
    (autoload #'symbol-overlay-jump-prev "symbol-overlay" nil t))
  (unless (fboundp 'symbol-overlay-jump-next)
    (autoload #'symbol-overlay-jump-next "symbol-overlay" nil t))

  (dolist (hook '(prog-mode-hook html-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook #'symbol-overlay-mode))

  (with-eval-after-load "symbol-overlay"
    (diminish 'symbol-overlay-mode))

  (bind-keys :package symbol-overlay
             ("M-p" . symbol-overlay-jump-prev)
             ("M-n" . symbol-overlay-jump-next)))


(progn
  (unless (fboundp 'global-hl-todo-mode)
    (autoload #'global-hl-todo-mode "hl-todo" nil t))

  (run-at-time 5 nil #'global-hl-todo-mode)

  (with-eval-after-load "hl-todo"
    (defvar hl-todo-highlight-punctuation)
    (defvar hl-todo-keyword-faces)

    (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces (append '(("LATER"    . "#d0bf8f")
                                          ("ISSUE"    . "#ff8c00")
                                          ("DEBUG"    . "#ff8c00")
                                          ("TEST"     . "tomato")
                                          ("WARNING"  . "#cc0000")
                                          ("BEWARE"   . "#aa0000")
                                          ("REFACTOR" . "#cc9393"))
                                        hl-todo-keyword-faces))))


(progn
  (unless (fboundp 'highlight-numbers-mode)
    (autoload #'highlight-numbers-mode "highlight-numbers" nil t))

  (dolist (hook '(prog-mode-hook conf-mode-hook css-mode-hook html-mode-hook))
    (add-hook hook #'highlight-numbers-mode)))


(when nil
  (progn
    (declare-function number-separator-mode "number-separator")

    (unless (fboundp 'number-separator-mode)
      (autoload #'number-separator-mode "number-separator" nil t))

    (with-eval-after-load "number-separator"
      (defvar number-separator)
      (defvar number-separator-interval)
      (defvar number-separator-ignore-threshold)
      (defvar number-separator-decimal-char)

      (setq number-separator ","
            number-separator-interval 3
            number-separator-ignore-threshold 4
            number-separator-decimal-char ".")

      (diminish 'number-sepator-mode))
    ))


(progn
  (unless (fboundp 'hes-mode)
    (autoload #'hes-mode "highlight-escape-sequences" nil t))

  (add-hook 'prog-mode-hook #'hes-mode))


(when nil
  (progn
    ;; First mark the word, then add more cursors. Use `mc/edit-lines' to add a cursor to each
    ;; line in an active region that spans multiple lines.

    (unless (fboundp 'mc/mark-previous-like-this)
      (autoload #'mc/mark-previous-like-this "multiple-cursors" nil t))
    (unless (fboundp 'mc/mark-next-like-this)
      (autoload #'mc/mark-next-like-this "multiple-cursors" nil t))
    (unless (fboundp 'mc/mark-all-like-this)
      (autoload #'mc/mark-all-like-this "multiple-cursors" nil t))

    (bind-keys :package multiple-cursor
               ("C-<"     . mc/mark-previous-like-this)
               ("C->"     . mc/mark-next-like-this)
               ("C-c C-<" . mc/mark-all-like-this))
    ))


(progn
  (unless (fboundp 'global-page-break-lines-mode)
    (autoload #'global-page-break-lines-mode "page-break-lines" nil t))

  (run-at-time 5 nil #'global-page-break-lines-mode)

  (with-eval-after-load "page-break-lines"
    (diminish 'page-break-lines-mode)))


;; Edit remote file: `/method:user@host#port:filename'. Shortcut /ssh:: will connect to default
;; user@host#port.
;; Edit local file with sudo: `C-x C-f /sudo::/etc/hosts'.
;; Open a remote file with ssh + sudo: `C-x C-f /ssh:host|sudo:root:/etc/passwd'.
;; Use bookmarks to speed up remote file access: upon visiting a location with TRAMP, save it as a
;; bookmark with `bookmark-set'. To revisit that bookmark, use `bookmark-jump'.

(defvar tramp-auto-save-directory)
(defvar tramp-persistency-file-name)
(defvar tramp-completion-reread-directory-timeout)
(defvar tramp-default-method)
(defvar tramp-default-user)
(defvar tramp-default-remote-shell)
(defvar tramp-verbose)
(defvar tramp-remote-path)

;; Auto-save to a local directory for better performance
(unless (bound-and-true-p sb/use-no-littering)
  (setq tramp-auto-save-directory (expand-file-name "tramp-auto-save" sb/temp-directory)
        tramp-persistency-file-name (expand-file-name "tramp" sb/temp-directory)))

(setq tramp-completion-reread-directory-timeout nil
      ;; tramp-default-method "ssh" ; SSH is faster than the default SCP
      ;; tramp-default-remote-shell "/bin/bash"
      tramp-default-user "swarnendu"
      remote-file-name-inhibit-cache nil ; Remote files are not updated outside of Tramp
      tramp-verbose 1
      ;; Disable version control
      vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                   vc-ignore-dir-regexp tramp-file-name-regexp))

(defalias 'exit-tramp 'tramp-cleanup-all-buffers)
(setenv "SHELL" "/bin/bash")
;; Disable backup
(add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

(with-eval-after-load "tramp"
  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; https://www.gnu.org/software/tramp/
(setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))

(declare-function tramp-cleanup-connection "tramp")
(bind-key "C-S-q" #'tramp-cleanup-connection)

(declare-function sb/sshlist "private")


(when (eq sb/selection 'ivy)
  (progn
    (declare-function sb/ivy-tramp "init-autoload")

    (defun sb/ivy-tramp ()
      "Invoke remote hosts with ivy and tramp."
      (interactive)
      (counsel-find-file (ivy-read "Remote Tramp targets: " (sb/sshlist))))

    (bind-key "C-c d t" #'sb/ivy-tramp)))

(when (eq sb/selection 'selectrum)
  (progn
    (declare-function sb/selectrum-tramp "init-autoload")

    (defun sb/selectrum-tramp ()
      "Invoke remote hosts with selectrum and tramp."
      (interactive)
      (find-file (completing-read "Remote Tramp targets: " (sb/sshlist))))

    (bind-key "C-c d t" #'sb/selectrum-tramp)))


(with-eval-after-load "imenu"
  (defvar imenu-auto-rescan)
  (defvar imenu-max-items)
  (defvar imenu-max-item-length)
  (defvar imenu-use-popup-menu)

  (setq imenu-auto-rescan t
        imenu-max-items 500
        imenu-max-item-length 100
        ;; `t' will use a popup menu rather than a minibuffer prompt, `on-mouse' might be useful
        ;; with mouse support enabled
        imenu-use-popup-menu nil
        ;; `nil' implies no sorting or listing by position in the buffer
        imenu-sort-function nil))


(defvar tags-revert-without-query)

(setq large-file-warning-threshold (* 500 1024 1024) ; MB
      tags-add-tables nil
      tags-case-fold-search nil ; t=case-insensitive, nil=case-sensitive
      ;; Do not ask before rereading the `TAGS' files if they have changed
      tags-revert-without-query t)


(declare-function xref-etags-mode "xref")
(declare-function xref-show-location-at-point "xref")
(declare-function xref-quit-and-goto-xref "xref")
(declare-function xref-query-replace-in-results "xref")

(unless (fboundp 'xref-find-definitions)
  (autoload #'xref-find-definitions "xref" nil t))
(unless (fboundp 'xref-find-references)
  (autoload #'xref-find-references "xref" nil t))
(unless (fboundp 'xref-find-apropos)
  (autoload #'xref-find-apropos "xref" nil t))
(unless (fboundp 'xref-pop-marker-stack)
  (autoload #'xref-pop-marker-stack "xref" nil t))
(unless (fboundp 'xref-show-location-at-point)
  (autoload #'xref-show-location-at-point "xref" nil t))
(unless (fboundp 'xref-quit-and-goto-xref)
  (autoload #'xref-quit-and-goto-xref "xref" nil t))
(unless (fboundp 'xref-query-replace-in-results)
  (autoload #'xref-query-replace-in-results "xref" nil t))
(unless (fboundp 'xref-etags-mode)
  (autoload #'xref-etags-mode "xref" nil t))

(with-eval-after-load "xref"
  ;; (xref-etags-mode)

  (when (eq sb/selection 'ivy)
    (with-eval-after-load "ivy"
      (require 'ivy-xref)

      (defvar xref-show-definitions-function)
      (defvar xref-show-xrefs-function)

      (declare-function ivy-xref-show-defs "ivy-xref")
      (declare-function ivy-xref-show-xrefs "ivy-xref")

      (setq xref-show-definitions-function #'ivy-xref-show-defs
            xref-show-xrefs-function #'ivy-xref-show-xrefs)
      ))

  (progn
    (unless (fboundp 'dump-jump-xref-activate)
      (autoload #'dumb-jump-xref-activate "dumb-jump" nil t))

    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

    (defvar dumb-jump-force-searcher)
    (defvar dumb-jump-prefer-searcher)
    (defvar dumb-jump-quiet)

    (setq dumb-jump-force-searcher 'rg
          dumb-jump-prefer-searcher 'rg
          dumb-jump-quiet t))
  )


(defvar xref--xref-buffer-mode-map)
(bind-keys :package xref
           ("M-'"   . xref-find-definitions)
           ("M-?"   . xref-find-references)
           ("C-M-." . xref-find-apropos)
           ("M-,"   . xref-pop-marker-stack)
           :map xref--xref-buffer-mode-map
           ("C-o"   . xref-show-location-at-point)
           ("<tab>" . xref-quit-and-goto-xref)
           ("r"     . xref-query-replace-in-results))


;; Gtags is less maintained than `universal-ctags'
(when (and (eq system-type 'gnu/linux)
           (eq sb/tags-scheme 'gtags))
  (declare-function counsel-gtags-dwim "counsel-gtags")
  (declare-function counsel-gtags-mode "counsel-gtags")
  (declare-function counsel-gtags-go-backward "counsel-gtags")
  (declare-function counsel-gtags-find-reference "counsel-gtags")
  (declare-function counsel-gtags-find-symbol "counsel-gtags")
  (declare-function counsel-gtags-find-definition "counsel-gtags")
  (declare-function counsel-gtags-create-tags "counsel-gtags")
  (declare-function counsel-gtags-update-tags "counsel-gtags")

  (unless (fboundp 'counsel-gtags-mode)
    (autoload #'counsel-gtags-mode "counsel-gtags" nil t))
  (unless (fboundp 'counsel-gtags-dwim)
    (autoload #'counsel-gtags-dwim "counsel-gtags" nil t))
  (unless (fboundp 'counsel-gtags-go-backward)
    (autoload #'counsel-gtags-go-backward "counsel-gtags" nil t))
  (unless (fboundp 'counsel-gtags-find-reference)
    (autoload #'counsel-gtags-find-reference "counsel-gtags" nil t))
  (unless (fboundp 'counsel-gtags-find-symbol)
    (autoload #'counsel-gtags-find-symbol "counsel-gtags" nil t))
  (unless (fboundp 'counsel-gtags-find-definition)
    (autoload #'counsel-gtags-find-definition "counsel-gtags" nil t))
  (unless (fboundp 'counsel-gtags-create-tags)
    (autoload #'counsel-gtags-create-tags "counsel-gtags" nil t))
  (unless (fboundp 'counsel-gtags-update-tags)
    (autoload #'counsel-gtags-update-tags "counsel-gtags" nil t))

  (dolist (hook '(prog-mode-hook protobuf-mode-hook LaTex-mode-hook latex-mode-hook))
    (add-hook hook #'counsel-gtags-mode))

  (with-eval-after-load "counsel-gtags"
    (defvar counsel-gtags-auto-update)

    (setq counsel-gtags-auto-update t)

    (diminish 'counsel-gtags-mode)

    ;; Make xref and gtags work together
    (require 'global-tags)

    (add-to-list 'xref-backend-functions 'global-tags-xref-backend))

  (defvar counsel-gtags-mode-map)
  (bind-keys :package counsel-gtags :map counsel-gtags-mode-map
             ("M-'"     . counsel-gtags-dwim)
             ("M-,"     . counsel-gtags-go-backward)
             ("M-?"     . counsel-gtags-find-reference)
             ("C-c g s" . counsel-gtags-find-symbol)
             ("C-c g d" . counsel-gtags-find-definition)
             ("C-c g c" . counsel-gtags-create-tags)
             ("C-c g u" . counsel-gtags-update-tags)))


(when (and (eq system-type 'gnu/linux)
           (eq sb/tags-scheme 'ctags))
  (declare-function counsel-etags-find-symbol-at-point "counsel-etags")
  (declare-function counsel-etags-find-tag-at-point "counsel-etags")
  (declare-function counsel-etags-find-tag "counsel-etags")
  (declare-function counsel-etags-list-tag "counsel-etags")
  (declare-function counsel-etags-scan-code "counsel-etags")
  (declare-function counsel-etags-virtual-update-tags "counsel-etags")

  (unless (fboundp 'counsel-etags-find-tag-at-point)
    (autoload #'counsel-etags-find-tag-at-point "counsel-etags" nil t))
  (unless (fboundp 'counsel-etags-find-symbol-at-point)
    (autoload #'counsel-etags-find-symbol-at-point "counsel-etags" nil t))
  (unless (fboundp 'counsel-etags-find-tag)
    (autoload #'counsel-etags-find-tag "counsel-etags" nil t))
  (unless (fboundp 'counsel-etags-list-tag)
    (autoload #'counsel-etags-list-tag "counsel-etags" nil t))
  (unless (fboundp 'counsel-etags-scan-code)
    (autoload #'counsel-etags-scan-code "counsel-etags" nil t))

  (with-eval-after-load "counsel-etags"
    (defalias 'list-tags 'counsel-etags-list-tag-in-current-file)

    (add-hook 'prog-mode-hook
              (lambda nil
                (add-hook 'after-save-hook #'counsel-etags-virtual-update-tags
                          'append 'local)))

    (defvar counsel-etags-ignore-directories)
    (defvar counsel-etags-ignore-filenames)

    (dolist  (ignore-dirs '(".vscode" "build" ".metadata" ".recommenders" ".clangd"))
      (add-to-list 'counsel-etags-ignore-directories ignore-dirs))

    (dolist (ignore-files '(".clang-format" ".clang-tidy" "*.json" "*.html" "*.xml"))
      (add-to-list 'counsel-etags-ignore-filenames ignore-files)))

  (bind-keys :package counsel-etags
             ("M-]"     . counsel-etags-find-tag-at-point)
             ("C-c g s" . counsel-etags-find-symbol-at-point)
             ("C-c g f" . counsel-etags-find-tag)
             ("C-c g l" . counsel-etags-list-tag)
             ("C-c g c" . counsel-etags-scan-code)))


(progn
  (declare-function helpful-kill-buffers "helpful")

  (unless (fboundp 'helpful-variable)
    (autoload #'helpful-variable "helpful" nil t))
  (unless (fboundp 'helpful-key)
    (autoload #'helpful-key "helpful" nil t))
  (unless (fboundp 'helpful-callable)
    (autoload #'helpful-callable "helpful" nil t))
  (unless (fboundp 'helpful-symbol)
    (autoload #'helpful-symbol "helpful" nil t))
  (unless (fboundp 'helpful-command)
    (autoload #'helpful-command "helpful" nil t))
  (unless (fboundp 'helpful-at-point)
    (autoload #'helpful-at-point "helpful" nil t))
  (unless (fboundp 'helpful-kill-buffers)
    (autoload #'helpful-kill-buffers "helpful" nil t))

  ;; The built-in `describe-function' includes both functions and macros. `helpful-function' is
  ;; functions only, so `helpful-callable' as a drop-in replacement.
  (defvar helpful-mode-map)
  (bind-keys :package helpful
             ;; ([remap describe-variable] . helpful-variable)
             ;; ([remap describe-key]      . helpful-key)
             ;; ([remap describe-function] . helpful-callable)
             ;; ([remap describe-symbol]   . helpful-symbol)
             ("C-h v" . helpful-variable)
             ("C-h k" . helpful-key)
             ("C-h f" . helpful-callable)
             ("C-h c" . helpful-command)
             ("C-h p" . helpful-at-point)
             ("C-h o" . helpful-symbol)
             :map helpful-mode-map
             ("q"     . helpful-kill-buffers)))


;; Speed up Emacs for large files: `M-x vlf <PATH-TO-FILE>'
(progn
  (unless (fboundp 'vlf)
    (autoload #'vlf "vlf" nil t))

  (with-eval-after-load "vlf"
    (defvar vlf-application)

    (setq vlf-application 'dont-ask)

    (require 'vlf-setup)))


;; Erase all consecutive white space characters in a given direction
(progn
  (unless (fboundp 'hungry-delete-mode)
    (autoload #'hungry-delete-mode "hungry-delete" nil t))
  (unless (fboundp 'global-hungry-delete-mode)
    (autoload #'global-hungry-delete-mode "hungry-delete" nil t))

  (add-hook 'after-init-hook #'global-hungry-delete-mode)
  (add-hook 'minibuffer-setup-hook (lambda nil
                                     (hungry-delete-mode -1)))

  (with-eval-after-load "hungry-delete"
    (diminish 'hungry-delete-mode)))


;; Move lines with `M-<up>' and `M-<down>'
(progn
  (unless (fboundp 'move-text-up)
    (autoload #'move-text-up "move-text" nil t))
  (unless (fboundp 'move-text-down)
    (autoload #'move-text-down "move-text" nil t))
  (unless (fboundp 'move-text-default-bindings)
    (autoload #'move-text-default-bindings "move-text" nil t))

  (move-text-default-bindings))


(progn
  (unless (fboundp 'duplicate-thing)
    (autoload #'duplicate-thing "duplicate-thing" nil t))

  (bind-keys* :package duplicate-thing
              ("C-c C-d" . duplicate-thing)))


;; Discover key bindings and their meaning for the current Emacs major mode
(progn
  (unless (fboundp 'discover-my-major)
    (autoload #'discover-my-major "discover-my-major" nil t))
  (unless (fboundp 'discover-my-mode)
    (autoload #'discover-my-mode "discover-my-major" nil t))

  (bind-keys :package discover-my-major
             ("C-h C-m" . discover-my-major)
             ("C-h M-m" . discover-my-mode)))


;; Manage minor-mode on the dedicated interface buffer
(unless (fboundp 'manage-minor-mode)
  (autoload #'manage-minor-mode "manage-minor-mode" nil t))


(when nil
  (unless (fboundp 'jgraph-mode)
    (autoload #'jgraph-mode "jgraph-mode" nil t)))


(when nil
  (unless (fboundp 'graphviz-dot-mode)
    (autoload #'graphviz-dot-mode "graphviz-dot-mode" nil t)))


(when nil
  (progn
    (declare-function gnuplot "gnuplot")

    (unless (fboundp 'gnuplot)
      (autoload #'gnuplot "gnuplot" nil t))
    (unless (fboundp 'gnuplot-mode)
      (autoload #'gnuplot-mode "gnuplot" nil t))

;;;###autoload
    (add-to-list 'auto-mode-alist '("\\.gp\\'" . gnuplot))

;;;###autoload
    (add-to-list 'interpreter-mode-alist '("gnuplot" . gnuplot-mode))
    ))


;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(when nil
  (progn
    (unless (fboundp 'popwin-mode)
      (autoload #'popwin-mode "popwin" nil t))

    (add-hook 'after-init-hook #'popwin-mode)

    (with-eval-after-load "popwin"
      (defvar popwin:special-display-config)

      (push '("*Help*" :noselect t) popwin:special-display-config)
      (push '(compilation-mode :noselect t) popwin:special-display-config)
      (push '("*Compile-Log*" :noselect t) popwin:special-display-config)
      (push '("*manage-minor-mode*" :noselect t) popwin:special-display-config)
      (push '("*Paradox Report*" :noselect t) popwin:special-display-config)
      (push '("*Selection Ring:") popwin:special-display-config)
      (push '("*Flycheck errors*" :noselect nil) popwin:special-display-config)
      (push '("*Flycheck checkers*" :noselect nil) popwin:special-display-config)
      (push '("*ripgrep-search*" :noselect nil) popwin:special-display-config)
      (push '("^*magit:.+*$" :noselect nil) popwin:special-display-config)
      (push '("*xref*" :noselect nil) popwin:special-display-config)
      (push '(helpful-mode :noselect t) popwin:special-display-config)
      (push "*Shell Command Output*" popwin:special-display-config)
      (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
      (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
      (add-to-list 'popwin:special-display-config '("*Backtrace*"))
      (add-to-list 'popwin:special-display-config '("*Apropos*"))
      (add-to-list 'popwin:special-display-config '("*Warnings*"))
      (add-to-list 'popwin:special-display-config '("*prettier errors*"))
      (add-to-list 'popwin:special-display-config '("*explain-pause-top*"))
      (add-to-list 'popwin:special-display-config '(ivy-occur-grep-mode))
      (add-to-list 'popwin:special-display-config '(deadgrep-mode))
      (add-to-list 'popwin:special-display-config '("*lsp session*")))
    ))

;; Learn about display actions, see [[info:elisp#Display Action Functions]]
;; https://emacs.stackexchange.com/questions/22499/how-can-i-tell-emacs-to-always-open-help-buffers-in-the-current-window
(add-to-list 'display-buffer-alist '("*Faces*"                  display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Flycheck checkers*"      display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Flycheck errors*"        display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Help*"                   display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Bufler*"                 display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*manage-minor-mode*"      display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*use-package statistics*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*deadgrep*"               display-buffer-same-window))
;; Open shell in same window.
(add-to-list 'display-buffer-alist `(,(regexp-quote "*shell")   display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Compile-Log\\*"       display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Warnings\\*"          display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Backtrace\\*"         display-buffer-same-window))

;; ;; Do not popup the *Async Shell Command* buffer
;; (add-to-list 'display-buffer-alist
;;              (cons "\\*Async Shell Command\\*.*"
;;                    (cons #'display-buffer-no-window nil)))


;; Expand region by semantic units
(progn
  (declare-function er/contract-region "expand-region")

  (unless (fboundp 'er/expand-region)
    (autoload #'er/expand-region "expand-region" nil t))
  (unless (fboundp 'er/contract-region)
    (autoload #'er/contract-region "expand-region" nil t))

  (bind-keys :package er/expand-region
             ("C-="   . er/expand-region)
             ("C-M-=" . er/contract-region)))


;; Restore point to the initial location with `C-g' after marking a region
(progn
  (unless (fboundp 'smart-mark-mode)
    (autoload #'smart-mark-mode "smart-mark" nil t))

  (run-at-time 5 nil #'smart-mark-mode))


;; Cut/copy the current line if no region is active
(progn
  (unless (fboundp 'whole-line-or-region-global-mode)
    (autoload #'whole-line-or-region-global-mode "whole-line-or-region" nil t))
  (unless (fboundp 'whole-line-or-region-local-mode)
    (autoload #'whole-line-or-region-local-mode "whole-line-or-region" nil t))

  (run-at-time 5 nil #'whole-line-or-region-global-mode)

  (with-eval-after-load "whole-line-or-region"
    (diminish 'whole-line-or-region-local-mode)))


(progn
  (unless (fboundp 'goto-last-change)
    (autoload #'goto-last-change "goto-last-change" nil t))

  (bind-keys :package goto-last-change
             ("C-x C-\\" . goto-last-change)))


(progn
  (unless (fboundp 'beginend-global-mode)
    (autoload #'beginend-global-mode "beginend" nil t))

  (run-at-time 5 nil #'beginend-global-mode)

  (with-eval-after-load "beginend"
    (defvar beginend-modes)

    (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
      (diminish mode))))


(progn
  (declare-function undo-tree-visualize "undo-tree")

  (unless (fboundp 'undo-tree-visualize)
    (autoload #'undo-tree-visualize "undo-tree" nil t))
  (unless (fboundp 'global-undo-tree-mode)
    (autoload #'global-undo-tree-mode "undo-tree" nil t))

  (with-eval-after-load "undo-tree"
    (defvar undo-tree-auto-save-history)
    (defvar undo-tree-mode-lighter)
    (defvar undo-tree-visualizer-diff)
    (defvar undo-tree-visualizer-relative-timestamps)
    (defvar undo-tree-visualizer-timestamps)

    (setq undo-tree-auto-save-history t
          undo-tree-mode-lighter ""
          undo-tree-visualizer-diff t
          undo-tree-visualizer-relative-timestamps t
          undo-tree-visualizer-timestamps t)

    (global-undo-tree-mode 1)

    (diminish 'undo-tree-mode))

  (bind-keys :package undo-tree
             ("C-x u" . undo-tree-visualize)))


;; Edit multiple regions in the same way simultaneously
(progn
  (unless (fboundp 'iedit-mode)
    (autoload #'iedit-mode "iedit" nil t))

  (bind-keys* :package iedit
              ("C-." . iedit-mode)))


;; Avoid the "Overwrite old session file (not loaded)?" warning
(when nil
  (progn
    (declare-function session-initialize "session")

    (unless (fboundp 'session-initialize)
      (autoload #'session-initialize "session" nil t))

    (add-hook 'after-init-hook #'session-initialize)

    (with-eval-after-load "session"
      (defvar session-save-file)

      (unless (bound-and-true-p sb/use-no-littering)
        (setq session-save-file (expand-file-name "session" sb/temp-directory))))
    ))


(progn
  (unless (fboundp 'immortal-scratch-mode)
    (autoload #'immortal-scratch-mode "immortal-scratch" nil t))

  (run-with-idle-timer 3 nil #'immortal-scratch-mode))


;; I use the *scratch* buffer for taking notes, it helps to make the data persist
(progn
  (unless (fboundp 'persistent-scratch-setup-default)
    (autoload #'persistent-scratch-setup-default "persistent-scratch" nil t))

  ;; Delaying loading contents in the `*scratch*' buffer does not look good
  (add-hook 'after-init-hook #'persistent-scratch-setup-default)

  (with-eval-after-load "persistent-scratch"
    (defvar persistent-scratch-autosave-interval)
    (defvar persistent-scratch-save-file)

    (setq persistent-scratch-autosave-interval 300)

    (unless (bound-and-true-p sb/use-no-littering)
      (setq persistent-scratch-save-file (expand-file-name "persistent-scratch"
                                                           sb/temp-directory)))))


;; crux-smart-open-line-above, crux-smart-open-line, crux-smart-kill-line
(progn
  (unless (fboundp 'crux-sudo-edit)
    (autoload #'crux-sudo-edit "crux" nil t))
  (unless (fboundp 'crux-kill-other-buffers)
    (autoload #'crux-kill-other-buffers "crux" nil t))
  (unless (fboundp 'crux-ispell-word-then-abbrev)
    (autoload #'crux-ispell-word-then-abbrev "crux" nil t))

  (bind-keys :package crux
             ("C-c d i" . crux-ispell-word-then-abbrev)
             ("<f12>"   . crux-kill-other-buffers)
             ("C-c d s" . crux-sudo-edit)))


;; TODO: Is this causing the mouse-movement error and breaking out of key bindings?
(progn
  (when (display-mouse-p)
    (unless (fboundp 'global-disable-mouse-mode)
      (autoload #'global-disable-mouse-mode "disable-mouse" nil t))

    (add-hook 'after-init-hook #'global-disable-mouse-mode)

    (with-eval-after-load "disable-mouse"
      (diminish 'disable-mouse-global-mode))))

(when t
  (progn
    (unless (fboundp 'mouse-avoidance-mode)
      (autoload #'mouse-avoidance-mode "avoid" nil t))

    (mouse-avoidance-mode 'banish)))


(unless (fboundp 'apt-sources-list-mode)
  (autoload #'apt-sources-list-mode "apt-sources-list" nil t))


(progn
  (unless (fboundp 'rainbow-delimiters-mode)
    (autoload #'rainbow-delimiters-mode "rainbow-delimiters" nil t))

  (dolist (hook '(prog-mode-hook latex-mode-hook LaTex-mode-hook org-src-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))


(progn
  (unless (fboundp 'ssh-config-mode)
    (autoload #'ssh-config-mode "ssh-config-mode" nil t))
  (unless (fboundp 'ssh-known-hosts-mode)
    (autoload #'ssh-known-hosts-mode "ssh-config-mode" nil t))
  (unless (fboundp 'ssh-authorized-keys-mode)
    (autoload #'ssh-authorized-keys-mode "ssh-config-mode" nil t))
  (unless (fboundp 'turn-on-font-lock)
    (autoload #'turn-on-font-lock "ssh-config-mode" nil t))

  (add-hook 'ssh-config-mode-hook #'turn-on-font-lock))


(when nil
  (progn
    (declare-function pomidor-quit "pomidor")
    (declare-function pomidor-break "pomidor")
    (declare-function pomidor-reset "pomidor")
    (declare-function pomidor-stop "pomidor")
    (declare-function pomidor-hold "pomidor")
    (declare-function pomidor-unhold "pomidor")

    (unless (fboundp 'pomidor-quit)
      (autoload #'pomidor-quit "pomidor" nil t))
    (unless (fboundp 'pomidor-break)
      (autoload #'pomidor-break "pomidor" nil t))
    (unless (fboundp 'pomidor-reset)
      (autoload #'pomidor-reset "pomidor" nil t))
    (unless (fboundp 'pomidor-stop)
      (autoload #'pomidor-stop "pomidor" nil t))
    (unless (fboundp 'pomidor-hold)
      (autoload #'pomidor-hold "pomidunlessor" nil t))
    (unless (fboundp 'pomidor-unhold)
      (autoload #'pomidor-unhold "pomidor" nil t))
    (unless (fboundp 'pomidor)
      (autoload #'pomidor "pomidor" nil t))
    ))


(progn
  (unless (fboundp 'ace-window)
    (autoload #'ace-window "ace-window" nil t))

  (bind-keys :package ace-window
             ([remap other-window] . ace-window)
             ("<f10>"              . ace-window)))


;; `Shift + direction' arrows
(progn
  (unless (fboundp 'windmove-default-keybindings)
    (autoload #'windmove-default-keybindings "windmove" nil t))

  (windmove-default-keybindings)

  (with-eval-after-load "windmove"
    (defvar windmove-wrap-around)

    ;; Wrap around at edges
    (setq windmove-wrap-around t)))


;; Save buffers when Emacs loses focus.
(when nil
  (progn
    (unless (fboundp 'super-save-mode)
      (autoload #'super-save-mode "super-save" nil t))

    ;; This causes additional saves which triggers the `after-save-hook' and leads to auto-formatters
    ;; being invoked more frequently.
    (run-with-idle-timer 3 nil #'super-save-mode)

    (with-eval-after-load "super-save"
      (defvar super-save-remote-files)
      (defvar super-save-triggers)

      (setq super-save-remote-files nil) ; Ignore remote files
      (add-to-list 'super-save-triggers 'ace-window)

      (diminish 'super-save-mode))))


;; `avy-setup-default' will bind `avy-isearch' to `C-'' in `isearch-mode-map', so that you can
;; select one of the currently visible `isearch' candidates using `avy'.
(progn
  (unless (fboundp 'avy-setup-default)
    (autoload #'avy-setup-default "avy" nil t))

  (with-eval-after-load "avy"
    (defvar avy-indent-line-overlay)
    (defvar avy-background)
    (defvar avy-highlight-first)
    (defvar avy-style))

  (bind-keys :package avy
             ("M-b" . avy-goto-word-1)
             ;; ("C-'" . avy-goto-char)
             ("C-'" . avy-goto-char-timer)
             ("C-/" . avy-goto-line)))


;; This package adds a "C-'" binding to Ivy minibuffer that uses Avy
(when (eq sb/selection 'ivy)
  (progn
    (with-eval-after-load "ivy"
      (declare-function ivy-avy "ivy-avy")
      (unless (fboundp 'ivy-avy)
        (autoload #'ivy-avy "ivy-avy" nil t))

      (defvar ivy-minibuffer-map)

      (bind-keys :package ivy-avy :map ivy-minibuffer-map
                 ("C-'" . ivy-avy)))
    ))


(progn
  (with-eval-after-load "bookmark"
    (defvar bookmark-default-file)

    (unless (bound-and-true-p sb/use-no-littering)
      (setq bookmark-default-file (expand-file-name "bookmarks" sb/temp-directory)))))


(progn
  (declare-function bm-buffer-save "bm")
  (declare-function bm-buffer-restore "bm")
  (declare-function bm-buffer-save-all "bm")
  (declare-function bm-repository-load "bm")
  (declare-function bm-repository-save "bm")

  ;; Must be set before `bm' is loaded
  (defvar bm-restore-repository-on-load)
  (setq bm-restore-repository-on-load t)

  (unless (fboundp 'bm-buffer-save)
    (autoload #'bm-buffer-save "bm" nil t))
  (unless (fboundp 'bm-buffer-restore)
    (autoload #'bm-buffer-restore "bm" nil t))
  (unless (fboundp 'bm-repository-load)
    (autoload #'bm-repository-load "bm" nil t))
  (unless (fboundp 'bm-toggle)
    (autoload #'bm-toggle "bm" nil t))
  (unless (fboundp 'bm-next)
    (autoload #'bm-next "bm" nil t))
  (unless (fboundp 'bm-previous)
    (autoload #'bm-previous "bm" nil t))
  (unless (fboundp 'bm-buffer-save-all)
    (autoload #'bm-buffer-save-all "bm" nil t))
  (unless (fboundp 'bm-repository-save)
    (autoload #'bm-repository-save "bm" nil t))

  (defun sb/bm-setup ()
    "Wrapper function to help call with a timer."
    ;; `kill-buffer-hook' is not called when Emacs is killed
    (add-hook 'kill-emacs-hook (lambda ()
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
    (add-hook 'after-save-hook #'bm-buffer-save)
    (add-hook 'kill-buffer-hook #'bm-buffer-save)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

    (add-hook 'after-revert-hook #'bm-buffer-restore)
    (add-hook 'find-file-hook #'bm-buffer-restore)
    (add-hook 'after-init-hook #'bm-repository-load))

  (run-with-idle-timer 2 nil #'sb/bm-setup)

  (with-eval-after-load "bm"
    (defvar bm-repository-file)

    (setq-default bm-buffer-persistence t)
    (unless (bound-and-true-p sb/use-no-littering)
      (setq bm-repository-file (expand-file-name "bm-bookmarks" sb/temp-directory))))

  (bind-keys :package bm
             ("C-<f1>" . bm-toggle)
             ("C-<f2>" . bm-next)
             ("C-<f3>" . bm-previous)))


(progn
  (declare-function esup "esup")

  (unless (fboundp 'esup)
    (autoload #'esup "esup" nil t)))


(when sb/debug-init-file
  (progn
    (declare-function bug-hunter-file "bug-hunter")
    (declare-function bug-hunter-init-file "bug-hunter")

    (unless (fboundp 'bug-hunter-file)
      (autoload #'bug-hunter-file "bug-hunter" nil t))
    (unless (fboundp 'bug-hunter-init-file)
      (autoload #'bug-hunter-init-file "bug-hunter" nil t))


    (declare-function explain-pause-mode "explain-pause-mode")
    (declare-function explain-pause-top "explain-pause-mode")

    (unless (fboundp 'explain-pause-mode)
      (autoload #'explain-pause-mode "explain-pause-mode" nil t))
    (unless (fboundp 'explain-pause-top)
      (autoload #'explain-pause-top "explain-pause-mode" nil t))

    (with-eval-after-load "explain-pause-mode"
      (diminish 'explain-pause-mode))
    ))


;; `text-mode' is a basic mode for `LaTeX-mode' and `org-mode', and so any hooks defined will also
;; get run for all modes derived from a basic mode such as `text-mode'.

;; Enabling `autfill-mode' makes it difficult to include long instructions verbatim, since they get
;; wrapped around automatically.
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

;; We need to enable lsp workspace to allow `lsp-grammarly' to work, which makes it ineffective for
;; temporary text files
(when nil
  (progn
    (setq lsp-grammarly-modes '(text-mode latex-mode org-mode markdown-mode gfm-mode))
    (add-hook 'text-mode-hook (lambda nil
                                (require 'lsp-grammarly)
                                (lsp-deferred)))
    ))


;; Identify weasel words, passive voice, and duplicate words
(progn
  (declare-function writegood-mode "writegood-mode")

  (unless (fboundp 'writegood-mode)
    (autoload #'writegood-mode "writegood-mode" nil t))

  (with-eval-after-load "writegood-mode"
    (when (fboundp 'writegood-mode)
      (diminish 'writegood-mode))))


(progn
  (with-eval-after-load "text-mode"
    (unless (fboundp 'langtool-check)
      (autoload #'langtool-check "langtool" nil t))

    (defvar langtool-default-language)
    (defvar langtool-disabled-rules)
    (defvar langtool-language-tool-jar)
    (defvar no-littering-etc-directory)

    (setq langtool-default-language "en"
          langtool-disabled-rules '("COMMA_PARENTHESIS_WHITESPACE"
                                    "COPYRIGHT"
                                    "DASH_RULE"
                                    "EN_QUOTES"
                                    "EN_UNPAIRED_BRACKETS"
                                    "UPPERCASE_SENTENCE_START"
                                    "WHITESPACE_RULE")
          langtool-language-tool-jar (no-littering-expand-etc-file-name
                                      "languagetool-5.3-commandline.jar"))))


(unless (fboundp 'wc-mode)
  (autoload #'wc-mode "wc-mode" nil t))


;; Gets the definition of word or phrase at point from https://wordnik.com/
(progn
  (unless (fboundp 'define-word)
    (autoload #'define-word "define-word" nil t))
  (unless (fboundp 'define-word-at-point)
    (autoload #'define-word-at-point "define-word" nil t)))


(when nil
  (progn
    (unless (fboundp 'emojify-mode)
      (autoload #'emojify-mode "emojify" nil t))
    (unless (fboundp 'global-emojify-mode)
      (autoload #'global-emojify-mode "emojify" nil t))

    (add-hook 'markdown-mode-hook #'emojify-mode)))


;; https://emacs.stackexchange.com/questions/19686/how-to-use-pdf-tools-pdf-view-mode-in-emacs
;; Use `isearch', `swiper' does not work
(progn
  (declare-function pdf-view-mode "pdf-tools")
  (declare-function pdf-annot-delete "pdf-tools")
  (declare-function pdf-annot-add-highlight-markup-annotation "pdf-tools")
  (declare-function pdf-annot-add-text-annotation "pdf-tools")

  (unless (fboundp 'pdf-view-mode)
    (autoload #'pdf-view-mode "pdf-tools" nil t))
  (unless (fboundp 'isearch-forward)
    (autoload #'isearch-forward "pdf-tools" nil t))
  (unless (fboundp 'pdf-annot-delete)
    (autoload #'pdf-annot-delete "pdf-tools" nil t))
  (unless (fboundp 'pdf-annot-add-highlight-markup-annotation)
    (autoload #'pdf-annot-add-highlight-markup-annotation "pdf-tools" nil t))
  (unless (fboundp 'pdf-annot-add-text-annotation)
    (autoload #'pdf-annot-add-text-annotation "pdf-tools" nil t))
  (unless (fboundp 'pdf-tools-install)
    (autoload #'pdf-tools-install "pdf-tools" nil t))
  (unless (fboundp 'pdf-loader-install)
    (autoload #'pdf-loader-install "pdf-tools" nil t))

  ;; Expensive to load
  (run-with-idle-timer 2 nil #'require 'pdf-tools nil t)

  (add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

  (with-eval-after-load "pdf-tools"
    ;; (pdf-tools-install :no-query)
    (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install)'

    (defvar pdf-annot-activate-created-annotations)
    (defvar pdf-view-resize-factor)

    (setq pdf-annot-activate-created-annotations t ; Automatically annotate highlights
          ;; Fine-grained zoom factor of 10%
          pdf-view-resize-factor 1.1)

    (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable

    ;; We do not enable `pdf-view-themed-minor-mode' since it can change plot colors
    (add-hook 'pdf-view-mode-hook
              (lambda()
                (pdf-links-minor-mode 1)
                (pdf-isearch-minor-mode 1)
                (pdf-outline-minor-mode 1)
                (pdf-history-minor-mode 1)))

    ;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.
    (require 'saveplace-pdf-view))

  (defvar pdf-view-mode-map)
  (bind-keys :package pdf-tools :map pdf-view-mode-map
             ("t"   . pdf-annot-add-text-annotation)
             ("d"   . pdf-annot-delete)
             ("h"   . pdf-annot-add-highlight-markup-annotation)))


(progn
  (unless (fboundp 'logview-mode)
    (autoload #'logview-mode "logview" nil t))

  (with-eval-after-load "logview"
    (defvar logview-cache-filename)

    (unless (bound-and-true-p sb/use-no-littering)
      (setq logview-cache-filename (expand-file-name "logview-cache.extmap"
                                                     sb/temp-directory)))))


(progn
  (unless (fboundp 'antlr-mode)
    (autoload #'antlr-mode "antlr-mode" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode)))


(progn
  (unless (fboundp 'bison-mode)
    (autoload #'bison-mode "bison-mode" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.bison\\'" . bison-mode)))


(when nil
  (progn
    (declare-function llvm-mode "llvm-mode")

    (unless (fboundp 'llvm-mode)
      (autoload #'llvm-mode "llvm-mode" nil t))
    ))


(when nil
  (progn
    (declare-function tablegen-mode "tablegen-mode")

    (unless (fboundp 'tablegen-mode)
      (autoload #'tablegen-mode "tablegen-mode" nil t))
    ))


(when nil
  (progn
    (declare-function autodisass-llvm-bitcode "autodisass-llvm-bitcode")

    (unless (fboundp 'autodisass-llvm-bitcode)
      (autoload #'autodisass-llvm-bitcode "autodisass-llvm-bitcode" nil t))

    (add-to-list 'auto-mode-alist '("\\.bc\\'" . autodisass-llvm-bitcode))
    ))


(progn
  (unless (fboundp 'markdown-mode)
    (autoload #'markdown-mode "markdown-mode" nil t))
  (unless (fboundp 'gfm-mode)
    (autoload #'gfm-mode "markdown-mode" nil t))

  ;; The order is important to associate "README.md" with `gfm-mode'
;;;###autoload
  (progn
    (add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode)))

  (with-eval-after-load "markdown-mode"
    ;; Looks good, but hiding markup makes it difficult to be consistent while editing
    ;; (setq-default markdown-hide-markup t)

    (defvar markdown-command)
    (defvar markdown-enable-math)
    (defvar markdown-enable-wiki-links)
    (defvar markdown-fontify-code-blocks-natively)
    (defvar markdown-indent-on-enter)
    (defvar markdown-list-indent-width)
    (defvar markdown-split-window-direction)
    (defvar markdown-hide-urls)

    (setq markdown-command
          "pandoc -f markdown -s --mathjax --standalone --quiet --highlight-style=pygments"
          markdown-enable-math t ; Syntax highlight for LaTeX fragments
          markdown-enable-wiki-links t
          ;;   https://emacs.stackexchange.com/questions/13189/github-flavored-markdown-mode-syntax-highlight-code-blocks/33497
          markdown-fontify-code-blocks-natively t
          markdown-indent-on-enter 'indent-and-new-item
          markdown-list-indent-width 2
          markdown-split-window-direction 'horizontal
          markdown-hide-urls t)

    ;; Generate TOC with `markdown-toc-generate-toc'

    (unless (fboundp 'markdown-toc-refresh-toc)
      (autoload #'markdown-toc-refresh-toc "markdown-toc" nil t))
    (unless (fboundp 'markdown-toc-generate-toc)
      (autoload #'markdown-toc-generate-toc "markdown-toc" nil t))
    (unless (fboundp 'markdown-toc-generate-or-refresh-toc)
      (autoload #'markdown-toc-generate-or-refresh-toc "markdown-toc" nil t))

    ;; Open preview of markdown file in a browser
    (unless (fboundp 'markdown-preview-mode)
      (autoload #'markdown-preview-mode "markdown-preview-mode" nil t))))


;; Use `pandoc-convert-to-pdf' to export markdown file to pdf
;; Convert `markdown' to `org': `pandoc -f markdown -t org -o output-file.org input-file.md'
(progn
  (declare-function pandoc-load-default-settings "pandoc-mode")

  (unless (fboundp 'pandoc-mode)
    (autoload #'pandoc-mode "pandoc-mode" nil t))
  (unless (fboundp 'pandoc-load-default-settings)
    (autoload #'pandoc-load-default-settings "pandoc-mode" nil t))

  (add-hook 'markdown-mode-hook #'pandoc-mode)

  (with-eval-after-load "pandoc-mode"
    (pandoc-load-default-settings)

    ;; Binds `C-c /' to `pandoc-main-hydra/body'.
    ;; (unbind-key "C-c /" pandoc-mode-map))

    (diminish 'pandoc-mode)))


;; Run `M-x grip-mode` to preview the markdown file with the default browser.
(when (executable-find "grip")
  (unless (fboundp 'grip-mode)
    (autoload #'grip-mode "grip-mode" nil t))

  (defvar markdown-mode-command-map)
  (bind-keys :package grip-mode :map markdown-mode-command-map
             ("g" . grip-mode)))


;; LATER: Prettier times out setting up the process on a remote machine. I am using `format-all'
;; for now.
;; https://github.com/jscheid/prettier.el/issues/84
(when nil
  (when (executable-find "prettier")
    (progn
      (unless (fboundp 'prettier-mode)
        (autoload #'prettier-mode "prettier" nil t))

      ;; Should work with `gfm-mode', `css-mode', and `html-mode' as they are derived modes
      (dolist (hook '(markdown-mode-hook web-mode-hook
                                         json-mode-hook jsonc-mode-hook js2-mode-hook))
        (add-hook hook (lambda ()
                         (when (and buffer-file-name ; Returns `nil' if not visiting a file
                                    (not (file-remote-p buffer-file-name)))
                           (prettier-mode 1))))
        (add-hook hook #'prettier-mode))

      (with-eval-after-load "prettier"
        (defvar prettier-lighter)

        (setq prettier-lighter nil)))))


;; Align fields with `C-c C-a'
(progn
  (unless (fboundp 'csv-mode)
    (autoload #'csv-mode "csv-mode" nil t))

  (with-eval-after-load "csv-mode"
    (defvar csv-separators)

    (setq csv-separators '("," ";" "|" " "))))


(progn
  (declare-function highlight-doxygen-global-mode "highlight-doxygen")

  (unless (fboundp 'highlight-doxygen-global-mode)
    (autoload #'highlight-doxygen-global-mode "highlight-doxygen" nil t))

  (with-eval-after-load "highlight-doxygen"
    (highlight-doxygen-global-mode)))


(when nil
  (progn
    (unless (fboundp 'rst-mode)
      (autoload #'rst-mode "rst" nil t))

;;;###autoload
    (add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))


    (unless (fboundp 'xref-rst-mode)
      (autoload #'xref-rst-mode "xref-rst" nil t))

    (add-hook 'rst-mode-hook #'xref-rst-mode)))


(when nil
  (progn
    (unless (fboundp 'z3-smt2-mode)
      (autoload #'z3-smt2-mode "boogie-friends" nil t))

;;;###autoload
    (add-to-list 'auto-mode-alist '("\\.smt\\'" . z3-smt2-mode))


    (unless (fboundp 'z3-mode)
      (autoload #'z3-mode "z3-mode" nil t))

    (with-eval-after-load "z3-mode"
      (defvar z3-solver-cmd)

      (setq z3-solver-cmd "z3"))))


(progn
  (unless (fboundp 'makefile-mode)
    (autoload #'makefile-mode "make-mode" nil t))
  (unless (fboundp 'makefile-gmake-mode)
    (autoload #'makefile-gmake-mode "make-mode" nil t))

;;;###autoload
  (progn
    (add-to-list 'auto-mode-alist '("\\Makefile\\'"       . makefile-mode))
    ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
    (add-to-list 'auto-mode-alist '("makefile\\.rules\\'" . makefile-gmake-mode))))


;; The variable-height minibuffer and extra eldoc buffers are distracting
(when (symbol-value 'sb/IS-LINUX)
  (progn
    (unless (fboundp 'turn-on-eldoc-mode)
      (autoload #'turn-on-eldoc-mode "eldoc" nil t))

    (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
      (add-hook hook #'turn-on-eldoc-mode))

    (with-eval-after-load "eldoc"
      ;; Always truncate ElDoc messages to one line. This prevents the echo area from resizing
      ;; itself unexpectedly when point is on a variable with a multiline docstring.
      (setq eldoc-echo-area-use-multiline-p nil)

      (diminish 'eldoc-mode)))


  (progn
    (declare-function eldoc-box-hover-mode "eldoc-box")
    (declare-function eldoc-box-hover-at-point-mode "eldoc-box")

    (unless (fboundp 'eldoc-box-hover-mode)
      (autoload #'eldoc-box-hover-mode "eldoc-box" nil t))
    (unless (fboundp 'eldoc-box-hover-at-point-mode)
      (autoload #'eldoc-box-hover-at-point-mode "eldoc-box" nil t))

    (add-hook 'eldoc-mode-hook #'eldoc-box-hover-mode)
    (add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)

    (with-eval-after-load "eldoc-box"
      (defvar eldoc-box-clear-with-C-g)
      (defvar eldoc-box-fringe-use-same-bg)

      (setq eldoc-box-clear-with-C-g t
            eldoc-box-fringe-use-same-bg nil)

      ;; (when (eq sb/theme 'modus-operandi)
      ;;   (set-face-background 'eldoc-box-body "cornsilk"))

      ;; (when (eq sb/theme 'modus-vivendi)
      ;;   (set-face-background 'eldoc-box-body "gray"))

      (diminish 'eldoc-box-hover-mode))))


;; We use LSP
(when nil
  (progn
    (unless (fboundp 'c-turn-on-eldoc-mode)
      (autoload #'c-turn-on-eldoc-mode "c-eldoc" nil t))

    (add-hook 'c-mode-common-hook #'c-turn-on-eldoc-mode)
    ))


;; We use LSP
(when nil
  (progn
    (with-eval-after-load "css-mode"
      (unless (fboundp 'css-eldoc-enable)
        (autoload #'css-eldoc-enable "css-eldoc" nil t))

      (css-eldoc-enable))
    ))


(when nil
  (progn
    (unless (fboundp 'matlab-mode)
      (autoload #'matlab-mode "matlab-mode" nil t))

;;;###autoload
    (add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))))


(when nil
  (progn
    (unless (fboundp 'R-mode)
      (autoload #'R-mode "ess" nil t))

    (add-to-list 'auto-mode-alist '("/R/.*\\.q\\'" . R-mode))

    (with-eval-after-load "R-mode"
      (require 'ess-smart-underscore))))


(when nil
  (progn
    (with-eval-after-load "ess"
      (defvar ess-indent-offset)
      (defvar ess-indent-from-lhs)
      (defvar inferior-R-args)

      (setq ess-indent-from-lhs 4
            ess-indent-offset 4
            inferior-R-args "--quiet --no-restore-history --no-save")

      (require 'ess-smart-underscore))))


(unless (fboundp 'ini-mode)
  (autoload #'ini-mode "ini-mode" nil t))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))


(when nil
  (unless (fboundp 'pkgbuild-mode)
    (autoload #'pkgbuild-mode "pkgbuild-mode" nil t)))


(progn
  (unless (fboundp 'emacs-lisp-mode)
    (autoload #'emacs-lisp-mode "elisp-mode" nil t))
  (unless (fboundp 'elisp-byte-code-mode)
    (autoload #'elisp-byte-code-mode "elisp-mode" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.el\\'"  . emacs-lisp-mode))
;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.elc\\'" . elisp-byte-code-mode))

  (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
    (add-hook hook (lambda ()
                     (when buffer-file-name
                       (add-hook 'after-save-hook #'check-parens nil t)
                       (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-checkdoc))))))


;; LSP support

(declare-function lsp-find-definition "lsp-mode")
(declare-function lsp-find-declaration "lsp-mode")
(declare-function lsp-goto-implementation "lsp-mode")
(declare-function lsp-goto-type-definition "lsp-mode")
(declare-function lsp-rename "lsp-mode")
(declare-function lsp-symbol-highlight "lsp-mode")
(declare-function lsp-format-buffer "lsp-mode")
(declare-function lsp-find-references "lsp-mode")
(declare-function lsp--set-configuration "lsp-mode")
(declare-function lsp-completion--regex-fuz "lsp-completion")
(declare-function lsp-register-client "lsp-mode")
(declare-function lsp-tramp-connection "lsp-mode")
(declare-function make-lsp-client "lsp-mode")
(declare-function lsp-configuration-section "lsp-mode")
(declare-function lsp-package-ensure "lsp-mode")
(declare-function with-lsp-workspace "lsp-mode")
(declare-function lsp-ht "lsp-mode")
(declare-function lsp-enable-which-key-integration "lsp-mode")

(unless (fboundp 'lsp-deferred)
  (autoload #'lsp-deferred "lsp-mode" nil t))
(unless (fboundp 'lsp-enable-which-key-integration)
  (autoload #'lsp-enable-which-key-integration "lsp-mode" nil t))
(unless (fboundp 'lsp-modeline-diagnostics-mode)
  (autoload #'lsp-modeline-diagnostics-mode "lsp-mode" nil t))
(unless (fboundp 'lsp-modeline-code-actions-mode)
  (autoload #'lsp-modeline-code-actions-mode "lsp-mode" nil t))
(unless (fboundp 'lsp-find-definition)
  (autoload #'lsp-find-definition "lsp-mode" nil t))
(unless (fboundp 'lsp-find-declaration)
  (autoload #'lsp-find-declaration "lsp-mode" nil t))
(unless (fboundp 'lsp-goto-implementation)
  (autoload #'lsp-goto-implementation "lsp-mode" nil t))
(unless (fboundp 'lsp-goto-type-definition)
  (autoload #'lsp-goto-type-definition "lsp-mode" nil t))
(unless (fboundp 'lsp-rename)
  (autoload #'lsp-rename "lsp-mode" nil t))
(unless (fboundp 'lsp-symbol-highlight)
  (autoload #'lsp-symbol-highlight "lsp-mode" nil t))
(unless (fboundp 'lsp-format-buffer)
  (autoload #'lsp-format-buffer "lsp-mode" nil t))
(unless (fboundp 'lsp-find-references)
  (autoload #'lsp-find-references "lsp-mode" nil t))
(unless (fboundp 'lsp--set-configuration)
  (autoload #'lsp--set-configuration "lsp-mode" nil t))
(unless (fboundp 'lsp-completion--regex-fuz)
  (autoload #'lsp-completion--regex-fuz "lsp-mode" nil t))
(unless (fboundp 'lsp-register-client)
  (autoload #'lsp-register-client "lsp-mode" nil t))
(unless (fboundp 'lsp-tramp-connection)
  (autoload #'lsp-tramp-connection "lsp-mode" nil t))
(unless (fboundp 'make-lsp-client)
  (autoload #'make-lsp-client "lsp-mode" nil t))
(unless (fboundp 'lsp-configuration-section)
  (autoload #'lsp-configuration-section "lsp-mode" nil t))
(unless (fboundp 'lsp-package-ensure)
  (autoload #'lsp-package-ensure "lsp-mode" nil t))

(dolist (hooks '(css-mode-hook less-mode-hook sgml-mode-hook typescript-mode-hook))
  (add-hook hooks #'lsp-deferred))

(dolist (hooks '(c++-mode-hook java-mode-hook nxml-mode-hook))
  (add-hook hooks (lambda ()
                    (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

(with-eval-after-load "lsp-mode"
  (diminish 'lsp-mode)

  (defvar lsp-pyls-configuration-sources)
  (defvar lsp-pyls-plugins-autopep8-enable)
  (defvar lsp-pyls-plugins-mccabe-enabled)
  (defvar lsp-pyls-plugins-pycodestyle-enabled)
  (defvar lsp-pyls-plugins-pycodestyle-max-line-length)
  (defvar lsp-pyls-plugins-pydocstyle-convention)
  (defvar lsp-pyls-plugins-pydocstyle-enabled)
  (defvar lsp-pyls-plugins-pydocstyle-ignore)
  (defvar lsp-pyls-plugins-pyflakes-enabled)
  (defvar lsp-pyls-plugins-pylint-args)
  (defvar lsp-pyls-plugins-pylint-enabled)
  (defvar lsp-pyls-plugins-yapf-enabled)
  (defvar lsp-pyright-langserver-command-args)
  (defvar lsp-clients-clangd-args)
  (defvar lsp-completion-enable-additional-text-edit)
  (defvar lsp-completion-provider)
  (defvar lsp-enable-dap-auto-configure)
  (defvar lsp-enable-file-watchers)
  (defvar lsp-enable-folding)
  (defvar lsp-enable-on-type-formatting)
  (defvar lsp-enable-semantic-tokens)
  (defvar lsp-enable-snippet)
  (defvar lsp-headerline-breadcrumb-enable)
  (defvar lsp-headerline-breadcrumb-enable-diagnostics)
  (defvar lsp-html-format-wrap-line-length)
  (defvar lsp-html-format-end-with-newline)
  (defvar lsp-html-format-indent-inner-html)
  (defvar lsp-html-format-max-preserve-new-lines)
  (defvar lsp-imenu-sort-methods)
  (defvar lsp-keep-workspace-alive)
  (defvar lsp-log-io)
  (defvar lsp-modeline-diagnostics-scope)
  (defvar lsp-signature-auto-activate)
  (defvar lsp-signature-render-documentation)
  (defvar lsp-xml-logs-client)
  (defvar lsp-xml-jar-file)
  (defvar lsp-yaml-print-width)
  (defvar lsp-session-file)
  (defvar lsp-completion-show-detail)
  (defvar lsp-eldoc-enable-hover)
  (defvar lsp-signature-function)
  (defvar lsp-completion-show-kind)
  (defvar lsp-modeline-diagnostics-enable)
  (defvar lsp-enable-which-key-integration)

  ;; We can add "--compile-commands-dir=build" option to indicate the directory where
  ;; `compile_commands.json' reside
  (setq lsp-clients-clangd-args '("-j=2"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--fallback-style=LLVM"
                                  "--header-insertion=never"
                                  "--log=error"
                                  "--pch-storage=memory"
                                  "--pretty")
        lsp-completion-enable-additional-text-edit t
        lsp-completion-provider :none ; Enable integration with company
        ;; Disable completion metadata
        lsp-completion-show-detail nil
        lsp-completion-show-kind nil
        lsp-eldoc-enable-hover t
        lsp-enable-dap-auto-configure nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-on-type-formatting nil
        lsp-enable-semantic-tokens t
        lsp-enable-snippet t
        lsp-enable-which-key-integration t
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-html-format-wrap-line-length sb/fill-column
        lsp-html-format-end-with-newline t
        lsp-html-format-indent-inner-html t
        lsp-html-format-max-preserve-new-lines nil
        lsp-imenu-sort-methods '(position)
        lsp-keep-workspace-alive nil
        lsp-log-io nil ; `texlab' communication is huge
        ;; We already have `flycheck' error summary listed on the modeline, but the `lsp' server may
        ;; report additional errors
        lsp-modeline-diagnostics-enable t
        lsp-modeline-diagnostics-scope :file ; Focus on the errors at hand
        lsp-signature-auto-activate nil ; Manually request via `lsp-signature-activate'
        lsp-signature-render-documentation nil ; Disable function documentation
        lsp-signature-function 'lsp-signature-posframe
        lsp-xml-logs-client nil
        lsp-xml-jar-file (expand-file-name "org.eclipse.lemminx-0.16.2-uber.jar"
                                           sb/extras-directory)
        lsp-yaml-print-width sb/fill-column)

  (unless (bound-and-true-p sb/use-no-littering)
    (setq lsp-session-file (expand-file-name "lsp-session" sb/temp-directory)))

  (custom-set-faces
   (backquote
    (lsp-headerline-breadcrumb-symbols-face
     ((t (:inherit font-lock-doc-face :weight bold :height 0.9))))))

  (custom-set-faces
   (backquote
    (lsp-headerline-breadcrumb-prefix-face
     ((t (:inherit font-lock-string-face :height 0.9))))))

  (custom-set-faces
   (backquote
    (lsp-headerline-breadcrumb-project-prefix-face
     ((t (:inherit font-lock-string-face :weight bold :height 0.9))))))

  (when (eq sb/python-langserver 'pyls)
    (setq lsp-pyls-configuration-sources []
          lsp-pyls-plugins-autopep8-enable nil
          lsp-pyls-plugins-mccabe-enabled nil
          lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-pycodestyle-max-line-length sb/fill-column
          lsp-pyls-plugins-pydocstyle-convention "pep257"
          lsp-pyls-plugins-pydocstyle-enabled nil
          lsp-pyls-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213"))
          lsp-pyls-plugins-pyflakes-enabled nil
          lsp-pyls-plugins-pylint-args
          (vconcat
           (list "-j 2"
                 (concat "--rcfile="
                         (expand-file-name ".config/pylintrc" sb/user-home))))
          lsp-pyls-plugins-pylint-enabled t
          lsp-pyls-plugins-yapf-enabled t))

  (when (eq sb/python-langserver 'pyls)
    (lsp-register-client
     (make-lsp-client :new-connection
                      (lsp-tramp-connection "pyls")
                      :major-modes
                      '(python-mode)
                      :remote\? t :server-id 'pyls-remote)))

  (when (eq sb/python-langserver 'pyright)
    (lsp-register-client
     (make-lsp-client :new-connection
                      (lsp-tramp-connection
                       (lambda nil
                         (cons "pyright-langserver" lsp-pyright-langserver-command-args)))
                      :major-modes
                      '(python-mode)
                      :remote\? t :server-id 'pyright-remote :multi-root t
                      :initialization-options
                      (lambda nil
                        (ht-merge
                         (lsp-configuration-section "pyright")
                         (lsp-configuration-section "python")))
                      :initialized-fn
                      (lambda
                        (workspace)
                        (with-lsp-workspace workspace
                          (lsp--set-configuration
                           (ht-merge
                            (lsp-configuration-section "pyright")
                            (lsp-configuration-section "python")))))
                      :download-server-fn
                      (lambda
                        (_client callback error-callback _update\?)
                        (lsp-package-ensure 'pyright callback error-callback))
                      :notification-handlers
                      (lsp-ht
                       ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                       ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                       ("pyright/endProgress" 'lsp-pyright--end-progress-callback)))))

  (when (eq sb/python-langserver 'jedi)
    (lsp-register-client
     (make-lsp-client :new-connection
                      (lsp-tramp-connection "jedi-language-server")
                      :major-modes
                      '(python-mode)
                      :remote\? t :server-id 'jedils-remote)))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection "clangd")
                    :major-modes
                    '(c-mode c++-mode)
                    :remote\? t :server-id 'clangd-remote))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection
                     '("bash-language-server" "start"))
                    :major-modes
                    '(sh-mode)
                    :remote\? t :server-id 'bashls-remote))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection "intelephense")
                    :major-modes
                    '(php-mode)
                    :remote\? t :server-id 'intelephense-remote))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection "cmake-language-server")
                    :major-modes
                    '(cmake-mode)
                    :remote\? t :server-id 'cmakels-remote))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection
                     '("typescript-language-server" "--stdio"))
                    :major-modes
                    '(js-mode typescript-mode)
                    :remote\? t :server-id 'typescript-remote))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection
                     '("vscode-json-languageserver" "--stdio"))
                    :major-modes
                    '(json-mode jsonc-mode)
                    :remote\? t :server-id 'jsonls-remote))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection
                     '("css-languageserver" "--stdio"))
                    :major-modes
                    '(css-mode less-mode sass-mode scss-mode)
                    :remote\? t :server-id 'cssls-remote))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection
                     '("html-languageserver" "--stdio"))
                    :major-modes
                    '(html-mode web-mode mhtml-mode sgml-mode)
                    :remote\? t :server-id 'htmlls-remote))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection '("java" "-jar"
                                            (expand-file-name
                                             "org.eclipse.lemminx-0.16.2-uber.jar"
                                             sb/extras-directory)))
                    :major-modes
                    '(xml-mode nxml-mode)
                    :remote\? t :server-id 'xmlls-remote))

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection
                     '("yaml-language-server" "--stdio"))
                    :major-modes
                    '(yaml-mode)
                    :remote\? t :server-id 'yamlls-remote))

  (defvar lsp-perl-language-server-path)
  (defvar lsp-perl-language-server-port)
  (defvar lsp-perl-language-server-client-version)

  (lsp-register-client
   (make-lsp-client :new-connection
                    (lsp-tramp-connection
                     (lambda nil
                       (list
                        lsp-perl-language-server-path "-MPerl::LanguageServer" "-e"
                        "Perl::LanguageServer::run" "--"
                        (format "--port %d --version %s"
                                lsp-perl-language-server-port
                                lsp-perl-language-server-client-version))))
                    :major-modes
                    '(perl-mode cperl-mode)
                    :remote\? t :initialized-fn
                    (lambda
                      (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "perl"))))
                    :priority -1 :server-id 'perlls-remote))

  ;; TODO: What is the utility of this?
  ;; (advice-add #'lsp-completion--regex-fuz :override #'identity)

  )

;; `lsp-imenu-create-categorised-index' - sorts the items by kind.
;; `lsp-imenu-create-uncategorized-index' - will have the items sorted by position.
(bind-keys :package lsp-mode
           ("M-."     . lsp-find-definition)
           ("C-c l d" . lsp-find-declaration)
           ("C-c l i" . lsp-goto-implementation)
           ("C-c l t" . lsp-goto-type-definition)
           ("C-c l r" . lsp-rename)
           ("C-c l h" . lsp-symbol-highlight)
           ("C-c l f" . lsp-format-buffer)
           ("C-c l r" . lsp-find-references))


(when (eq sb/selection 'ivy)
  (with-eval-after-load "ivy"
    (unless (fboundp 'lsp-ivy-global-workspace-symbol)
      (autoload #'lsp-ivy-global-workspace-symbol "lsp-ivy" nil t))
    (unless (fboundp 'lsp-ivy-workspace-symbol)
      (autoload #'lsp-ivy-workspace-symbol "lsp-ivy" nil t))

    (bind-keys :package lsp-ivy
               ("C-c l g" . lsp-ivy-global-workspace-symbol)
               ("C-c l w" . lsp-ivy-workspace-symbol))))


(when (eq sb/selection 'selectrum)
  (with-eval-after-load "selectrum"
    (declare-function consult-lsp-symbols "consult-lsp")
    (declare-function consult-lsp-diagnostics "consult-lsp")

    (unless (fboundp 'consult-lsp-symbols)
      (autoload #'consult-lsp-symbols "consult-lsp"))
    (unless (fboundp 'consult-lsp-diagnostics)
      (autoload #'consult-lsp-diagnostics "consult-lsp"))

    ))


(when (eq sb/python-langserver 'pyls)
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer nil t))))


(with-eval-after-load "lsp-mode"
  (declare-function lsp-ui-doc-mode "lsp-ui-doc")
  (declare-function lsp-ui-peek-find-definitions "lsp-ui-peek")
  (declare-function lsp-ui-peek-find-references "lsp-ui-peek")
  (declare-function lsp-ui-doc--hide-frame "lsp-ui-doc")

  (unless (fboundp 'lsp-ui-mode)
    (autoload #'lsp-ui-mode "lsp-ui" nil t))
  (unless (fboundp 'lsp-ui-doc-mode)
    (autoload #'lsp-ui-doc-mode "lsp-ui-doc" nil t))

  (lsp-ui-mode 1)
  (lsp-ui-doc-mode 1)

  (defvar lsp-ui-doc-enable)
  (defvar lsp-ui-imenu-auto-refresh)
  (defvar lsp-ui-sideline-enable)
  (defvar lsp-ui-modeline-code-actions-enable)
  (defvar lsp-ui-imenu-window-width)
  (defvar lsp-ui-sideline-show-diagnostics)
  (defvar lsp-ui-doc-max-width)
  (defvar lsp-ui-doc-max-height)
  (defvar lsp-ui-doc-include-signature)
  (defvar lsp-ui-peek-enable)

  ;; https://github.com/emacs-lsp/lsp-ui/issues/578
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (lsp-ui-doc--hide-frame)))

  (setq lsp-ui-doc-enable t ; Enable on-hover dialogs
        lsp-ui-doc-max-width 60
        lsp-ui-doc-max-height 8
        lsp-ui-doc-include-signature t
        lsp-ui-imenu-auto-refresh 'after-save
        lsp-ui-imenu-window-width 16
        lsp-ui-sideline-enable nil ; Disable whole sideline
        lsp-ui-sideline-show-diagnostics nil ; Do not show diagnostics when typing
        lsp-ui-modeline-code-actions-enable t)

  (when (not (display-graphic-p))
    (setq lsp-ui-doc-enable nil
          lsp-ui-peek-enable nil))

  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (eldoc-box-hover-mode -1)
              (eldoc-box-hover-at-point-mode -1)
              (eldoc-mode -1)))

  (defvar lsp-ui-mode-map)

  ;; FIXME: Bind only if the server has launched successfully
  (bind-keys :package lsp-ui :map lsp-ui-mode-map
             ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
             ([remap xref-find-references]  . lsp-ui-peek-find-references)))


;; Sync workspace folders and treemacs projects
(when nil
  (unless (fboundp 'lsp-treemacs-errors-list)
    (autoload #'lsp-treemacs-errors-list "lsp-treemacs" nil t))
  (unless (fboundp 'lsp-treemacs-sync-mode)
    (autoload #'lsp-treemacs-sync-mode "lsp-treemacs" nil t))

  (with-eval-after-load "lsp-treemacs"
    (lsp-treemacs-sync-mode 1)))


(when nil
  (unless (fboundp 'global-origami-mode)
    (autoload #'global-origami-mode "origami" nil t))
  (unless (fboundp 'origami-toggle-node)
    (autoload #'origami-toggle-node "origami" nil t))
  (unless (fboundp 'origami-recursively-toggle-node)
    (autoload #'origami-recursively-toggle-node "origami" nil t))
  (unless (fboundp 'origami-toggle-all-nodes)
    (autoload #'origami-toggle-all-node "origami" nil t))

  (dolist (hook '(python-mode-hook java-mode-hook c-mode-hook c++-mode-hook))
    (add-hook hook #'global-origami-mode))

  (bind-keys :package origami :map origami-mode-map
             ("C-c l t" . origami-recursively-toggle-node)
             ("C-c l n" . origami-toggle-all-nodes))

  (with-eval-after-load "origami"
    (unless (fboundp 'lsp-origami-mode)
      (autoload #'lsp-origami-mode "lsp-origami" nil t))

    (lsp-origami-mode 1)))


;; (setq url-cookie-file (expand-file-name (format "%s/emacs/url/cookies/" xdg-data)))


;; Call this in c-mode-common-hook:
;; (define-key (current-local-map) "}" (lambda () (interactive) (c-electric-brace 1)))
(progn
  (declare-function c-beginning-of-defun "cc-mode")
  (declare-function c-end-of-defun "cc-mode")
  (declare-function c-fill-paragraph "cc-mode")

  (unless (fboundp 'c++-mode)
    (autoload #'c++-mode "cc-mode" nil t))
  (unless (fboundp 'c-beginning-of-defun)
    (autoload #'c-beginning-of-defun "cc-mode" nil t))
  (unless (fboundp 'c-end-of-defun)
    (autoload #'c-end-of-defun "cc-mode" nil t))
  (unless (fboundp 'c-fill-paragraph)
    (autoload #'c-fill-paragraph "cc-mode" nil t))

;;;###autoload
  (dolist (pattern '("\\.h\\'" "\\.c\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'c++-mode)))

  (add-hook 'c++-mode-hook #'lsp-deferred)

  (with-eval-after-load "cc-mode"
    (defvar c-electric-indent)
    (defvar c-set-style)
    (defvar c-basic-offset)
    (defvar c-enable-auto-newline)
    (defvar c-electric-brace)
    (defvar c-auto-newline)
    (defvar c-electric-flag)
    (defvar c-syntactic-indentation)

    (setq c-set-style "cc-mode"
          c-basic-offset 2)

    ;; Disable electric indentation and on-type formatting
    (add-hook 'c++-mode-hook (lambda nil
                               (setq-local c-auto-newline nil
                                           c-electric-brace nil
                                           c-electric-flag nil
                                           c-electric-indent nil
                                           c-enable-auto-newline nil
                                           c-syntactic-indentation nil)))

    ;;   (unbind-key "C-M-a" c-mode-map)

    )

  (defvar c-mode-base-map)
  (bind-keys :package cc-mode :map c-mode-base-map
             ("C-c c a" . c-beginning-of-defun)
             ("C-c c e" . c-end-of-defun)
             ("M-q"     . c-fill-paragraph))


  (with-eval-after-load "c++-mode"
    (unless (fboundp 'modern-c++-font-lock-mode)
      (autoload #'modern-c++-font-lock-mode "modern-cpp-font-lock" nil t))

    (modern-c++-font-lock-mode 1)

    (diminish 'modern-c++-font-lock-mode)))


(unless (fboundp 'cuda-mode)
  (autoload #'cuda-mode "cuda-mode" nil t))


(progn
  (declare-function opencl-mode "opencl-mode")

  (unless (fboundp 'opencl-mode)
    (autoload #'opencl-mode "opencl-mode" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode)))


(progn
  (unless (fboundp 'cmake-mode)
    (autoload #'cmake-mode "cmake-mode" nil t))

  (add-hook 'cmake-mode-hook #'lsp-deferred)

  (with-eval-after-load "cmake-mode"
    (unless (fboundp 'cmake-font-lock-activate)
      (autoload #'cmake-font-lock-activate "cmake-font-lock" nil t))

    (cmake-font-lock-activate)))


(progn
  (declare-function python-nav-backward-block "python")
  (declare-function python-nav-forward-block "python")
  (declare-function python-indent-shift-left "python")
  (declare-function python-indent-shift-right "python")

  (unless (fboundp 'python-nav-backward-block)
    (autoload #'python-nav-backward-block "python" nil t))
  (unless (fboundp 'python-nav-forward-block)
    (autoload #'python-nav-forward-block "python" nil t))
  (unless (fboundp 'python-indent-shift-left)
    (autoload #'python-indent-shift-left "python" nil t))
  (unless (fboundp 'python-indent-shift-right)
    (autoload #'python-indent-shift-right "python" nil t))

  (with-eval-after-load "python"
    (setenv "PYTHONPATH" "python3")

    (defvar python-shell-completion-native-enable)
    (defvar python-fill-docstring-style)
    (defvar python-indent-guess-indent-offset)
    (defvar python-indent-guess-indent-offset-verbose)
    (defvar python-indent-offset)
    (defvar python-shell-exec-path)
    (defvar python-shell-interpreter)

    (setq python-shell-completion-native-enable nil ; Disable readline based native completion
          python-fill-docstring-style 'django
          python-indent-guess-indent-offset-verbose nil ; Remove guess indent python message
          python-indent-guess-indent-offset nil
          python-indent-offset 4
          python-shell-exec-path "python3"
          python-shell-interpreter "python3")

    (setq auto-mode-alist (append
                           '(("SConstruct\\'" . python-mode)
                             ("SConscript\\'" . python-mode))
                           auto-mode-alist))


    (unless (fboundp 'python-docstring-mode)
      (autoload #'python-docstring-mode "python-docstring" nil t))
    (unless (fboundp 'python-docstring-install)
      (autoload #'python-docstring-install "python-docstring" nil t))

    (python-docstring-install)

    (diminish 'python-docstring-mode))

  (defvar python-mode-map)
  (bind-keys :package python :map python-mode-map
             ("M-["   . python-nav-backward-block)
             ("M-]"   . python-nav-forward-block)
             ;; Assigning a keybinding such as "C-[" is involved
             ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c
             ("C-c <" . python-indent-shift-left)
             ("C-c >" . python-indent-shift-right)))


(progn
  (unless (fboundp 'pyvenv-mode)
    (autoload #'pyvenv-mode "pyvenv" nil t))
  (unless (fboundp 'pyvenv-tracking-mode)
    (autoload #'pyvenv-tracking-mode "pyvenv" nil t))


  (add-hook 'python-mode-hook #'pyvenv-mode)

  (with-eval-after-load "pyvenv"
    (defvar pyvenv-mode-line-indicator)
    (defvar pyvenv-post-activate-hooks)
    (defvar pyvenv-post-deactivate-hooks)
    (defvar pyvenv-virtual-env)
    (defvar python-shell-interpreter)

    (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                       (" [venv:" pyvenv-virtual-env-name "] "))
          pyvenv-post-activate-hooks (list
                                      (lambda nil
                                        (setq python-shell-interpreter
                                              (concat pyvenv-virtual-env "bin/python"))))
          pyvenv-post-deactivate-hooks (list
                                        (lambda nil
                                          (setq python-shell-interpreter "python3"))))

    (diminish 'pyvenv-mode)))


;; FIXME: Leads to errors over tramp and possibly blocks Emacs
(when (and (executable-find "isort")
           (eq sb/python-langserver 'pyright))
  (unless (fboundp 'py-isort-before-save)
    (autoload #'py-isort-before-save "py-isort" nil t))

  (add-hook 'python-mode-hook
            (lambda nil
              (add-hook 'before-save-hook #'py-isort-before-save)))

  (with-eval-after-load "py-isort"
    (defvar py-isort-options)
    (setq py-isort-options '("-l 100"))))


(when nil
  (unless (fboundp 'pip-requirements-mode)
    (autoload #'pip-requirements-mode "pip-requirements" nil t)))


;; `pyright --createstub pandas'
(progn
  (declare-function lsp-pyright-locate-python "lsp-pyright")
  (declare-function lsp-pyright-locate-venv "lsp-pyright")

  (when (and (eq sb/python-langserver 'pyright)
             (executable-find "pyright"))
    (unless (fboundp 'lsp-pyright-locate-python)
      (autoload #'lsp-pyright-locate-python "lsp-pyright" nil t))
    (unless (fboundp 'lsp-pyright-locate-venv)
      (autoload #'lsp-pyright-locate-venv "lsp-pyright" nil t))

    (add-hook 'python-mode-hook (lambda ()
                                  (require 'lsp-pyright)))

    (defvar lsp-pyright-python-executable-cmd)
    (setq lsp-pyright-python-executable-cmd "python3")))


(progn
  (when (and (eq sb/python-langserver 'jedi)
             (executable-find "jedi-language-server"))
    (add-hook 'python-mode-hook (lambda nil
                                  (require 'lsp-jedi)))

    (defvar lsp-jedi-diagnostics-enable)
    (setq lsp-jedi-diagnostics-enable t)))

;; Initiate the lsp server after all the language server code has been processed
(add-hook 'python-mode-hook
          (lambda()
            (lsp-deferred)
            ;; We already have `flycheck' error summary listed on the modeline, but it seems `lsp'
            ;; may report additional errors
            ;; (lsp-modeline-diagnostics-mode -1)
            ))


;; Yapfify works on the original file, so that any project settings supported by YAPF itself are
;; used.
(when (and (eq sb/python-langserver 'pyright)
           (executable-find "yapf"))
  (unless (fboundp 'yapf-mode)
    (autoload #'yapf-mode "yapfify" nil t))

  ;; FIXME: The cursor loses its position after formatting, which is annoying
  (add-hook 'python-mode-hook #'yapf-mode)

  (with-eval-after-load "yapfify"
    (diminish 'yapf-mode)))


(when nil
  (unless (fboundp 'ein:ipynb-mode)
    (autoload #'ein:ipynb-mode "ein" nil t)))


(when nil
  (unless (fboundp 'cython-mode)
    (autoload #'cython-mode "cython-mode" nil t)))


(when nil
  (unless (fboundp 'jinja2-mode)
    (autoload #'jinja2-mode "jinja2-mode" nil t)))


(progn
  (unless (fboundp 'cperl-mode)
    (autoload #'cperl-mode "cperl-mode" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("latexmkrc\\'" . cperl-mode))

  (add-hook 'cperl-mode-hook #'lsp-deferred)

  (with-eval-after-load "cperl-mode"
    ;; Prefer CPerl mode to Perl mode
    (fset 'perl-mode 'cperl-mode)))


;; Try to delete `lsp-java-workspace-dir' if the JDTLS fails
(progn
  (add-hook 'java-mode-hook (lambda nil
                              (setq-default c-basic-offset 4
                                            c-set-style "java")
                              (lsp-deferred)))

  (with-eval-after-load "lsp-java"
    (defvar lsp-java-inhibit-message)
    (defvar lsp-java-java-path)
    (defvar lsp-java-save-actions-organize-imports)
    (defvar lsp-java-format-settings-url)
    (defvar lsp-java-format-settings-profile)

    (setq lsp-java-inhibit-message t
          lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java" ; Requires Java 11
          lsp-java-save-actions-organize-imports t
          lsp-java-format-settings-profile "Swarnendu"
          lsp-java-format-settings-url (expand-file-name
                                        "github/dotfiles/java/eclipse-format-swarnendu.xml"
                                        sb/user-home))))


(progn
  (unless (fboundp 'ant)
    (autoload #'ant "ant" nil t))
  (unless (fboundp 'ant-clean)
    (autoload #'ant-clean "ant" nil t))
  (unless (fboundp 'ant-compile)
    (autoload #'ant-compile "ant" nil t))
  (unless (fboundp 'ant-test)
    (autoload #'ant-test "ant" nil t)))


;; Can disassemble `.class' files from within jars
(progn
  (declare-function autodisass-java-bytecode "autodisass-java-bytecode")

  (unless (fboundp 'autodisass-java-bytecode)
    (autoload #'autodisass-java-bytecode "autodisass-java-bytecode" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.class\\'" . autodisass-java-bytecode)))


;; Syntax highlighting for Gradle files
(unless (fboundp 'groovy-mode)
  (autoload #'groovy-mode "groovy-mode" nil t))


;; SVG
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))


;; Shell script mode
(progn
  (unless (fboundp 'sh-mode)
    (autoload #'sh-mode "sh-script" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.zsh\\'"   . sh-mode))
;;;###autoload
  (add-to-list 'auto-mode-alist '("\\bashrc\\'" . sh-mode))

  (add-hook 'sh-mode-hook #'lsp-deferred)

  (with-eval-after-load "sh-script"
    (defvar sh-basic-offset)
    (defvar sh-indent-comment)
    (defvar sh-indent-after-continuation)

    (setq sh-basic-offset 2
          sh-indent-after-continuation 'always
          sh-indent-comment t)

    ;; (unbind-key "C-c C-d" sh-mode-map) ; Was bound to `sh-cd-here'

    (flycheck-add-next-checker 'sh-bash 'sh-shellcheck)))


(progn
  (unless (fboundp 'fish-mode)
    (autoload #'fish-mode "fish-mode" nil t))

  (unless (fboundp 'fish_indent-before-save)
    (autoload #'fish_indent-before-save "fish-mode" nil t))

  (add-hook 'fish-mode-hook
            (lambda nil
              (add-hook 'before-save-hook #'fish_indent-before-save))))


(progn
  (unless (fboundp 'shfmt-on-save-mode)
    (autoload #'shfmt-on-save-mode "shfmt" nil t))

  (add-hook 'sh-mode-hook #'shfmt-on-save-mode)

  (with-eval-after-load "shfmt"
    (defvar shfmt-arguments)

    (setq shfmt-arguments '("-i" "4" "-p" "-ci"))))


;; The following section helper ensures that files are given `+x' permissions when they are saved,
;; if they contain a valid shebang line
(progn
  (unless (fboundp 'executable-make-buffer-file-executable-if-script-p)
    (autoload #'executable-make-buffer-file-executable-if-script-p "executable" nil t))

  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))


;; Remove `vc-refresh-state' if we are not using `vc', i.e., `vc-handled-backends' is nil
(if (boundp 'vc-handled-backends)
    (add-hook 'find-file-hook #'vc-refresh-state)
  (remove-hook 'find-file-hook #'vc-refresh-state))


(with-eval-after-load "with-editor"
  (diminish 'with-editor-mode))


(progn
  (declare-function transient-bind-q-to-quit "transient")
  (unless (fboundp 'transient-bind-q-to-quit)
    (autoload #'transient-bind-q-to-quit "transient" nil t))

  (with-eval-after-load "transient"
    (defvar transient-history-file)
    (defvar transient-levels-file)
    (defvar transient-values-file)
    (defvar transient-display-buffer-action)

    (unless (bound-and-true-p sb/use-no-littering)
      (setq transient-history-file (expand-file-name "transient/history.el" sb/temp-directory)
            transient-levels-file (expand-file-name "transient/levels.el" sb/temp-directory)
            transient-values-file (expand-file-name "transient/values.el" sb/temp-directory)))

    (setq transient-display-buffer-action '(display-buffer-below-selected))
    ;; Allow using `q' to quit out of popups, in addition to `C-g'
    (transient-bind-q-to-quit)))


(progn
  (declare-function magit-display-buffer-fullframe-status-v1 "magit")

  (unless (fboundp 'magit-status)
    (autoload #'magit-status "magit" nil t))
  (unless (fboundp 'magit-file-dispatch)
    (autoload #'magit-file-dispatch "magit" nil t))
  (unless (fboundp 'magit-dispatch)
    (autoload #'magit-dispatch "magit" nil t))

  (with-eval-after-load "magit"
    (defvar magit-display-buffer-function)
    (defvar magit-no-message)
    (defvar magit-section-initial-visibility-alist)

    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
          ;; Suppress the message about "Turning on magit-auto-revert-mode" when loading Magit
          magit-no-message '("Turning on magit-auto-revert-mode...")
          ;; https://irreal.org/blog/?p=8877
          magit-section-initial-visibility-alist '((stashes   . show)
                                                   (untracked . show)
                                                   (unpushed  . show)
                                                   (unpulled  . show)))

    ;; These give a performance boost to magit
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

    (require 'magit-diff)

    (defvar magit-diff-refine-hunk)
    (defvar magit-diff-paint-whitespace)
    (defvar magit-diff-highlight-trailing)

    (setq magit-diff-refine-hunk t
          magit-diff-highlight-trailing nil
          magit-diff-paint-whitespace nil)

    (require 'ediff)

    (defvar ediff-window-setup-function)
    (defvar ediff-split-window-function)
    (declare-function ediff-setup-windows-plain "ediff")

    ;; Change default ediff style: do not start another frame with `ediff-setup-windows-default'
    (setq ediff-window-setup-function #'ediff-setup-windows-plain)
    ;; Split windows horizontally in ediff (instead of vertically)
    (setq ediff-split-window-function #'split-window-horizontally))

  (bind-keys :package magit
             ("C-x g"   . magit-status)
             ("C-c M-g" . magit-file-dispatch)
             ("C-x M-g" . magit-dispatch)))


(unless (fboundp 'gitignore-mode)
  (autoload #'gitignore-mode "gitignore-mode" nil t))


(unless (fboundp 'gitattributes-mode)
  (autoload #'gitattributes-mode "gitattributes-mode" nil t))


(unless (fboundp 'gitconfig-mode)
  (autoload #'gitconfig-mode "gitconfig-mode" nil t))


(unless (boundp 'vc-handled-backends)
  (declare-function git-gutter:previous-hunk "git-gutter")
  (declare-function git-gutter:next-hunk "git-gutter")
  (declare-function global-git-gutter-mode "git-gutter")

  (unless (fboundp 'git-gutter:previous-hunk)
    (autoload #'git-gutter:previous-hunk "git-gutter" nil t))
  (unless (fboundp 'git-gutter:next-hunk)
    (autoload #'git-gutter:next-hunk "git-gutter" nil t))
  (unless (fboundp 'global-git-gutter-mode)
    (autoload #'global-git-gutter-mode "git-gutter" nil t))

  (add-hook 'after-init-hook #'global-git-gutter-mode)

  (with-eval-after-load "git-gutter"
    (defvar git-gutter:added-sign)
    (defvar git-gutter:deleted-sign)
    (defvar git-gutter:modified-sign)
    (defvar git-gutter:update-interval)
    (defvar git-gutter:disabled-modes)

    (setq git-gutter:added-sign " "
          git-gutter:deleted-sign " "
          git-gutter:modified-sign " "
          git-gutter:update-interval 1
          ;; https://github.com/syl20bnr/spacemacs/issues/10555
          ;; https://github.com/syohex/emacs-git-gutter/issues/24
          git-gutter:disabled-modes '(fundamental-mode org-mode
                                                       image-mode doc-view-mode pdf-view-mode))

    (diminish 'git-gutter-mode))

  (bind-keys :package git-gutter
             ("C-x p" . git-gutter:previous-hunk)
             ("C-x n" . git-gutter:next-hunk)))


;; Diff-hl looks nicer than `git-gutter', based on `vc'
(when (boundp 'vc-handled-backends)
  (declare-function diff-hl-magit-pre-refresh "diff-hl")
  (declare-function diff-hl-magit-post-refresh "diff-hl")
  (declare-function diff-hl-dired-mode-unless-remote "diff-hl")
  (declare-function global-diff-hl-mode "diff-hl")

  (unless (fboundp 'diff-hl-magit-post-refresh)
    (autoload #'diff-hl-magit-post-refresh "diff-hl" nil t))
  (unless (fboundp 'diff-hl-magit-pre-refresh)
    (autoload #'diff-hl-magit-pre-refresh "diff-hl" nil t))
  (unless (fboundp 'diff-hl-dired-mode-unless-remote)
    (autoload #'diff-hl-dired-mode-unless-remote "diff-hl" nil t))
  (unless (fboundp 'global-diff-hl-mode)
    (autoload #'global-diff-hl-mode "diff-hl" nil t))

  (add-hook 'after-init-hook #'global-diff-hl-mode)
  (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)

  (with-eval-after-load "diff-hl"
    (defvar diff-hl-draw-borders)
    ;; Highlight without a border looks nicer
    (setq diff-hl-draw-borders nil)))


(progn
  (declare-function git-commit-turn-on-flyspell "git-commit")
  (unless (fboundp 'git-commit-turn-on-flyspell)
    (autoload #'git-commit-turn-on-flyspell "git-commit" nil t))

  (add-hook 'git-commit-setup-hook #'git-commit-turn-on-flyspell)

  (with-eval-after-load "git-commit"
    (defvar git-commit-summary-max-length)
    (setq git-commit-summary-max-length 50)))


;; Use the minor mode `smerge-mode' to move between conflicts and resolve them

(declare-function smerge-next "smerge-mode")
(declare-function smerge-prev "smerge-mode")
(declare-function smerge-keep-current "smerge-mode")
(declare-function smerge-keep-upper "smerge-mode")
(declare-function smerge-keep-lower "smerge-mode")
(declare-function smerge-keep-base "smerge-mode")
(declare-function smerge-keep-all "smerge-mode")
(declare-function smerge-kill-current "smerge-mode")
(declare-function smerge-context-menu "smerge-mode")
(declare-function smerge-popup-context-menu "smerge-mode")
(declare-function smerge-auto-leave "smerge-mode")
(declare-function smerge-diff-base-lower "smerge-mode")
(declare-function smerge-diff-base-upper "smerge-mode")
(declare-function smerge-diff-upper-lower "smerge-mode")
(declare-function smerge-refine "smerge-mode")
(declare-function smerge-combine-with-next "smerge-mode")
(declare-function smerge-resolve "smerge-mode")

(unless (fboundp 'smerge-next)
  (autoload #'smerge-next "smerge-mode" nil t))
(unless (fboundp 'smerge-prev)
  (autoload #'smerge-prev "smerge-mode" nil t))
(unless (fboundp 'smerge-keep-current)
  (autoload #'smerge-keep-current "smerge-mode" nil t))
(unless (fboundp 'smerge-keep-upper)
  (autoload #'smerge-keep-upper "smerge-mode" nil t))
(unless (fboundp 'smerge-keep-lower)
  (autoload #'smerge-keep-lower "smerge-mode" nil t))
(unless (fboundp 'smerge-keep-base)
  (autoload #'smerge-keep-base "smerge-mode" nil t))
(unless (fboundp 'smerge-keep-all)
  (autoload #'smerge-keep-all "smerge-mode" nil t))
(unless (fboundp 'smerge-ediff)
  (autoload #'smerge-ediff "smerge-mode" nil t))
(unless (fboundp 'smerge-kill-current)
  (autoload #'smerge-kill-current "smerge-mode" nil t))
(unless (fboundp 'smerge-context-menu)
  (autoload #'smerge-context-menu "smerge-mode" nil t))
(unless (fboundp 'smerge-popup-context-menu)
  (autoload #'smerge-popup-context-menu "smerge-mode" nil t))
(unless (fboundp 'smerge-auto-leave)
  (autoload #'smerge-auto-leave "smerge-mode" nil t))
(unless (fboundp 'smerge-diff-base-lower)
  (autoload #'smerge-diff-base-lower "smerge-mode" nil t))
(unless (fboundp 'smerge-diff-base-upper)
  (autoload #'smerge-diff-base-upper "smerge-mode" nil t))
(unless (fboundp 'smerge-diff-upper-lower)
  (autoload #'smerge-diff-upper-lower "smerge-mode" nil t))
(unless (fboundp 'smerge-refine)
  (autoload #'smerge-refine "smerge-mode" nil t))
(unless (fboundp 'smerge-combine-with-next)
  (autoload #'smerge-combine-with-next "smerge-mode" nil t))
(unless (fboundp 'smerge-resolve)
  (autoload #'smerge-resolve "smerge-mode" nil t))

(defvar smerge-mode)
(declare-function sb/smerge-hydra/body "init-autoload")

(add-hook 'find-file-hook #'sb/enable-smerge-maybe2 :append)
(add-hook 'magit-diff-visit-file-hook (lambda nil
                                        (when smerge-mode
                                          (sb/smerge-hydra/body))))

(eval-and-compile
  (defun sb/enable-smerge-maybe nil
    "Enable smerge automatically based on conflict markers."
    (when (and buffer-file-name
               (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1)))))

  (defun sb/enable-smerge-maybe2 nil
    "Enable `smerge-mode' automatically."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1)))))

(defvar smerge-mode-map)
(bind-keys :package smerge-mode :map smerge-mode-map
           ("M-g n"   . smerge-next)
           ("M-g p"   . smerge-prev)
           ("M-g k c" . smerge-keep-current)
           ("M-g k m" . smerge-keep-upper)
           ("M-g k o" . smerge-keep-lower)
           ("M-g k b" . smerge-keep-base)
           ("M-g k a" . smerge-keep-all)
           ("M-g e"   . smerge-ediff)
           ("M-g K"   . smerge-kill-current)
           ("M-g m"   . smerge-context-menu)
           ("M-g M"   . smerge-popup-context-menu))


(progn
  (unless (fboundp 'yaml-mode)
    (autoload #'yaml-mode "yaml-mode" nil t))

;;;###autoload
  (progn
    (add-to-list 'auto-mode-alist '(".clang-format" . yaml-mode))
    (add-to-list 'auto-mode-alist '(".clang-tidy"   . yaml-mode)))

  (add-hook 'yaml-mode-hook (lambda ()
                              (spell-fu-mode -1) ; `yaml-mode' is derived from `text-mode'
                              (lsp-deferred)))

  (with-eval-after-load "yaml-mode"
    (unless (fboundp 'yaml-imenu-enable)
      (autoload #'yaml-imenu-enable "yaml-imenu" nil t))

    (yaml-imenu-enable)))


(progn
  (unless (fboundp 'bat-mode)
    (autoload #'bat-mode "bat-mode" nil t))

;;;###autoload
  (progn
    (add-to-list 'auto-mode-alist '("\\.bat\\'" . bat-mode))
    (add-to-list 'auto-mode-alist '("\\.cmd\\'" . bat-mode))))


(progn
  (unless (fboundp 'web-mode)
    (autoload #'web-mode "web-mode" nil t))

;;;###autoload
  (dolist (pattern '("\\.html?\\'" "\\.djhtml\\'" "\\.phtml\\'" "\\.hb\\.html\\'"
                     "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'"
                     "\\.mustache\\'" "\\.handlebars\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'web-mode)))

  (add-hook 'web-mode-hook #'lsp-deferred)

  (with-eval-after-load "web-mode"
    (defvar web-mode-enable-auto-closing)
    (defvar web-mode-enable-auto-pairing)
    (defvar web-mode-enable-auto-quoting)
    (defvar web-mode-enable-block-face)
    (defvar web-mode-enable-css-colorization)
    (defvar web-mode-enable-current-element-highlight)
    (defvar web-mode-enable-current-column-highlight)

    (setq web-mode-enable-auto-closing t
          web-mode-enable-auto-pairing t
          web-mode-enable-auto-quoting t
          web-mode-enable-block-face t
          web-mode-enable-css-colorization t
          web-mode-enable-current-element-highlight t
          web-mode-enable-current-column-highlight t)))


(progn
  (unless (fboundp 'emmet-mode)
    (autoload #'emmet-mode "emmet-mode" nil t))

  (dolist (hook '(web-mode-hook sgml-mode-hook css-mode-hook html-mode-hook))
    (add-hook hook #'emmet-mode)))


(progn
  (unless (fboundp 'rainbow-mode)
    (autoload #'rainbow-mode "rainbow-mode" nil t))

  (dolist (hook '(html-mode-hook css-mode-hook sass-mode-hook))
    (add-hook hook #'rainbow-mode))

  (with-eval-after-load "rainbow-mode"
    (diminish 'rainbow-mode)))


(when nil
  (add-hook 'php-mode-hook #'lsp-deferred))


(progn
  (unless (fboundp 'nxml-mode)
    (autoload #'nxml-mode "nxml-mode" nil t))

  (add-hook 'nxml-mode-hook (lambda()
                              (spell-fu-mode -1)
                              (lsp-deferred)))

;;;###autoload
  (dolist (pattern '("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$"))
    (add-to-list 'auto-mode-alist (cons pattern 'nxml-mode)))

  (with-eval-after-load "nxml-mode"
    (fset 'xml-mode 'nxml-mode)

    (defvar nxml-auto-insert-xml-declaration-flag)
    (defvar nxml-slash-auto-complete-flag)

    (setq nxml-auto-insert-xml-declaration-flag t
          nxml-slash-auto-complete-flag t)))


;; https://github.com/ROCKTAKEY/lsp-latex/issues/26
;; Texlab seems to have high overhead
(when t
  (progn
    (dolist (hook '(latex-mode-hook LaTeX-mode-hook))
      (add-hook hook (lambda nil
                       (require 'lsp-latex)
                       (lsp-deferred))))

    (with-eval-after-load "lsp-latex"
      (defvar lsp-latex-bibtex-formatting-formatter)
      (defvar lsp-latex-bibtex-formatting-line-length)
      (defvar lsp-latex-build-on-save)
      (defvar lsp-latex-lint-on-save)
      (defvar lsp-latex-build-args)

      (setq lsp-latex-bibtex-formatting-formatter "latexindent"
            lsp-latex-bibtex-formatting-line-length sb/fill-column
            lsp-latex-build-on-save t
            lsp-latex-lint-on-save t)

      (add-to-list 'lsp-latex-build-args "-c")
      (add-to-list 'lsp-latex-build-args "-pvc"))
    ))


;; Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides the tex
;; package.

(declare-function LaTeX-math-mode "tex")
(declare-function TeX-PDF-mode "tex")
(declare-function TeX-source-correlate-mode "tex")
(declare-function TeX-save-document "tex")
(declare-function TeX-command-menu "tex")
(declare-function TeX-revert-document-buffer "tex")

(unless (fboundp 'tex-site)
  (autoload 'tex-site "tex-site.el" nil t))
(unless (fboundp 'LaTeX-mode)
  (autoload #'LaTeX-mode "tex" nil t))
(unless (fboundp 'LaTeX-math-mode)
  (autoload #'LaTeX-math-mode "tex" nil t))
(unless (fboundp 'TeX-PDF-mode)
  (autoload #'TeX-PDF-mode "tex" nil t))
(unless (fboundp 'TeX-source-correlate-mode)
  (autoload #'TeX-source-correlate-mode "tex" nil t))
(unless (fboundp 'rainbow-delimiters-mode)
  (autoload #'rainbow-delimiters-mode "tex" nil t))
(unless (fboundp 'TeX-active-process)
  (autoload #'TeX-active-process "tex" nil t))
(unless (fboundp 'TeX-save-document)
  (autoload #'TeX-save-document "tex" nil t))
(unless (fboundp 'TeX-command-menu)
  (autoload #'TeX-command-menu "tex" nil t))
(unless (fboundp 'TeX-revert-document-buffer)
  (autoload #'TeX-revert-document-buffer "tex" nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(dolist (hook '(LaTex-mode-hook latex-mode-hook))
  (add-hook hook (lambda()
                   (LaTeX-math-mode)
                   (TeX-PDF-mode) ; Use `pdflatex'
                   (TeX-source-correlate-mode))))

(with-eval-after-load "tex"
  (defvar TeX-auto-save)
  (defvar TeX-auto-untabify)
  (defvar TeX-clean-confirm)
  (defvar TeX-electric-sub-and-superscript)
  (defvar TeX-parse-self)
  (defvar TeX-quote-after-quote)
  (defvar TeX-save-query)
  (defvar TeX-source-correlate-method)
  (defvar TeX-source-correlate-start-server)
  (defvar TeX-syntactic-comment)
  (defvar TeX-view-program-selection)
  (defvar TeX-view-program-list)
  (defvar LaTeX-item-indent)
  (defvar LaTeX-syntactic-comments)
  (defvar LaTeX-fill-break-at-separators)
  (defvar tex-fontify-script)
  (defvar font-latex-fontify-script)
  (defvar font-latex-fontify-sectioning)

  (setq TeX-auto-save t ; Enable parse on save, stores parsed information in an `auto' directory
        TeX-auto-untabify t ; Remove all tabs before saving
        TeX-clean-confirm nil
        TeX-electric-sub-and-superscript t ; Automatically insert braces in math mode
        TeX-parse-self t ; Parse documents
        TeX-quote-after-quote nil ; Allow original LaTeX quotes
        TeX-save-query nil ; Save buffers automatically when compiling
        TeX-source-correlate-method 'synctex
        ;; Do not start the emacs server when correlating sources
        TeX-source-correlate-start-server t
        TeX-syntactic-comment t
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        LaTeX-item-indent 0 ; Indent lists by two spaces
        LaTeX-syntactic-comments t
        LaTeX-fill-break-at-separators nil ; Do not insert line-break at inline math
        tex-fontify-script nil ; Avoid raising of superscripts and lowering of subscripts
        ;; Avoid superscripts and subscripts from being displayed in a different font size
        font-latex-fontify-script nil
        ;; Avoid emphasizing section headers
        font-latex-fontify-sectioning 1.0)

  (setq-default TeX-master nil) ; Query for master file

  ;; Revert PDF buffer after TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)

  ;; Disable "LaTeX-insert-item" in favor of imenu
  ;; (unbind-key "C-c C-d" LaTeX-mode-map)
  ;; Unset "C-c ;" since we want to bind it to 'comment-line
  ;; (unbind-key "C-c ;" LaTeX-mode-map)
  )


(with-eval-after-load "tex-mode"
  (declare-function auctex-latexmk "auctex-latexmk")

  (unless (fboundp 'auctex-latexmk)
    (autoload #'auctex-latexmk "auctex-latexmk" nil t))

  (auctex-latexmk-setup)

  (defvar auctex-latexmk-inherit-TeX-PDF-mode)
  (defvar TeX-command-default)

  ;; Pass the `-pdf' flag when `TeX-PDF-mode' is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t
        TeX-command-default "LatexMk"))


;; (add-hook 'bibtex-mode-hook #'lsp-deferred) ;; LATER: LaTeX LS is not good yet


(with-eval-after-load "bibtex"
  (defvar bibtex-align-at-equal-sign)
  (defvar bibtex-maintain-sorted-entries)

  (setq bibtex-align-at-equal-sign t
        bibtex-maintain-sorted-entries t))


(when (eq sb/selection 'ivy)
  (progn
    (declare-function ivy-bibtex "ivy-bibtex")

    (unless (fboundp 'ivy-bibtex)
      (autoload #'ivy-bibtex "ivy-bibtex" nil t))

    (with-eval-after-load "ivy-bibtex"
      (defvar ivy-bibtex-default-action)
      (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

      (require 'bibtex-completion)

      (defvar bibtex-completion-cite-default-as-initial-input)
      (defvar bibtex-completion-cite-prompt-for-optional-arguments)
      (defvar bibtex-completion-display-formats)

      (setq bibtex-completion-cite-default-as-initial-input t
            bibtex-completion-cite-prompt-for-optional-arguments nil
            bibtex-completion-display-formats
            '((t . "${author:24} ${title:*} ${=key=:16} ${=type=:12}"))))

    (bind-keys :package ivy-bibtex
               ("C-c x b" . ivy-bibtex))))


(declare-function reftex-reference "reftex")
(declare-function reftex-label "reftex")
(declare-function reftex-get-bibfile-list "reftex")
(declare-function reftex-default-bibliography "reftex")
(declare-function bibtex-parse-keys "reftex")
(declare-function bib-apropos "reftex")
(declare-function bib-make-bibliography "reftex")
(declare-function bib-display "reftex")
(declare-function bib-etags "reftex")
(declare-function bib-find "reftex")
(declare-function bib-find-next "reftex")
(declare-function bib-highlight-mouse "reftex")

(unless (fboundp 'reftex-mode)
  (autoload #'reftex-mode "reftex" nil t))
(unless (fboundp 'reftex-citation)
  (autoload #'reftex-citation "reftex" nil t))
(unless (fboundp 'reftex-reference)
  (autoload #'reftex-reference "reftex" nil t))
(unless (fboundp 'reftex-label)
  (autoload #'reftex-label "reftex" nil t))
(unless (fboundp 'reftex-get-bibfile-list)
  (autoload #'reftex-get-bibfile-list "reftex" nil t))
(unless (fboundp 'bibtex-parse-keys)
  (autoload #'bibtex-parse-keys "reftex" nil t))
(unless (fboundp 'reftex-default-bibliography)
  (autoload #'reftex-default-bibliography "reftex" nil t))

(add-hook 'LaTeX-mode-hook #'reftex-mode)
(add-hook 'latex-mode-hook #'reftex-mode)

;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
(eval-and-compile
  (defun sb/get-bibtex-keys (file)
    (with-current-buffer
        (find-file-noselect file)
      (mapcar 'car
              (bibtex-parse-keys))))

  (defun sb/reftex-add-all-bibitems-from-bibtex nil
    (interactive)
    (mapc 'LaTeX-add-bibitems
          (apply 'append
                 (mapcar 'sb/get-bibtex-keys
                         (reftex-get-bibfile-list)))))

  (defun sb/find-bibliography-file nil
    "Try to find a bibliography file using RefTeX.
Returns a string with text properties (as expected by
read-file-name) or empty string if no file can be found"
    (interactive)
    (let
        ((bibfile-list nil))
      (condition-case nil
          (setq bibfile-list
                (reftex-get-bibfile-list))
        (error
         (ignore-errors
           (setq bibfile-list
                 (reftex-default-bibliography)))))
      (if bibfile-list
          (car bibfile-list)
        "")))

  (defun sb/reftex-try-add-all-bibitems-from-bibtex nil
    "Try to find a bibliography file using RefTex and parse the bib keys.
Ignore if no file is found."
    (interactive)
    (let
        ((bibfile-list nil))
      (condition-case nil
          (setq bibfile-list
                (reftex-get-bibfile-list))
        (error
         (ignore-errors
           (setq bibfile-list
                 (reftex-default-bibliography)))))
      (mapc 'LaTeX-add-bibitems
            (apply 'append
                   (mapcar 'sb/get-bibtex-keys bibfile-list))))))

(with-eval-after-load "reftex"
  (defvar reftex-enable-partial-scans)
  (defvar reftex-highlight-selection)
  (defvar reftex-plug-into-AUCTeX)
  (defvar reftex-save-parse-info)
  (defvar reftex-toc-follow-mode)
  (defvar reftex-use-multiple-selection-buffers)
  (defvar reftex-toc-split-windows-horizontally)
  (defvar reftex-toc-split-windows-fraction)

  (setq reftex-enable-partial-scans t
        reftex-highlight-selection 'both
        reftex-plug-into-AUCTeX t
        reftex-save-parse-info t
        reftex-toc-follow-mode t ; Other buffer follows the point in toc buffer
        reftex-use-multiple-selection-buffers t
        ;; Make the toc displayed on the left
        reftex-toc-split-windows-horizontally t
        ;; Adjust the fraction
        reftex-toc-split-windows-fraction 0.3)

  (sb/reftex-try-add-all-bibitems-from-bibtex)

  ;; (add-hook 'reftex-load-hook #'sb/reftex-add-all-bibitems-from-bibtex)
  ;; (add-hook 'reftex-toc-mode-hook #'reftex-toc-rescan)

  (diminish 'reftex-mode))

(bind-keys :package reftex
           ("C-c [" . reftex-citation)
           ("C-c )" . reftex-reference)
           ("C-c (" . reftex-label))


(progn
  (unless (fboundp 'bib-apropos)
    (autoload #'bib-apropos "bib-cite" nil t))
  (unless (fboundp 'bib-make-bibliography)
    (autoload #'bib-make-bibliography "bib-cite" nil t))
  (unless (fboundp 'bib-display)
    (autoload #'bib-display "bib-cite" nil t))
  (unless (fboundp 'bib-etags)
    (autoload #'bib-etags "bib-cite" nil t))
  (unless (fboundp 'bib-find)
    (autoload #'bib-find "bib-cite" nil t))
  (unless (fboundp 'bib-find-next)
    (autoload #'bib-find-next "bib-cite" nil t))
  (unless (fboundp 'bib-highlight-mouse)
    (autoload #'bib-highlight-mouse "bib-cite" nil t))
  (unless (fboundp 'bib-cite-minor-mode)
    (autoload #'bib-cite-minor-mode "bib-cite" nil t))

  (dolist (hook '(latex-mode-hook LaTex-mode-hook))
    (add-hook hook (lambda nil
                     (bib-cite-minor-mode 1))))

  (with-eval-after-load "bib-cite"
    (defvar bib-cite-use-reftex-view-crossref)
    (setq bib-cite-use-reftex-view-crossref t)

    (diminish 'bib-cite-minor-mode))

  (defvar bib-cite-minor-mode-map)
  (bind-keys :package bib-cite :map bib-cite-minor-mode-map
             ("C-c b") ; We use `C-c b' for `comment-box'
             ("C-c l a" . bib-apropos)
             ("C-c l b" . bib-make-bibliography)
             ("C-c l d" . bib-display)
             ("C-c l t" . bib-etags)
             ("C-c l f" . bib-find)
             ("C-c l n" . bib-find-next)
             ("C-c l h" . bib-highlight-mouse)))


;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
(declare-function TeX-active-process "tex" ())

(defun sb/save-buffer-and-run-latexmk ()
  "Save the current buffer and run LaTeXMk also."
  (interactive)
  (require 'tex)
  (require 'tex-buf)
  (let ((process (TeX-active-process)))
    (if process (delete-process process)))
  (let ((TeX-save-query nil))
    (TeX-save-document ""))
  (TeX-command-menu "LaTeXMk"))

;; (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
;;   (add-hook hook
;;             (lambda ()
;;               (add-hook 'after-save-hook
;;                         (lambda ()
;;                           (sb/save-buffer-and-run-latexmk)) nil t))))

(with-eval-after-load "latex"
  (defvar LaTeX-mode-map)

  (bind-key "C-x C-s" #'sb/save-buffer-and-run-latexmk LaTeX-mode-map))


(progn
  (unless (fboundp 'math-preview-all)
    (autoload #'math-preview-all "math-preview" nil t))
  (unless (fboundp 'math-preview-at-point)
    (autoload #'math-preview-at-point "math-preview" nil t))
  (unless (fboundp 'math-preview-region)
    (autoload #'math-preview-region "math-preview" nil t))

  (with-eval-after-load "math-preview"
    (defvar math-preview-command)

    (setq math-preview-command (expand-file-name "node_modules/.bin/math-preview" sb/user-tmp))))


(progn
  (unless (fboundp 'texinfo-mode)
    (autoload #'texinfo-mode "texinfo" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.texi\\'" . texinfo-mode)))


(when nil
  (progn
    (unless (fboundp 'js2-mode)
      (autoload #'js2-mode "js2-mode" nil t))
    (unless (fboundp 'js2-imenu-extras-mode)
      (autoload #'js2-imenu-extras-mode "js2-mode" nil t))

;;;###autoload
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

    (add-hook 'js2-mode-hook #'lsp-deferred)
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

    (with-eval-after-load "js2-mode"
      (defalias 'javascript-mode 'js2-mode "`js2-mode' is aliased to `javascript' mode")

      (defvar js-indent-level)
      (defvar js2-basic-offset)

      ;; TODO: Are the two variables aliased?
      (setq js-indent-level 2
            js2-basic-offset 2)

      (unless (fboundp 'js2-refactor-mode)
        (autoload #'js2-refactor-mode "js2-refactor" nil t))

      (js2-refactor-mode 1)

      (diminish 'js2-refactor-mode))


    (when (executable-find "rg")
      (unless (fboundp 'xref-js2-xref-backend)
        (autoload #'xref-js2-xref-backend "xref-js2" nil t))

      (add-hook 'js2-mode-hook
                (lambda ()
                  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

      (with-eval-after-load "xref-js2"
        (defvar xref-js2-search-program)

        (setq xref-js2-search-program 'rg)))

    ;; LATER: `js-mode' (which js2 is based on) binds `M-.' which conflicts with `xref', so unbind it
    ;; (define-key js-mode-map (kbd "M-.") nil)
    ))


;; The Melpa package does not include support for `jsonc-mode'. A pull request is pending.
(progn
  (declare-function json-mode "json")
  (declare-function jsonc-mode "json")

  (unless (fboundp 'json-mode)
    (autoload #'json-mode "json-mode" nil t))
  (unless (fboundp 'jsonc-mode)
    (autoload #'jsonc-mode "json-mode" nil t))

;;;###autoload
  (progn
    (add-to-list 'auto-mode-alist '("\\.json\\'"                  . json-mode))
    (add-to-list 'auto-mode-alist '(".*/\\.vscode/settings.json$" . jsonc-mode))
    (add-to-list 'auto-mode-alist '("User/settings.json$"         . jsonc-mode)))

  (dolist (hook '(json-mode-hook jsonc-mode-hook))
    (add-hook hook (lambda nil
                     (defvar js-indent-level)
                     (make-local-variable 'js-indent-level)
                     (setq js-indent-level 2)
                     (lsp-deferred)))))


(when nil
  (progn
    (unless (fboundp 'scss-mode)
      (autoload #'scss-mode "scss-mode" nil t))

    (add-hook 'scss-mode-hook #'lsp-deferred)

    (with-eval-after-load "scss-mode"
      (defvar scss-compile-at-save)

      (setq scss-compile-at-save t))))


(when nil
  (progn
    (unless (fboundp 'sass-mode)
      (autoload #'sass-mode "sass-mode" nil t))

    (add-hook 'sass-mode-hook #'lsp-deferred)))


(when nil
  (progn
    (declare-function bazel-mode "bazel-mode")

    (unless (fboundp 'bazel-mode)
      (autoload #'bazel-mode "bazel-mode" nil t))
    (unless (fboundp 'bazelrc-mode)
      (autoload #'bazelrc-mode "bazel-mode" nil t))

    (add-hook 'bazel-mode-hook #'flycheck-mode)))


(when nil
  (progn
    (unless (fboundp 'protobuf-mode)
      (autoload #'protobuf-mode "protobuf-mode" nil t))

;;;###autoload
    (add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

    (add-hook 'protobuf-mode-hook #'flycheck-mode)))


(when nil
  (unless (fboundp 'mlir-mode)
    (autoload #'mlir-mode "mlir-mode")))


(when nil
  (progn
    (with-eval-after-load "mlir-mode"
      (unless (fboundp 'clang-format)
        (autoload #'clang-format "clang-format" nil t))
      (unless (fboundp 'clang-format-buffer)
        (autoload #'clang-format-buffer "clang-format" nil t))
      (unless (fboundp 'clang-format-region)
        (autoload #'clang-format-region "clang-format" nil t)))

    (unless (fboundp 'clang-format+-mode)
      (autoload #'clang-format+-mode "clang-format+" nil t))

    (add-hook 'mlir-mode-hook #'clang-format+-mode)

    (with-eval-after-load "clang-format+-mode"
      (defvar clang-format+-always-enable)

      (setq clang-format+-always-enable t))
    ))


;; Use for major modes which do not provide a formatter. `aphelia' allows for formatting via a
;; background process but does not support tramp and supports fewer formatters.
(progn
  (declare-function format-all-ensure-formatter "format-all")

  (unless (fboundp 'format-all-ensure-formatter)
    (autoload #'format-all-ensure-formatter "format-all" nil t))
  (unless (fboundp 'format-all-buffer)
    (autoload #'format-all-buffer "format-all" nil t))

  (eval-and-compile
    (defun sb/enable-format-all ()
      "Delay enabling format-all to avoid slowing down Emacs startup."
      (dolist (hook '(bazel-mode-hook latex-mode-hook LaTeX-mode-hook json-mode-hook
                                      markdown-mode-hook gfm-mode-hook))
        (add-hook hook #'format-all-mode))
      (add-hook 'format-all-mode-hook #'format-all-ensure-formatter)))

  (run-with-idle-timer 2 nil #'sb/enable-format-all))


(when nil
  (progn
    (unless (fboundp 'global-tree-sitter-mode)
      (autoload #'global-tree-sitter-mode "tree-sitter" nil t))
    (unless (fboundp 'tree-sitter-hl-mode)
      (autoload #'tree-sitter-hl-mode "tree-sitter-hl" nil t))

    (dolist (hook '(sh-mode-hook c-mode-hook c++-mode-hook
                                 css-mode-hook html-mode-hook
                                 java-mode-hook js-mode-hook
                                 js2-mode-hook json-mode-hook
                                 jsonc-mode-hook php-mode-hook
                                 python-mode-hook
                                 typescript-mode-hook))
      (add-hook hook (lambda nil
                       (require 'tree-sitter)
                       (require 'tree-sitter-langs)
                       (require 'tree-sitter-hl)

                       (global-tree-sitter-mode 1))))

    (with-eval-after-load "tree-sitter"
      (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

      (diminish 'tree-sitter-mode))))


(when nil
  (progn
    (unless (fboundp 'adoc-mode)
      (autoload #'adoc-mode "adoc-mode" nil t))

;;;###autoload
    (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))))


(when nil
  (when (executable-find "editorconfig")
    (progn
      (unless (fboundp 'editorconfig-mode)
        (autoload #'editorconfig-mode "editorconfig" nil t))

      (with-eval-after-load "editorconfig"
        (diminish 'editorconfig-mode)))))


;; Hooks into to `find-file-hook' to add all visited files and directories to `fasd'
(when (executable-find "fasd")
  (progn
    (unless (fboundp 'global-fasd-mode)
      (autoload #'global-fasd-mode "fasd" nil t))
    (unless (fboundp 'fasd-find-file)
      (autoload #'fasd-find-file "fasd" nil t))

    (run-with-idle-timer 2 nil #'global-fasd-mode)

    (with-eval-after-load "fasd"
      (defvar fasd-enable-initial-prompt)

      (setq fasd-enable-initial-prompt nil))

    (bind-keys :package fasd
               ("C-c /" . fasd-find-file))))


(when nil
  (unless (fboundp 'toml-mode)
    (autoload #'toml-mode "toml-mode" nil t)))


(when nil
  (unless (fboundp 'nix-mode)
    (autoload #'nix-mode "nix-mode" nil t)))


(when nil
  (progn
    (unless (fboundp 'rust-mode)
      (autoload #'rust-mode "rust-mode" nil t))

    (add-hook 'rust-mode-hook #'lsp-deferred)

    (with-eval-after-load "rust-mode"
      (defvar rust-format-on-save)

      (setq rust-format-on-save t))))


;; https://github.com/purcell/emacs.d/blob/master/lisp/init-compile.el
(declare-function ansi-color-apply-on-region "ansi-color")

(with-eval-after-load "compile"
  (require 'ansi-color)
  (defvar compilation-filter-start)

  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))


;; (defun sb/colorize-compilation-buffer ()
;;   "Colorize compile mode output."
;;   (require 'ansi-color)
;;   (let ((inhibit-read-only t))
;;     (ansi-color-apply-on-region (point-min) (point-max))))

;; (add-hook 'compilation-filter-hook #'sb/colorize-compilation-buffer)


(progn
  (declare-function info-colors-fontify-node "info-colors")

  (unless (fboundp 'info-colors-fontify-node)
    (autoload #'info-colors-fontify-node "info-colors" nil t))

  (with-eval-after-load "info"
    (add-hook 'Info-selection-hook #'info-colors-fontify-node)))


(progn
  (with-eval-after-load "flycheck"
    ;; https://github.com/flycheck/flycheck/issues/1762
    (defvar-local sb/flycheck-local-cache nil)

    (defun sb/flycheck-checker-get (fn checker property)
      (or (alist-get property (alist-get checker sb/flycheck-local-cache))
          (funcall fn checker property)))

    (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get)

    ;; Use per-project directory local variables
    ;; (add-hook 'lsp-managed-mode-hook
    ;;           (lambda ()
    ;;             (when (derived-mode-p 'python-mode)
    ;;               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (python-pylint)))))))

    ;;             (when (derived-mode-p 'sh-mode)
    ;;               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (sh-shellcheck)))))))

    ;;             (when (derived-mode-p 'c++-mode)
    ;;               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (c/c++-cppcheck)))))))

    ;;             (when (derived-mode-p 'css-mode)
    ;;               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (css-stylelint)))))))

    ;;             (when (derived-mode-p 'html-mode)
    ;;               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (html-tidy)))))))

    ;;             (when (derived-mode-p 'xml-mode)
    ;;               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (xml-xmllint)))))))

    ;;             (when (derived-mode-p 'yaml-mode)
    ;;               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (yaml-yamllint)))))))

    ;;             (when (derived-mode-p 'json-mode)
    ;;               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (json-jsonlint)))))))
    ;;             ))
    ))


;; A few backends are applicable to all modes and can be blocking: `company-yasnippet',
;; `company-ispell', and `company-dabbrev'. `company-dabbrev' returns a non-nil prefix in almost any
;; context (major mode, inside strings or comments). That is why it is better to put it at the end.

;; https://tychoish.com/post/better-company/
;; https://www.reddit.com/r/emacs/comments/l03dy1/priority_for_companymode/
;; https://emacs.stackexchange.com/questions/64038/how-to-use-multiple-backends-in-priority-for-company-mode

;; Try completion backends in order till there is a non-empty completion list
;; (setq company-backends '(company-xxx company-yyy company-zzz))
;; Merge completions of all the backends
;; (setq company-backends '((company-xxx company-yyy company-zzz)))
;; Merge completions of all the backends, give priority to `company-xxx'
;; (setq company-backends '((company-xxx :separate company-yyy company-zzz)))

;; If the group contains keyword `:with', the backends listed after this keyword are ignored for
;; the purpose of the `prefix' command. If the group contains keyword `:separate', the candidates
;; that come from different backends are sorted separately in the combined list.

(declare-function company-capf "company-capf")

(unless (fboundp 'company-dabbrev)
  (autoload #'company-dabbrev "company-dabbrev" nil t))
(unless (fboundp 'company-capf)
  (autoload #'company-capf "company-capf" nil t))
(unless (fboundp 'company-files)
  (autoload #'company-files "company-files" nil t))
(unless (fboundp 'company-ispell)
  (autoload #'company-ispell "company-ispell" nil t))
(unless (fboundp 'company-yasnippet)
  (autoload #'company-yasnippet "company-yasnippet" nil t))


(progn
  (defun sb/company-text-mode ()
    "Add backends for text completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    ;; Slightly larger value to have more precise matches and so that the popup does not block
    (setq-local company-minimum-prefix-length 3)
    (set (make-local-variable 'company-backends)
         '(company-files
           ;; Give priority to dabbrev completions over ispell
           (:separate
            company-dabbrev
            company-ispell)
           )))

  (dolist (hook '(text-mode-hook)) ; Extends to `markdown-mode' and `org-mode'
    (add-hook hook #'sb/company-text-mode)))


(defun sb/company-xml-mode ()
  "Add backends for completion with company."
  (defvar company-minimum-prefix-length)
  (defvar company-backends)

  (setq-local company-minimum-prefix-length 2)
  (make-local-variable 'company-backends)
  (setq company-backends '(company-capf
                           company-files
                           company-yasnippet
                           company-dabbrev-code
                           company-dabbrev)))

(dolist (hook '(nxml-mode-hook))
  (add-hook hook (lambda ()
                   (sb/company-xml-mode))))


(progn
  (defun sb/company-prog-mode ()
    "Add backends for program completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev-code
                             company-dabbrev)))

  (add-hook 'prog-mode-hook #'sb/company-prog-mode))


(progn
  (defun sb/company-java-mode ()
    "Add backends for Java completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev-code
                             company-dabbrev)))

  (add-hook 'java-mode-hook #'sb/company-java-mode))


;; https://emacs.stackexchange.com/questions/19072/company-completion-very-slow
;; `company-clang' is slow
(progn
  (defun sb/company-c-mode ()
    "Add backends for C/C++ completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev-code
                             company-dabbrev)))

  (add-hook 'c-mode-common-hook #'sb/company-c-mode))


(progn
  (unless (fboundp 'company-shell)
    (autoload #'company-shell "company-shell" nil t))
  (unless (fboundp 'company-shell-env)
    (autoload #'company-shell-env "company-shell" nil t))

  (defun sb/company-sh-mode ()
    "Add backends for shell script completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)
    (defvar company-shell-delete-duplictes)

    (setq company-shell-delete-duplictes t)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    (setq company-backends '((:separate
                              company-capf
                              company-shell
                              company-shell-env
                              company-fish-shell
                              company-dabbrev-code)
                             company-yasnippet
                             company-files
                             company-dabbrev)))

  (add-hook 'sh-mode-hook #'sb/company-sh-mode))

(progn
  (unless (fboundp 'company-fish-shell)
    (autoload #'company-fish-shell "company-shell" nil t))

  (defun sb/company-fish-mode ()
    "Add backends for fish shell script completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    (setq company-backends '((:separate
                              company-capf
                              company-fish-shell
                              company-dabbrev-code)
                             company-yasnippet
                             company-files
                             company-dabbrev)))

  (add-hook 'fish-mode-hook #'sb/company-fish-mode))


(defun sb/company-elisp-mode ()
  "Set up company for elisp mode."
  (defvar company-minimum-prefix-length)
  (defvar company-backends)

  (setq-local company-minimum-prefix-length 2)
  (make-local-variable 'company-backends)
  (setq company-backends '(company-capf
                           company-files
                           company-yasnippet
                           company-dabbrev-code
                           company-dabbrev
                           company-ispell)))

(add-hook 'emacs-lisp-mode-hook #'sb/company-elisp-mode)


(progn
  (defun sb/company-python-mode ()
    "Add backends for Python completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    ;; `company-dabbrev-code' is useful for variable names.
    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev-code
                             company-dabbrev)))

  (add-hook 'python-mode-hook #'sb/company-python-mode))


(progn
  ;; (unless (fboundp 'company-bibtex)
  ;;   (autoload #'company-bibtex "company-bibtex" nil t))
  (unless (fboundp 'company-math-symbols-latex)
    (autoload #'company-math-symbols-latex "company-math" nil t))
  (unless (fboundp 'company-math-symbols-unicode)
    (autoload #'company-math-symbols-unicode "company-math" nil t))
  (unless (fboundp 'company-latex-commands)
    (autoload #'company-latex-commands "company-math" nil t))
  (unless (fboundp 'company-reftex-labels)
    (autoload #'company-reftex-labels "company-reftex" nil t))
  (unless (fboundp 'company-reftex-citations)
    (autoload #'company-reftex-citations "company-reftex" nil t))
  (unless (fboundp 'company-auctex-labels)
    (autoload #'company-auctex-labels "company-auctex" nil t))
  (unless (fboundp 'company-auctex-bibs)
    (autoload #'company-auctex-bibs "company-auctex" nil t))
  (unless (fboundp 'company-auctex-init)
    (autoload #'company-auctex-init "company-auctex" nil t))
  (unless (fboundp 'company-auctex-macros)
    (autoload #'company-auctex-macros "company-auctex" nil t))
  (unless (fboundp 'company-auctex-symbols)
    (autoload #'company-auctex-symbols "company-auctex" nil t))
  (unless (fboundp 'company-auctex-environments)
    (autoload #'company-auctex-environments "company-auctex" nil t))

  (defun sb/company-latex-mode ()
    "Add backends for latex completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)

    ;; `company-reftex' should be considerably more powerful than `company-auctex' backends for
    ;; labels and citations

    (setq company-backends '((:separate
                              company-capf
                              company-files
                              company-reftex-citations
                              company-reftex-labels
                              company-auctex-environments
                              company-auctex-macros
                              company-latex-commands
                              company-math-symbols-latex
                              company-math-symbols-unicode

                              ;; company-auctex-symbols
                              ;; company-auctex-bibs
                              ;; company-auctex-labels
                              ;; company-bibtex
                              ;; company-capf
                              )
                             (:separate
                              company-dabbrev
                              company-ispell)))

    ;; (setq company-backends '((:separate
    ;;                           company-capf
    ;;                           ;; company-latex-commands
    ;;                           company-files
    ;;                           company-yasnippet
    ;;                           company-dabbrev
    ;;                           company-ispell)))
    )

  (dolist (hook '(latex-mode-hook LaTeX-mode-hook tex-mode-hook TeX-mode-hook))
    (add-hook hook #'sb/company-latex-mode)))


(progn
  (defun sb/company-web-mode ()
    "Add backends for web completion in company mode."
    (defvar company-backends)

    (make-local-variable 'company-backends)
    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev
                             company-ispell)))

  (dolist (hook '(web-mode-hook))
    (add-hook hook #'sb/company-web-mode)))


(when (eq sb/selection 'selectrum)
  (progn
    (declare-function selectrum-mode "selectrum")

    (unless (fboundp 'selectrum-mode)
      (autoload #'selectrum-mode "selectrum" nil t))

    (add-hook 'after-init-hook #'selectrum-mode)

    (with-eval-after-load "selectrum"
      (defvar selectrum-fix-vertical-window-height)

      (setq selectrum-fix-vertical-window-height t))

    (bind-keys
     ("<f3>" . switch-to-buffer))


    ;; Enable richer annotations in the minibuffer
    (declare-function marginalia-mode "marginalia")

    (unless (fboundp 'marginalia-mode)
      (autoload #'marginalia-mode "marginalia" nil t))

    (add-hook 'selectrum-mode-hook #'marginalia-mode)


    (declare-function consult-error "consult")
    (declare-function consult-customize "consult")
    (declare-function consult-rg "consult")
    (declare-function consult-yank-pop "consult")
    (declare-function consult-flycheck "consult-flycheck")
    (declare-function consult-xref "consult")
    (declare-function consult-recent-file "consult")
    (declare-function consult-grep "consult")
    (declare-function consult-git-grep "consult")
    (declare-function consult-line "consult")
    (declare-function consult-outline "consult")
    (declare-function consult-apropos "consult")
    (declare-function consult-buffer "consult")
    (declare-function consult-bookmark "consult")
    (declare-function consult-goto-line "consult")
    (declare-function consult-project-imenu "consult")
    (declare-function consult-find "consult")
    (declare-function consult-locate "consult")
    (declare-function consult-ripgrep "consult")
    (declare-function consult-isearch "consult")
    (declare-function consult-multi-occur "consult")
    (declare-function consult-mode-command "consult")
    (declare-function consult-imenu "consult")

    (unless (fboundp 'consult-imenu)
      (autoload #'consult-imenu "consult"))
    (unless (fboundp 'consult-outline)
      (autoload #'consult-outline "consult"))
    (unless (fboundp 'consult-apropos)
      (autoload #'consult-apropos "consult"))
    (unless (fboundp 'consult-buffer)
      (autoload #'consult-buffer "consult"))
    (unless (fboundp 'consult-bookmark)
      (autoload #'consult-bookmark "consult"))
    (unless (fboundp 'consult-outline)
      (autoload #'consult-outline "consult"))
    (unless (fboundp 'consult-goto-line)
      (autoload #'consult-goto-line "consult"))
    (unless (fboundp 'consult-imenu)
      (autoload #'consult-imenu "consult"))
    (unless (fboundp 'consult-project-imenu)
      (autoload #'consult-project-imenu "consult"))
    (unless (fboundp 'consult-error)
      (autoload #'consult-error "consult"))
    (unless (fboundp 'consult-find)
      (autoload #'consult-find "consult"))
    (unless (fboundp 'consult-locate)
      (autoload #'consult-locate "consult"))
    (unless (fboundp 'consult-grep)
      (autoload #'consult-grep "consult"))
    (unless (fboundp 'consult-git-grep)
      (autoload #'consult-git-grep "consult"))
    (unless (fboundp 'consult-ripgrep)
      (autoload #'consult-ripgrep "consult"))
    (unless (fboundp 'consult-line)
      (autoload #'consult-line "consult"))
    (unless (fboundp 'consult-multi-occur)
      (autoload #'consult-multi-occur "consult"))
    (unless (fboundp 'consult-isearch)
      (autoload #'consult-isearch "consult"))
    (unless (fboundp 'consult-yank-pop)
      (autoload #'consult-yank-pop "consult"))
    (unless (fboundp 'consult-mode-command)
      (autoload #'consult-mode-command "consult"))
    (unless (fboundp 'consult-bookmark)
      (autoload #'consult-bookmark "consult"))
    (unless (fboundp 'consult-apropos)
      (autoload #'consult-apropos "consult"))
    (unless (fboundp 'consult-customize)
      (autoload #'consult-customize "consult"))

    (unless (fboundp 'consult-flycheck)
      (autoload #'consult-flycheck "consult"))


    (defvar xref-show-xrefs-function)
    (defvar xref-show-definitions-function)
    (defvar consult-project-root-function)
    (defvar consult-bookmark)
    (defvar consult-git-grep)
    (defvar consult-grep)
    (defvar consult-ripgrep)
    (defvar consult-recent-file)
    (defvar consult--source-file)
    (defvar consult--source-bookmark)
    (defvar consult--source-project-file)
    (defvar consult-xref)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref
          consult-project-root-function #'projectile-project-root)

    ;; Enable automatic preview at point in the *Completions* buffer. This is relevant when you use
    ;; the default completion UI, and not necessary for Selectrum, Vertico etc.
    ;; (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-file consult--source-project-file consult--source-bookmark
     :preview-key (kbd "M-."))

    (bind-keys :package consult
               ("<f1>"       . amx)
               ("<f2>"       . find-file)
               ("<f4>"       . consult-line)
               ("C-c s g"    . consult-git-grep)
               ("<f9>"       . consult-recent-file)
               ("C-c s r"    . consult-rg)
               ("M-y"        . consult-yank-pop))


    (declare-function selectrum-prescient-mode "selectrum-prescient")

    (unless (fboundp 'selectrum-prescient-mode)
      (autoload #'selectrum-prescient-mode "selectrum-prescient" nil t))

    (add-hook 'selectrum-mode-hook #'selectrum-prescient-mode)

    (defvar selectrum-is-active)
    (defvar selectrum-highlight-candidates-function)
    (defvar magit-completing-read-function)

    (declare-function selectrum-completing-read "selectrum")

    (setq file-name-shadow-properties '(invisible t)
          completion-styles '(orderless)
          orderless-skip-highlighting (lambda () selectrum-is-active)
          selectrum-highlight-candidates-function #'orderless-highlight-matches
          magit-completing-read-function #'selectrum-completing-read)
    ))


;; https://andreyorst.gitlab.io/posts/2020-06-29-using-single-emacs-instance-to-edit-files/
;; (when (not (string-equal "root" (getenv "USER"))) ; Only start server mode if not root
;;   (eval-after-load 'server
;;     '(progn
;;        (unless (server-running-p)
;;          (server-start))
;;        t)))


;; Function definitions

;; http://stackoverflow.com/questions/15254414/how-to-silently-save-all-buffers-in-emacs
(defun sb/save-all-buffers ()
  "Save all modified buffers without prompting."
  (interactive)
  (save-some-buffers t))


;; http://endlessparentheses.com/implementing-comment-line.html
(defun sb/comment-line (n)
  "Comment or uncomment current line and leave point after it.

With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above. If region is
active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))


;; http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html
(defun sb/toggle-line-spacing ()
  "Toggle line spacing.

Increase the line spacing to help readability. Increase line spacing by two line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq line-spacing 2)
    (setq line-spacing nil))
  (redraw-frame (selected-frame)))


(defun sb/byte-compile-current-file ()
  "Byte compile the current file."
  (interactive)
  (byte-compile-file buffer-file-name))


;; http://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
(defun sb/byte-compile-init-dir ()
  "Byte-compile all elisp files in the user init directory."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))


;; https://github.com/thomasf/dotfiles-thomasf-emacs/blob/e14a7e857a89b7488ba5bdae54877abdc77fa9e6/emacs.d/init.el
(defun sb/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))


(defun sb/switch-to-scratch ()
  "Switch to the *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))


;; https://www.emacswiki.org/emacs/InsertDate
(defun sb/insert-date (arg)
  "Insert today's date.  With prefix argument ARG, use a different format."
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%\"Mmmm\" %d, %Y"))))


;; http://zck.me/emacs-move-file
(defun sb/move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))


;; https://www.emacswiki.org/emacs/BuildTags
(defun sb/create-ctags (dir-name)
  "Create tags file with ctags in DIR-NAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -eR %s" sb/ctags-path (directory-file-name dir-name))))


(defun sb/create-gtags (dir-name)
  "Create tags file with gtags in DIR-NAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -cv --gtagslabel=new-ctags %s" sb/gtags-path (directory-file-name dir-name))))


;; https://emacs.stackexchange.com/questions/33332/recursively-list-all-files-and-sub-directories
(when (eq sb/selection 'ivy)
  (declare-function sb/counsel-all-files-recursively "init-autoload")

  (defun sb/counsel-all-files-recursively (dir-name)
    "List all files recursively in DIR-NAME."
    (interactive "DDirectory: ")
    (let* ((cands (split-string
                   (shell-command-to-string (format "find %s -type f" dir-name)) "\n" t)))
      (ivy-read "File: " cands
                :action #'find-file
                :caller 'sb/counsel-all-files-recursively))))


;; https://emacs.stackexchange.com/questions/17687/make-previous-buffer-and-next-buffer-to-ignore-some-buffers
;; You need to check for either major modes or buffer names, since a few major modes are commonly
;; used.
(defcustom sb/skippable-buffers
  '(
    "TAGS" "*Messages*" "*Backtrace*" "*scratch*"
    ;; "*company-documentation*" ; Major mode is `python-mode'
    ;; "*Help*" "*Packages*" "*prettier (local)*" "*emacs*" "*Warnings*"
    ;; "*Compile-Log* *lsp-log*" "*pyright*" "*texlab::stderr*" "*texlab*" "*Paradox Report*"
    ;; "*perl-language-server*" "*perl-language-server::stderr*" "*json-ls*" "*json-ls::stderr*"
    ;; "*xmlls*" "*xmlls::stderr*" "*pyright::stderr*" "*yamlls*" "*yamlls::stderr*" "*jdtls*"
    ;; "*jdtls::stderr*" "*clangd::stderr*" "*shfmt errors*"
    )
  "Buffer names (not regexps) ignored by `sb/next-buffer' and `sb/previous-buffer'."
  :type '(repeat string))


;; https://stackoverflow.com/questions/2238418/emacs-lisp-how-to-get-buffer-major-mode
(defun sb/get-buffer-major-mode (buffer-or-string)
  "Return the major mode associated with BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
    major-mode))


(defcustom sb/skippable-modes '(dired-mode fundamental-mode
                                           helpful-mode
                                           special-mode
                                           paradox-menu-mode
                                           lsp-log-io-mode
                                           help-mode
                                           magit-status-mode
                                           magit-process-mode
                                           magit-diff-mode
                                           tags-table-mode
                                           compilation-mode)
  "List of major modes to skip over when calling `change-buffer'."
  :type '(repeat string))


(defun sb/change-buffer (change-buffer)
  "Call CHANGE-BUFFER.
Keep trying until current buffer is not in `sb/skippable-buffers' or the major
mode is not in `sb/skippable-modes'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (or (member (buffer-name) sb/skippable-buffers)
                   (member (sb/get-buffer-major-mode (buffer-name)) sb/skippable-modes))
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))


(defun sb/next-buffer ()
  "Variant of `next-buffer' that skips `sb/skippable-buffers'."
  (interactive)
  (sb/change-buffer 'next-buffer))


(defun sb/previous-buffer ()
  "Variant of `previous-buffer' that skips `sb/skippable-buffers'."
  (interactive)
  (sb/change-buffer 'previous-buffer))


;; https://emacsredux.com/blog/2020/09/12/reinstalling-emacs-packages/
(defun sb/reinstall-package (package)
  "Reinstall PACKAGE without restarting Emacs."
  (interactive)
  (unload-feature package)
  (package-reinstall package)
  (require package))


;; https://emacs.stackexchange.com/questions/58073/how-to-find-inheritance-of-modes
(defun sb/get-derived-modes (mode)
  "Return a list of the ancestor modes that MODE is derived from."
  (let ((modes ())
        (parent nil))
    (while (setq parent (get mode 'derived-mode-parent))
      (push parent modes)
      (setq mode parent))
    (setq modes (nreverse modes))))


(defun sb/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (forward-line (read-number "Goto line: ")))
    (linum-mode -1)))


;; Generic keybindings, package-specific are usually in their own modules. Use `C-h b' to see
;; available bindings in a buffer. Use `M-x describe-personal-keybindings' to see modifications.

;; `bind-key*', `bind*' overrides all minor mode bindings. The `kbd` macro is not required with
;; `bind-key' variants. With `bind-key', you do not need an explicit `(kbd ...)'.

;; Other variants:
;; `(global-set-key (kbd "RET") 'newline-and-indent)'
;; `(define-key global-map (kbd "RET") 'newline-and-indent)'

;; https://www.masteringemacs.org/article/mastering-key-bindings-emacs

(bind-keys
 ("RET"       . newline-and-indent)
 ("C-l"       . goto-line)
 ("C-c z"     . repeat)
 ("C-z"       . undo)
 ("<f11>"     . delete-other-windows)
 ("C-x k"     . kill-this-buffer)
 ("M-<left>"  . previous-buffer)
 ("M-<right>" . next-buffer)
 ("C-c d f"   . auto-fill-mode)
 ("M-c"       . capitalize-dwim)
 ("M-u"       . upcase-dwim)
 ("M-l"       . downcase-dwim)
 ("<f7>"      . previous-error)
 ("<f8>"      . next-error))

;; In a line with comments, `C-u M-;' removes the comments altogether. That means deleting the
;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
(bind-keys*
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box)
 ("C-s"   . save-buffer)
 ("C-S-s" . sb/save-all-buffers))

(unbind-key "C-x s") ; Bound to save-some-buffers
(bind-key "C-x s" #'sb/switch-to-scratch)
(bind-key "C-x j" #'sb/counsel-all-files-recursively)

(unbind-key "C-j") ; Interferes with imenu `C-c C-j'

(progn
  (unless (fboundp 'package-quickstart-refresh)
    (autoload #'package-quickstart-refresh "package" nil t))

  (when sb/EMACS27+
    (bind-key "C-c d p" #'package-quickstart-refresh)))

(global-set-key [remap next-buffer] #'sb/next-buffer)
(global-set-key [remap previous-buffer] #'sb/previous-buffer)

(with-eval-after-load "centaur-tabs"
  (declare-function centaur-tabs-forward "centaur-tabs")
  (declare-function centaur-tabs-backward "centaur-tabs")

  (global-set-key [remap next-buffer] #'centaur-tabs-forward)
  (global-set-key [remap previous-buffer] #'centaur-tabs-backward))


(progn
  (unless (fboundp 'default-text-scale-increase)
    (autoload #'default-text-scale-increase "default-text-scale" nil t))
  (unless (fboundp 'default-text-scale-decrease)
    (autoload #'default-text-scale-decrease "default-text-scale" nil t))

  (bind-keys :package default-text-scale
             ("C-M-+" . default-text-scale-increase)
             ("C-M--" . default-text-scale-decrease)))


(unless (fboundp 'free-keys)
  (autoload #'free-keys "free-keys" nil t))


;; Show help popups for prefix keys
(progn
  (unless (fboundp 'which-key-mode)
    (autoload #'which-key-mode "which-key" nil t))
  (unless (fboundp 'which-key-setup-side-window-right-bottom)
    (autoload #'which-key-setup-side-window-right-bottom "which-key" nil t))

  (run-with-idle-timer 3 nil #'which-key-mode)

  (with-eval-after-load "which-key"
    ;; Allow C-h to trigger which-key before it is done automatically
    (defvar which-key-show-early-on-C-h)

    (which-key-setup-side-window-right-bottom)

    (diminish 'which-key-mode)

    (unless (fboundp 'which-key-posframe-mode)
      (autoload #'which-key-posframe-mode "which-key-posframe" nil t))

    (which-key-posframe-mode 1)

    ;; The posframe has a low contrast
    ;; (set-face-attribute 'which-key-posframe nil :background "floralwhite" :foreground "black")

    (defvar which-key-posframe-border-width)
    (defvar which-key-posframe-poshandler)

    (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center)))


;; Hydras

(unless (fboundp 'defhydra)
  (autoload #'defhydra "hydra" nil t))

(with-eval-after-load "hydra"
  ;; `:exit t' will quit the hydra
  (defhydra sb/hydra-spelling (:color blue)
    "
  ^
  ^Spelling^          ^Errors^            ^Checker^             ^Spell fu^
  ^────────^──────────^──────^────────────^───────^─────────────^────────^────────
  _q_ quit            _<_ previous        _c_ correction        _n_ next error
  ^^                  _>_ next            _d_ dictionary        _p_ previous error
  ^^                  _f_ check           _m_ mode              _a_ add word
  ^^                  ^^                  ^^                    ^^
  "
    ("q" nil "quit")
    ("<" flyspell-correct-previous :color pink)
    (">" flyspell-correct-next :color pink)
    ("c" ispell)
    ("d" ispell-change-dictionary)
    ("f" flyspell-buffer)
    ("m" flyspell-mode)
    ("n" spell-fu-goto-next-error)
    ("p" spell-fu-goto-previous-error)
    ("a" spell-fu-word-add))

  (defhydra sb/hydra-text-scale-zoom ()
    "Zoom the text"
    ("i" default-text-scale-increase "in")
    ("o" default-text-scale-decrease "out")
    ("q" nil "quit"))

  (defhydra sb/hydra-error (global-map "C-c h e")
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit"))

  ;; https://github.com/abo-abo/hydra/wiki/avy
  (defhydra sb/hydra-avy (:exit t :hint nil)
    "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
    ("c" avy-goto-char-timer)
    ("C" avy-goto-char)
    ("w" avy-goto-word-1)
    ("W" avy-goto-word-0)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("y" avy-copy-line)
    ("Y" avy-copy-region))

  (defhydra sb/hydra-projectile (:color teal :hint nil)
    "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: find tags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir
"
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("s-f" projectile-find-file)
    ("ff"  projectile-find-file-dwim)
    ("fd"  projectile-find-file-in-directory)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("s-p" projectile-switch-project "switch project")
    ("p"   projectile-switch-project)
    ("s"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("a"   projectile-ag)
    ("g"   projectile-find-tag)
    ("q"   nil "cancel" :color blue))

  (defhydra sb/hydra-flycheck (:color blue)
    "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
    ("q" nil)
    ("<" flycheck-previous-error :color pink)
    (">" flycheck-next-error :color pink)
    ("?" flycheck-describe-checker)
    ("M" flycheck-manual)
    ("d" flycheck-disable-checker)
    ("f" flycheck-buffer)
    ("l" flycheck-list-errors)
    ("m" flycheck-mode)
    ("s" flycheck-select-checker)
    ("v" flycheck-verify-setup))

  (with-eval-after-load "python"
    (defhydra sb/hydra-python-indent (python-mode-map "C-c")
      "Adjust Python indentation."
      (">" python-indent-shift-right "right")
      ("<" python-indent-shift-left "left")))

  (defhydra sb/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue))
  )


;; Mark safe variables

(put 'bibtex-completion-bibliography 'safe-local-variable #'listp)
(put 'company-bibtex-bibliography 'safe-local-variable #'listp)
(put 'company-clang-arguments 'safe-local-variable #'listp)
(put 'counsel-etags-project-root 'safe-local-variable #'stringp)
(put 'counsel-find-file-ignore-regexp 'safe-local-variable #'stringp)
(put 'counsel-projectile-default-file 'safe-local-variable #'stringp)
(put 'dotemacs-projectile-default-file 'safe-local-variable #'stringp)
(put 'flycheck-checker 'safe-local-variable #'listp)
(put 'flycheck-clang-include-path 'safe-local-variable #'listp)
(put 'flycheck-gcc-include-path 'safe-local-variable #'listp)
(put 'flycheck-python-pylint-executable 'safe-local-variable #'stringp)
(put 'lsp-clients-clangd-args 'safe-local-variable #'listp)
(put 'lsp-latex-root-directory 'safe-local-variable #'stringp)
(put 'lsp-pyright-extra-paths 'safe-local-variable #'listp)
(put 'projectile-enable-caching 'safe-local-variable #'stringp)
(put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
(put 'projectile-project-root 'safe-local-variable #'stringp)
(put 'pyvenv-activate 'safe-local-variable #'stringp)
(put 'reftex-default-bibliography 'safe-local-variable #'listp)
(put 'tags-table-list 'safe-local-variable #'listp)


;; https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; (setq debug-on-error nil)
            (message "Emacs is ready in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

(provide 'init-autoload)
;;; init-autoload.el ends here
