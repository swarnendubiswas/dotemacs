;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
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
  :type  'string
  :group 'sb/emacs)

(defcustom sb/theme
  'modus-operandi
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "eclipse"         eclipse)
           (const :tag "leuven"          leuven)
           (const :tag "solarized-light" solarized-light)
           (const :tag "solarized-dark"  solarized-dark)
           (const :tag "spacemacs-light" spacemacs-light)
           (const :tag "tangotango"      tangotango)
           (const :tag "zenburn"         zenburn)
           (const :tag "doom-molokai"    doom-molokai)
           (const :tag "doom-one-light"  doom-one-light)
           (const :tag "monokai"         monokai)
           (const :tag "modus-operandi"  modus-operandi)
           (const :tag "modus-vivendi"   modus-vivendi)
           (const :tag "customized"      sb/default) ; Customizations over the default theme
           (const :tag "none"            none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme
  'doom-modeline
  "Specify the mode-line theme to use."
  :type  '(radio
           (const :tag "powerline"       powerline)
           (const :tag "smart-mode-line" sml)
           (const :tag "spaceline"       spaceline)
           (const :tag "airline"         airline)
           (const :tag "doom-modeline"   doom-modeline)
           (const :tag "awesome-tray"    awesome-tray)
           (const :tag "moody"           moody)
           (const :tag "default"         default))
  :group 'sb/emacs)

(defcustom sb/window-split
  'horizontal
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type  '(radio
           ;; Split into two windows one above the other (`split-window-below')
           (const :tag "vertical"   vertical)
           ;; Split into two side-by-side windows (`split-window-right')
           (const :tag "horizontal" horizontal))
  :group 'sb/emacs)

;; Large values make reading difficult when the window is split side-by-side
(defcustom sb/fill-column
  100
  "Column beyond which lines should not extend."
  :type  'number
  :group 'sb/emacs)

(defcustom sb/selection
  'ivy
  "Choose the framework to use for narrowing and selection."
  :type  '(radio
           (const :tag "ivy"       ivy)
           (const :tag "selectrum" selectrum)
           (const :tag "none"      none))
  :group 'dotemacs)

(defcustom sb/delete-trailing-whitespace-p
  nil
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.
Sometimes we do not want to unnecessarily add differences due to
whitespaces."
  :type  'boolean
  :group 'sb/emacs)

;; We use `lsp-mode' and `dumb-jump' for jumping to tags and browsing source code
(defcustom sb/tags-scheme
  'none
  "Choose whether to use gtags or ctags."
  :type  '(radio
           (const :tag "ctags" ctags)
           (const :tag "gtags" gtags)
           (const :tag "none"  none))
  :group 'sb/emacs)

(defcustom sb/ctags-path
  "/usr/local/bin/ctags"
  "Absolute path to Universal Ctags executable."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/gtags-path
  "/usr/local/bin/gtags"
  "Absolute path to GNU Global executable."
  :type  'string
  :group 'sb/emacs)

;; Keep enabled until the configuration is stable
(defcustom sb/debug-init-file
  nil
  "Enable features to debug errors and performance bottlenecks."
  :type  'boolean
  :group 'sb/emacs)

(when (bound-and-true-p sb/debug-init-file)
  (setq garbage-collection-messages t
        debug-on-error t
        debug-on-event 'sigusr2)
  (debug-on-entry 'projectile-remove-known-project))

(defconst sb/user-home
  (getenv "HOME")
  "User HOME directory.")

(defconst sb/user-tmp
  (expand-file-name "tmp" sb/user-home)
  "User temp directory.
This location is used for temporary installations and files.")

(defcustom sb/textlint-home
  (expand-file-name "textlint-workspace" sb/user-tmp)
  "Absolute path to textlint workspace."
  :type  'string
  :group 'sb/emacs)

;; `pyls' and `mspyls' are not actively maintained, and improvements to `py-lsp' is slow
(defcustom sb/python-langserver
  'pyright
  "Choose the Python Language Server implementation."
  :type  '(radio
           (const :tag "pyright" pyright)
           (const :tag "jedi"    jedi)
           (const :tag "none"    none))
  :group 'sb/emacs)

;; Another option is to construct the `load-path' manually
;; (add-to-list 'load-path sb/extras-directory)
;; (add-to-list 'load-path (concat package-user-dir "magit-20170715.1731"))
(package-initialize)

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
(add-hook 'emacs-startup-hook    #'sb/restore-garbage-collection)
(add-hook 'minibuffer-setup-hook #'sb/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook  #'sb/restore-garbage-collection)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' before loading
(defvar use-package-always-defer)
(defvar use-package-always-ensure)
(defvar use-package-compute-statistics)
(defvar use-package-enable-imenu-support)
(defvar use-package-expand-minimally)
(defvar use-package-verbose)

(setq use-package-enable-imenu-support t)
(when (bound-and-true-p sb/debug-init-file)
  (setq debug-on-error nil
        use-package-always-ensure t
        use-package-compute-statistics t ; Use `M-x use-package-report' to see results
        use-package-expand-minimally nil
        use-package-verbose t))

;; Always load features lazily unless told otherwise. This implies we should use `after-init' hook
;; or `:init' instead of `:config', since otherwise packages may not be loaded. Be careful about
;; using `:after' and always deferring loading, because then we will need to specifiy alternate ways
;; of loading the package. https://github.com/jwiegley/use-package#notes-about-lazy-loading
(unless (bound-and-true-p sb/debug-init-file)
  (setq use-package-always-defer t
        use-package-always-ensure nil
        use-package-compute-statistics nil
        ;; Avoid printing errors and warnings since the configuration is known to work
        use-package-expand-minimally t
        use-package-verbose nil))

(eval-when-compile
  (require 'use-package))

;; If we omit `:defer', `:hook', `:commands', or `:after', then the package is loaded immediately.
;; Hooks in the `:hook' section, run in reverse order. Example:
;; (use-package package-name
;;   :hook
;;   (x-mode . last)
;;   (x-mode . second)
;;   (x-mode . first))

;; Installation is one-time, so avoid the overhead of run-time checks
(use-package system-packages
  :disabled t
  :commands (system-packages-ensure system-packages-install)
  :config (setq system-packages-use-sudo t))

(use-package use-package-ensure-system-package
  :disabled t)

;; `C-h b' lists all the bindings available in a buffer. `C-h m' shows the keybindings for the major
;; and the minor modes.
(use-package bind-key
  :functions bind-key--remove
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish)

(use-package dash
  :commands (-tree-map --remove))

(use-package f
  :commands (f-join f-exists? f-glob))

(use-package s
  :commands s-starts-with?)

;; Use `C-c h' consistently for invoking a hydra
(use-package hydra
  :commands (hydra-default-pre hydra-keyboard-quit defhydra
                               hydra-show-hint hydra-set-transient-map
                               hydra--call-interactively-remap-maybe))

(use-package use-package-hydra
  :disabled t)

(use-package use-package-chords
  :disabled t
  :config (key-chord-mode 1))

(defcustom sb/use-no-littering
  t
  "Use the `no-littering' package to keep `.emacs.d' clean."
  :type 'boolean
  :group 'sb/emacs)

(use-package no-littering
  :if (bound-and-true-p sb/use-no-littering)
  :demand t)

(defcustom sb/custom-file
  (no-littering-expand-etc-file-name "custom.el")
  "File to write Emacs customizations."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/private-file
  (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type  'string
  :group 'sb/emacs)

(setq custom-file sb/custom-file)
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(when (file-exists-p sb/private-file)
  (load sb/private-file 'noerror))

(defcustom sb/temp-directory
  (expand-file-name "tmp" user-emacs-directory)
  "Storage location for various configuration files."
  :type  'string
  :group 'sb/emacs)

;; We do not need this with `no-littering'
(unless (or (bound-and-true-p sb/use-no-littering)
            (file-exists-p sb/temp-directory))
  (make-directory sb/temp-directory))

(when nil
  (progn
    (unless (package-installed-p 'quelpa)
      (package-refresh-contents)
      (package-install 'quelpa))
    (defvar quelpa-update-melpa-p)
    (defvar quelpa-upgrade-interval)
    (defvar quelpa-self-upgrade-p)
    (setq quelpa-self-upgrade-p nil
          quelpa-update-melpa-p nil
          quelpa-upgrade-interval 30)))

;; Using quelpa is convenient but slow
(use-package quelpa-use-package
  :disabled t
  :demand t)

;; This is not a great idea, but I expect most warnings will arise from third-party packages
(use-package warnings
  :demand t
  :config (setq warning-minimum-level :emergency))

;; Allow GC to happen after a period of idle time
(use-package gcmh
  :diminish
  :commands (gcmh-mode gcmh-idle-garbage-collect)
  :hook (after-init . gcmh-mode)
  :config
  (when (bound-and-true-p sb/debug-init-file)
    (setq gcmh-verbose t)))

(use-package paradox
  :commands (paradox-enable)
  :bind
  (("C-c d l" . paradox-list-packages)
   ("C-c d u" . paradox-upgrade-packages))
  :config
  (setq paradox-display-star-count nil
        paradox-execute-asynchronously t
        paradox-github-token t)
  (paradox-enable))

(use-package auto-package-update ; Default update interval is 7 days
  :disabled t
  :commands (auto-package-update-now auto-package-update-maybe)
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-prompt-before-update t)
  (auto-package-update-maybe))

;; Get PATH with `(getenv "PATH")'
;; Set PATH with `(setenv "PATH" (concat (getenv "PATH") ":/home/swarnendu/bin"))'
(use-package exec-path-from-shell
  :defines exec-path-from-shell-check-startup-files
  :commands exec-path-from-shell-initialize
  :if (or (daemonp) (memq window-system '(x ns)))
  :init
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
      ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first
      ;; error
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
      ring-bell-function 'ignore ; Disable beeping sound when spacing backspace
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

(use-package request
  :if (unless (bound-and-true-p sb/use-no-littering))
  :init (setq request-storage-directory (expand-file-name "request" no-littering-var-directory)))

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

(use-package autorevert ; Auto-refresh all buffers
  :ensure nil
  :commands global-auto-revert-mode
  :diminish auto-revert-mode
  :init (run-with-idle-timer 2 nil #'global-auto-revert-mode)
  :config
  (setq auto-revert-interval 5 ; Faster (seconds) would mean less likely to use stale data
        auto-revert-remote-files nil ; Emacs seems to hang with auto-revert and Tramp
        auto-revert-use-notify nil
        auto-revert-verbose nil
        auto-revert-check-vc-info nil ; Should improve performance
        ;; Revert only file-visiting buffers, set to non-nil value to revert dired buffers if the
        ;; contents of the "main" directory changes
        global-auto-revert-non-file-buffers nil))

;; Revert all (e.g., PDF) files without asking
(setq revert-without-query '("\\.*"))

(use-package saveplace ; Remember cursor position in files
  :ensure nil
  :hook
  ;; We may open a file immediately after starting Emacs, hence we are using a hook instead of a
  ;; timer.
  (after-init . save-place-mode)
  :config
  (unless (bound-and-true-p sb/use-no-littering)
    (setq save-place-file (expand-file-name "places" sb/temp-directory))))

(use-package savehist ; Save minibuffer history across sessions
  :ensure nil
  :commands savehist-mode
  :init (run-with-idle-timer 3 nil #'savehist-mode)
  :config
  (setq savehist-additional-variables '(extended-command-history
                                        kill-ring
                                        regexp-search-ring
                                        search-ring)
        savehist-save-minibuffer-history t)

  (unless (bound-and-true-p sb/use-no-littering)
    (setq savehist-file (expand-file-name "savehist" sb/temp-directory))))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-after-kill-buffer-p t
        uniquify-buffer-name-style 'forward
        uniquify-ignore-buffers-re "^\\*"
        uniquify-separator "/"
        uniquify-strip-common-suffix t))

;; Replace `dabbrev-exp' with `hippie-expand'. Use `C-M-/' for `dabbrev-completion' which finds all
;; expansions in the current buffer and presents suggestions for completion.
(use-package hippie-exp
  :ensure nil
  :config
  (setq hippie-expand-try-functions-list '(yas-hippie-try-expand
                                           try-expand-dabbrev
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
  :bind ("M-/" . hippie-expand))

(use-package subword
  :ensure nil
  :commands subword-mode
  :diminish
  :hook (prog-mode . subword-mode))

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
(progn
  (defun sb/auto-save-wrapper (save-fn &rest args)
    "Hide 'Auto-saving...done' messages by calling the method.
SAVE-FN with non-nil ARGS."
    (ignore args)
    (apply save-fn '(t)))

  (advice-add 'do-auto-save :around #'sb/auto-save-wrapper))

;; We open the `*scratch*' buffer in `text-mode', so enabling `abbrev-mode' quickly can be useful
(use-package abbrev
  :ensure nil
  :commands abbrev-mode
  :diminish
  :hook (text-mode . abbrev-mode)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory)
        save-abbrevs 'silently))

;; Disable the following modes
(dolist (mode '(blink-cursor-mode ; Blinking cursor is distracting
                ;; desktop-save-mode
                ;; global-prettify-symbols-mode ; Makes it difficult to edit the buffer
                shell-dirtrack-mode
                size-indication-mode
                tooltip-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(use-package hl-line
  :hook (after-init . global-hl-line-mode))

;; Enable the following modes
(dolist (mode '(;; auto-compression-mode
                auto-save-visited-mode ; Autosave file-visiting buffers at idle time intervals
                column-number-mode
                delete-selection-mode ; Typing with the mark active will overwrite the marked region
                ;; Soft wraps, Wrap lines without the ugly continuation marks
                global-visual-line-mode
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

;; This puts the buffer in read-only mode and disables font locking, revert with `C-c C-c'
(use-package so-long
  :ensure nil
  :commands global-so-long-mode
  :init (run-with-idle-timer 3 nil #'global-so-long-mode)
  :config (setq so-long-threshold 500))

;; Install fonts with `M-x all-the-icons-install-fonts'
;; https://github.com/domtronn/all-the-icons.el/issues/120
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :preface
  (defun sb/font-installed-p (font-name)
    "Check if font with FONT-NAME is available."
    (if (find-font (font-spec :name font-name))
        t
      nil))
  :init
  (unless (sb/font-installed-p "all-the-icons")
    (all-the-icons-install-fonts t))
  (setq all-the-icons-scale-factor 1.0
        all-the-icons-color-icons nil))

;; Make the cursor a thin horizontal bar, not a block
;; (set-default 'cursor-type '(bar . 4))

;; Set `sb/theme' to `none' if you use this package
(use-package circadian
  :commands circadian-setup
  :disabled t
  :init
  (require 'solar)
  (setq calendar-latitude 26.50
        calendar-location-name "Kanpur, UP, India"
        calendar-longitude 80.23
        circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))

  (circadian-setup))

(use-package leuven-theme
  :if (eq sb/theme 'leuven)
  :init (load-theme 'leuven t)
  :disabled t)

(use-package eclipse-theme
  :if (eq sb/theme 'eclipse)
  :disabled t
  :init
  (load-theme 'eclipse t)
  (set-background-color "white")
  (set-face-attribute 'region nil :background "LemonChiffon" :foreground "black")
  (set-face-attribute 'mode-line nil :background "grey88" :foreground "black" :box nil))

(use-package spacemacs-common
  :ensure spacemacs-theme
  :disabled t
  :if (eq sb/theme 'spacemacs-light)
  :init
  (load-theme 'spacemacs-light t)
  ;; (add-to-list 'default-frame-alist '(background-color . "#fbf8ef"))
  )

(use-package zenburn-theme
  :if (eq sb/theme 'zenburn)
  :disabled t
  :init (load-theme 'zenburn t))

(use-package solarized-light-theme
  :ensure solarized-theme
  :disabled t
  :if (eq sb/theme 'solarized-light)
  :init
  (setq solarized-distinct-fringe-background t)
  (load-theme 'solarized-light t))

(use-package solarized-dark-theme
  :ensure solarized-theme
  :disabled t
  :if (eq sb/theme 'solarized-dark)
  :init (load-theme 'solarized-dark t))

(use-package doom-themes
  :disabled t
  :if (eq sb/theme 'doom-molokai)
  :commands (doom-themes-org-config doom-themes-treemacs-config)
  :init
  (load-theme 'doom-molokai t)
  (set-face-attribute 'font-lock-comment-face nil
                      ;; :foreground "#cccccc"
                      ;; :foreground "#b2b2b2"
                      :foreground "#999999")
  :config
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

(use-package doom-themes
  :disabled t
  :if (eq sb/theme 'doom-one-light)
  :commands (doom-themes-org-config doom-themes-treemacs-config)
  :init (load-theme 'doom-one-light t)
  :config
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

(use-package monokai-theme
  :disabled t
  :if (eq sb/theme 'monokai)
  :init (load-theme 'monokai t))

(use-package modus-operandi-theme
  ;;  :ensure moody
  :ensure modus-themes
  :if (eq sb/theme 'modus-operandi)
  :init
  (setq modus-themes-completions 'opinionated
        modus-themes-fringes 'subtle
        modus-themes-intense-hl-line nil
        modus-themes-scale-headings nil
        modus-themes-prompts 'intense-accented
        modus-themes-variable-pitch-headings nil
        modus-themes-org-blocks 'tinted-background)

  (when (eq sb/modeline-theme 'default)
    (setq modus-themes-mode-line 'accented-3d))

  (when (eq sb/modeline-theme 'moody)
    (setq modus-themes-mode-line 'borderless-moody))

  (load-theme 'modus-operandi t)

  ;; :custom-face
  ;; (mode-line ((t (:background "#d7d7d7" :foreground "#0a0a0a"
  ;;                             :box (:line-width 1 :color "#505050")
  ;;                             :height 0.8))))
  ;; (mode-line-inactive ((t (:background "#efefef" :foreground "#404148"
  ;;                                      :box (:line-width 1 :color "#bcbcbc")
  ;;                                      :height 0.8))))
  )

(use-package modus-vivendi-theme
  ;; :ensure moody
  :ensure modus-themes
  :if (eq sb/theme 'modus-vivendi)
  :disabled t
  :init
  ;; (setq modus-themes-completions 'opinionated
  ;;       modus-themes-fringes 'subtle
  ;;       modus-themes-intense-hl-line t
  ;;       modus-themes-scale-headings nil
  ;;       modus-themes-prompts 'intense-accented
  ;;       modus-themes-variable-pitch-headings nil)

  (when (eq sb/modeline-theme 'default)
    (setq modus-themes-mode-line 'accented-3d))

  (when (eq sb/modeline-theme 'moody)
    (setq modus-themes-mode-line 'borderless-moody))

  (load-theme 'modus-vivendi t))

(when (and (eq sb/theme 'sb/default)
           (display-graphic-p))
  (progn
    ;; (setq frame-background-mode 'light)
    ;; (set-background-color "#ffffff")
    (set-foreground-color "#333333")
    (with-eval-after-load "hl-line"
      (set-face-attribute 'hl-line nil :background "light yellow"))
    (set-face-attribute 'region nil :background "gainsboro")))

;; The Python virtualenv information is not shown on the modeline
(use-package powerline
  :if (eq sb/modeline-theme 'powerline)
  :disabled t
  :commands powerline-default-theme
  :init
  (setq powerline-display-hud nil ; Visualization of the position in the buffer is not useful
        ;; powerline-default-separator 'box
        ;; powerline-display-buffer-size nil
        powerline-display-mule-info nil ; File encoding information is not useful
        powerline-gui-use-vcs-glyph t
        powerline-height 20)

  (when (eq sb/theme 'leuven)
    (set-face-attribute 'mode-line nil :background "grey88" :foreground "black")
    (set-face-attribute 'mode-line-buffer-id nil :weight 'bold
                        :foreground "black" :background "gray88"))

  (powerline-default-theme))

(use-package smart-mode-line
  :disabled t
  :if (eq sb/modeline-theme 'sml)
  :defines (sml/theme sml/mode-width sml/no-confirm-load-theme
                      sml/shorten-modes sml/shorten-directory)
  :commands sml/setup
  :init
  (use-package smart-mode-line-powerline-theme
    :demand t)
  (setq sml/theme 'light
        sml/mode-width 'full
        sml/no-confirm-load-theme t
        sml/shorten-modes t
        sml/shorten-directory t)

  (sml/setup))

(use-package spaceline
  :disabled t
  :if (eq sb/modeline-theme 'spaceline)
  :defines (spaceline-hud-p spaceline-selection-info-p
                            spaceline-version-control-p spaceline-input-method-p
                            spaceline-persp-name-p)
  :functions spaceline-emacs-theme
  :init
  ;; (require 'spaceline-config)
  (setq spaceline-hud-p nil
        spaceline-input-method-p nil
        spaceline-persp-name-p nil
        spaceline-selection-info-p nil
        spaceline-version-control-p t)

  (use-package spaceline-all-the-icons
    :commands spaceline-all-the-icons-theme
    :init (spaceline-all-the-icons-theme)))

(use-package airline-themes
  :disabled t
  :if (eq sb/modeline-theme 'airline)
  :init
  (require 'airline-themes)
  (setq airline-eshell-colors nil
        airline-hide-eyebrowse-on-inactive-buffers t)
  (load-theme 'airline-cool t))

(use-package doom-modeline
  ;; Requires the fonts included with `all-the-icons', run `M-x all-the-icons-install-fonts'
  :ensure all-the-icons
  :ensure doom-modeline
  :if (eq sb/modeline-theme 'doom-modeline)
  :commands doom-modeline-mode
  :init
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-checker-simple-format t
        doom-modeline-indent-info nil
        doom-modeline-lsp nil
        doom-modeline-minor-modes t
        ;; doom-modeline-height 20
        doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-mode 1)
  ;; :custom-face
  ;; (doom-modeline-bar ((t (:inherit default :height 0.8))))
  )

(use-package awesome-tray
  :ensure nil
  :disabled t
  :commands awesome-tray-mode
  :if (eq sb/modeline-theme 'awesome-tray)
  :load-path "extras"
  ;; :quelpa ((awesome-tray :fetcher github :repo "manateelazycat/awesome-tray"))
  :hook (after-init . awesome-tray-mode)
  :config
  (setq awesome-tray-active-modules '("file-path" "buffer-name" "mode-name" "location" "git"))
  :custom-face
  (awesome-tray-default-face ((t (:inherit default :height 0.8))))
  (awesome-tray-module-awesome-tab-face ((t (:foreground "#b83059" :weight bold :height 0.8))))
  (awesome-tray-module-buffer-name-face ((t (:foreground "#cc7700" :weight bold :height 0.8))))
  (awesome-tray-module-date-face ((t (:foreground "#717175" :weight bold :height 0.8))))
  (awesome-tray-module-file-path-face ((t (:foreground "#5e8e2e" :weight normal :height 0.8))))
  (awesome-tray-module-git-face ((t (:foreground "#cc2444" :weight normal :height 0.8))))
  (awesome-tray-module-last-command-face ((t (:foreground "#0061cc" :weight bold :height 0.8))))
  (awesome-tray-module-location-face ((t (:foreground "#cc7700" :weight normal :height 0.8))))
  (awesome-tray-module-mode-name-face ((t (:foreground "#00a400" :weight bold :height 0.8))))
  (awesome-tray-module-parent-dir-face ((t (:foreground "#5e8e2e" :weight bold :height 0.8)))))

(use-package moody
  :if (eq sb/modeline-theme 'moody)
  :disabled t
  :commands (moody-replace-vc-mode moody-replace-mode-line-buffer-identification)
  :init
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package mode-icons ; The icons do not look good
  :disabled t
  :init (mode-icons-mode 1))

(use-package auto-dim-other-buffers
  :commands (adob--rescan-windows auto-dim-other-buffers-mode)
  :init (run-with-idle-timer 3 nil #'auto-dim-other-buffers-mode))

(use-package centaur-tabs
  :disabled t
  :commands (centaur-tabs-group-by-projectile-project centaur-tabs-mode)
  :hook (after-init . centaur-tabs-mode)
  :config
  (setq centaur-tabs-cycle-scope 'tabs
        centaur-tabs-icon-scale-factor 0.7
        centaur-tabs-set-close-button nil
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t)

  (centaur-tabs-group-by-projectile-project)
  :bind
  (([remap next-buffer]     . centaur-tabs-forward)
   ([remap previous-buffer] . centaur-tabs-backward)))

(use-package hide-mode-line
  :disabled t
  :commands (hide-mode-line-mode))

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
  (set-face-attribute 'default nil :height 170)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120))

(when (string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
  (set-face-attribute 'default nil :height 140)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

;; Decrease minibuffer font
;; https://stackoverflow.com/questions/7869429/altering-the-font-size-for-the-emacs-minibuffer-separately-from-default-emacs
(progn
  (defun sb/minibuffer-font-setup ()
    "Customize minibuffer font."
    (set (make-local-variable 'face-remapping-alist)
         '((default :height 0.90))))

  (add-hook 'minibuffer-setup-hook #'sb/minibuffer-font-setup))

;; Changing height of the echo area is jarring, but limiting the height makes it difficult to see
;; useful information
(when nil
  (progn
    (add-hook 'emacs-startup-hook (lambda ()
                                    (setq resize-mini-windows nil)))
    ))

;; LATER: Check the impact on performance, do I really need it?
(use-package beacon
  :commands beacon-mode
  :diminish
  :init (run-with-idle-timer 3 nil #'beacon-mode))

(use-package ibuffer
  :ensure nil
  :config
  (defalias 'list-buffers 'ibuffer)
  ;; (setq ibuffer-default-sorting-mode 'alphabetic
  ;;       ibuffer-display-summary nil
  ;;       ibuffer-use-header-line t)
  :bind ("C-x C-b" . ibuffer))

(use-package ibuf-ext
  :load-path "extras"
  :commands ibuffer-auto-mode
  ;; :quelpa ((ibuf-ext :fetcher github :repo "emacs-mirror/emacs"
  ;;                    :files ("lisp/ibuf-ext.el")))
  :config
  ;; Do not show filter groups if there are no buffers in that group
  (setq ibuffer-show-empty-filter-groups nil)
  :hook (ibuffer . ibuffer-auto-mode))

(use-package ibuffer-projectile ; Group buffers by projectile project
  :commands ibuffer-projectile-set-filter-groups
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

(use-package all-the-icons-ibuffer
  :if (display-graphic-p)
  :commands all-the-icons-ibuffer-mode
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :config
  (setq all-the-icons-ibuffer-human-readable-size t
        all-the-icons-ibuffer-icon-size 0.9))

(use-package bufler ; IBuffer works well, I do not understand the advantages of this package
  :disabled t
  :commands bufler-mode
  ;; :quelpa (bufler :fetcher github :repo "alphapapa/bufler.el"
  ;;                 :files (:defaults (:exclude "helm-bufler.el")))
  :diminish bufler-workspace-mode
  :config (bufler-mode 1))

(use-package dired
  :ensure nil
  :commands (dired-next-line dired-jump)
  :preface
  (defun sb/dired-go-home ()
    (interactive)
    (dired sb/user-home))

  (defun sb/dired-jump-to-top ()
    (interactive)
    (goto-char (point-min)) ; Faster than `(beginning-of-buffer)'
    (dired-next-line 2))

  (defun sb/dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max)) ; Faster than `(end-of-buffer)'
    (dired-next-line -1))
  :bind
  (("C-x C-j"  . dired-jump)
   :map dired-mode-map
   ("M-<home>" . sb/dired-go-home)
   ("M-<up>"   . sb/dired-jump-to-top)
   ("M-<down>" . sb/dired-jump-to-bottom)
   ("i"        . find-file))
  :hook (dired-mode . auto-revert-mode) ;; Auto refresh dired when files change
  :config
  (setq dired-auto-revert-buffer t ; Revert each dired buffer automatically when you revisit it
        dired-dwim-target t ; Guess a default target directory
        ;; Check `ls' for additional options
        dired-listing-switches "-ABhl --si --group-directories-first"
        dired-ls-F-marks-symlinks t ; -F marks links with @
        dired-recursive-copies 'always ; Single prompt for all n directories
        ;; Single prompt for all n directories
        dired-recursive-deletes 'always))

(use-package dired-x
  :ensure nil
  :defines dired-cleanup-buffers-too
  :commands (dired-omit-mode)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-cleanup-buffers-too t
        dired-omit-verbose nil) ; Do not show messages when omitting files

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

  ;; `:diminish dired-omit-mode' does not work
  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Remove 'Omit' from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map))

;; Do not create multiple dired buffers
(use-package dired+
  :load-path "extras"
  ;; :quelpa ((dired+ :fetcher github :repo "emacsmirror/dired-plus"
  ;;                  :files (dired+.el)))
  :commands diredp-toggle-find-file-reuse-dir
  :config
  (setq diredp-hide-details-initially-flag nil
        diredp-hide-details-propagate-flag nil)
  (unbind-key "r" dired-mode-map) ; Bound to `diredp-rename-this-file'
  :hook
  (dired-mode . (lambda ()
                  (diredp-toggle-find-file-reuse-dir 1))))

;; Bound to `diredp-rename-this-file', prefer `dired-efap'. This binding only works if we load after
;; `dired+' and not `dired', even with `bind-keys*'.
(use-package dired-efap
  :after dired+
  :defines dired-efap-initial-filename-selection
  :config (setq dired-efap-initial-filename-selection nil)
  :bind*
  (:map dired-mode-map
        ("r" . dired-efap)))

(use-package dired-narrow ; Narrow dired to match filter
  :after dired
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

(use-package diredfl ; More detailed colors, but can be jarring with certain themes
  :disabled t
  :commands (diredfl-mode diredfl-global-mode)
  :hook (dired-mode . diredfl-mode))

;; Byte compile asynchronously packages installed with `package.el'
(use-package async
  :functions async-bytecomp-package-mode
  :commands async-bytecomp-package-mode
  :init (async-bytecomp-package-mode 1))

(use-package dired-async
  :ensure async
  :diminish
  :hook (dired-mode . dired-async-mode))

(use-package all-the-icons-dired
  :commands all-the-icons-dired-mode
  :diminish
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package treemacs
  :if (display-graphic-p)
  :functions treemacs-tag-follow-mode
  :commands (treemacs-current-workspace treemacs--find-current-user-project
                                        treemacs-do-add-project-to-workspace
                                        treemacs-add-project-to-workspace
                                        treemacs-git-mode
                                        treemacs-follow-mode
                                        treemacs-fringe-indicator-mode
                                        treemacs-filewatch-mode
                                        treemacs-goto-file-node
                                        treemacs--propagate-new-icons
                                        treemacs-scope->current-scope
                                        treemacs--restore-eldoc-after-log
                                        treemacs-load-theme
                                        treemacs-find-file-node
                                        treemacs-resize-icons
                                        treemacs-select-window
                                        treemacs-add-and-display-current-project
                                        treemacs-display-current-project-exclusively)
  :preface
  (defun sb/setup-treemacs-quick ()
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
      (if (treemacs-workspace->is-empty?)
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
          (adob--rescan-windows)))))

  :config
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
        treemacs-width 20
        ;; Hide the mode-line in the Treemacs buffer
        treemacs-user-mode-line-format 'none)

  (unless (bound-and-true-p sb/use-no-littering)
    (setq treemacs-persist-file (expand-file-name "treemacs-persist" sb/temp-directory)))

  (treemacs-filewatch-mode 1)
  (treemacs-follow-mode 1)
  (treemacs-git-mode 'deferred)
  (treemacs-fringe-indicator-mode 'always) ; Always show the file indicator

  ;; Disables `treemacs-follow-mode', focuses the tag, but following tags in noisy
  ;; (add-hook 'prog-mode-hook (lambda ()
  ;;                             (treemacs-tag-follow-mode 1)))

  (use-package treemacs-all-the-icons
    :demand t)

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

  (set-face-attribute 'treemacs-directory-collapsed-face nil :height 0.8)
  (set-face-attribute 'treemacs-directory-face nil :height 0.7)
  (set-face-attribute 'treemacs-file-face nil :height 0.7)
  (set-face-attribute 'treemacs-root-face nil :height 0.7)
  (set-face-attribute 'treemacs-tags-face nil :height 0.7)
  (set-face-attribute 'treemacs-git-ignored-face nil :height 0.7)
  (set-face-attribute 'treemacs-git-untracked-face nil :height 0.7)

  (when (or (eq sb/theme 'modus-operandi) (eq sb/theme 'modus-vivendi))
    (set-face-attribute 'treemacs-git-modified-face nil :height 0.7)
    (set-face-attribute 'treemacs-git-unmodified-face nil :height 0.7))

  (when (eq sb/theme 'sb/default)
    (set-face-attribute 'treemacs-git-modified-face nil :height 0.8)
    (set-face-attribute 'treemacs-git-unmodified-face nil :height 1.0))

  (treemacs-resize-icons 16)

  ;; Ignore files

  ;; (defun sb/treemacs-ignore-files (filename absolute-path)
  ;;   (or (string-equal filename "foo")
  ;;       (string-prefix-p "/x/y/z/" absolute-path)))

  ;; (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-files)

  :bind*
  (;; ("C-j" . treemacs) ; Interferes with `dired-jump'
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

;; Allows to quickly add projectile projects to the treemacs workspace
(use-package treemacs-projectile
  :after (treemacs projectile)
  :commands treemacs-projectile
  :disabled t)

(use-package treemacs-magit
  :after (treemacs magit)
  :disabled t
  :demand t)

(use-package org
  :defines (org-hide-leading-stars-before-indent-mode
            org-src-strip-leading-and-trailing-blank-lines
            org-src-tabs-acts-natively)
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . prettify-symbols-mode))
  :config
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
        org-footnote-auto-adjust t)
  :bind
  (:map org-mode-map
        ("M-<left>"  . nil)
        ("M-<right>" . nil)
        ("M-<up>"    . nil)
        ("M-<down>"  . nil)
        ("C-'"       . nil)))

(use-package org-indent
  :ensure nil
  :commands (org-indent-mode org-indent-item org-outdent-item)
  :hook (org-mode . org-indent-mode)
  :diminish
  :bind
  (:map org-mode-map
        ("<tab>"     . org-indent-item)
        ("<backtab>" . org-outdent-item)))

(use-package org-bullets
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))

;; Make invisible parts of Org elements appear visible
(use-package org-appear
  :commands org-appear-mode
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autolinks t))

(use-package ox-gfm
  :demand t
  :after org)

;; TODO: Use `C-c o' as the binding for `org-mode-map'

;; Use `C-'' in `isearch-mode-map' to use `avy-isearch' to select one of the currently visible
;; isearch candidates
(use-package isearch
  :ensure nil
  :commands (isearch-forward-regexp isearch-repeat-forward isearch-occur)
  :config (setq search-highlight t) ; Highlight incremental search
  :bind
  ;; Change the bindings for `isearch-forward-regexp' and `isearch-repeat-forward'
  (("C-s"     . nil)
   ("C-f"     . isearch-forward-regexp)
   :map isearch-mode-map
   ("C-s"     . nil)
   ("C-f"     . isearch-repeat-forward)
   ("C-c C-o" . isearch-occur)))

(use-package isearch-symbol-at-point
  :after isearch
  :disabled t
  :commands (isearch-symbol-at-point isearch-backward-symbol-at-point))

(use-package isearch-dabbrev
  :after isearch
  :disabled t
  :bind
  (:map isearch-mode-map
        ("<tab>" . isearch-dabbrev-expand)))

(use-package anzu
  :diminish anzu-mode
  :commands global-anzu-mode
  :init
  (setq anzu-search-threshold 10000
        anzu-minimum-input-length 2)

  ;; (when (eq sb/modeline-theme 'spaceline)
  ;;   (setq anzu-cons-mode-line-p nil))
  ;; (unless (eq sb/theme 'leuven)
  ;;   (set-face-attribute 'anzu-mode-line nil
  ;;                       :foreground "blue"
  ;;                       :weight 'light))

  (global-anzu-mode 1)
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)))

(use-package swiper
  :commands (swiper swiper-isearch)
  :config (setq swiper-action-recenter t))

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
(use-package wgrep ; Writable grep
  :bind
  (:map grep-mode-map
        ("C-c C-c" . wgrep-finish-edit)
        ("C-x C-s" . wgrep-finish-edit)
        ("C-x C-q" . wgrep-exit)
        ("C-c C-k" . wgrep-abort-changes)
        ("C-x C-p" . wgrep-change-to-wgrep-mode))
  :config (setq wgrep-auto-save-buffer t))

(use-package deadgrep
  :bind ("C-c s d" . deadgrep))

(use-package ripgrep
  :commands ripgrep-regexp
  :disabled t)

(use-package ctrlf
  :disabled t
  :commands (ctrlf-mode ctrlf-local-mode)
  :config
  (ctrlf-mode 1)
  (add-hook 'pdf-isearch-minor-mode-hook (lambda ()
                                           (ctrlf-local-mode -1)))
  :bind
  (("C-f"   . ctrlf-forward-literal)
   ("C-r"   . ctrlf-backward-literal)
   ("C-M-s" . ctrlf-forward-regexp)
   ("C-M-r" . ctrlf-backward-regexp)))

(progn
  (defvar reb-re-syntax)

  (setq reb-re-syntax 'string))

(use-package visual-regexp
  :commands (vr/replace vr/query-replace vr/mark)
  :bind ([remap query-replace] . vr/query-replace))

(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-add-file recentf-save-file
                          recentf-save-list
                          recentf-apply-filename-handlers
                          recentf-cleanup)
  :config
  (setq recentf-auto-cleanup 'never ; Do not stat remote files
        ;; Check regex with `re-builder', use `recentf-cleanup' to update the list
        recentf-exclude '("[/\\]elpa/"
                          "[/\\]\\.git/"
                          ".*\\.gz\\'"
                          ".*\\.xz\\'"
                          ".*\\.zip\\'"
                          ".*-autoloads.el\\'"
                          "[/\\]archive-contents\\'"
                          "[/\\]\\.loaddefs\\.el\\'"
                          "[/\\]tmp/.*"
                          ".*/recentf\\'"
                          ".*/recentf-save.el\\'"
                          "~$"
                          "/.autosaves/"
                          ".*/TAGS\\'"
                          "*.cache"
                          ".*/treemacs/persist.org")
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
    (add-to-list 'recentf-exclude (file-truename no-littering-etc-directory))
    (add-to-list 'recentf-exclude (file-truename no-littering-var-directory)))

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
  ;; after idling for 30 seconds.
  (run-with-idle-timer 30 t #'recentf-save-list)

  ;; Adding many functions to `kill-emacs-hook' slows down Emacs exit, hence we are only using idle
  ;; timers.
  (run-with-idle-timer 60 t #'recentf-cleanup)
  :hook
  ;; Load immediately after start since I use it often
  (after-init . recentf-mode))

(defun sb/inhibit-message-call-orig-fun (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN, forward ARGS."
  (let ((inhibit-message t))
    (apply orig-fun args)))

;; Hide the "Wrote to recentf" message which is irritating
(advice-add 'recentf-save-list :around #'sb/inhibit-message-call-orig-fun)

;; TODO: Is this causing tramp to fail? I have disabled it to test.
;; Hide the "Wrote ..." message which is irritating
;; (advice-add 'write-region :around #'sb/inhibit-message-call-orig-fun)

;; The module does not specify an `autoload'
(use-package company-capf
  :ensure company
  :commands company-capf)

;; Use `M-x company-diag' or the modeline status to see the backend used. Try `M-x
;; company-complete-common' when there are no completions. Use `C-M-i' for `complete-symbol' with
;; regex search.
(use-package company
  :commands (company-abort company-files company-yasnippet
                           company-ispell company-dabbrev
                           company-capf company-dabbrev-code
                           company-clang-set-prefix
                           global-company-mode)
  :defines (company-dabbrev-downcase company-dabbrev-ignore-case
                                     company-dabbrev-other-buffers
                                     company-ispell-available
                                     company-ispell-dictionary)
  ;; :diminish
  :preface
  (defun sb/quit-company-save-buffer ()
    "Quit company popup and save the buffer."
    (interactive)
    (company-abort)
    (save-buffer))
  :hook (after-init . global-company-mode)
  :config
  (setq company-dabbrev-downcase nil ; Do not downcase returned candidates
        company-dabbrev-ignore-case nil ; Do not ignore case when collecting completion candidates
        company-dabbrev-other-buffers t ; Search in other buffers with the same major mode
        company-ispell-available t
        company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory)
        company-minimum-prefix-length 3 ; Small words are faster to type
        company-require-match nil ; Allow input string that do not match candidates
        company-selection-wrap-around t
        company-show-quick-access t ; Speed up completion
        ;; Align additional metadata, like type signatures, to the right-hand side
        company-tooltip-align-annotations t)

  ;; We set `company-backends' as a local variable, so it is not important to delete backends
  ;; (dolist (backends '(company-semantic company-bbdb company-oddmuse company-cmake))
  ;;   (delq backends company-backends))

  ;; Ignore matches that consist solely of numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  (push (apply-partially #'cl-remove-if
                         (lambda (c)
                           (string-match-p "\\`[0-9]+\\'" c)))
        company-transformers)

  (remove-hook 'kill-emacs-hook #'company-clang-set-prefix)
  :bind
  (:map company-active-map
        ("C-n"      . company-select-next)
        ("C-p"      . company-select-previous)
        ;; Insert the common part of all candidates, or select the next one
        ("<tab>"    . company-complete-common-or-cycle)
        ;; ("C-M-/" . company-other-backend) ; Was bound to `dabbrev-completion'
        ("C-s"      . sb/quit-company-save-buffer)
        ("<escape>" . company-abort)))

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; Hide the "Starting new Ispell process" message
(advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)
(advice-add 'ispell-lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; Posframes do not have unaligned rendering issues with variable `:height' unlike an overlay.
;; However, the width of the frame popup is often not enough and the right side gets cut off.
;; https://github.com/company-mode/company-mode/issues/1010
(use-package company-posframe
  :after company
  :demand t
  :commands company-posframe-mode
  :diminish
  :config
  (setq company-posframe-show-metadata nil ; Difficult to distinguish and distracting
        company-posframe-show-indicator nil
        company-posframe-quickhelp-delay nil)
  (company-posframe-mode 1))

(use-package company-quickhelp
  :after company
  :commands company-quickhelp-mode
  :init (run-at-time 3 nil #'company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :disabled t
  :unless (display-graphic-p)
  :after company-quickhelp
  :demand t
  :commands company-quickhelp-terminal-mode
  :config (company-quickhelp-terminal-mode 1))

;; Company now has in-built support for completion icons
(use-package company-box
  :if (display-graphic-p)
  :disabled t
  :after company
  :demand t
  :commands company-box-mode
  :diminish
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-show-single-candidate t
        company-frontends '(company-box-frontend))

  ;; (set-face-background 'company-box-background "cornsilk")
  ;; (set-face-background 'company-box-selection "light blue")

  (company-box-mode 1))

;; Typing `TabNine::config' in any buffer should open the extension settings, deep local mode is
;; computationally expensive. Completions seem to be laggy with TabNine enabled.
(use-package company-tabnine
  :after company
  :disabled t)

;; Nice but slows completions
(use-package company-fuzzy
  :after company
  :disabled t
  :diminish
  :commands (global-company-fuzzy-mode company-fuzzy-mode)
  :hook (company-mode . global-company-fuzzy-mode)
  :config
  (setq company-fuzzy-show-annotation t
        company-fuzzy-sorting-backend 'flx))

;; We are currently trying out `company-prescient'
(use-package company-statistics
  :disabled t
  :after company
  :commands company-statistics-mode
  :config (company-statistics-mode 1))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-global-mode snippet-mode)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook ((text-mode prog-mode) . yas-global-mode)
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))
        yas-verbosity 1)
  (unbind-key "<tab>" yas-minor-mode-map))

(use-package yasnippet-snippets
  :after yasnippet
  :demand t
  :commands yasnippet-snippets-initialize
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
  :if (eq sb/selection 'ivy)
  :after (yasnippet ivy)
  :bind ("C-M-y" . ivy-yasnippet))

;; `amx-major-mode-commands' limits to commands that are relevant to the current major mode
;; `amx-show-unbound-commands' shows frequently used commands that have no key bindings
(use-package amx
  :commands amx-mode
  :hook (after-init . amx-mode)
  :config
  (setq amx-auto-update-interval 5) ; Update the command list every n minutes
  (unless (bound-and-true-p sb/use-no-littering)
    (setq amx-save-file (expand-file-name "amx-items" sb/temp-directory))))

(use-package ivy
  :if (eq sb/selection 'ivy)
  :functions ivy-format-function-line
  :commands (ivy-read ivy-mode)
  :preface
  ;; https://github.com/abo-abo/swiper/wiki/Hiding-dired-buffers
  (defun sb/ignore-dired-buffers (str)
    "Return non-nil if STR names a Dired buffer.
    This function is intended for use with `ivy-ignore-buffers'."
    (let ((buf (get-buffer str)))
      (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))
  :hook (after-init . ivy-mode)
  :config
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
        ;; We update this after loading `orderless'
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

  :diminish
  :bind
  (("C-c r"    . ivy-resume)
   ("<f3>"     . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<return>" . ivy-alt-done) ; Continue completion
   ("<left>"   . ivy-previous-line)
   ("<right>"  . ivy-next-line)))

(use-package counsel
  :ensure amx
  :ensure t
  ;; :ensure-system-package fasd
  :commands counsel-mode
  :preface
  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
  (defun sb/counsel-goto-recent-directory ()
    "Open recent directories with `dired'."
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
            (append (mapcar 'file-name-directory recentf-list)
                    (if (executable-find "fasd")
                        (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "Directories:" collection :action 'dired)))
  :bind
  (([remap execute-extended-command] . counsel-M-x)
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
   ("C-c C-m"                        . counsel-mark-ring)
   ;; Enabling preview can make switching over remote buffers slow
   ;; ("<f3>"                        . counsel-switch-buffer)
   ("<f4>"                           . counsel-grep-or-swiper))
  :bind* ("C-c C-j"                  . counsel-imenu)
  :diminish
  :hook (ivy-mode . counsel-mode)
  :config
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

  ;; `counsel-flycheck' shows less information than `flycheck-list-errors', and there is an argument
  ;; error
  ;; (defalias 'flycheck-list-errors 'counsel-flycheck)

  ;; (add-to-list 'ivy-display-functions-alist
  ;;              '(counsel-company . ivy-display-function-overlay))
  )

(use-package ivy-hydra
  :after (ivy hydra)
  :demand t
  :disabled t
  :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body))

(use-package ivy-posframe
  :after ivy
  :disabled t ; The width is narrow and the text at the end gets cut off
  :diminish
  :config
  ;; Different command can use different display function.
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-window-bottom-left)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
          (t               . ivy-posframe-display))
        ivy-display-function #'ivy-posframe-display-at-frame-center
        ivy-posframe-parameters '((left-fringe  . 4)
                                  (right-fringe . 4)))
  (ivy-posframe-mode 1))

(use-package prescient
  :commands prescient-persist-mode
  :hook (after-init . prescient-persist-mode)
  :config
  (unless (bound-and-true-p sb/use-no-littering)
    (setq prescient-save-file (expand-file-name "prescient-save.el"
                                                sb/temp-directory))))

;; https://github.com/raxod502/prescient.el/issues/65
(use-package ivy-prescient
  :disabled t ; No longer maintained
  :hook (counsel-mode . ivy-prescient-mode)
  :config
  (setq ivy-prescient-enable-sorting nil ; Allow prescient to override sorting logic
        ivy-prescient-sort-function '(not swiper swiper-isearch
                                          ivy-switch-buffer
                                          counsel-recentf counsel-grep
                                          flyspell-correct-ivy )))

;; https://www.reddit.com/r/emacs/comments/9o6inu/sort_ivys_counselrecentf_results_by_timestamp/e7ze1c8/
;; LATER: This is expensive, we can possibly reduce the size of the list. But we can also search
;; easily with `ivy', so maybe sorting is not very important given the overhead.
;; (with-eval-after-load "ivy"
;;   (add-to-list 'ivy-sort-functions-alist '(counsel-recentf . file-newer-than-file-p)))

;; (setq ivy-sort-matches-functions-alist '((t . ivy--prefix-sort)))

;; (add-to-list 'ivy-sort-matches-functions-alist
;;              '(read-file-name-internal . ivy--sort-files-by-date))

;; We want `capf' sort for programming modes, not with recency. This breaks the support for the
;; `:separate' keyword in `company'
(use-package company-prescient
  :after company
  :demand t
  :commands company-prescient-mode
  :config (company-prescient-mode 1))

(use-package all-the-icons-ivy
  :after ivy
  :demand t
  :disabled t
  :commands all-the-icons-ivy-setup
  :config (all-the-icons-ivy-setup))

(use-package orderless
  :after ivy
  :demand t
  :defines orderless-component-separator
  :functions just-one-face
  :config
  (when (eq sb/selection 'ivy)
    (defvar ivy-re-builders-alist)
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

  (setq completion-styles '(orderless initials basic partial-completion emacs22)
        orderless-component-separator 'orderless-escapable-split-on-space)

  ;; (declare-function sb/just-one-face "init-use-package")

  ;; (defun just-one-face (fn &rest args)
  ;;   (let ((orderless-match-faces [completions-common-part]))
  ;;     (apply fn args)))

  ;; (advice-add 'company-capf--candidates :around #'just-one-face)

  )

(use-package ispell
  :ensure nil
  :if (symbol-value 'sb/IS-LINUX)
  :config
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
  (add-to-list 'ispell-skip-region-alist '("=" "=")))

;; Hide the "Starting new Ispell process" message
(advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)

(use-package flyspell
  :ensure nil
  :if (symbol-value 'sb/IS-LINUX)
  :commands (flyspell-overlay-p flyspell-correct-previous flyspell-correct-next)
  :preface
  ;; Move point to previous error
  ;; http://emacs.stackexchange.com/a/14912/2017
  ;; http://pragmaticemacs.com/emacs/jump-back-to-previous-typo/
  (defun sb/flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; goto beginning of buffer
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; save the current location for next invocation
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
            (progn
              (message "No more misspelled word!")
              (setq arg 0))
          (forward-word)))))
  :config
  (setq flyspell-abbrev-p t
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook
  ;; Enabling `flyspell-prog-mode' does not seem to be very useful and highlights links and
  ;; language-specific words
  ((text-mode  . flyspell-mode)
   ;; (before-save-hook . flyspell-buffer) ; Saving files will be slow
   ;; `find-file-hook' will not work for buffers with no associated files
   (after-init . (lambda ()
                   (when (string= (buffer-name) "*scratch*")
                     (flyspell-mode 1)))))
  :diminish
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   :map flyspell-mode-map
   ("C-;"     . nil)
   ("C-,"     . sb/flyspell-goto-previous-error)))

;; Flyspell popup is more efficient. Ivy-completion does not show the "Save" option in a few cases.
(or
 (use-package flyspell-popup
   :bind ("C-;" . flyspell-popup-correct)
   :config (setq flyspell-popup-correct-delay 0.2))

 (use-package flyspell-correct-ivy
   :ensure flyspell-correct
   :disabled t
   :if (eq sb/selection 'ivy)
   :ensure t
   :bind ("C-;" . flyspell-correct-wrapper)))

;; As of Emacs 28, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.
(use-package spell-fu
  :defines spell-fu-directory
  :commands spell-fu-mode
  :hook (text-mode . spell-fu-mode)
  :config
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
  :bind
  (("C-c f n" . spell-fu-goto-next-error)
   ("C-c f p" . spell-fu-goto-previous-error)
   ("C-c f a" . spell-fu-word-add)))

(or
 (use-package highlight-indentation
   :commands highlight-indentation-mode
   :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
   :hook ((yaml-mode python-mode) . highlight-indentation-mode))

 (use-package highlight-indent-guides
   :disabled t
   :diminish
   :hook ((yaml-mode python-mode) . highlight-indent-guides-mode)
   :config (setq highlight-indent-guides-method 'character)))

;; Claims to be better than `electric-indent-mode'
(use-package aggressive-indent
  :diminish
  :commands aggressive-indent-mode
  :hook ((lisp-mode emacs-lisp-mode lisp-interaction-mode) . aggressive-indent-mode)
  :config
  (setq aggressive-indent-comments-too t
        ;; Never use `electric-indent-mode'
        aggressive-indent-dont-electric-modes t))

(use-package paren
  :ensure nil
  :init (run-at-time 3 nil #'show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis ; `mixed' may lead to performance problems
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package elec-pair
  :ensure nil
  :commands (electric-pair-mode)
  :init
  ;; Enable autopairing, `smartparens' seems slow
  (run-at-time 2 nil #'electric-pair-mode)
  :config
  ;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
  (defvar sb/markdown-pairs '((?` . ?`)) "Electric pairs for `markdown-mode'.")
  (defvar electric-pair-pairs)
  (defvar electric-pair-text-pairs)
  (defvar electric-pair-preserve-balance)

  (declare-function sb/add-markdown-pairs "init-use-package")

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
                                    (electric-pair-mode 1))))

;; https://web.archive.org/web/20201109035847/http://ebzzry.io/en/emacs-pairs/
;; Seems to have performance issue with `latex-mode', `markdown-mode', and large JSON files.
;; `sp-cheat-sheet' will show you all the commands available, with examples.
(use-package smartparens-config
  :ensure smartparens
  :disabled t
  ;; :diminish (smartparens-global-mode smartparens-mode
  ;;                                    show-smartparens-mode show-smartparens-global-mode)
  :hook
  (((latex-mode LaTeX-mode) . (lambda ()
                                (require 'smartparens-latex)))
   (after-init . (lambda ()
                   (smartparens-global-mode 1)
                   (show-smartparens-global-mode 1))))
  :config
  (setq sp-show-pair-from-inside t
        sp-autoskip-closing-pair 'always)

  ;; (smartparens-strict-mode -1)

  ;; Stop pairing single quotes in elisp
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

  (sp-local-pair 'markdown-mode "<" ">")

  ;; Do not insert a parenthesis pair when the point is at the beginning of a word
  ;; (sp-pair "(" nil :unless '(sp-point-before-word-p))
  ;; (sp-pair "[" nil :unless '(sp-point-before-word-p))
  ;; (sp-pair "{" nil :unless '(sp-point-before-word-p))
  ;; (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p))
  :bind
  (("C-M-a" . sp-beginning-of-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-e" . sp-end-of-sexp) ; "f_oo bar" -> "foo bar_"
   ("C-M-u" . sp-up-sexp) ; "f_oo bar" -> "foo bar"_
   ("C-M-w" . sp-down-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-f" . sp-forward-sexp) ; "foo ba_r" -> "foo bar"_
   ("C-M-b" . sp-backward-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-n" . sp-next-sexp) ; ))" -> ((foo) (bar))"
   ("C-M-p" . sp-previous-sexp) ; "(foo (b|ar baz))" -> "(foo| (bar baz))"
   ("C-S-b" . sp-backward-symbol) ; "foo bar| baz" -> "foo |bar baz"
   ("C-S-f" . sp-forward-symbol) ; "|foo bar baz" -> "foo| bar baz"
   ;; "(foo bar)" -> "foo bar"
   ("C-M-k" . sp-splice-sexp)))

(use-package projectile
  ;; :ensure-system-package fd
  :commands (projectile-project-p projectile-project-name
                                  projectile-expand-root
                                  projectile-project-root
                                  projectile-mode)
  :preface
  (eval-and-compile
    (defun sb/projectile-kill ()
      (interactive)
      (projectile-kill-buffers)
      (treemacs)))
  :config
  (setq projectile-enable-caching t ; Caching will not watch for new files automatically
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
    (setq projectile-cache-file (expand-file-name "projectile.cache" sb/temp-directory)
          projectile-known-projects-file (expand-file-name "projectile-known-projects.eld"
                                                           sb/temp-directory)))

  ;; https://github.com/MatthewZMD/.emacs.d
  (when (and sb/IS-WINDOWS
             (executable-find "tr"))
    (setq projectile-indexing-method 'alien))

  ;; Disable computing the project type that is shown on the modeline
  (defun projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      (format " %s [%s] "
              projectile-mode-line-prefix
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

  ;; Set search path for finding projects when `projectile-mode' is enabled, however auto-search for
  ;; projects is disabled for faster startup
  (setq projectile-auto-discover nil
        projectile-project-search-path (list
                                        (concat `,(getenv "HOME") "/bitbucket")
                                        (expand-file-name "github" sb/user-home)
                                        (expand-file-name "iitk-workspace" sb/user-home)
                                        (expand-file-name "iitkgp-workspace" sb/user-home)
                                        (expand-file-name "iss-workspace" sb/user-home)
                                        (expand-file-name "plass-workspace" sb/user-home)
                                        (expand-file-name "prospar-workspace" sb/user-home)
                                        ))

  (dolist (prjs (list
                 (expand-file-name sb/user-home) ; Do not consider $HOME as a project
                 "~/" ; Do not consider $HOME as a project
                 (expand-file-name "/tmp")
                 (expand-file-name "bitbucket/.metadata" sb/user-home)
                 (expand-file-name "github/.metadata" sb/user-home)
                 (expand-file-name "iitk-workspace/.metadata" sb/user-home)
                 (expand-file-name "plass-workspace/.metadata" sb/user-home)
                 ))
    (add-to-list 'projectile-ignored-projects prjs))

  ;; Filtering works with `alien' indexing
  (dolist (dirs
           '(".cache" ".clangd" ".dropbox" ".git" ".hg" ".metadata" ".nx" ".recommenders" ".svn"
             ".vscode" "__pycache__" "auto" "elpa" "node_modules"))
    (add-to-list 'projectile-globally-ignored-directories dirs))

  (dolist (items
           '("GPATH" "GRTAGS" "GTAGS" "GSYMS" "TAGS" "tags" ".tags" "__init__.py"))
    (add-to-list 'projectile-globally-ignored-files items))

  (dolist (exts
           '(".a" ".aux" ".bak" ".blg" ".class" ".deb" ".doc" ".docx" ".elc" ".o" ".odt" ".ppt"
             ".pptx" ".pt" ".pyc" ".rel" ".rip" ".rpm" ".so" ".xls" ".xlsx" "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes exts))

  (projectile-mode 1)

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

  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  ;; Use idle timer in case we open a project file without enabling projectile via bind-keys
  (run-with-idle-timer 3 nil #'projectile-mode)
  :bind
  ;; Set these in case `counsel-projectile' is disabled
  (("<f5>" . projectile-switch-project)
   ("<f6>" . projectile-find-file)
   :map projectile-command-map
   ("A"    . projectile-add-known-project)))


(use-package counsel-projectile
  :disabled t
  :defines counsel-projectile-default-file
  :commands (counsel-projectile-switch-project-by-name counsel-projectile-mode)
  :preface
  (defun sb/counsel-projectile-switch-project-magit (project)
    "Open Magit for the PROJECT."
    (let ((projectile-switch-project-action 'magit-status))
      (counsel-projectile-switch-project-by-name project)))

  (defun sb/counsel-projectile-open-default-file ()
    "Open the current project's default file.
        This file is specified in `counsel-projectile-default-file'."
    (interactive)
    (let ((file counsel-projectile-default-file))
      (if (and file
               (setq file (projectile-expand-root file))
               (file-exists-p file))
          (find-file file)
        (message "File %s doesn't exist." file))))

  ;; Set `counsel-projectile-switch-project-action' to the following action
  (defun sb/counsel-projectile-switch-project-action-default-file (project)
    "Open PROJECT's default file.
        This file is specified in `counsel-projectile-default-file'."
    (let ((projectile-switch-project-action #'sb/counsel-projectile-open-default-file))
      (counsel-projectile-switch-project-by-name project)))
  :config
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
  :bind
  ;; The `counsel' actions seem to be slower than base `projectile'
  (("<f5>" . counsel-projectile-switch-project)
   ("<f6>" . counsel-projectile-find-file)
   ;; ("<f7>" . counsel-projectile-rg)
   ;; ([remap projectile-switch-project]   . counsel-projectile-switch-project)
   ;; ([remap projectile-find-file]        . counsel-projectile-find-file)
   ;; ([remap projectile-find-dir]         . counsel-projectile-find-dir)
   ;; ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
   ))

;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el
;; Enable before `ivy-rich-mode' for better performance
;; The new transformers (file permissions) seem more of an overkill and buggy, and it hides the
;; file names
(use-package all-the-icons-ivy-rich
  :ensure t
  :ensure ivy-rich
  :disabled t
  :commands all-the-icons-ivy-rich-mode
  :if (and (eq sb/selection 'ivy) (display-graphic-p))
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :config (setq all-the-icons-ivy-rich-icon-size 1.0))

(use-package ivy-rich
  :if (eq sb/selection 'ivy)
  :commands ivy-rich-modify-column
  :hook (ivy-mode . ivy-rich-mode)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  ;; Increase the width to see the major mode clearly
  ;; FIXME: `ivy-rich-modify-column' is not taking effect.
  (ivy-rich-modify-column 'ivy-switch-buffer
                          'ivy-rich-switch-buffer-major-mode
                          '(:width 18 :face warning)))

(use-package counsel-fd
  :disabled t
  :if (and (eq sb/selection 'ivy) (executable-find "fd"))
  :bind
  (("C-x d" . counsel-fd-dired-jump) ; Jump to a directory below the current directory
   ;; Jump to a file below the current directory
   ("C-x f" . counsel-fd-file-jump)))

(use-package flycheck
  :commands (flycheck-add-next-checker flycheck-next-checker
                                       flycheck-mode
                                       global-flycheck-mode
                                       flycheck-previous-error
                                       flycheck-describe-checker
                                       flycheck-buffer
                                       flycheck-list-errors
                                       flycheck-select-checker
                                       flycheck-verify-setup
                                       flycheck-next-error
                                       flycheck-disable-checker
                                       flycheck-add-mode
                                       flycheck-manual
                                       flycheck-display-error-messages-unless-error-list
                                       flycheck-sexp-to-string)
  :init
  ;; There are no checkers for modes like `csv-mode', and many program modes use lsp `yaml-mode' is
  ;; derived from `text-mode'
  (run-at-time 2 nil #'global-flycheck-mode)
  :config
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
        flycheck-global-modes '(not csv-mode)
        flycheck-chktexrc "chktexrc")

  ;; TODO: Is this the reason why `flycheck' and `doom-modeline' does not work well?
  (when (or (eq sb/modeline-theme 'spaceline)
            (eq sb/modeline-theme 'doom-modeline))
    (setq flycheck-mode-line nil))

  (setq-default flycheck-markdown-markdownlint-cli-config (expand-file-name ".markdownlint.json"
                                                                            sb/user-home)
                flycheck-pylintrc (expand-file-name ".config/pylintrc" sb/user-home)
                flycheck-python-pylint-executable "python3"
                flycheck-shellcheck-follow-sources nil
                flycheck-textlint-config (expand-file-name "textlintrc.json" sb/textlint-home)
                flycheck-textlint-executable (expand-file-name "node_modules/.bin/textlint"
                                                               sb/textlint-home))

  (add-to-list 'flycheck-textlint-plugin-alist '(tex-mode . "latex"))
  (add-to-list 'flycheck-textlint-plugin-alist '(rst-mode . "rst"))

  ;; Add support for `org-lint' as a checker
  (defconst flycheck-org-lint-form
    (flycheck-prepare-emacs-lisp-form
      (require 'org)
      (require 'org-attach)
      (let ((source (car command-line-args-left))
            (process-default-directory default-directory))
        (with-temp-buffer
          (insert-file-contents source 'visit)
          (setq buffer-file-name source)
          (setq default-directory process-default-directory)
          (delay-mode-hooks (org-mode))
          (setq delayed-mode-hooks nil)
          (dolist (err (org-lint))
            (let ((inf (cl-second err)))
              (princ (elt inf 0))
              (princ ": ")
              (princ (elt inf 2))
              (terpri)))))))

  (defconst flycheck-org-lint-variables
    '(org-directory
      org-id-locations
      org-id-locations-file
      org-attach-id-dir
      org-attach-use-inheritance
      org-attach-id-to-path-function-list)
    "Variables inherited by the org-lint subprocess.")

  (defun flycheck-org-lint-variables-form ()
    (require 'org-attach)  ; Needed to make variables available
    `(progn
       ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
                  (seq-filter #'boundp flycheck-org-lint-variables))))

  (flycheck-define-checker org-lint
    "Org buffer checker using `org-lint'."
    :command ("emacs" (eval flycheck-emacs-args)
              "--eval" (eval (concat "(add-to-list 'load-path \""
                                     (file-name-directory (locate-library "org"))
                                     "\")"))
              "--eval" (eval (flycheck-sexp-to-string
                              (flycheck-org-lint-variables-form)))
              "--eval" (eval flycheck-org-lint-form)
              "--" source)
    :error-patterns
    ((error line-start line ": " (message) line-end))
    :modes (org-mode))

  (add-to-list 'flycheck-checkers 'org-lint t)

  ;; https://github.com/flycheck/flycheck/issues/1833
  (add-to-list 'flycheck-hooks-alist '(after-revert-hook . flycheck-buffer))

  ;; Exclude directories and files from being checked
  ;; https://github.com/flycheck/flycheck/issues/1745

  (declare-function sb/flycheck-may-check-automatically "init-use-package.el")

  (defvar sb/excluded-directory-regexps
    '(".git/" "elpa/"))

  (defun sb/flycheck-may-check-automatically (&rest _conditions)
    (or (null buffer-file-name)
        (let ((bufname (file-truename buffer-file-name)))
          (not (seq-some (lambda (re) (string-match-p re bufname))
                         sb/excluded-directory-regexps)))))

  (advice-add 'flycheck-may-check-automatically
              :after-while #'sb/flycheck-may-check-automatically)

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

  )

(or
 ;; Does not display popup under TTY, check possible workarounds at
 ;; https://github.com/flycheck/flycheck-popup-tip
 (use-package flycheck-pos-tip
   :commands flycheck-pos-tip-mode
   :if (display-graphic-p)
   :hook (flycheck-mode . flycheck-pos-tip-mode))

 ;; Showing errors/warnings in a posframe seems more intrusive than showing errors in the minibuffer
 (use-package flycheck-posframe
   :disabled t
   :if (display-graphic-p)
   :commands (flycheck-posframe-mode flycheck-posframe-configure-pretty-defaults)
   :hook (flycheck-mode . flycheck-posframe-mode)
   :config
   (setq flycheck-posframe-position 'point-bottom-left-corner
         flycheck-posframe-border-width 1)

   (flycheck-posframe-configure-pretty-defaults)))

(use-package whitespace
  :disabled t
  :commands (whitespace-mode global-whitespace-mode
                             whitespace-buffer whitespace-cleanup
                             whitespace-turn-off)
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  :hook (markdown-mode . whitespace-mode)
  :config
  (setq show-trailing-whitespace t
        whitespace-line-column sb/fill-column
        whitespace-style '(face lines-tail trailing)))

;; This is different from `whitespace-cleanup-mode' since this is unconditional
(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (setq delete-trailing-lines t) ; `M-x delete-trailing-whitespace' deletes trailing lines
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

;; Call `whitespace-cleanup' only if the initial buffer was clean
;; To enable it for an entire project, set `whitespace-cleanup-mode' to `t' in your
;; `.dir-locals.el' file.
(use-package whitespace-cleanup-mode
  :diminish
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  ;; :hook (after-init . global-whitespace-cleanup-mode)
  :config
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)
  (setq whitespace-cleanup-mode-preserve-point t))

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :commands ws-butler-mode
  :diminish
  :hook (prog-mode . ws-butler-mode))

;; Highlight symbol under point
(use-package symbol-overlay
  :commands (symbol-overlay-mode)
  :diminish
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind
  (("M-p" . symbol-overlay-jump-prev)
   ("M-n" . symbol-overlay-jump-next)))

(use-package hl-todo
  :commands global-hl-todo-mode
  :init (run-at-time 5 nil #'global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces (append '(("LATER"    . "#d0bf8f")
                                        ("ISSUE"    . "#ff8c00")
                                        ("DEBUG"    . "#ff8c00")
                                        ("TEST"     . "tomato")
                                        ("WARNING"  . "#cc0000")
                                        ("BEWARE"   . "#aa0000")
                                        ("REFACTOR" . "#cc9393"))
                                      hl-todo-keyword-faces)))
(use-package highlight-numbers
  :commands highlight-numbers-mode
  :hook ((prog-mode conf-mode css-mode html-mode) . highlight-numbers-mode))

(use-package page-break-lines
  :diminish
  :commands (global-page-break-lines-mode page-break-lines-mode)
  :init (run-at-time 5 nil #'global-page-break-lines-mode))

(use-package number-separator
  :ensure nil
  :load-path "extras"
  :commands number-separator-mode
  :disabled t
  ;; :quelpa ((number-separator :fetcher github :repo "legalnonsense/number-separator.el"
  ;;                            :files ("number-separator.el")))
  :diminish
  :config
  (setq number-separator ","
        number-separator-interval 3
        number-separator-ignore-threshold 4
        number-separator-decimal-char "."))

(use-package highlight-escape-sequences
  :commands hes-mode
  :hook (prog-mode . hes-mode))

;; First mark the word, then add more cursors. Use `mc/edit-lines' to add a cursor to each line in
;; an active region that spans multiple lines.
(use-package multiple-cursors
  :disabled t
  :bind
  (("C-<"     . mc/mark-previous-like-this)
   ("C->"     . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; Edit remote file: `/method:user@host#port:filename'. Shortcut /ssh:: will connect to default
;; user@host#port.
;; Edit local file with sudo: `C-x C-f /sudo::/etc/hosts'.
;; Open a remote file with ssh + sudo: `C-x C-f /ssh:host|sudo:root:/etc/passwd'.
;; Use bookmarks to speed up remote file access: upon visiting a location with TRAMP, save it as a
;; bookmark with `bookmark-set'. To revisit that bookmark, use `bookmark-jump'.
(use-package tramp
  :config
  ;; Auto-save to a local directory for better performance
  (unless (bound-and-true-p sb/use-no-littering)
    (setq tramp-auto-save-directory (expand-file-name "tramp-auto-save" sb/temp-directory)
          tramp-persistency-file-name (expand-file-name "tramp" sb/temp-directory)))

  (setq tramp-default-user "swarnendu"
        ;; tramp-default-method "ssh" ; SSH is faster than the default SCP
        ;; tramp-default-remote-shell "/bin/bash"
        remote-file-name-inhibit-cache nil ; Remote files are not updated outside of Tramp
        tramp-verbose 1
        ;; Disable version control
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp tramp-file-name-regexp))

  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
  (setenv "SHELL" "/bin/bash") ; Recommended to connect with bash

  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; https://www.gnu.org/software/tramp/
  (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  :bind ("C-S-q" . tramp-cleanup-all-buffers))

(declare-function sb/sshlist "private")

(when (eq sb/selection 'ivy)
  (progn
    (declare-function sb/ivy-tramp "init-use-package")

    (defun sb/ivy-tramp ()
      "Invoke remote hosts with ivy and tramp."
      (interactive)
      (counsel-find-file (ivy-read "Remote Tramp targets: " (sb/sshlist))))

    (bind-key "C-c d t" #'sb/ivy-tramp)))

(when (eq sb/selection 'selectrum)
  (progn
    (declare-function sb/selectrum-tramp "init-use-package")

    (defun sb/selectrum-tramp ()
      "Invoke remote hosts with selectrum and tramp."
      (interactive)
      (find-file (completing-read "Remote Tramp targets: " (sb/sshlist))))

    (bind-key "C-c d t" #'sb/selectrum-tramp)))

;; TODO: SSH into Gcloud
;; https://gist.github.com/jackrusher/36c80a2fd6a8fe8ddf46bc7e408ae1f9
;; Make sure you've set your default project with:
;; gcloud config set project <project-name>
;; C-x C-f /gcssh:compute-instance:/path/to/filename.clj

(use-package imenu
  :ensure nil
  :after (:any markdown-mode yaml-mode prog-mode)
  :config
  (setq imenu-auto-rescan t
        imenu-max-items 500
        imenu-max-item-length 100
        ;; `t' will use a popup menu rather than a minibuffer prompt, `on-mouse' might be useful
        ;; with mouse support enabled
        imenu-use-popup-menu nil
        ;; `nil' implies no sorting or listing by position in the buffer
        imenu-sort-function nil))

;; `imenu-anywhere' provides navigation for imenu tags across all buffers that satisfy grouping
;; criteria. Available criteria include - all buffers with the same major mode, same project buffers
;; and user defined list of friendly mode buffers.
(use-package imenu-anywhere
  :after imenu
  :demand t
  :disabled t)

(defvar tags-revert-without-query)
(setq large-file-warning-threshold (* 500 1024 1024) ; MB
      tags-add-tables nil
      tags-case-fold-search nil ; t=case-insensitive, nil=case-sensitive
      ;; Do not ask before rereading the `TAGS' files if they have changed
      tags-revert-without-query t)

;; Gtags is less maintained than `universal-ctags'
(use-package counsel-gtags
  :disabled t
  :if (and (eq system-type 'gnu/linux) (eq sb/tags-scheme 'gtags))
  :diminish
  ;; :ensure-system-package global
  ;; :init
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'c-mode 'c++-mode)
  ;;               (counsel-gtags-mode 1))))
  :hook ((prog-mode protobuf-mode latex-mode LaTeX-mode) . counsel-gtags-mode)
  :config (setq counsel-gtags-auto-update t)
  :bind
  (:map counsel-gtags-mode-map
        ("M-'"     . counsel-gtags-dwim)
        ("M-,"     . counsel-gtags-go-backward)
        ("M-?"     . counsel-gtags-find-reference)
        ("C-c g s" . counsel-gtags-find-symbol)
        ("C-c g d" . counsel-gtags-find-definition)
        ("C-c g c" . counsel-gtags-create-tags)
        ("C-c g u" . counsel-gtags-update-tags)))

(use-package global-tags ; Make xref and gtags work together
  :disabled t
  :after counsel-gtags
  :demand t
  :config (add-to-list 'xref-backend-functions 'global-tags-xref-backend))

(use-package xref
  :commands xref-etags-mode
  :bind
  (("M-'"   . xref-find-definitions)
   ("M-?"   . xref-find-references)
   ("C-M-." . xref-find-apropos)
   ("M-,"   . xref-pop-marker-stack)
   :map xref--xref-buffer-mode-map
   ("C-o"   . xref-show-location-at-point)
   ("<tab>" . xref-quit-and-goto-xref)
   ("r"     . xref-query-replace-in-results)))

(use-package ivy-xref
  :after (ivy xref)
  :if (eq sb/selection 'ivy)
  :config
  (setq xref-show-definitions-function #'ivy-xref-show-defs
        xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel-etags
  ;; :ensure-system-package (ctags . "snap install universal-ctags")
  :if (and (eq system-type 'gnu/linux) (eq sb/tags-scheme 'ctags))
  :disabled t
  :bind
  (("M-]"     . counsel-etags-find-tag-at-point)
   ("C-c g s" . counsel-etags-find-symbol-at-point)
   ("C-c g f" . counsel-etags-find-tag)
   ("C-c g l" .  counsel-etags-list-tag)
   ("C-c g c" . counsel-etags-scan-code))
  :config
  (defalias 'list-tags 'counsel-etags-list-tag-in-current-file)

  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        #'counsel-etags-virtual-update-tags 'append
                        'local)))

  (dolist (ignore-dirs '(".vscode" "build" ".metadata" ".recommenders" ".clangd"))
    (add-to-list 'counsel-etags-ignore-directories ignore-dirs))
  (dolist (ignore-files '(".clang-format" ".clang-tidy" "*.json" "*.html" "*.xml"))
    (add-to-list 'counsel-etags-ignore-filenames ignore-files)))

(use-package dumb-jump
  :after xref
  :demand t
  :commands dumb-jump-xref-activate
  :config
  (setq dumb-jump-force-searcher 'rg
        dumb-jump-prefer-searcher 'rg
        dumb-jump-quiet t)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; The built-in `describe-function' includes both functions and macros. `helpful-function' is
;; functions only, so `helpful-callable' as a drop-in replacement.
(use-package helpful
  :bind
  (("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h f" . helpful-callable)
   ("C-h c" . helpful-command)
   ("C-h p" . helpful-at-point)
   ("C-h o" . helpful-symbol)
   :map helpful-mode-map
   ("q"     . helpful-kill-buffers)))

(use-package vlf ; Speed up Emacs for large files: `M-x vlf <PATH-TO-FILE>'
  :commands vlf
  :defines vlf-application
  :config
  (setq vlf-application 'dont-ask)
  (require 'vlf-setup))

;; Erase all consecutive white space characters in a given direction
(use-package hungry-delete
  :commands (hungry-delete-mode global-hungry-delete-mode)
  :diminish
  :hook
  ((minibuffer-setup . (lambda ()
                         (hungry-delete-mode -1)))
   (after-init . global-hungry-delete-mode)))

(use-package move-text ; Move lines with `M-<up>' and `M-<down>'
  :commands (move-text-up move-text-down move-text-default-bindings)
  :init (move-text-default-bindings))

(use-package duplicate-thing
  :bind* ("C-c C-d" . duplicate-thing))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind
  (("C-h C-m" . discover-my-major)
   ("C-h M-m" . discover-my-mode)))

;; Manage minor-mode on the dedicated interface buffer
(use-package manage-minor-mode
  :commands manage-minor-mode)

(use-package jgraph-mode
  :disabled t
  :commands jgraph-mode)

(use-package graphviz-dot-mode
  :disabled t
  :commands graphviz-dot-mode)

(use-package gnuplot
  :disabled t
  :commands (gnuplot-mode gnuplot)
  :mode "\\.gp\\'"
  :interpreter ("gnuplot" . gnuplot-mode))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(use-package popwin
  :disabled t
  :commands popwin-mode
  :hook (after-init . popwin-mode)
  :config
  (defvar popwin:special-display-config-backup popwin:special-display-config)

  (push '("*Help*" :noselect t) popwin:special-display-config)
  (push '(compilation-mode :noselect t) popwin:special-display-config)
  (push '("*Compile-Log*" :noselect t) popwin:special-display-config)
  (push '("*manage-minor-mode*" :noselect t) popwin:special-display-config)
  (push '("*Paradox Report*" :noselect t) popwin:special-display-config)
  (push '("*Selection Ring:") popwin:special-display-config)
  (push '("*Flycheck errors*" :noselect nil) popwin:special-display-config)
  (push '("*Flycheck checkers*" :noselect nil) popwin:special-display-config)
  (push '("*ripgrep-search*" :noselect nil) popwin:special-display-config)
  (push '("^\*magit:.+\*$" :noselect nil) popwin:special-display-config)
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

(use-package expand-region ; Expand region by semantic units
  :bind
  (("C-="   . er/expand-region)
   ("C-M-=" . er/contract-region)))

;; Restore point to the initial location with `C-g' after marking a region
(use-package smart-mark
  :init (run-at-time 5 nil #'smart-mark-mode))

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :commands (whole-line-or-region-local-mode whole-line-or-region-global-mode)
  :diminish (whole-line-or-region-local-mode)
  :init (run-at-time 5 nil #'whole-line-or-region-global-mode))

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

(use-package beginend
  :init (run-at-time 5 nil #'beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))

(use-package undo-tree
  :disabled t
  :defines undo-tree-map
  :commands (global-undo-tree-mode)
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-mode-lighter ""
        undo-tree-visualizer-diff t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode 1)
  (unbind-key "C-/" undo-tree-map)
  :diminish
  :bind ("C-x u" . undo-tree-visualize))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :bind* ("C-." . iedit-mode))

;; Avoid the "Overwrite old session file (not loaded)?" warning
(use-package session
  :disabled t
  :commands (session-initialize)
  :config
  (unless (bound-and-true-p sb/use-no-littering)
    (setq session-save-file (expand-file-name "session" sb/temp-directory)))
  :hook (after-init . session-initialize))

(use-package immortal-scratch
  :commands immortal-scratch-mode
  :init (run-with-idle-timer 3 nil #'immortal-scratch-mode))

;; I use the *scratch* buffer for taking notes, it helps to make the data persist
(use-package persistent-scratch
  :commands persistent-scratch-setup-default
  :hook (after-init . persistent-scratch-setup-default)
  :config
  (setq persistent-scratch-autosave-interval 300)

  (unless (bound-and-true-p sb/use-no-littering)
    (setq persistent-scratch-save-file (expand-file-name "persistent-scratch" sb/temp-directory))))

;; `crux-smart-open-line-above', `crux-smart-open-line', `crux-smart-kill-line'
(use-package crux
  :bind
  (("C-c d i" . crux-ispell-word-then-abbrev)
   ("<f12>"   . crux-kill-other-buffers)
   ("C-c d s" . crux-sudo-edit)))

;; TODO: Is this causing the mouse-movement error and breaking out of key bindings?
(use-package disable-mouse
  :if (display-mouse-p)
  :disabled t
  :commands global-disable-mouse-mode
  :diminish disable-mouse-global-mode
  :hook (after-init . global-disable-mouse-mode))

;; TODO: Is this causing the mouse-movement error and breaking out of key bindings?
(use-package avoid
  :ensure nil
  :commands mouse-avoidance-mode
  :if (display-mouse-p)
  :init (mouse-avoidance-mode 'banish))

(use-package apt-sources-list
  :disabled t
  :commands apt-sources-list-mode)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook ((prog-mode latex-mode LaTeX-mode org-src-mode) . rainbow-delimiters-mode))

(use-package ssh-config-mode
  :commands (ssh-config-mode ssh-known-hosts-mode ssh-authorized-keys-mode turn-on-font-lock)
  :hook (ssh-config-mode . turn-on-font-lock))

(use-package pomidor
  :disabled t
  :commands (pomidor-quit pomidor-break pomidor-reset
                          pomidor-stop pomidor-hold
                          pomidor-unhold pomidor))

(use-package ace-window
  :bind
  (([remap other-window] . ace-window)
   ("<f10>"              . ace-window)))

(use-package windmove ; `Shift + direction' arrows
  :ensure nil
  :commands windmove-default-keybindings
  :init (windmove-default-keybindings)
  :config
  ;; Wrap around at edges
  (setq windmove-wrap-around t))

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `after-save-hook' and leads to auto-formatters being invoked more frequently.
(use-package super-save
  :commands super-save-mode
  :disabled t
  :diminish
  :init (run-with-idle-timer 3 nil #'super-save-mode)
  :config
  (setq super-save-remote-files nil) ; Ignore remote files
  (add-to-list 'super-save-triggers 'ace-window))

;; `avy-setup-default' will bind `avy-isearch' to `C-'' in `isearch-mode-map', so that you can
;; select one of the currently visible `isearch' candidates using `avy'.
(use-package avy
  :commands avy-setup-default
  :bind
  (("M-b" . avy-goto-word-1)
   ("C-'" . avy-goto-char-timer)
   ("C-/" . avy-goto-line)))

;; This package adds a `C-'' binding to Ivy minibuffer that uses Avy
(use-package ivy-avy
  :if (eq sb/selection 'ivy)
  :after (ivy avy)
  :bind
  (:map ivy-minibuffer-map
        ("C-'" . ivy-avy)))

(use-package bookmark
  :ensure nil
  :config
  (unless (bound-and-true-p sb/use-no-littering)
    (setq bookmark-default-file (expand-file-name "bookmarks" sb/temp-directory))))

(use-package bm
  :commands (bm-buffer-save-all bm-repository-save bm-toggle bm-next bm-previous
                                bm-repository-load bm-buffer-save bm-buffer-restore)
  :preface
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
  :init
  ;; Must be set before `bm' is loaded
  (setq bm-restore-repository-on-load t)
  (run-with-idle-timer 2 nil #'sb/bm-setup)
  :config
  (setq-default bm-buffer-persistence t)

  (unless (bound-and-true-p sb/use-no-littering)
    (setq bm-repository-file (expand-file-name "bm-bookmarks" sb/temp-directory)))
  :bind
  (("C-<f1>" . bm-toggle)
   ("C-<f2>" . bm-next)
   ("C-<f3>" . bm-previous)))

(use-package esup
  :disabled t
  :commands esup
  :if (bound-and-true-p sb/debug-init-file))

(use-package bug-hunter
  :disabled t
  :if (bound-and-true-p sb/debug-init-file)
  :commands (bug-hunter-init-file bug-hunter-file))

(use-package explain-pause-mode
  :ensure nil
  :if (bound-and-true-p sb/debug-init-file)
  :load-path "extras"
  :disabled t
  ;; Generates the following warning with `quelpa'
  ;; "Warning (package): Unnecessary call to ‘package-initialize’ in init file"
  ;; :quelpa ((explain-pause-mode :fetcher github :repo "lastquestion/explain-pause-mode"))
  :commands (explain-pause-mode explain-pause-top)
  :diminish)

;; `text-mode' is a basic mode for `LaTeX-mode' and `org-mode', and so any hooks defined will also
;; get run for all modes derived from a basic mode such as `text-mode'.

;; Enabling `autfill-mode' makes it difficult to include long instructions verbatim, since they get
;; wrapped around automatically.
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

(use-package writegood-mode ; Identify weasel words, passive voice, and duplicate words
  :disabled t ; `textlint' includes writegood
  :commands writegood-mode
  :diminish
  :hook (text-mode . writegood-mode))

(use-package langtool
  :after text-mode
  :commands langtool-check
  :disabled t
  :config
  (setq langtool-default-language "en"
        langtool-disabled-rules '("COMMA_PARENTHESIS_WHITESPACE"
                                  "COPYRIGHT"
                                  "DASH_RULE"
                                  "EN_QUOTES"
                                  "EN_UNPAIRED_BRACKETS"
                                  "UPPERCASE_SENTENCE_START"
                                  "WHITESPACE_RULE")
        langtool-language-tool-jar (no-littering-expand-etc-file-name
                                    "languagetool-5.3-commandline.jar")))

(use-package wc-mode
  :commands wc-mode)

;; Gets the definition of word or phrase at point from https://wordnik.com/
(use-package define-word
  :commands (define-word define-word-at-point)
  :disabled t)

(use-package emojify
  :disabled t
  :commands (global-emojify-mode emojify-mode)
  :hook (markdown-mode . emojify-mode))

;; https://emacs.stackexchange.com/questions/19686/how-to-use-pdf-tools-pdf-view-mode-in-emacs
;; Use `isearch', `swiper' will not work
(use-package pdf-tools
  :defines pdf-annot-activate-created-annotations
  :commands (pdf-tools-install pdf-loader-install pdf-view-mode
                               pdf-annot-delete pdf-annot-add-highlight-markup-annotation
                               pdf-annot-add-text-annotation)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init (run-with-idle-timer 2 nil #'require 'pdf-tools nil t) ; Expensive to load
  :config
  ;; (pdf-tools-install :no-query)
  (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install)'

  (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable

  (setq pdf-annot-activate-created-annotations t  ; Automatically annotate highlights
        pdf-view-resize-factor 1.1) ; Fine-grained zoom factor of 10%

  ;; We do not enable `pdf-view-themed-minor-mode' since it can change plot colors
  (add-hook 'pdf-view-mode-hook
            (lambda()
              ;; (pdf-links-minor-mode 1)
              ;; (pdf-isearch-minor-mode 1)
              ;; (pdf-outline-minor-mode 1)
              ;; (pdf-history-minor-mode 1)
              (pdf-tools-enable-minor-modes)
              ))
  :bind
  (:map pdf-view-mode-map
        ("C-s" . isearch-forward)
        ("d"   . pdf-annot-delete)
        ("h"   . pdf-annot-add-highlight-markup-annotation)
        ("t"   . pdf-annot-add-text-annotation)))

;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.
(use-package saveplace-pdf-view
  :after pdf-tools
  :after saveplace
  :demand t)

(use-package logview
  :commands logview-mode
  :disabled t
  :config
  (unless (bound-and-true-p sb/use-no-littering)
    (setq logview-cache-filename (expand-file-name "logview-cache.extmap" sb/temp-directory))))

(use-package antlr-mode
  :ensure nil
  :mode "\\.g4\\'")

(use-package bison-mode
  :disabled t
  :mode ("\\.bison\\'"))

(use-package llvm-mode
  :ensure nil
  :load-path "extras"
  ;; Generates the following warning with `quelpa'
  ;; "Warning (package): Unnecessary call to ‘package-initialize’ in init file [3 times]"
  ;; This will clone the llvm project
  ;; :quelpa ((llvm-mode :fetcher github :repo "llvm/llvm-project"
  ;;                     :files ("llvm/utils/emacs/llvm-mode.el")))
  :commands llvm-mode
  :mode "\\.ll\\'")

(use-package tablegen-mode
  :ensure nil
  :load-path "extras"
  ;; Generates the following warning with `quelpa'
  ;; "Warning (package): Unnecessary call to ‘package-initialize’ in init file [3 times]"
  ;; This will clone the llvm project
  ;; :quelpa ((tablegen-mode :fetcher github :repo "llvm/llvm-project"
  ;;                         :files ("llvm/utils/emacs/tablegen-mode.el")))
  :commands tablegen-mode
  :disabled t
  :mode "\\.td\\'")

(use-package autodisass-llvm-bitcode
  :disabled t
  :commands autodisass-llvm-bitcode
  :mode "\\.bc\\'")

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  ;; The order is important to associate "README.md" with `gfm-mode'
  (("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  ;; :init
  ;; Looks good, but hiding markup makes it difficult to be consistent while editing
  ;; (setq-default markdown-hide-markup t)
  :config
  (setq markdown-command
        "pandoc -f markdown -s --mathjax --standalone --quiet --highlight-style=pygments"
        markdown-enable-math t ; Syntax highlight for LaTeX fragments
        markdown-enable-wiki-links t
        ;;   https://emacs.stackexchange.com/questions/13189/github-flavored-markdown-mode-syntax-highlight-code-blocks/33497
        markdown-fontify-code-blocks-natively t
        markdown-indent-on-enter 'indent-and-new-item
        markdown-list-indent-width 2
        markdown-split-window-direction 'horizontal
        ;; markdown-make-gfm-checkboxes-buttons nil
        markdown-hide-urls t)

  (unbind-key "C-c C-j"))

;; Generate TOC with `markdown-toc-generate-toc'
(use-package markdown-toc
  :after markdown-mode
  :disabled t
  :commands (markdown-toc-refresh-toc markdown-toc-generate-toc
                                      markdown-toc-generate-or-refresh-toc))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf
;; Convert `markdown' to `org': `pandoc -f markdown -t org -o output-file.org input-file.md'
(use-package pandoc-mode
  ;; :ensure-system-package pandoc
  :commands (pandoc-load-default-settings pandoc-mode)
  :diminish
  :hook (markdown-mode . pandoc-mode)
  :config
  (pandoc-load-default-settings)
  ;; (unbind-key "C-c /" pandoc-mode-map) ; Binds `C-c /' to `pandoc-main-hydra/body'
  )

(use-package grip-mode
  :if (executable-find "grip")
  ;; :hook ((markdown-mode org-mode) . grip-mode)
  :diminish
  :config
  (setq grip-preview-use-webkit nil) ; Do not use the embedded webkit for preview
  :bind
  (:map markdown-mode-command-map
        ("g" . grip-mode)))

;; Open preview of markdown file in a browser
(use-package markdown-preview-mode
  :disabled t
  :commands markdown-preview-mode)

;; Search the current buffer's parent directories for `node_modules/.bin'. Traverse the directory
;; structure up, until reaching the user's home directory, or hitting `add-node-modules-max-depth'.
;; Any path found is added to the `exec-path'.
(use-package add-node-modules-path
  :disabled t
  :init
  (dolist (mode '(typescript-mode js-mode js2-mode coffee-mode))
    (add-hook (derived-mode-hook-name mode) #'add-node-modules-path)))

;; LATER: Prettier times out setting up the process on a remote machine. I am using `format-all'
;; for now.
;; https://github.com/jscheid/prettier.el/issues/84
(use-package prettier
  :if (executable-find "prettier")
  :disabled t
  :commands prettier-mode
  :hook
  ;; Should work with `gfm-mode', `css-mode', and `html-mode' as they are derived modes
  ((markdown-mode web-mode json-mode jsonc-mode js2-mode)
   . (lambda ()
       (when (and buffer-file-name ; Returns `nil' if not visiting a file
                  (not (file-remote-p buffer-file-name)))
         (prettier-mode 1))))
  :config (setq prettier-lighter nil))

;; Align fields with `C-c C-a'
(use-package csv-mode
  :commands csv-mode
  :config (setq csv-separators '("," ";" "|" " ")))

(use-package highlight-doxygen
  :commands highlight-doxygen-global-mode
  :config (highlight-doxygen-global-mode))

(use-package rst
  :ensure nil
  :disabled t
  :mode ("\\.rst\\'" . rst-mode))

(use-package xref-rst
  :disabled t
  :commands xref-rst-mode
  :hook (rst-mode . xref-rst-mode))

(use-package boogie-friends
  :disabled t
  :mode ("\\.smt\\'" . z3-smt2-mode))

(use-package z3-mode
  :disabled t
  :commands z3-mode
  :config (setq z3-solver-cmd "z3"))

(use-package make-mode
  :ensure nil
  :mode
  (("\\Makefile\\'"       . makefile-mode)
   ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
   ("makefile\\.rules\\'" . makefile-gmake-mode)))

;; The variable-height minibuffer and extra eldoc buffers are distracting
(use-package eldoc
  :ensure nil
  :if (symbol-value 'sb/IS-LINUX)
  :commands turn-on-eldoc-mode
  :diminish
  :hook ((emacs-lisp-mode lisp-mode lisp-interaction-mode) . turn-on-eldoc-mode)
  :config
  ;; Always truncate ElDoc messages to one line. This prevents the echo area from resizing itself
  ;; unexpectedly when point is on a variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package c-eldoc
  :disabled t ;; We use LSP
  :hook (c-mode-common . c-turn-on-eldoc-mode))

(use-package css-mode
  :commands css-mode
  :config (setq css-indent-offset 2))

(use-package css-eldoc
  :disabled t ;; We use LSP
  :after css-mode
  :demand t
  :commands css-eldoc-enable
  :config (css-eldoc-enable))

(use-package eldoc-box
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :hook
  ((eldoc-mode . eldoc-box-hover-at-point-mode)
   (eldoc-mode . eldoc-box-hover-mode))
  :config
  (setq eldoc-box-clear-with-C-g t
        eldoc-box-fringe-use-same-bg nil)

  ;; (when (eq sb/theme 'modus-operandi)
  ;;   (set-face-background 'eldoc-box-body "cornsilk"))

  ;; (when (eq sb/theme 'modus-vivendi)
  ;;   (set-face-background 'eldoc-box-body "gray"))
  :diminish eldoc-box-hover-mode eldoc-box-hover-at-point-mode)

(use-package octave
  :ensure nil
  :disabled t)

(use-package matlab-mode
  :disabled t
  :mode ("\\.m$" . matlab-mode))

(use-package ess
  :disabled t
  :mode ("/R/.*\\.q\\'" . R-mode)
  :config
  (setq ess-indent-from-lhs 4
        ess-indent-offset 4
        inferior-R-args "--quiet --no-restore-history --no-save"))

(use-package ess-smart-underscore
  :disabled t
  :after (:any R-mode ess)
  :demand t)

(use-package ini-mode
  :commands ini-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))

(use-package pkgbuild-mode
  :commands pkgbuild-mode
  :disabled t)

(use-package elisp-mode
  :ensure nil
  :mode
  (("\\.el\\'"  . emacs-lisp-mode)
   ("\\.elc\\'" . elisp-byte-code-mode))
  :hook
  ((lisp-mode emacs-lisp-mode) . (lambda ()
                                   (when buffer-file-name
                                     (add-hook 'after-save-hook #'check-parens nil t)
                                     (flycheck-add-next-checker 'emacs-lisp
                                                                'emacs-lisp-checkdoc 'append))
                                   (outline-minor-mode))))

(use-package lsp-mode
  :defines (lsp-perl-language-server-path
            lsp-perl-language-server-port
            lsp-perl-language-server-client-version
            lsp-completion--regex-fuz
            lsp-clients-clangd-args
            lsp-clients-clangd-executable
            lsp-completion-enable-additional-text-edit
            lsp-completion-show-detail
            lsp-completion-provider
            lsp-completion-show-kind
            lsp-enable-semantic-tokens
            lsp-enable-which-key-integration
            lsp-headerline-breadcrumb-mode
            lsp-html-format-wrap-line-length
            lsp-html-format-end-with-newline
            lsp-html-format-indent-inner-html
            lsp-html-format-max-preserve-new-lines
            lsp-xml-logs-client
            lsp-xml-jar-file
            lsp-yaml-print-width
            lsp-headerline-breadcrumb-enable-diagnostics
            lsp-modeline-diagnostics-scope)
  :commands (lsp--set-configuration lsp-completion--regex-fuz
                                    lsp-register-client
                                    lsp-tramp-connection
                                    make-lsp-client
                                    lsp-format-buffer
                                    lsp-configuration-section lsp
                                    lsp-deferred
                                    lsp--set-configuration
                                    lsp-package-ensure
                                    lsp-enable-which-key-integration
                                    lsp-modeline-diagnostics-mode
                                    lsp-modeline-code-actions-mode
                                    lsp-symbol-highlight ht-merge
                                    lsp-completion--regex-fuz)
  ;; https://justin.abrah.ms/dotfiles/emacs.html
  ;; :ensure-system-package
  ;; ((typescript-language-server . "npm install -g typescript-language-server")
  ;;  (javascript-typescript-langserver . "npm install -g javascript-typescript-langserver")
  ;;  (yaml-language-server . "npm install -g yaml-language-server")
  ;;  (tsc . "npm install -g typescript"))
  :hook
  (
   ;; https://github.com/emacs-lsp/lsp-mode/issues/2598#issuecomment-776506077
   ((css-mode less-mode sgml-mode typescript-mode) . lsp-deferred)
   ;; TODO: Registering `lsp-format-buffer' makes sense only if the server is active
   ((c++-mode java-mode nxml-mode yaml-mode) . (lambda ()
                                                 (add-hook 'before-save-hook #'lsp-format-buffer
                                                           nil t)))
   )
  :custom-face
  (lsp-headerline-breadcrumb-symbols-face ((t (:inherit
                                               font-lock-doc-face :weight bold :height 0.9))))
  (lsp-headerline-breadcrumb-prefix-face ((t (:inherit font-lock-string-face :height 0.9))))
  (lsp-headerline-breadcrumb-project-prefix-face ((t (:inherit font-lock-string-face
                                                               :weight bold :height 0.9))))
  :config
  ;; We can add "--compile-commands-dir=build" option to indicate the directory where
  ;; `compile_commands.json' reside
  (setq lsp-clients-clangd-args '("-j=4"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--fallback-style=LLVM"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators=0"
                                  "--log=error"
                                  "--pch-storage=memory"
                                  ;; "--suggest-missing-includes"
                                  "--pretty")
        lsp-clients-clangd-executable "clangd-12"
        lsp-completion-enable-additional-text-edit t
        lsp-completion-provider :none ; Enable integration with `company'
        lsp-completion-show-detail nil ; Disable completion metadata
        lsp-completion-show-kind nil
        lsp-eldoc-enable-hover t
        lsp-enable-dap-auto-configure nil
        lsp-enable-file-watchers nil ; Could be a directory-local variable
        lsp-enable-folding nil
        lsp-enable-on-type-formatting nil
        lsp-enable-semantic-tokens t
        lsp-enable-snippet t ; Autocomplete parentheses
        ;; lsp-enabled-clients '(clangd clangd-remote jsts-ls flow-ls
        ;;                               ts-ls eslint json-ls
        ;;                               jsonls-remote cmakels
        ;;                               cmakels-remote html-ls
        ;;                               htmlls-remote angular-ls texlab
        ;;                               texlab-remote jdtls bash-ls
        ;;                               bashls-remote typescript-remote
        ;;                               css-ls cssls-remote
        ;;                               intelephense-remote
        ;;                               perl-language-server
        ;;                               perlls-remote php-ls xmlls
        ;;                               xmlls-remote yamlls yamlls-remote)
        lsp-enable-which-key-integration t
        lsp-headerline-breadcrumb-enable nil ; Breadcrumb is not useful for all modes
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-html-format-wrap-line-length sb/fill-column
        lsp-html-format-end-with-newline t
        lsp-html-format-indent-inner-html t
        lsp-html-format-max-preserve-new-lines nil
        lsp-imenu-sort-methods '(position)
        lsp-keep-workspace-alive nil
        lsp-log-io nil ; Increases memory usage because of JSON parsing if enabled
        ;; We already have `flycheck' error summary listed on the modeline, but the `lsp' server may
        ;; report additional errors. However, the modeline is getting too congested.
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-diagnostics-scope :file ; Focus on the errors at hand
        ;; Sudden changes in the height of the echo area causes the cursor to lose position,
        ;; manually request via `lsp-signature-activate'
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil ; Disable function documentation
        lsp-signature-function 'lsp-signature-posframe
        lsp-xml-logs-client nil
        lsp-xml-jar-file (expand-file-name "org.eclipse.lemminx-0.17.1-uber.jar"
                                           sb/extras-directory)
        lsp-yaml-print-width sb/fill-column)

  (unless (bound-and-true-p sb/use-no-littering)
    (setq lsp-session-file (expand-file-name "lsp-session" sb/temp-directory)))

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

  (when (eq sb/python-langserver 'pyls)
    (setq lsp-pyls-configuration-sources []
          lsp-pyls-plugins-autopep8-enable nil
          ;; Do not turn on fuzzy completion with jedi, `lsp-mode' is fuzzy on the client side
          ;; lsp-pyls-plugins-jedi-completion-fuzzy nil
          lsp-pyls-plugins-mccabe-enabled nil
          ;; Set this per-project
          ;; lsp-pyls-plugins-preload-modules ["numpy", "csv", "pandas", "statistics"]
          lsp-pyls-plugins-pycodestyle-enabled nil
          lsp-pyls-plugins-pycodestyle-max-line-length sb/fill-column
          lsp-pyls-plugins-pydocstyle-convention "pep257"
          lsp-pyls-plugins-pydocstyle-enabled nil
          lsp-pyls-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213"))
          lsp-pyls-plugins-pyflakes-enabled nil
          lsp-pyls-plugins-pylint-args (vconcat
                                        (list "-j 2"
                                              (concat "--rcfile="
                                                      (expand-file-name ".config/pylintrc"
                                                                        sb/user-home))))
          lsp-pyls-plugins-pylint-enabled t ; Pylint can be expensive
          lsp-pyls-plugins-yapf-enabled t))

  (when (eq sb/python-langserver 'pyright)
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection
                       (lambda ()
                         (cons "pyright-langserver"
                               lsp-pyright-langserver-command-args)))
      :major-modes '(python-mode)
      :remote? t
      :server-id 'pyright-remote
      :multi-root t
      :priority 3
      :initialization-options (lambda ()
                                (ht-merge (lsp-configuration-section "pyright")
                                          (lsp-configuration-section "python")))
      :initialized-fn (lambda (workspace)
                        (with-lsp-workspace workspace
                          (lsp--set-configuration
                           (ht-merge (lsp-configuration-section "pyright")
                                     (lsp-configuration-section "python")))))
      :download-server-fn (lambda (_client callback error-callback _update?)
                            (lsp-package-ensure 'pyright callback error-callback))
      :notification-handlers
      (lsp-ht
       ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
       ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
       ("pyright/endProgress" 'lsp-pyright--end-progress-callback)))))

  (when (eq sb/python-langserver 'jedi)
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection "jedi-language-server")
      :major-modes '(python-mode)
      :remote? t
      :server-id 'jedils-remote)))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "clangd")
    :major-modes '(c-mode c++-mode)
    :remote? t
    :server-id 'clangd-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("bash-language-server" "start"))
    :major-modes '(sh-mode)
    :remote? t
    :server-id 'bashls-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "intelephense")
    :major-modes '(php-mode)
    :remote? t
    :server-id 'intelephense-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "cmake-language-server")
    :major-modes '(cmake-mode)
    :remote? t
    :server-id 'cmakels-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "texlab")
    :major-modes '(tex-mode latex-mode LaTeX-mode bibtex-mode)
    :remote? t
    :server-id 'texlab-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("vscode-json-languageserver" "--stdio"))
    :major-modes '(json-mode jsonc-mode)
    :remote? t
    :server-id 'jsonls-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("css-languageserver" "--stdio"))
    :major-modes '(css-mode less-mode sass-mode scss-mode)
    :remote? t
    :server-id 'cssls-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("html-languageserver" "--stdio"))
    :major-modes '(html-mode web-mode mhtml-mode sgml-mode)
    :remote? t
    :server-id 'htmlls-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("java" "-jar" (expand-file-name "org.eclipse.lemminx-0.16.2-uber.jar"
                                                       sb/extras-directory)))
    :major-modes '(xml-mode nxml-mode)
    :remote? t
    :server-id 'xmlls-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("yaml-language-server" "--stdio"))
    :major-modes '(yaml-mode)
    :remote? t
    :server-id 'yamlls-remote))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     (lambda ()
                       (list lsp-perl-language-server-path
                             "-MPerl::LanguageServer" "-e"
                             "Perl::LanguageServer::run" "--"
                             (format "--port %d --version %s"
                                     lsp-perl-language-server-port
                                     lsp-perl-language-server-client-version))))
    :major-modes '(perl-mode cperl-mode)
    :remote? t
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "perl"))))
    :priority -1
    :server-id 'perlls-remote))

  ;; (defvar lsp-grammarly-active-modes)

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection #'lsp-grammarly--server-command)
  ;;   :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-grammarly-active-modes))
  ;;   :priority -1
  ;;   :remote? t
  ;;   :add-on? t
  ;;   :server-id 'grammarly-ls-remote
  ;;   :download-server-fn (lambda (_client callback error-callback _update?)
  ;;                         (lsp-package-ensure 'grammarly-ls callback error-callback))
  ;;   :after-open-fn #'lsp-grammarly--init
  ;;   :async-request-handlers
  ;;   (ht ("$/getCredentials" #'lsp-grammarly--get-credentials)
  ;;       ("$/getToken" #'lsp-grammarly--get-token)
  ;;       ("$/storeToken" #'lsp-grammarly--store-token)
  ;;       ("$/showError" #'lsp-grammarly--show-error)
  ;;       ("$/updateDocumentState" #'lsp-grammarly--update-document-state))))

  ;; (defvar lsp-ltex-active-modes)

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-stdio-connection
  ;;                    #'lsp-ltex--server-command
  ;;                    (lambda () (f-exists? (lsp-ltex--extension-root))))
  ;;   :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-ltex-active-modes))
  ;;   :priority -2
  ;;   :add-on? t
  ;;   :remote? t
  ;;   :server-id 'ltex-ls-remote
  ;;   :download-server-fn
  ;;   (lambda (_client _callback error-callback _update?)
  ;;     (lsp-package-ensure
  ;;      'ltex-ls
  ;;      (lambda ()
  ;;        (let ((dest (f-dirname (lsp-ltex--downloaded-extension-path))))
  ;;          (unless (lsp-ltex--execute "tar" "-xvzf" (lsp-ltex--downloaded-extension-path)
  ;;                                     "-C" dest)
  ;;            (error "Error during the unzip process: tar"))))
  ;;      error-callback))))

  ;; Disable fuzzy matching, TODO: What is the utility of this?
  ;; (advice-add #'lsp-completion--regex-fuz :override #'identity)

  ;; :bind-keymap ("C-c l" . lsp-keymap-prefix)

  :bind
  ;; `lsp-imenu-create-categorised-index' - sorts the items by kind.
  ;; `lsp-imenu-create-uncategorized-index' - will have the items sorted by position.
  (("M-."     . lsp-find-definition)
   ("C-c l d" . lsp-find-declaration)
   ("C-c l i" . lsp-goto-implementation)
   ("C-c l t" . lsp-goto-type-definition)
   ("C-c l r" . lsp-rename)
   ("C-c l h" . lsp-symbol-highlight)
   ("C-c l f" . lsp-format-buffer)
   ("C-c l r" . lsp-find-references)
   ("C-c l a" . lsp-execute-code-action)))

(use-package lsp-ui
  :defines lsp-ui-modeline-code-actions-enable
  :commands (lsp-ui-doc-mode lsp-ui-mode lsp-ui-doc--hide-frame)
  :after lsp-mode
  :demand t
  :config
  (setq lsp-ui-doc-enable t ; Enable on-hover dialogs
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 10
        lsp-ui-doc-include-signature t
        lsp-ui-imenu-auto-refresh 'after-save
        lsp-ui-imenu-window-width 16
        lsp-ui-sideline-enable nil ; Disable whole sideline
        lsp-ui-sideline-show-diagnostics nil ; Do not show diagnostics when typing
        lsp-ui-modeline-code-actions-enable t)

  (when (not (display-graphic-p))
    (setq lsp-ui-doc-enable nil
          lsp-ui-peek-enable nil))

  (lsp-ui-mode 1)
  (lsp-ui-doc-mode 1)

  ;; https://github.com/emacs-lsp/lsp-ui/issues/578
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (lsp-ui-doc--hide-frame)))

  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (eldoc-box-hover-mode -1)
              (eldoc-box-hover-at-point-mode -1)
              (eldoc-mode -1)))
  :bind
  ;; FIXME: Bind only if the server has launched successfully
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references]  . lsp-ui-peek-find-references)))

;; Sync workspace folders and treemacs projects
(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list lsp-treemacs-sync-mode)
  :config (lsp-treemacs-sync-mode 1))

(use-package origami
  :disabled t
  :commands (global-origami-mode origami-toggle-node
                                 origami-recursively-toggle-node origami-toggle-all-nodes)
  :hook ((java-mode python-mode c-mode c++-mode) . global-origami-mode)
  :bind
  (("C-c l t" . origami-recursively-toggle-node)
   ("C-c l n" . origami-toggle-all-nodes)))

(use-package lsp-origami
  :after origami
  :disabled t
  :functions lsp-origami-mode
  :commands lsp-origami-mode
  :demand t
  :config (lsp-origami-mode 1))

(use-package lsp-ivy
  :after (lsp-mode ivy-mode)
  :bind
  (("C-c l g" . lsp-ivy-global-workspace-symbol)
   ("C-c l w" . lsp-ivy-workspace-symbol)))

(use-package consult-lsp
  :disabled
  :if (eq sb/selection 'selectrum)
  :commands (consult-lsp-diagnostics consult-lsp-symbols))

(use-package dap-mode
  :commands (dap-debug dap-hydra dap-mode dap-ui-mode))

(use-package url-cookie
  :ensure nil
  :disabled t
  :config (setq url-cookie-file (expand-file-name (format "%s/emacs/url/cookies/"
                                                          xdg-data))))

;; Call this in c-mode-common-hook:
;; (define-key (current-local-map) "}" (lambda () (interactive) (c-electric-brace 1)))
(use-package cc-mode
  :ensure nil
  :defines (c-electric-brace c-enable-auto-newline c-set-style)
  :commands (c-fill-paragraph c-end-of-defun c-beginning-of-defun c++-mode)
  :mode ("\\.h\\'" . c++-mode)
  :mode ("\\.c\\'" . c++-mode)
  :hook (c++-mode . lsp-deferred)
  :config
  (setq c-set-style "cc-mode"
        c-basic-offset 2)

  (defvar c-electric-indent)

  ;; Disable electric indentation and on-type formatting
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq-local c-auto-newline nil
                          c-electric-brace nil
                          c-electric-flag nil
                          c-electric-indent nil
                          c-enable-auto-newline nil
                          c-syntactic-indentation nil)))

  (unbind-key "C-M-a" c-mode-map)
  :bind
  (:map c-mode-base-map
        ("C-c c a" . c-beginning-of-defun)
        ("C-c c e" . c-end-of-defun)
        ("M-q"     . c-fill-paragraph)))

(use-package modern-cpp-font-lock
  :after c++-mode
  :demand t
  :commands modern-c++-font-lock-mode
  :diminish modern-c++-font-lock-mode
  :config (modern-c++-font-lock-mode 1))

(use-package flycheck-clang-analyzer
  :after (flycheck cc-mode)
  :disabled t
  :demand t
  :commands flycheck-clang-analyzer-setup
  :config (flycheck-clang-analyzer-setup))

(use-package flycheck-clang-tidy
  :after (flycheck cc-mode)
  :disabled t
  :demand t
  :commands flycheck-clang-tidy-setup
  :config (flycheck-clang-tidy-setup))

(use-package cuda-mode
  :commands cuda-mode
  :mode
  (("\\.cu\\'"  . c++-mode)
   ("\\.cuh\\'" . c++-mode)))

(use-package opencl-mode
  :commands opencl-mode
  :mode "\\.cl\\'")

(use-package cmake-mode
  :commands cmake-mode
  :hook (cmake-mode . lsp-deferred))

(use-package cmake-font-lock
  :after cmake-mode
  :demand t
  :commands cmake-font-lock-activate
  :config (cmake-font-lock-activate))

(use-package python
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :bind
  (:map python-mode-map
        ("M-["   . python-nav-backward-block)
        ("M-]"   . python-nav-forward-block)
        ;; Assigning a keybinding such as "C-[" is involved, `[' is treated as `meta'
        ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c
        ("C-c <" . python-indent-shift-left)
        ("C-c >" . python-indent-shift-right))
  :config
  (setenv "PYTHONPATH" "python3")

  (setq python-shell-completion-native-enable nil ; Disable readline based native completion
        python-fill-docstring-style 'django
        python-indent-guess-indent-offset-verbose nil ; Remove guess indent python message
        python-indent-guess-indent-offset nil
        python-indent-offset 4
        python-shell-exec-path "python3"
        python-shell-interpreter "python3")

  ;; (with-eval-after-load "lsp-mode"
  ;;   (when (and (eq sb/python-langserver 'pyls) (executable-find "pyls"))
  ;;     (progn
  ;;       (dolist (ls '(pyright pyright-remote mspyls mspyls-remote jedi jedils-remote))
  ;;         (add-to-list 'lsp-disabled-clients ls))
  ;;       (add-to-list 'lsp-enabled-clients 'pyls)
  ;;       (add-to-list 'lsp-enabled-clients 'pyls-remote))))

  (setq auto-mode-alist (append '(("SConstruct\\'" . python-mode)
                                  ("SConscript\\'" . python-mode))
                                auto-mode-alist)))

(use-package python-docstring
  :after python-mode
  :demand t
  :commands (python-docstring-mode python-docstring-install)
  :diminish
  :config (python-docstring-install))

(use-package pip-requirements
  :disabled t
  :commands pip-requirements-mode)

(use-package pyvenv
  :diminish
  :commands (pyvenv-mode pyvenv-tracking-mode)
  :hook (python-mode . pyvenv-mode)
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:"
                                                              pyvenv-virtual-env-name "] "))
        pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python"))))
        pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

;; FIXME: Leads to errors over tramp and possibly blocks Emacs
(use-package py-isort
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :commands py-isort-before-save
  :hook
  (python-mode . (lambda ()
                   (add-hook 'before-save-hook #'py-isort-before-save)))
  :config (setq py-isort-options '("-l 100")))

(use-package lsp-python-ms
  :if (eq sb/python-langserver 'mspyls)
  :disabled t
  :after (lsp-mode python)
  :init (setq lsp-python-ms-auto-install-server t)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms)
                   ;; (dolist (ls '(pyls pyls-remote pyright pyright-remote jedi jedils-remote))
                   ;;   (add-to-list 'lsp-disabled-clients ls))
                   ;; (add-to-list 'lsp-enabled-clients 'mspyls)
                   ;; (add-to-list 'lsp-enabled-clients 'mspyls-remote)
                   ))
  :config
  (setq lsp-python-ms-python-executable-cmd "python3"))

;; `pyright --createstub pandas'
(use-package lsp-pyright
  :if (and (eq sb/python-langserver 'pyright) (executable-find "pyright"))
  :commands (lsp-pyright-locate-python lsp-pyright-locate-venv)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   ;; (dolist (ls '(pyls pyls-remote mspyls mspyls-remote jedi jedils-remote))
                   ;;   (add-to-list 'lsp-disabled-clients ls))
                   ;; (add-to-list 'lsp-enabled-clients 'pyright)
                   ;; (add-to-list 'lsp-enabled-clients 'pyright-remote)
                   ))
  :config (setq lsp-pyright-python-executable-cmd "python3"))

(use-package lsp-jedi
  :if (and (eq sb/python-langserver 'jedi) (executable-find "jedi-language-server"))
  :disabled t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-jedi)
                   ;; (dolist (ls '(pyls pyls-remote mspyls mspyls-remote pyright pyright-remote))
                   ;;   (add-to-list 'lsp-disabled-clients ls))
                   ;; (add-to-list 'lsp-enabled-clients 'jedi)
                   ;; (add-to-list 'lsp-enabled-clients 'jedils-remote)
                   ))
  :config (setq lsp-jedi-diagnostics-enable t))

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
(use-package yapfify
  :diminish yapf-mode
  :if (and (eq sb/python-langserver 'pyright) (executable-find "yapf"))
  :commands yapf-mode
  :hook (python-mode . yapf-mode))

(use-package ein
  :commands ein:ipynb-mode
  :disabled t)

(use-package cython-mode
  :commands cython-mode
  :disabled t)

(use-package jinja2-mode
  :commands jinjia2-mode
  :disabled t)

(use-package cperl-mode
  :ensure nil
  :mode ("latexmkrc\\'")
  :hook (cperl-mode . lsp-deferred)
  :config
  ;; Prefer CPerl mode to Perl mode
  (fset 'perl-mode 'cperl-mode))

;; Try to delete `lsp-java-workspace-dir' if the JDTLS fails
(use-package lsp-java
  :commands (lsp-java-organize-imports lsp-java-build-project
                                       lsp-java-update-project-configuration
                                       lsp-java-actionable-notifications
                                       lsp-java-update-user-settings
                                       lsp-java-update-server
                                       lsp-java-generate-to-string
                                       lsp-java-generate-equals-and-hash-code
                                       lsp-java-generate-overrides
                                       lsp-java-generate-getters-and-setters
                                       lsp-java-type-hierarchy
                                       lsp-java-dependency-list
                                       lsp-java-extract-to-constant
                                       lsp-java-add-unimplemented-methods
                                       lsp-java-create-parameter
                                       lsp-java-create-field
                                       lsp-java-create-local
                                       lsp-java-extract-method
                                       lsp-java-add-import)
  :hook (java-mode . (lambda ()
                       (setq-default c-basic-offset 4
                                     c-set-style "java")
                       (lsp-deferred)))
  :config
  (setq lsp-java-inhibit-message t
        lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java" ; Requires Java 11
        lsp-java-save-actions-organize-imports t
        lsp-java-format-settings-profile "Swarnendu"
        lsp-java-format-settings-url (expand-file-name
                                      "github/dotfiles/java/eclipse-format-swarnendu.xml"
                                      sb/user-home)))

(use-package ant
  :disabled t
  :commands (ant ant-clean ant-compile ant-test))

;; Can disassemble `.class' files from within jars
(use-package autodisass-java-bytecode
  :disabled t
  :commands autodisass-java-bytecode
  :mode "\\.class\\'")

;; Syntax highlighting for Gradle files
(use-package groovy-mode
  :commands groovy-mode
  :disabled t)

;; SVG
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

(use-package sh-script ; Shell script mode
  :ensure nil
  :mode
  (("\\.zsh\\'"   . sh-mode)
   ("\\bashrc\\'" . sh-mode))
  :hook (sh-mode . lsp-deferred)
  :config
  ;; (unbind-key "C-c C-d" sh-mode-map) ; Was bound to `sh-cd-here'
  (setq sh-basic-offset 2
        sh-indent-after-continuation 'always
        ;; Indent comments as a regular line
        sh-indent-comment t))

(use-package fish-mode
  :mode "\\.fish\\'"
  :interpreter "fish"
  :commands (fish-mode fish_indent-before-save)
  :hook
  (fish-mode . (lambda ()
                 (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package shfmt
  :hook (sh-mode . shfmt-on-save-mode)
  :config
  (shfmt-on-save-mode 1)
  (setq shfmt-arguments '("-i" "4" "-p" "-ci")))

;; The following section helper ensures that files are given `+x' permissions when they are saved,
;; if they contain a valid shebang line
(use-package executable
  :commands (executable-make-buffer-file-executable-if-script-p)
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

;; Remove `vc-refresh-state' if we are not using `vc', i.e., `vc-handled-backends' is nil
(use-package vc
  :init
  (if (boundp 'vc-handled-backends)
      (add-hook 'find-file-hook #'vc-refresh-state)
    (remove-hook 'find-file-hook #'vc-refresh-state)))


(use-package transient
  :commands transient-bind-q-to-quit
  :defines transient-display-buffer-action
  :config
  (unless (bound-and-true-p sb/use-no-littering)
    (setq transient-history-file (expand-file-name "transient/history.el" sb/temp-directory)
          transient-levels-file (expand-file-name "transient/levels.el" sb/temp-directory)
          transient-values-file (expand-file-name "transient/values.el" sb/temp-directory)))

  (setq transient-display-buffer-action '(display-buffer-below-selected))

  ;; Allow using `q' to quit out of popups, in addition to `C-g'
  (transient-bind-q-to-quit))

(use-package with-editor
  :diminish with-editor-mode)

(use-package magit
  :commands magit-display-buffer-fullframe-status-v1
  :bind
  (("C-x g"   . magit-status)
   ("C-c M-g" . magit-file-dispatch)
   ("C-x M-g" . magit-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        ;; Suppress the message we get about "Turning on magit-auto-revert-mode" when loading Magit
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

  (use-package magit-diff
    :ensure nil
    :demand t
    :config
    (setq magit-diff-refine-hunk t
          magit-diff-highlight-trailing nil
          magit-diff-paint-whitespace nil)))

(use-package gitignore-mode
  :commands gitignore-mode)

(use-package gitattributes-mode
  :disabled t
  :commands gitattributes-mode)

(use-package gitconfig-mode
  :disabled t
  :commands gitconfig-mode)

(use-package git-gutter
  :if (unless (boundp 'vc-handled-backends))
  :disabled t
  :commands global-git-gutter-mode
  :diminish
  :bind
  (("C-x p" . git-gutter:previous-hunk)
   ("C-x n" . git-gutter:next-hunk))
  :hook (after-init . global-git-gutter-mode)
  :config
  (setq git-gutter:added-sign " "
        git-gutter:deleted-sign " "
        git-gutter:modified-sign " "
        git-gutter:update-interval 1)

  ;; https://github.com/syl20bnr/spacemacs/issues/10555
  ;; https://github.com/syohex/emacs-git-gutter/issues/24
  (git-gutter:disabled-modes '(fundamental-mode org-mode image-mode doc-view-mode pdf-view-mode)))

;; Diff-hl looks nicer than git-gutter, based on `vc'
(use-package diff-hl
  :if (boundp 'vc-handled-backends)
  :commands (diff-hl-magit-pre-refresh diff-hl-magit-post-refresh
                                       diff-hl-dired-mode-unless-remote global-diff-hl-mode)
  :config
  ;; Highlight without a border looks nicer
  (setq diff-hl-draw-borders nil)
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (dired-mode         . diff-hl-dired-mode-unless-remote)
   (diff-hl-mode       . diff-hl-flydiff-mode)
   (after-init         . global-diff-hl-mode)))

(use-package git-commit
  :commands git-commit-turn-on-flyspell
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :config (setq git-commit-summary-max-length 50))

;; Use the minor mode `smerge-mode' to move between conflicts and resolve them
(use-package smerge-mode
  :ensure nil
  :commands (smerge-next smerge-prev smerge-auto-leave
                         smerge-keep-base smerge-keep-upper
                         smerge-keep-lower smerge-keep-all
                         smerge-diff-base-lower
                         smerge-diff-base-upper
                         smerge-diff-upper-lower smerge-refine
                         smerge-combine-with-next smerge-resolve)
  :preface
  (defun sb/enable-smerge-maybe ()
    "Enable smerge automatically based on conflict markers."
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1)))))

  (defun sb/enable-smerge-maybe2 ()
    "Enable `smerge-mode' automatically."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  :init
  (add-hook 'find-file-hook #'sb/enable-smerge-maybe2 :append)
  (add-hook 'magit-diff-visit-file-hook (lambda nil
                                          (when smerge-mode
                                            (sb/smerge-hydra/body))))
  :bind-keymap ("C-c v" . smerge-command-prefix)
  :bind
  (:map smerge-mode-map
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
        ("M-g M"   . smerge-popup-context-menu)))

(use-package ediff
  :ensure nil
  :after magit
  :demand t
  :defines ediff-window-setup-function
  :commands (ediff-setup-windows-plain ediff-set-diff-options)
  :config
  ;; Change default ediff style: do not start another frame with `ediff-setup-windows-default'
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split windows horizontally in ediff (instead of vertically)
  (setq ediff-split-window-function #'split-window-horizontally)
  (ediff-set-diff-options 'ediff-diff-options "-w"))

(use-package yaml-mode
  :commands yaml-mode
  :mode
  ((".clang-format" . yaml-mode)
   (".clang-tidy"   . yaml-mode))
  :hook
  (yaml-mode . (lambda ()
                 (setq-local lsp-ltex-enabled nil)
                 (spell-fu-mode -1) ; `yaml-mode' is derived from `text-mode'
                 (lsp-deferred))))

(use-package yaml-imenu
  :after yaml-mode
  :demand t
  :config (yaml-imenu-enable))

(use-package bat-mode
  :ensure nil
  :commands bat-mode
  :mode
  (("\\.bat\\'" . bat-mode)
   ("\\.cmd\\'" . bat-mode)))

(use-package web-mode
  :commands web-mode
  :mode
  (("\\.html?\\'"      . web-mode)
   ("\\.djhtml\\'"     . web-mode)
   ("\\.phtml\\'"      . web-mode)
   ("\\.hb\\.html\\'"  . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.[agj]sp\\'"    . web-mode)
   ("\\.as[cp]x\\'"    . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.handlebars\\'" . web-mode))
  :hook (web-mode . lsp-deferred)
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-quoting t
        web-mode-enable-block-face t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t ; Highlight the element under the cursor
        web-mode-enable-current-column-highlight t
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2))

(use-package emmet-mode
  :commands emmet-mode
  :defines emmet-move-cursor-between-quote
  :hook ((web-mode sgml-mode css-mode html-mode) . emmet-mode)
  :config (setq emmet-move-cursor-between-quote t))

(use-package rainbow-mode
  :diminish
  :commands rainbow-mode
  :hook ((css-mode html-mode sass-mode) . rainbow-mode))

(use-package php-mode
  :disabled t
  :hook (php-mode . lsp-deferred))

(use-package nxml-mode
  :ensure nil
  :commands nxml-mode
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$")
  :hook (nxml-mode . (lambda ()
                       (spell-fu-mode -1)
                       (lsp-deferred)))
  :config
  (fset 'xml-mode 'nxml-mode)
  (setq nxml-auto-insert-xml-declaration-flag t
        nxml-slash-auto-complete-flag t))

;; The advantage with `flycheck-grammarly' over `lsp-grammarly' is that you need not set up lsp
;; support, so you can use it anywhere. But `flycheck-grammarly' does not support a PRO Grammarly
;; account.
(use-package flycheck-grammarly
  :ensure websocket
  :ensure grammarly
  :ensure keytar
  :ensure t
  :after flycheck
  :demand t
  :config
  (setq flycheck-grammarly-check-time 3
        ;; LATER: Can we combine the delete operations?
        flycheck-checkers (delete 'proselint flycheck-checkers)
        ;; flycheck-checkers (delete 'textlint flycheck-checkers)
        ;; Remove from the beginning of the list `flycheck-checkers' and append to the end
        flycheck-checkers (delete 'grammarly flycheck-checkers))

  (add-to-list 'flycheck-checkers 'grammarly t))

;; https://languagetool.org/download/
(use-package flycheck-languagetool
  :after flycheck
  :defines (flycheck-languagetool-commandline-jar flycheck-languagetool-check-time)
  :demand t
  :disabled t
  :config
  (setq flycheck-languagetool-commandline-jar (no-littering-expand-etc-file-name
                                               "languagetool-commandline.jar")
        flycheck-languagetool-server-jar (no-littering-expand-etc-file-name
                                          "languagetool-server.jar")
        flycheck-checkers (delete 'languagetool flycheck-checkers)
        flycheck-languagetool-check-time 3)

  (add-to-list 'flycheck-checkers 'languagetool t))

;; org -> grammarly -> languagetool
(add-hook 'org-mode-hook
          (lambda ()
            (flycheck-select-checker 'org-lint)
            (when (featurep 'flycheck-grammarly)
              (flycheck-add-next-checker 'org-lint 'grammarly))
            (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
              (flycheck-add-next-checker 'grammarly 'languagetool))
            (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
              (flycheck-add-next-checker 'org-lint 'languagetool))))

;; We prefer not to use `textlint' and `proselint', `proselint' is not maintained.
;; grammarly -> languagetool
(add-hook 'text-mode-hook
          (lambda ()
            (when (featurep 'flycheck-grammarly)
              (flycheck-select-checker 'grammarly))
            (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
              (flycheck-add-next-checker 'grammarly 'languagetool))
            (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
              (flycheck-select-checker 'languagetool))))

;; `markdown-mode' is derived from `text-mode'
;; markdown-markdownlint-cli -> grammarly -> languagetool
(add-hook 'markdown-mode-hook
          (lambda()
            (flycheck-select-checker 'markdown-markdownlint-cli)
            (when (featurep 'flycheck-grammarly)
              ;; (make-local-variable 'flycheck-error-list-minimum-level)
              ;; (setq flycheck-error-list-minimum-level 'warning
              ;;       flycheck-navigation-minimum-level 'warning)
              ;; (flycheck-add-next-checker 'markdown-markdownlint-cli '(warning . grammarly) 'append)
              (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly))
            (when (featurep 'flycheck-grammarly)
              (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly))
            (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
              (flycheck-add-next-checker 'grammarly 'languagetool))
            (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
              (flycheck-add-next-checker 'markdown-markdownlint-cli 'languagetool))))

(dolist (hook '(LaTex-mode-hook latex-mode-hook))
  (add-hook hook (lambda ()
                   (flycheck-select-checker 'tex-chktex)
                   (when (featurep 'flycheck-grammarly)
                     (flycheck-add-next-checker 'tex-chktex 'grammarly))
                   (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
                     (flycheck-add-next-checker 'grammarly 'languagetool))
                   (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
                     (flycheck-add-next-checker 'tex-chktex 'languagetool)))))

;; We need to enable lsp workspace to allow `lsp-grammarly' to work, which makes it ineffective for
;; temporary text files. However, `lsp-grammarly' supports PRO Grammarly accounts. If there are
;; failures, then try logging out of Grammarly and logging in again. But it does not solve the issue
;; for me, so I have disabled the package and I am again trying `flycheck-grammarly'.
(use-package lsp-grammarly
  :ensure keytar
  :disabled t
  :hook
  ((text-mode markdown-mode org-mode gfm-mode latex-mode LaTeX-mode) . (lambda ()
                                                                         (require 'lsp-grammarly)
                                                                         (lsp-deferred)))
  :config
  (setq lsp-grammarly-active-modes '(text-mode latex-mode
                                               LaTeX-mode org-mode markdown-mode gfm-mode)
        lsp-grammarly-user-words '(
                                   )))

(use-package lsp-ltex
  :hook
  ((text-mode markdown-mode org-mode gfm-mode latex-mode LaTeX-mode) . (lambda ()
                                                                         (require 'lsp-ltex)
                                                                         (lsp-deferred)))
  :init
  (setq lsp-ltex-enabled t
        lsp-ltex-check-frequency "save"
        lsp-ltex-dictionary '("microbenchmarks")
        lsp-ltex-java-path "/usr/lib/jvm/java-13-openjdk-amd64"))

(use-package lsp-latex
  :disabled t
  :hook
  ((latex-mode LaTeX-mode) . (lambda()
                               (require 'lsp-latex)
                               (lsp-deferred)))
  :config
  (setq lsp-latex-bibtex-formatter "latexindent"
        lsp-latex-latex-formatter "latexindent"
        lsp-latex-bibtex-formatter-line-length sb/fill-column
        lsp-latex-chktex-on-open-and-save t
        lsp-latex-build-is-continuous t
        ;; Delay time in milliseconds before reporting diagnostics
        lsp-latex-diagnostics-delay 2000)

  (add-to-list 'lsp-latex-build-args "-c")
  (add-to-list 'lsp-latex-build-args "-pvc"))

;; (use-package tex-site
;;   :ensure nil
;;   :commands tex-site)

;; Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides the tex
;; package.
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :defines (tex-fontify-script font-latex-fontify-script
                               font-latex-fontify-sectioning
                               TeX-syntactic-comment TeX-save-query
                               LaTeX-item-indent LaTeX-syntactic-comments
                               LaTeX-fill-break-at-separators)
  :functions TeX-active-process
  :commands (TeX-active-process TeX-save-document tex-site LaTeX-mode LaTeX-math-mode TeX-PDF-mode
                                TeX-source-correlate-mode TeX-active-process
                                TeX-command-menu
                                TeX-revert-document-buffer)
  :hook
  (((latex-mode LaTeX-mode) . LaTeX-math-mode)
   ((latex-mode LaTeX-mode) . TeX-PDF-mode) ; Use `pdflatex'
   ((latex-mode LaTeX-mode) . TeX-source-correlate-mode))
  :config
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

;; We can disable this once `lsp-latex-build' works well
(use-package auctex-latexmk
  :after tex-mode
  :demand t
  :commands (auctex-latexmk-setup auctex-latexmk)
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t ; Pass the `-pdf' flag when `TeX-PDF-mode' is active
        TeX-command-default "LatexMk")

  (auctex-latexmk-setup))

(use-package bibtex
  :ensure nil
  :hook
  ((bibtex-mode . turn-on-auto-revert-mode)
   (bibtex-mode . lsp-deferred))
  :config
  (setq bibtex-align-at-equal-sign t
        bibtex-maintain-sorted-entries t))

(use-package bibtex-utils
  :after bibtex
  :disabled t
  :demand t)

(use-package ivy-bibtex
  :if (eq sb/selection 'ivy)
  :bind ("C-c x b" . ivy-bibtex)
  :config (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package bibtex-completion
  :ensure nil
  :after ivy-bibtex
  :demand t
  :config
  (setq bibtex-completion-cite-default-as-initial-input t
        bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-display-formats '((t . "${author:24} ${title:*} ${=key=:16} ${=type=:12}"))))

;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
(use-package reftex
  :ensure nil
  :commands (reftex-get-bibfile-list bibtex-parse-keys reftex-mode
                                     reftex-default-bibliography)
  :diminish
  :hook ((LaTeX-mode latex-mode) . reftex-mode)
  :bind
  (("C-c [" . reftex-citation)
   ("C-c )" . reftex-reference)
   ("C-c (" . reftex-label))
  :preface
  (defun sb/get-bibtex-keys (file)
    (with-current-buffer (find-file-noselect file)
      (mapcar 'car (bibtex-parse-keys))))

  (defun sb/reftex-add-all-bibitems-from-bibtex ()
    (interactive)
    (mapc 'LaTeX-add-bibitems
          (apply 'append
                 (mapcar 'sb/get-bibtex-keys (reftex-get-bibfile-list)))))

  (defun sb/find-bibliography-file ()
    "Try to find a bibliography file using RefTeX.
      Returns a string with text properties (as expected by read-file-name) or empty string if no
      file can be found"
    (interactive)
    (let ((bibfile-list nil))
      (condition-case nil
          (setq bibfile-list (reftex-get-bibfile-list))
        (error (ignore-errors
                 (setq bibfile-list (reftex-default-bibliography)))))
      (if bibfile-list
          (car bibfile-list) "")))

  (defun sb/reftex-try-add-all-bibitems-from-bibtex ()
    "Try to find a bibliography file using RefTex and parse the bib keys.
Ignore if no file is found."
    (interactive)
    (let ((bibfile-list nil))
      (condition-case nil
          (setq bibfile-list (reftex-get-bibfile-list))
        (error (ignore-errors
                 (setq bibfile-list (reftex-default-bibliography)))))
      ;; (message "%s" bibfile-list)
      (mapc 'LaTeX-add-bibitems
            (apply 'append
                   (mapcar 'sb/get-bibtex-keys bibfile-list)))))
  ;; :init (add-hook 'reftex-load-hook #'sb/reftex-add-all-bibitems-from-bibtex)
  :config
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

  )

(use-package bib-cite
  :ensure nil
  :disabled t
  :diminish bib-cite-minor-mode
  :commands bib-cite-minor-mode
  :hook ((LaTeX-mode latex-mode) . bib-cite-minor-mode )
  :config (setq bib-cite-use-reftex-view-crossref t)
  :bind (:map bib-cite-minor-mode-map
              ("C-c b"   . nil) ; We use `C-c b' for `comment-box'
              ("C-c l a" . bib-apropos)
              ("C-c l b" . bib-make-bibliography)
              ("C-c l d" . bib-display)
              ("C-c l t" . bib-etags)
              ("C-c l f" . bib-find)
              ("C-c l n" . bib-find-next)
              ("C-c l h" . bib-highlight-mouse)))

;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
(declare-function TeX-active-process "tex.el")

(defun sb/save-buffer-and-run-latexmk ()
  "Save the current buffer and run LaTeXMk also."
  (interactive)
  (require 'tex)
  (require 'tex-buf)
  (let ((process (TeX-active-process))) (if process (delete-process process)))
  (let ((TeX-save-query nil)) (TeX-save-document ""))
  (TeX-command-menu "LaTeXMk"))

;; (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
;;   (add-hook hook
;;             (lambda ()
;;               (add-hook 'after-save-hook
;;                         (lambda ()
;;                           (sb/save-buffer-and-run-latexmk)) nil t))))

(with-eval-after-load "tex-mode"
  (defvar latex-mode-map)
  (bind-key "C-x C-s" #'sb/save-buffer-and-run-latexmk latex-mode-map))

(with-eval-after-load "latex"
  (defvar LaTeX-mode-map)
  (bind-key "C-x C-s" #'sb/save-buffer-and-run-latexmk LaTeX-mode-map))

(use-package math-preview
  :disabled t
  :commands (math-preview-all math-preview-at-point math-preview-region)
  :config
  (setq math-preview-command (expand-file-name "node_modules/.bin/math-preview"
                                               sb/user-tmp)))

(use-package texinfo
  :disabled t
  :commands texinfo-mode
  :mode ("\\.texi\\'" . texinfo-mode))

(use-package js2-mode
  :disabled t
  :mode "\\.js\\'"
  :commands (js2-mode js2-imenu-extras-mode)
  :hook
  ((js2-mode . js2-imenu-extras-mode)
   (js2-mode . lsp-deferred))
  :config
  ;; TODO: Are the two variables aliased?
  (setq js-indent-level 2
        js2-basic-offset 2)
  (defalias 'javascript-mode 'js2-mode "`js2-mode' is aliased to `javascript' mode"))

(use-package js2-refactor
  :after js2-mode
  :disabled t
  :demand t
  :commands js2-refactor-mode
  :diminish
  :config (js2-refactor-mode 1))

(use-package xref-js2
  :disabled t
  :if (executable-find "rg")
  :commands xref-js2-xref-backend
  :hook
  (js2-mode . (lambda ()
                (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  :config
  (setq xref-js2-search-program 'rg)
  ;; LATER: `js-mode' (which js2 is based on) binds `M-.' which conflicts with `xref', so unbind it
  ;; (define-key js-mode-map (kbd "M-.") nil)
  )

;; The Melpa package does not include support for `jsonc-mode'. A pull request is pending.
(use-package json-mode
  :ensure nil
  :ensure json-reformat
  :ensure json-snatcher
  :load-path "extras"
  :commands (json-mode jsonc-mode json-mode-beautify)
  :mode
  (("\\.json\\'"                  . json-mode)
   (".*/\\.vscode/settings.json$" . jsonc-mode)
   ("User/settings.json$"         . jsonc-mode))
  :hook
  ((json-mode jsonc-mode) . (lambda ()
                              (make-local-variable 'js-indent-level)
                              (setq js-indent-level 2)
                              (lsp-deferred))))

(use-package json-reformat
  :after json-mode)

(use-package json-snatcher
  :after json-mode)

(use-package scss-mode
  :disabled t
  :commands scss-mode
  :hook (scss-mode . lsp-deferred)
  :config (setq scss-compile-at-save t))

(use-package sass-mode
  :disabled t
  :commands sass-mode
  :hook (sass-mode . lsp-deferred))

(use-package bazel
  :disabled t
  :commands (bazel-mode bazelrc-mode)
  :hook (bazel-mode . flycheck-mode))

(use-package protobuf-mode
  :disabled t
  :commands protobuf-mode
  :mode "\\.proto$"
  :hook (protobuf-mode . flycheck-mode))

(use-package mlir-mode
  :ensure nil
  :commands mlir-mode
  :load-path "extras"
  ;; Generates the following warning with `quelpa'
  ;; "Warning (package): Unnecessary call to ‘package-initialize’ in init file [3 times]"
  ;; This will clone the llvm project
  ;; :quelpa ((mlir-mode :fetcher github :repo "llvm/llvm-project"
  ;;                     :files ("mlir/utils/emacs/mlir-mode.el")))
  :mode "\\.mlir\\'"
  :disabled t)

(use-package clang-format
  :after (mlir-mode)
  :commands (clang-format clang-format-buffer clang-format-region)
  :disabled t)

(use-package clang-format+
  :ensure clang-format
  :ensure t
  :disabled t
  :hook (mlir-mode . clang-format+-mode)
  :config (setq clang-format+-always-enable t))

;; Use for major modes which do not provide a formatter. `aphelia' allows for formatting via a
;; background process but does not support tramp and supports fewer formatters.
(use-package format-all
  :commands (format-all-ensure-formatter format-all-buffer)
  :preface
  (defun sb/enable-format-all ()
    "Delay enabling format-all to avoid slowing down Emacs startup."
    (dolist (hook '(bazel-mode-hook latex-mode-hook LaTeX-mode-hook json-mode-hook
                                    markdown-mode-hook gfm-mode-hook))
      (add-hook hook #'format-all-mode))
    (add-hook 'format-all-mode-hook #'format-all-ensure-formatter))
  :init (run-with-idle-timer 2 nil #'sb/enable-format-all))

(use-package tree-sitter
  :disabled t
  :ensure tree-sitter-langs
  :functions tree-sitter-hl-mode
  :commands (global-tree-sitter-mode tree-sitter-hl-mode)
  :diminish tree-sitter-mode
  :init
  (dolist (hook '(sh-mode-hook c-mode-hook c++-mode-hook
                               css-mode-hook html-mode-hook java-mode-hook js-mode-hook
                               js2-mode-hook json-mode-hook jsonc-mode-hook php-mode-hook
                               python-mode-hook typescript-mode-hook))
    (add-hook hook (lambda ()
                     (require 'tree-sitter)
                     (require 'tree-sitter-langs)
                     (require 'tree-sitter-hl)

                     (global-tree-sitter-mode 1))))
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package adoc-mode
  :disabled t
  :commands adoc-mode
  :mode "\\.adoc\\'")

(use-package editorconfig
  :disabled t
  :if (executable-find "editorconfig")
  :diminish editorconfig-mode
  :commands editorconfig-mode)

;; Hooks into to `find-file-hook' to add all visited files and directories to `fasd'
(use-package fasd
  :disabled t
  :if (executable-find "fasd")
  :commands (global-fasd-mode fasd-find-file)
  :init (run-with-idle-timer 2 nil #'global-fasd-mode)
  :config (setq fasd-enable-initial-prompt nil)
  :bind ("C-c /" . fasd-find-file))

(use-package dotenv-mode
  :mode "\\.env\\'"
  :mode "\\.env\\.example\\'")

(use-package toml-mode
  :disabled t
  :commands toml-mode)

(use-package nix-mode
  :disabled t
  :commands nix-mode)

(use-package rust-mode
  :disabled t
  :commands rust-mode
  :hook (rust-mode . lsp-deferred)
  :config (setq rust-format-on-save t))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-compile.el
(use-package ansi-color
  :commands ansi-color-apply-on-region
  :preface
  (defun sb/colorize-compilation-buffer ()
    "Colorize compile mode output."
    (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :config
  ;; (add-hook 'compilation-filter-hook #'sb/colorize-compilation-buffer)
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))

(use-package info-colors
  :commands info-colors-fontify-node
  :hook (Info-selection . info-colors-fontify-node))

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

;; LATER: I do not understand the difference between the following two, and the explanation.
;; (add-to-list 'company-backends '(company-capf company-dabbrev))
;; (add-to-list 'company-backends '(company-capf :with company-dabbrev))


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

(progn
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
                     (sb/company-xml-mode)))))

(progn
  (defun sb/company-prog-mode ()
    "Add backends for program completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)

    ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
    (setq company-backends '((company-capf :with company-yasnippet)
                             (company-files :with company-yasnippet)
                             (company-dabbrev-code :with company-yasnippet)
                             company-dabbrev)))

  (add-hook 'prog-mode-hook #'sb/company-prog-mode))

(progn
  (defun sb/company-java-mode ()
    "Add backends for Java completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    (setq company-backends '((company-capf :with company-yasnippet)
                             (company-files : with company-yasnippet)
                             (company-dabbrev-code :with company-yasnippet)
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
    (setq company-backends '((company-capf :with company-yasnippet)
                             (company-files :with company-yasnippet)
                             (company-dabbrev-code :with company-yasnippet)
                             company-dabbrev
                             company-ispell)))

  (add-hook 'c-mode-common-hook #'sb/company-c-mode))


(progn
  (use-package company-shell
    :defines company-shell-delete-duplictes
    :commands (company-shell company-shell-env)
    :config (setq company-shell-delete-duplictes t))

  (defun sb/company-sh-mode ()
    "Add backends for shell script completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

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
  (use-package company-shell
    :commands company-fish-shell)

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


(progn
  (defun sb/company-elisp-mode ()
    "Set up company for elisp mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    (setq company-backends '((company-capf :with company-yasnippet)
                             (company-files :with company-yasnippet)
                             (company-dabbrev-code :with company-yasnippet)
                             company-dabbrev
                             company-ispell)))

  (add-hook 'emacs-lisp-mode-hook #'sb/company-elisp-mode))


(progn
  (defun sb/company-python-mode ()
    "Add backends for Python completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    ;; `company-dabbrev-code' is useful for variable names
    (setq company-backends '((company-capf :with company-yasnippet)
                             (company-files :with company-yasnippet)
                             (company-dabbrev-code :with company-yasnippet)
                             company-dabbrev)))

  (add-hook 'python-mode-hook #'sb/company-python-mode))


(progn
  (defun sb/company-latex-mode ()
    "Add backends for latex completion in company mode."
    (use-package company-auctex
      :demand t
      :commands (company-auctex-init company-auctex-labels
                                     company-auctex-bibs company-auctex-macros
                                     company-auctex-symbols company-auctex-environments)
      :config (company-auctex-init))
    (use-package math-symbol-lists ; Required by `ac-math' and `company-math'
      :demand t)
    (use-package company-math
      :demand t
      :commands (company-math-symbols-latex company-math-symbols-unicode company-latex-commands))
    (use-package company-reftex
      :demand t
      :commands (company-reftex-labels company-reftex-citations))
    (use-package company-bibtex
      :disabled t
      :demand t
      :commands company-bibtex)

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
                             company-dabbrev
                             company-ispell))

    ;; (setq company-backends '((:separate
    ;;                           company-capf
    ;;                           ;; company-latex-commands
    ;;                           company-files
    ;;                           company-yasnippet
    ;;                           company-dabbrev
    ;;                           company-ispell)))
    )

  (dolist (hook '(latex-mode-hook LaTeX-mode-hook TeX-mode-hook tex-mode-hook))
    (add-hook hook #'sb/company-latex-mode)))

(progn
  (defun sb/company-web-mode ()
    "Add backends for web completion in company mode."

    (make-local-variable 'company-backends)
    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev
                             company-ispell)))
  (dolist (hook '(web-mode-hook))
    (add-hook hook #'sb/company-web-mode)))

;; https://andreyorst.gitlab.io/posts/2020-06-29-using-single-emacs-instance-to-edit-files/
(use-package server
  :unless (string-equal "root" (getenv "USER")) ; Only start server if not root
  :commands server-running-p
  :init
  (unless (server-running-p)
    (server-start)))

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
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
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
  "Toggle line spacing.  Increase the line spacing to help readability.
Increase line spacing by two line height."
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
  (interactive (list (if buffer-file-name
                         (read-file-name "Move file to: ")
                       (read-file-name "Move file to: "
                                       default-directory
                                       (expand-file-name (file-name-nondirectory (buffer-name))
                                                         default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
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
(defun sb/counsel-all-files-recursively (dir-name)
  "List all files recursively in DIR-NAME."
  (interactive "DDirectory: ")
  (let* ((cands (split-string
                 (shell-command-to-string (format "find %s -type f" dir-name)) "\n" t)))
    (ivy-read "File: " cands
              :action #'find-file
              :caller 'sb/counsel-all-files-recursively)))

;; https://emacs.stackexchange.com/questions/17687/make-previous-buffer-and-next-buffer-to-ignore-some-buffers
;; You need to check for either major modes or buffer names, since a few major modes are commonly
;; used.
(defcustom sb/skippable-buffers
  '(
    "TAGS" "*Messages*" "*Backtrace*" "*scratch*"
    ;; "*company-documentation*" ; Major mode is `python-mode'
    ;; "*Help*" "*Packages*" "*prettier (local)*" "*emacs*" "*Warnings*" "*Compile-Log* *lsp-log*"
    ;; "*pyright*" "*texlab::stderr*" "*texlab*" "*Paradox Report*" "*perl-language-server*"
    ;; "*perl-language-server::stderr*" "*json-ls*" "*json-ls::stderr*" "*xmlls*" "*xmlls::stderr*"
    ;; "*pyright::stderr*" "*yamlls*" "*yamlls::stderr*" "*jdtls*" "*jdtls::stderr*"
    ;; "*clangd::stderr*" "*shfmt errors*"
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
Keep trying until current buffer is not in `sb/skippable-buffers'
or the major mode is not in `sb/skippable-modes'."
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

;; Generic keybindings, package-specific are usually in their own modules. The `C-c' binding is
;; reserved for the user. Use `C-h b' to see available bindings in a buffer. Use `M-x
;; describe-personal-keybindings' to see modifications.

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
(bind-key   "C-x s" #'sb/switch-to-scratch)
(bind-key   "C-x j" #'sb/counsel-all-files-recursively)

(unbind-key "C-j") ; Interferes with imenu `C-c C-j'

(use-package package
  :if sb/EMACS27+
  :bind ("C-c d p" . package-quickstart-refresh))

(global-set-key [remap next-buffer]     #'sb/next-buffer)
(global-set-key [remap previous-buffer] #'sb/previous-buffer)

(use-package default-text-scale
  :bind
  (("C-M-+" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(use-package free-keys
  :commands free-keys)

(use-package which-key ; Show help popups for prefix keys
  :diminish
  :commands (which-key-mode which-key-setup-side-window-right-bottom)
  :init (run-with-idle-timer 3 nil #'which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t))

(use-package which-key-posframe
  :disabled t
  :commands which-key-posframe-mode
  :hook (which-key-mode . which-key-posframe-mode)
  :config
  ;; The posframe has a low contrast
  ;; (set-face-attribute 'which-key-posframe nil :background "floralwhite" :foreground "black")
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))

;; Hydras

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

;; Mark safe variables

;; (add-to-list 'safe-local-variable-values '(auto-fill-function . nil))
;; (add-to-list 'safe-local-eval-forms '(visual-line-mode +1))

(put 'bibtex-completion-bibliography 'safe-local-variable #'listp)
(put 'company-bibtex-bibliography 'safe-local-variable #'listp)
(put 'company-clang-arguments 'safe-local-variable #'listp)
(put 'counsel-find-file-ignore-regexp 'safe-local-variable #'stringp)
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
            (message "Emacs is ready in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

(use-package selectrum
  :disabled t
  :defines selectrum-fix-vertical-window-height
  :if (eq sb/selection 'selectrum)
  :commands selectrum-mode
  :hook (after-init . selectrum-mode)
  :config
  (setq selectrum-fix-vertical-window-height t
        file-name-shadow-properties '(invisible t)
        completion-styles '(orderless)
        orderless-skip-highlighting (lambda () selectrum-is-active)
        selectrum-highlight-candidates-function #'orderless-highlight-matches
        magit-completing-read-function #'selectrum-completing-read)
  :bind ("<f3>" . switch-to-buffer))

;; Enable richer annotations in the minibuffer
(use-package marginalia
  :disabled t
  :commands marginalia
  :hook (selectrum-mode . marginalia-mode))

(use-package consult
  :disabled t
  :commands
  (consult-imenu consult-outline consult-apropos consult-buffer  consult-bookmark
                 consult-outline consult-goto-line consult-imenu
                 consult-project-imenu consult-error consult-find
                 consult-locate consult-grep consult-git-grep
                 consult-ripgrep consult-line consult-multi-occur
                 consult-isearch consult-yank-pop consult-mode-command
                 consult-bookmark consult-apropos consult-customize)
  :config
  (setq consult-project-root-function #'projectile-project-root
        ;; Use Consult to select xref locations with preview
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

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
  :bind
  (("<f1>"       . amx)
   ("<f2>"       . find-file)
   ("<f4>"       . consult-line)
   ("C-c s g"    . consult-git-grep)
   ("<f9>"       . consult-recent-file)
   ("C-c s r"    . consult-rg)
   ("M-y"        . consult-yank-pop)))

(use-package consult-flycheck
  :if (eq sb/selection 'selectrum)
  :disabled t
  :commands consult-flycheck)

(use-package selectrum-prescient
  :if (eq sb/selection 'selectrum)
  :disabled t
  :commands selectrum-prescient-mode
  :hook (selectrum-mode . selectrum-prescient-mode))

(use-package corfu
  :disabled t
  :hook (after-init . corfu-global-mode))

;;; init-use-package.el ends here
