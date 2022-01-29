;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar no-littering-etc-directory)
(defvar no-littering-var-directory)

(defgroup sb/emacs
  nil
  "Personal configuration for dotemacs."
  :group 'local)

(defcustom sb/extras-directory
  (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/gui-theme
  'modus-operandi
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "leuven"          leuven)
           (const :tag "zenburn"         zenburn)
           (const :tag "doom-one-light"  doom-one-light)
           (const :tag "doom-nord"       doom-nord)
           (const :tag "doom-molokai"    doom-molokai)
           (const :tag "doom-gruvbox"    doom-gruvbox)
           (const :tag "monokai"         monokai)
           (const :tag "modus-operandi"  modus-operandi)
           (const :tag "modus-vivendi"   modus-vivendi)
           (const :tag "customized"      sb/customized) ; Customizations over the default theme
           (const :tag "none"            none))
  :group 'sb/emacs)

;; A dark theme looks good on the TUI
(defcustom sb/tui-theme
  'modus-vivendi
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "leuven"          leuven)
           (const :tag "zenburn"         zenburn)
           (const :tag "doom-one-light"  doom-one-light)
           (const :tag "doom-molokai"    doom-molokai)
           (const :tag "doom-gruvbox"    doom-gruvbox)
           (const :tag "doom-nord"       doom-nord)
           (const :tag "monokai"         monokai)
           (const :tag "modus-operandi"  modus-operandi)
           (const :tag "modus-vivendi"   modus-vivendi)
           (const :tag "customized"      sb/customized) ; Customizations over the default theme
           (const :tag "none"            none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme
  'doom-modeline
  "Specify the mode-line theme to use."
  :type  '(radio
           (const :tag "powerline"       powerline)
           (const :tag "doom-modeline"   doom-modeline)
           (const :tag "awesome-tray"    awesome-tray)
           (const :tag "spaceline"       spaceline)
           (const :tag "moody"           moody)
           (const :tag "mini-modeline"   mini)
           (const :tag "none"            none))
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

(defcustom sb/delete-trailing-whitespace-p
  nil
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.
Sometimes we do not want to unnecessarily add differences due to
  whitespaces."
  :type  'boolean
  :group 'sb/emacs)

;; Use the snap installation of universal-ctags
(defcustom sb/ctags-path
  "/snap/bin/ctags"
  "Absolute path to Universal Ctags executable."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/debug-init-file
  nil
  "Enable features to debug errors and performance bottlenecks."
  :type  'boolean
  :group 'sb/emacs)

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
           (const :tag "pylsp"   pylsp)
           (const :tag "pyright" pyright)
           (const :tag "jedi"    jedi)
           (const :tag "none"    none))
  :group 'sb/emacs)

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

;; Configure `use-package' before loading
(defvar use-package-always-defer)
(defvar use-package-always-ensure)
(defvar use-package-compute-statistics)
(defvar use-package-enable-imenu-support)
(defvar use-package-expand-minimally)
(defvar use-package-verbose)

(setq use-package-enable-imenu-support t
      ;; Avoid manual modifications whenever I modify package installations
      use-package-always-ensure        t)

(when (bound-and-true-p sb/debug-init-file)
  (setq debug-on-error                 t
        debug-on-event                 'sigusr2
        garbage-collection-messages    t
        use-package-compute-statistics t ; Use `M-x use-package-report' to see results
        use-package-verbose            t
        use-package-expand-minimally   nil)
  (debug-on-entry 'projectile-remove-known-project))

;; Always load features lazily unless told otherwise. This implies we should use `after-init' hook
;; or `:init' instead of `:config', since otherwise packages may not be loaded. Be careful about
;; using `:after' and always deferring loading, because then we will need to specifiy alternate ways
;; of loading the package.
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading
(unless (bound-and-true-p sb/debug-init-file)
  (setq use-package-always-defer       t
        ;; Avoid printing errors and warnings since the configuration is known to work
        use-package-expand-minimally   t
        use-package-compute-statistics nil
        use-package-verbose            nil))

(eval-when-compile
  (require 'use-package))

;; If we omit `:defer', `:hook', `:commands', or `:after', then the package is loaded immediately.
;; Hooks in the `:hook' section run in reverse order. Example:
;; (use-package package-name
;;   :hook
;;   (x-mode . last)
;;   (x-mode . second)
;;   (x-mode . first))

;; `C-h b' lists all the bindings available in a buffer. `C-h m' shows the keybindings for the major
;; and the minor modes.
(use-package bind-key
  :functions bind-key--remove
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish)

(use-package f
  :commands (f-exists? f-join f-dirname))

(use-package s
  :commands s-starts-with? s-ends-with?)

(use-package dash
  :commands (-contains? -tree-map))

(use-package hydra
  :commands (hydra-default-pre hydra-keyboard-quit defhydra
                               hydra-show-hint hydra-set-transient-map
                               hydra--call-interactively-remap-maybe))

(defcustom sb/custom-file
  (no-littering-expand-etc-file-name "custom.el")
  "File to write Emacs customizations."
  :type  'string
  :group 'sb/emacs)

(setq custom-file sb/custom-file)

;; NOTE: Make a symlink to "private.el" under say "$HOME/.emacs.d/etc"
(defcustom sb/private-file
  (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type  'string
  :group 'sb/emacs)

(let ((gc-cons-threshold most-positive-fixnum))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror))
  (when (file-exists-p sb/private-file)
    (load sb/private-file 'noerror)))

(use-package warnings
  :init
  ;; This is not a great idea, but I expect most warnings will arise from third-party packages
  (setq warning-minimum-level :emergency))

(use-package gcmh ; Allow GC to happen after a period of idle time
  :diminish
  :commands (gcmh-mode gcmh-idle-garbage-collect)
  :hook (after-init . gcmh-mode)
  :config
  (when (bound-and-true-p sb/debug-init-file)
    (setq gcmh-verbose t)))

;; We can do `package-list-packages', then press `U' and `x'. The only thing missing from paradox
;; is `paradox-upgrade-packages' in a single command.
(or
 (use-package package
   :if sb/EMACS27+
   :bind
   (("C-c d p" . package-quickstart-refresh)
    ("C-c d l" . package-list-packages)))

 (use-package paradox
   :disabled t
   :commands (paradox-enable)
   :bind
   (("C-c d l" . paradox-list-packages)
    ("C-c d u" . paradox-upgrade-packages))
   :config
   (setq paradox-display-star-count nil
         paradox-execute-asynchronously t
         paradox-github-token t)
   (paradox-enable)))

;; Get PATH with `(getenv "PATH")'. Set PATH with
;; `(setenv "PATH" (concat (getenv "PATH") ":/home/swarnendu/bin"))'.
(use-package exec-path-from-shell
  :defines exec-path-from-shell-check-startup-files
  :commands exec-path-from-shell-initialize
  :if (or (daemonp) (memq window-system '(x ns)))
  :init
  ;; "-i" is expensive but Tramp is unable to find executables without the option
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "NODE_PATH" "JAVA_HOME" "PYTHONPATH"
                                         "LANG" "LC_CTYPE" "LC_ALL" "TERM"))
  (exec-path-from-shell-initialize))

;; (setq exec-path (append exec-path (expand-file-name "node_modules/.bin" sb/user-tmp)))
;; (add-to-list 'exec-path (expand-file-name "node_modules/.bin" sb/user-tmp))

;; Silence "assignment to free variable" warning
(defvar apropos-do-all)
(defvar bookmark-save-flag)
(defvar compilation-always-kill)
(defvar compilation-scroll-output)
(defvar sort-fold-case)

(setq ad-redefinition-action 'accept ; Turn off warnings due to redefinitions
      apropos-do-all t ; Make `apropos' search more extensively
      auto-mode-case-fold nil ; Avoid a second pass through `auto-mode-alist'
      ;; Unlike `auto-save-mode', `auto-save-visited-mode' saves the buffer contents to the visiting
      ;; file and runs all save-related hooks
      auto-save-default nil ; Disable `auto-save-mode', prefer `auto-save-visited-mode' instead
      auto-save-no-message t ; Allow for debugging frequent autosave triggers if `nil'
      auto-save-interval 0 ; Disable autosaving based on number of characters typed
      ;; Save buffer after idling for some time, the default of 5s is too frequent
      ;; `auto-save-visited-mode'
      auto-save-visited-interval 30
      backup-inhibited t ; Disable backup for a per-file basis
      blink-matching-paren t ; Distracting
      bookmark-save-flag 1 ; Save bookmark after every bookmark edit and also when Emacs is killed
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
      ;; Keeping track of the minibuffer nesting is difficult, so it is better to keep it disabled
      ;; enable-recursive-minibuffers t
      ;; The Emacs documentation warns about performance slowdowns with enabling remote directory
      ;; variables, but I edit files over Tramp a lot, so I am unsure.
      enable-remote-dir-locals t
      ;; Expand truncated ellipsis:suspension points in the echo area, useful to see more
      ;; information
      eval-expression-print-length 500
      ;; Accelerate scrolling operations when non-nil. Only those portions of the buffer which are
      ;; actually going to be displayed get fontified.
      fast-but-imprecise-scrolling t
      ;; Disable the warning "X and Y are the same file" in case of symlinks
      find-file-suppress-same-file-warnings t
      find-file-visit-truename t ; Show true name, useful in case of symlinks
      frame-title-format (list '(buffer-file-name "%f" "%b") " - " invocation-name)
      help-window-select t ; Makes it easy to close the window
      history-delete-duplicates t
      indicate-buffer-boundaries nil
      kill-do-not-save-duplicates t
      kill-whole-line t
      make-backup-files nil ; Stop making backup `~' files
      ;; mouse-drag-copy-region nil ; Mouse is disabled
      ;; mouse-yank-at-point t ; Yank at point with mouse instead of at click
      ;; pop-up-frames nil ; Avoid making separate frames
      read-buffer-completion-ignore-case t ; Ignore case when reading a buffer name
      ;; Ignore case when reading a file name completion
      read-file-name-completion-ignore-case t
      read-process-output-max (* 5 1024 1024) ; 5 MB
      require-final-newline t ; Always end a file with a newline
      ring-bell-function 'ignore ; Disable beeping sound
      save-interprogram-paste-before-kill t
      save-silently t ; Error messages will still be printed
      ;; Enable use of system clipboard across Emacs and other applications, does not work on TUI
      select-enable-clipboard t
      sentence-end-double-space nil
      shift-select-mode nil ; Do not use `shift-select' for marking, use it for `windmove'
      sort-fold-case nil ; Do not ignore case when sorting
      standard-indent 2
      ;; suggest-key-bindings t
      ;; switch-to-buffer-preserve-window-point t
      use-dialog-box nil ; Do not use dialog boxes with mouse commands
      use-file-dialog nil
      vc-follow-symlinks t ; No need to ask
      vc-handled-backends '(Git) ; Disabling vc improves performance, alternate option '(Git)
      view-read-only t ; View mode for read-only buffers
      visible-bell nil
      x-gtk-use-system-tooltips nil ; Do not use system tooltips
      ;; Always trigger an immediate resize of the child frame
      x-gtk-resize-child-frames 'resize-mode
      ;; Underline looks a bit better when drawn lower
      x-underline-at-descent-line t)

;; Changing buffer-local variables will only affect a single buffer. `setq-default' changes the
;; buffer-local variable's default value.
(setq-default fill-column sb/fill-column
              ;; electric-indent-inhibit nil
              indent-tabs-mode nil ; Spaces instead of tabs
              indicate-empty-lines nil
              major-mode 'text-mode ; Major mode to use for files that do no specify a major mode
              ;; TAB first tries to indent the current line, and if the line was already indented,
              ;; then try to complete the thing at point.
              tab-always-indent 'complete
              tab-width 4
              truncate-lines nil)

;; https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
(setq-default bidi-inhibit-bpa nil ; Disabling BPA makes redisplay faster
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
(set-language-environment    "UTF-8")
;; (setq locale-coding-system   'utf-8)
;; (setq-default buffer-file-coding-system 'utf-8)
;; (prefer-coding-system        'utf-8)
;; (set-default-coding-systems  'utf-8)
;; (set-keyboard-coding-system  'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (set-terminal-coding-system  'utf-8)

;; Scroll settings from Doom Emacs
(setq scroll-margin 5 ; Add margin lines when scrolling vertically to have a sense of continuity
      ;; Emacs spends too much effort recentering the screen if you scroll the cursor more than N
      ;; lines past window edges, where N is the setting of `scroll-conservatively'. This is
      ;; especially slow in larger files during large-scale scrolling commands. If kept over 100,
      ;; the window is never automatically recentered.
      scroll-conservatively 101
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
  ;; :init (run-with-idle-timer 2 nil #'global-auto-revert-mode)
  :hook (after-init-hook . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 10 ; Faster (seconds) would mean less likely to use stale data
        ;; Emacs seems to hang with auto-revert and Tramp, disabling this should be okay if we only
        ;; use Emacs, but enabling auto-revert is always safe.
        auto-revert-remote-files t
        auto-revert-verbose nil
        ;; Revert only file-visiting buffers, set to non-nil value to revert dired buffers if the
        ;; contents of the directory changes
        global-auto-revert-non-file-buffers t))

;; Revert all (e.g., PDF) files without asking
(setq revert-without-query '("\\.*"))

(use-package saveplace ; Remember cursor position in files
  :ensure nil
  :hook
  ;; We may open a file immediately after starting Emacs, hence we are using a hook instead of a
  ;; timer.
  (after-init . save-place-mode))

(use-package savehist ; Save minibuffer history across sessions
  :ensure nil
  :commands savehist-mode
  ;; :init (run-with-idle-timer 2 nil #'savehist-mode)
  :hook (after-init-hook . savehist-mode)
  :config
  (setq savehist-additional-variables '(extended-command-history
                                        kill-ring
                                        regexp-search-ring
                                        search-ring)))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-after-kill-buffer-p t
        uniquify-buffer-name-style   'forward
        uniquify-ignore-buffers-re   "^\\*"
        uniquify-separator           "/"
        uniquify-strip-common-suffix t))

;; Replace `dabbrev-exp' with `hippie-expand'. Use `C-M-/' for `dabbrev-completion' which finds all
;; expansions in the current buffer and presents suggestions for completion.
(use-package hippie-exp
  :ensure nil
  :config
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
  :bind ("M-/" . hippie-expand))

(use-package subword
  :ensure nil
  :commands subword-mode
  :diminish
  :hook (prog-mode . subword-mode))

(use-package frame
  :ensure nil
  :config
  ;; Show dividers on the right of each window, more prominent than the default
  (window-divider-mode))

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

(use-package ffap
  :commands (ffip ffap))

(use-package abbrev
  :ensure nil
  :commands abbrev-mode
  :diminish
  :hook
  ;; We open the `*scratch*' buffer in `text-mode', so enabling `abbrev-mode' early is useful
  (after-init . abbrev-mode)
  :config
  ;; The "abbrev-defs" file is under version control
  (setq abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory)
        save-abbrevs 'silently))

;; Disable the unhelpful modes, ignore disabling for modes I am not bothered with
(dolist (mode '(blink-cursor-mode ; Blinking cursor is distracting
                ;; size-indication-mode
                tooltip-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(use-package hl-line
  :commands hl-line-highlight
  :if (display-graphic-p)
  :hook (after-init . global-hl-line-mode))

;; Enable the following modes
(dolist (mode '(auto-save-visited-mode ; Autosave file-visiting buffers at idle time intervals
                column-number-mode
                delete-selection-mode ; Typing with the mark active will overwrite the marked region
                ;; Soft wraps, wrap lines without the ugly continuation marks
                global-visual-line-mode))
  (when (fboundp mode)
    (funcall mode 1)))

(when (bound-and-true-p enable-recursive-minibuffers)
  (minibuffer-depth-indicate-mode 1))

;; Not a library/file, so `eval-after-load' does not work
(diminish 'auto-fill-function)

(with-eval-after-load "simple"
  (diminish 'visual-line-mode))

;; Default is 8 pixels, we have increased it to look good on TUI
(unless (display-graphic-p)
  (fringe-mode '(10 . 10)))

;; Make the cursor a thin horizontal bar, not a block
;; (set-default 'cursor-type '(bar . 4))

(use-package outline ; Edit outlines
  :hook (prog-mode . outline-minor-mode)
  :diminish outline-minor-mode)

;; Hide top-level code blocks. Enable code folding, which is useful for browsing large files. This
;; module is part of Emacs, and is better maintained than other alternatives like `origami'.
(use-package hideshow
  :ensure nil
  :commands (hs-hide-all hs-hide-initial-comment-block hs-show-all hs-show-block)
  :diminish hs-minor-mode
  :hook (prog-mode . hs-minor-mode)
  :config
  (setq hs-isearch-open t)
  ;; (hs-hide-initial-comment-block)
  )

;; This puts the buffer in read-only mode and disables font locking, revert with `C-c C-c'
(use-package so-long
  :ensure nil
  :commands global-so-long-mode
  ;; :init (run-with-idle-timer 2 nil #'global-so-long-mode)
  :hook (after-init . global-so-long-mode)
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
  (setq all-the-icons-scale-factor 0.9
        all-the-icons-color-icons nil))

;; Set `sb/gui-theme' to `none' if you use this package
(use-package circadian
  :disabled t
  :commands circadian-setup
  :if (display-graphic-p)
  :init
  (require 'solar)
  (setq calendar-latitude 26.50
        calendar-location-name "Kanpur, UP, India"
        calendar-longitude 80.23
        circadian-themes '((:sunrise . doom-gruvbox)
                           (:sunset  . doom-gruvbox)))
  (circadian-setup))

(use-package leuven-theme
  :if (or (and (display-graphic-p) (eq sb/gui-theme 'leuven))
          (and (not (display-graphic-p)) (eq sb/tui-theme 'leuven)))
  :init (load-theme 'leuven t))

(use-package zenburn-theme
  :if (or (and (display-graphic-p) (eq sb/gui-theme 'zenburn))
          (and (not (display-graphic-p)) (eq sb/tui-theme 'zenburn)))
  :init (load-theme 'zenburn t))

(use-package doom-themes
  :if (or (and (display-graphic-p)
               (or (eq sb/gui-theme 'doom-molokai)
                   (eq sb/gui-theme 'doom-one-light)
                   (eq sb/gui-theme 'doom-nord)
                   (eq sb/gui-theme 'doom-gruvbox)))
          (and (not (display-graphic-p))
               (or (eq sb/tui-theme 'doom-molokai)
                   (eq sb/tui-theme 'doom-one-light)
                   (eq sb/tui-theme 'doom-nord)
                   (eq sb/tui-theme 'doom-gruvbox))))
  :commands (doom-themes-org-config doom-themes-treemacs-config)
  :init
  (cond
   ((or (eq sb/gui-theme 'doom-molokai)
        (eq sb/tui-theme 'doom-molokai))   (load-theme 'doom-molokai t))
   ((or (eq sb/gui-theme 'doom-one-light)
        (eq sb/tui-theme 'doom-one-light)) (load-theme 'doom-one-light t))
   ((or (eq sb/gui-theme 'doom-nord)
        (eq sb/tui-theme 'doom-nord))      (load-theme 'doom-nord t))
   ((or (eq sb/gui-theme 'doom-gruvbox)
        (eq sb/tui-theme 'doom-gruvbox))   (load-theme 'doom-gruvbox t)))
  ;; (set-face-attribute 'font-lock-comment-face nil :foreground "#999999")
  :config
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

(use-package monokai-theme
  :if (or (and (display-graphic-p) (eq sb/gui-theme 'monokai))
          (and (not (display-graphic-p)) (eq sb/tui-theme 'monokai)))
  :init (load-theme 'monokai t))

(use-package modus-themes
  :ensure moody
  :defines (modus-themes-completions modus-themes-fringes
                                     modus-themes-prompts
                                     modus-themes-lang-checkers
                                     modus-themes-hl-line
                                     modus-themes-org-blocks
                                     modus-themes-mode-line)
  :if (or (and (display-graphic-p)
               (or (eq sb/gui-theme 'modus-operandi)
                   (eq sb/gui-theme 'modus-vivendi)))
          (and (not (display-graphic-p))
               (or (eq sb/tui-theme 'modus-operandi)
                   (eq sb/tui-theme 'modus-vivendi))))
  :init
  (setq modus-themes-completions 'opinionated
        modus-themes-fringes 'intense
        modus-themes-hl-line '(accented)
        modus-themes-prompts '(intense)
        modus-themes-lang-checkers '(background faint)
        modus-themes-org-blocks 'tinted-background)

  (when (eq sb/modeline-theme 'default)
    (setq modus-themes-mode-line 'accented-3d))

  (when (eq sb/modeline-theme 'moody)
    (setq modus-themes-mode-line 'borderless-moody))

  (when (display-graphic-p)
    (cond
     ((eq sb/gui-theme 'modus-operandi) (load-theme 'modus-operandi t))
     ((eq sb/gui-theme 'modus-vivendi) (load-theme 'modus-vivendi t))))

  (unless (display-graphic-p)
    (cond
     ((eq sb/tui-theme 'modus-operandi) (load-theme 'modus-operandi t))
     ((eq sb/tui-theme 'modus-vivendi) (load-theme 'modus-vivendi t)))))

(when (and (eq sb/gui-theme 'sb/customized)
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
  :commands powerline-default-theme
  :init
  (setq powerline-display-hud nil ; Visualization of the position in the buffer is not useful
        ;; powerline-default-separator 'box
        powerline-display-buffer-size nil
        powerline-display-mule-info nil ; File encoding information is not useful
        powerline-gui-use-vcs-glyph t
        powerline-height 20)

  (when (eq sb/gui-theme 'leuven)
    (set-face-attribute 'mode-line nil :background "grey88" :foreground "black")
    (set-face-attribute 'mode-line-buffer-id nil :weight 'bold
                        :foreground "black" :background "gray88"))

  (powerline-default-theme))

(use-package doom-modeline
  ;; Requires the fonts included with `all-the-icons', run `M-x all-the-icons-install-fonts'
  :ensure all-the-icons
  :ensure doom-modeline
  :if (eq sb/modeline-theme 'doom-modeline)
  :commands doom-modeline-mode
  :init
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-checker-simple-format nil
        doom-modeline-indent-info nil
        doom-modeline-lsp nil
        doom-modeline-minor-modes t
        ;; Reduce space on the modeline
        doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-mode 1))

(use-package spaceline
  :ensure t
  :defines (spaceline-hud-p spaceline-selection-info-p
                            spaceline-version-control-p spaceline-input-method-p
                            spaceline-persp-name-p
                            spaceline-buffer-encoding-abbrev-p
                            spaceline-buffer-encoding-p
                            spaceline-buffer-size-p)
  :if (eq sb/modeline-theme 'spaceline)
  :init
  (require 'spaceline-config)
  (setq spaceline-hud-p nil
        spaceline-selection-info-p nil
        spaceline-version-control-p t
        spaceline-input-method-p nil
        spaceline-buffer-size-p nil
        ;; Line ending convention used in the current buffer (unix, dos or mac)
        spaceline-buffer-encoding-abbrev-p nil
        ;; Line ending convention used in the current buffer (unix, dos or mac) without abbreviation
        spaceline-buffer-encoding-p nil
        spaceline-persp-name-p nil)

  ;; (set-face-attribute 'powerline-inactive1 nil
  ;;                     :background "gray40"
  ;;                     :foreground "white"
  ;;                     :weight 'light)
  ;; (set-face-attribute 'powerline-inactive2 nil
  ;;                     :background "grey50"
  ;;                     :foreground "white")
  ;; (when (eq dotemacs-theme 'leuven)
  ;;   (set-face-attribute 'powerline-active1 nil
  ;;                       :background "gray22"
  ;;                       :foreground "white"
  ;;                       :weight 'light)
  ;;   (set-face-attribute 'mode-line-inactive nil
  ;;                       :background "grey88"
  ;;                       :foreground "black"))

  (spaceline-emacs-theme))

(use-package awesome-tray ; Minimal modeline information
  :ensure nil
  :commands awesome-tray-mode
  :if (eq sb/modeline-theme 'awesome-tray)
  :load-path "extras"
  :hook (after-init . awesome-tray-mode)
  :config
  (setq awesome-tray-active-modules '("file-path" "buffer-name" "mode-name" "location" "git")
        awesome-tray-git-update-duration 30 ; seconds
        awesome-tray-file-path-full-dirname-levels 1)
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
  :commands (moody-replace-vc-mode moody-replace-mode-line-buffer-identification)
  :init
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package mini-modeline
  :diminish
  :if (eq sb/modeline-theme 'mini)
  :hook (after-init . mini-modeline-mode)
  :config
  (setq mini-modeline-r-format '("%e" mode-line-front-space
                                 mode-line-client
                                 mode-line-modified
                                 mode-line-remote
                                 " " mode-line-buffer-identification " "
                                 mode-line-position
                                 (:eval (string-trim (format-mode-line mode-line-modes)))
                                 mode-line-misc-info)))

;; This does not work well with Treemacs, and it is difficult to make out the highlighted current
;; line.
(use-package auto-dim-other-buffers
  :commands (adob--rescan-windows auto-dim-other-buffers-mode)
  ;; :init (run-with-idle-timer 3 nil #'auto-dim-other-buffers-mode)
  :hook (after-init . auto-dim-other-buffers-mode))

;; https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
(defun sb/font-available-p (font-name)
  "Find font specified by FONT-NAME."
  (find-font (font-spec :name font-name)))

;; (cond
;;  ((sb/font-available-p "Cascadia Code")
;;   (set-frame-font "Cascadia Code-14"))
;;  ((sb/font-available-p "Menlo")
;;   (set-frame-font "Menlo-14"))
;;  ((sb/font-available-p "Inconsolata")
;;   (set-frame-font "Inconsolata-14"))
;;  ((sb/font-available-p "DejaVu Sans Mono")
;;   (set-frame-font "DejaVu Sans Mono-14")))

;; Value is in 1/10pt, so 100 will give you 10pt
;; (set-frame-font "DejaVu Sans Mono" nil t)

;; https://github.com/wandersoncferreira/dotfiles
;; (when (member "Monaco" (font-family-list))
;;   (set-face-attribute 'default nil :font "Monaco" :height 120)
;;   (setq default-frame-alist '((font . "Monaco-12"))))

;; https://github.com/larstvei/dot-emacs
;; (cond ((member "Inconsolata" (font-family-list))
;;        (set-face-attribute 'default nil :font "Inconsolata-18")))

(when (string= (system-name) "inspiron-7572")
  (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120))

(when (string= (system-name) "dell-7506")
  (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120))

(when (string= (system-name) "swarnendu-Dell-XPS-L502X")
  (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
  (set-face-attribute 'default nil :font "Cascadia Code" :height 140)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "vindhya")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "himalaya")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "aravalli")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "satpura")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "nilgiri")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "sivalik")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

;; Decrease minibuffer font
;; https://stackoverflow.com/questions/7869429/altering-the-font-size-for-the-emacs-minibuffer-separately-from-default-emacs
(progn
  (defun sb/minibuffer-font-setup ()
    "Customize minibuffer font."
    (set (make-local-variable 'face-remapping-alist) '((default :height 0.90))))

  (add-hook 'minibuffer-setup-hook #'sb/minibuffer-font-setup))

;; Changing height of the echo area is jarring, but limiting the height makes it difficult to see
;; useful information
(when nil
  (progn
    (add-hook 'emacs-startup-hook (lambda ()
                                    (setq resize-mini-windows nil)))
    ))

(use-package beacon
  :commands beacon-mode
  :diminish
  ;; :init (run-with-idle-timer 3 nil #'beacon-mode)
  :hook (after-init . beacon-mode))

(use-package ibuffer
  :ensure nil
  :config
  (defalias 'list-buffers 'ibuffer)
  (setq ibuffer-display-summary nil
        ;; ibuffer-default-sorting-mode 'alphabetic ; Options: major-mode
        ;; ibuffer-expert t
        ;; ibuffer-use-header-line t
        )
  :bind ("C-x C-b" . ibuffer))

(use-package ibuf-ext
  :ensure nil
  :commands ibuffer-auto-mode
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
  :config (setq all-the-icons-ibuffer-icon-size 0.8))

(use-package dired
  :ensure nil
  :commands (dired-next-line dired-jump)
  :defines dired-clean-confirm-killing-deleted-buffers
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
  (:map dired-mode-map
        ("M-<home>" . sb/dired-go-home)
        ("M-<up>"   . sb/dired-jump-to-top)
        ("M-<down>" . sb/dired-jump-to-bottom)
        ("i"        . find-file))
  :hook
  ;; Auto refresh dired when files change
  (dired-mode . auto-revert-mode)
  :config
  (setq dired-auto-revert-buffer t ; Revert each dired buffer automatically when you revisit it
        ;; Guess a default target directory. When there are two dired buffers, Emacs will select
        ;; another buffer as the target (e.g., target for copying files).
        dired-dwim-target t
        ;; Check `ls' for additional options
        dired-listing-switches "-ABhl --si --group-directories-first"
        dired-ls-F-marks-symlinks t ; -F marks links with @
        dired-recursive-copies 'always ; Single prompt for all n directories
        dired-recursive-deletes 'always ; Single prompt for all n directories
        ;; Do not ask whether to kill buffers visiting deleted files
        dired-clean-confirm-killing-deleted-buffers nil)

  (when (boundp dired-kill-when-opening-new-dired-buffer)
    (setq dired-kill-when-opening-new-dired-buffer t)))

(use-package dired-x
  :ensure nil
  :defines dired-cleanup-buffers-too
  :commands (dired-omit-mode)
  :hook (dired-mode . dired-omit-mode)
  :bind ("C-x C-j"  . dired-jump)
  :config
  (setq dired-cleanup-buffers-too t
        ;; Do not show messages when omitting files
        dired-omit-verbose nil)

  (when (boundp 'dired-bind-jump) ;; Obsolete from Emacs 28+
    (setq dired-bind-jump t))

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

(use-package dired-narrow ; Narrow `dired' to match filter
  :after dired
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

;; Do not create multiple dired buffers
(use-package dired+
  :if (= emacs-major-version 27)
  :load-path "extras"
  :commands diredp-toggle-find-file-reuse-dir
  :init (setq diredp-bind-problematic-terminal-keys nil)
  :config
  (setq diredp-hide-details-initially-flag nil
        diredp-hide-details-propagate-flag nil)
  :hook
  (dired-mode . (lambda ()
                  (diredp-toggle-find-file-reuse-dir 1))))

;; `r' is bound to `diredp-rename-this-file', but I prefer `dired-efap'. This binding only works if
;; we load after `dired+' and not `dired', even with `bind-keys*'.
(use-package dired-efap
  :after dired+
  :defines dired-efap-initial-filename-selection
  :config (setq dired-efap-initial-filename-selection nil)
  :bind*
  (:map dired-mode-map
        ("r" . dired-efap)))

;; Asynchronously byte compile packages installed with `package.el'
(use-package async
  :functions async-bytecomp-package-mode
  :commands async-bytecomp-package-mode
  :init (async-bytecomp-package-mode 1))

(use-package dired-async
  :ensure async
  :diminish
  :hook (dired-mode . dired-async-mode))

(use-package all-the-icons-dired
  :commands (all-the-icons-dired-mode all-the-icons-dired--refresh-advice)
  :diminish
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  ;; Icons are not aligned after renaming a file.
  ;; https://github.com/jtbm37/all-the-icons-dired/issues/34
  (advice-add 'dired-add-entry :around #'all-the-icons-dired--refresh-advice)
  (advice-add 'dired-remove-entry :around #'all-the-icons-dired--refresh-advice))

(use-package treemacs
  :functions treemacs-tag-follow-mode
  :commands (treemacs-current-workspace
             treemacs--find-current-user-project
             treemacs-do-add-project-to-workspace
             treemacs-add-project-to-workspace treemacs-git-mode
             treemacs-follow-mode treemacs-fringe-indicator-mode
             treemacs-filewatch-mode treemacs-goto-file-node
             treemacs--propagate-new-icons
             treemacs-scope->current-scope
             treemacs--restore-eldoc-after-log
             treemacs-load-theme treemacs-find-file-node
             treemacs-indent-guide-mode treemacs-resize-icons
             treemacs-select-window
             treemacs-add-and-display-current-project
             treemacs-display-current-project-exclusively
             projectile-project-p treemacs--select-workspace-by-name)
  :preface
  ;; The problem is there is no toggle support.
  (defun sb/setup-treemacs-quick ()
    "Setup treemacs."
    (interactive)
    (when (projectile-project-p)
      (treemacs-display-current-project-exclusively)
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

  (defun sb/treemacs-ignore-files (filename absolute-path)
    "Ignore files in the Treemacs explorer"
    (or
     (-contains? '("__pycache__" "node_modules" "package-lock.json") filename)
	 (s-ends-with? ".pyc" filename)
	 (s-ends-with? ".elc" filename)
	 (s-ends-with? ".o" filename)
	 (s-ends-with? ".so" filename)
	 (s-ends-with? ".dll" filename)
     ))
  :config
  (setq treemacs-follow-after-init t
        treemacs-indentation 1
        ;; Prevents Treemacs from being selected with `other-window' if non-nil, but it hurts easy
        ;; navigability. Use `treemacs-select-window'.
        treemacs-is-never-other-window t
        treemacs-project-follow-cleanup t
        treemacs-missing-project-action 'remove
        treemacs-recenter-after-file-follow 'on-distance
        treemacs-recenter-after-tag-follow 'on-distance
        treemacs-silent-refresh t ; Silence all refresh messages including file watches
        treemacs-width 20
        ;; Hide the mode-line in the Treemacs buffer
        treemacs-user-mode-line-format 'none)

  ;; (if (display-graphic-p)
  ;;     (setq treemacs-indentation-string (propertize "⫶" 'face 'font-lock-comment-face))
  ;;   (setq treemacs-indentation-string (propertize "|" 'face 'font-lock-comment-face)))

  (treemacs-filewatch-mode 1)
  ;; `treemacs-tag-follow-mode' disables `treemacs-follow-mode', focuses the tag, but following tags
  ;; in noisy
  (treemacs-follow-mode 1)
  (treemacs-git-mode 'deferred)
  (treemacs-fringe-indicator-mode 'always) ; Always show the file indicator
  (treemacs-indent-guide-mode 1)
  ;; (treemacs-project-follow-mode 1) ; Ignores workspace features

  ;; https://github.com/Alexander-Miller/treemacs/issues/735
  ;; (treemacs-create-theme "Default-Tighter"
  ;;   :extends "Default"
  ;;   :config
  ;;   (let ((icons (treemacs-theme->gui-icons theme)))
  ;;     (maphash (lambda
  ;;                (ext icon)
  ;;                (puthash ext
  ;;                         (concat
  ;;                          (substring icon 0 1)
  ;;                          (propertize " " 'display
  ;;                                      '(space . (:width 0.5))))
  ;;                         icons))
  ;;              icons)))

  ;; (treemacs-create-theme "all-the-icons-tighter"
  ;;   :extends "all-the-icons"
  ;;   :config
  ;;   (let ((icons (treemacs-theme->gui-icons theme)))
  ;;     (maphash (lambda
  ;;                (ext icon)
  ;;                (puthash ext
  ;;                         (concat
  ;;                          (substring icon 0 1)
  ;;                          (propertize " " 'display
  ;;                                      '(space . (:width 0.5))))
  ;;                         icons))
  ;;              icons)))

  (set-face-attribute 'treemacs-directory-collapsed-face nil :height 0.8)
  (set-face-attribute 'treemacs-directory-face           nil :height 0.7)
  (set-face-attribute 'treemacs-file-face                nil :height 0.7)
  (set-face-attribute 'treemacs-root-face                nil :height 0.7)
  (set-face-attribute 'treemacs-tags-face                nil :height 0.7)
  (set-face-attribute 'treemacs-git-ignored-face         nil :height 0.7)
  (set-face-attribute 'treemacs-git-untracked-face       nil :height 0.7)

  (when (or (eq sb/gui-theme 'modus-operandi)
            (eq sb/gui-theme 'modus-vivendi)
            (eq sb/gui-theme 'doom-gruvbox))
    (set-face-attribute 'treemacs-git-modified-face   nil :height 0.7)
    (set-face-attribute 'treemacs-git-unmodified-face nil :height 0.7))

  (when (or (eq sb/gui-theme 'sb/customized)
            (eq sb/gui-theme 'none))
    (set-face-attribute 'treemacs-git-modified-face   nil :height 0.8)
    (set-face-attribute 'treemacs-git-unmodified-face nil :height 1.0))

  (when (display-graphic-p)
    (treemacs-resize-icons 16))

  (add-to-list 'treemacs-ignored-file-predicates #'sb/treemacs-ignore-files)
  :bind*
  (;; The keybinding interferes with `dired-jump' and imenu `C-c C-j'
   ("C-j"     . treemacs)
   ("C-c t d" . treemacs-add-and-display-current-project)
   ("C-c t e" . treemacs-display-current-project-exclusively)
   ("M-0"     . treemacs-select-window)))

(use-package treemacs-all-the-icons
  :if (display-graphic-p)
  :after treemacs
  :demand t
  :config (treemacs-load-theme "all-the-icons"))

;; Allows to quickly add projectile projects to the treemacs workspace
(use-package treemacs-projectile
  :after (treemacs projectile)
  :commands treemacs-projectile
  :demand t)

(use-package treemacs-magit
  :after (treemacs magit)
  :demand t)

(use-package org
  :defines (org-hide-leading-stars-before-indent-mode
            org-src-strip-leading-and-trailing-blank-lines
            org-src-tabs-acts-natively)
  :commands (org-indent-mode org-indent-item org-outdent-item)
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-fontify-done-headline nil
        org-fontify-whole-heading-line nil
        ;; org-hide-emphasis-markers t
        ;; org-hide-leading-stars t
        ;; org-hide-leading-stars-before-indent-mode t
        ;; Code block fontification using the major-mode of the code
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tabs-acts-natively t
        org-src-window-setup 'current-window
        ;; There is a lot of visible distortion with `org-indent-mode' enabled. Emacs performance feels
        ;; better with the mode disabled.
        org-startup-indented nil
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
        ("C-'"       . nil)
        ("<tab>"     . org-indent-item)
        ("<backtab>" . org-outdent-item)))

;; We have disabled the package to get consistent styles across themes.
(use-package org-bullets
  :disabled t
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))

;; Make invisible parts of Org elements appear visible
(use-package org-appear
  :commands org-appear-mode
  :disabled t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autosubmarkers t
        org-appear-autoentities   t
        org-appear-autolinks      t))

(use-package ox-gfm
  :after org
  :demand t)

;; TODO: Use `C-c o' as the binding for `org-mode-map'

;; Use `C-'' in `isearch-mode-map' to use `avy-isearch' to select one of the currently visible
;; `isearch' candidates
(use-package isearch
  :ensure nil
  :commands (isearch-forward-regexp isearch-repeat-forward isearch-occur)
  :config
  (setq search-highlight       t ; Highlight incremental search
        isearch-lazy-highlight t
        isearch-lazy-count     t)
  :bind
  ;; Change the bindings for `isearch-forward-regexp' and `isearch-repeat-forward'
  (("C-s"     . nil)
   ("C-M-f"   . nil) ;; Was bound to `isearch-forward-regexp', but we use it for `sp-forward-sexp'
   ("C-f"     . isearch-forward-regexp)
   :map isearch-mode-map
   ("C-s"     . nil)
   ("C-f"     . isearch-repeat-forward)
   ("C-c C-o" . isearch-occur)))

;; Auto populate `isearch' with the symbol at point
(use-package isearch-symbol-at-point
  :after isearch
  :commands (isearch-forward-symbol ; `M-s _'
             isearch-symbol-at-point
             isearch-forward-symbol-at-point ; `M-s .'
             isearch-backward-symbol-at-point))

(use-package anzu
  :diminish anzu-mode
  :commands global-anzu-mode
  :init
  (setq anzu-search-threshold     10000
        anzu-minimum-input-length 2)
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

    (setq grep-command           "grep -irHn "
          grep-highlight-matches t
          grep-scroll-output     t)

    (dolist (dirs '(".cache" "node_modules" "vendor" ".clangd"))
      (add-to-list 'grep-find-ignored-directories dirs))))

;; When the *grep* buffer is huge, `wgrep-change-to-wgrep-mode' might freeze Emacs for several
;; minutes.
(use-package wgrep ; Writable grep
  :bind
  ;; These keybindings are also defined in `wgrep-mode-map'
  (:map grep-mode-map
        ("C-x C-p" . wgrep-change-to-wgrep-mode)
        ("C-x C-s" . wgrep-finish-edit)
        ("C-x C-k" . wgrep-abort-changes)
        ("C-x C-q" . wgrep-exit))
  :config (setq wgrep-auto-save-buffer t))

;; Use `S' to change the search term, `D' to change the search directory, `g' to rerun the search,
;; `o' to view the result in another window.
(use-package deadgrep
  :bind ("C-c s d" . deadgrep))

;; (use-package ctrlf
;;   :disabled t
;;   :commands (ctrlf-mode ctrlf-local-mode)
;;   :config
;;   (ctrlf-mode 1)
;;   (add-hook 'pdf-isearch-minor-mode-hook (lambda ()
;;                                            (ctrlf-local-mode -1)))
;;   :bind
;;   (("C-f"   . ctrlf-forward-literal)
;;    ("C-r"   . ctrlf-backward-literal)
;;    ("C-M-s" . ctrlf-forward-regexp)
;;    ("C-M-r" . ctrlf-backward-regexp)))

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
        ;; Larger values help in lookup but takes more time to check if the files exist
        recentf-max-saved-items 100
        ;; Abbreviate the file name to make it easy to read the actual file name. Specifically,
        ;; `abbreviate-file-name' abbreviates home directory to "~/" in the file list.
        recentf-filename-handlers (append '(abbreviate-file-name) recentf-filename-handlers))

  ;; Use the true file name and not the symlink name
  (dolist (exclude `(,(file-truename no-littering-etc-directory)
                     ,(file-truename no-littering-var-directory)))
    (add-to-list 'recentf-exclude exclude))

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
  ;; after idling for 30 seconds.
  (run-with-idle-timer 30 t #'recentf-save-list)

  ;; Adding many functions to `kill-emacs-hook' slows down Emacs exit, hence we are only using idle
  ;; timers.
  (run-with-idle-timer 60 t #'recentf-cleanup)
  :hook (after-init . recentf-mode))

;; Load immediately after start since I use it often
(use-package init-open-recentf
  :after recentf
  :demand t
  :disabled t
  :config (init-open-recentf))

(defun sb/inhibit-message-call-orig-fun (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN, forward ARGS."
  (let ((inhibit-message t))
    (apply orig-fun args)))

;; Hide the "Wrote to recentf" message
(advice-add 'recentf-save-list :around #'sb/inhibit-message-call-orig-fun)
;; Hide the "Cleaning up the recentf list...done" message
(advice-add 'recentf-cleanup   :around #'sb/inhibit-message-call-orig-fun)

;; Hide the "Wrote ..." message
(advice-add 'write-region :around #'sb/inhibit-message-call-orig-fun)

;; The module does not specify an `autoload'. So we get the following error without the following
;; declaration.
;; "Company backend ’company-capf’ could not be initialized: Autoloading file failed to define
;; function company-capf"
(use-package company-capf
  :ensure company
  :commands company-capf)

;; Use "M-x company-diag" or the modeline status to see the backend used. Try `M-x
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
                                     company-ispell-dictionary
                                     company-clang-insert-arguments)
  :hook (after-init . global-company-mode)
  ;; The `company-posframe' completion kind indicator is not great, but we are now using
  ;; `company-fuzzy'.
  :diminish
  :config
  (setq company-dabbrev-downcase nil ; Do not downcase returned candidates
        company-dabbrev-ignore-case nil ; Do not ignore case when collecting completion candidates
        company-dabbrev-other-buffers t ; Search in other buffers with the same major mode
        company-ispell-available t
        company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory)
        company-minimum-prefix-length 3 ; Small words can be faster to type
        company-require-match nil ; Allow input string that do not match candidates
        company-selection-wrap-around t
        company-show-quick-access t ; Speed up completion
        ;; Align additional metadata, like type signatures, to the right-hand side
        company-tooltip-align-annotations t
        ;; Disable insertion of arguments
        company-clang-insert-arguments nil
        ;; Start a search using `company-filter-candidates' (bound to "C-s") to narrow out-of-order
        ;; strings
        ;; https://github.com/company-mode/company-mode/discussions/1211
        company-search-regexp-function 'company-search-words-in-any-order-regexp)

  ;; We set `company-backends' as a local variable, so it is not important to delete backends
  ;; (dolist (backends '(company-semantic company-bbdb company-oddmuse company-cmake company-clang))
  ;;   (delq backends company-backends))

  ;; Ignore matches that consist solely of numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  ;; (push (apply-partially #'cl-remove-if
  ;;                        (lambda (c)
  ;;                          (string-match-p "\\`[0-9]+\\'" c)))
  ;;       company-transformers)

  (remove-hook 'kill-emacs-hook #'company-clang-set-prefix)
  :bind
  (:map company-active-map
        ("C-n"      . company-select-next)
        ("C-p"      . company-select-previous)
        ;; Insert the common part of all candidates, or select the next one
        ("<tab>"    . company-complete-common-or-cycle)
        ("C-M-/"    . company-other-backend) ; Was bound to `dabbrev-completion'
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
  (setq company-posframe-show-metadata nil ; Difficult to distinguish the help text from completions
        company-posframe-show-indicator nil ; Hide the backends, the display is not great
        ;; Disable showing the help frame
        company-posframe-quickhelp-delay nil)
  (company-posframe-mode 1))

(use-package company-quickhelp
  :after company
  :commands company-quickhelp-mode
  ;; :init (run-with-idle-timer 3 nil #'company-quickhelp-mode)
  :hook (after-init . company-quickhelp-mode))

(use-package company-statistics
  :after company
  :demand t
  :commands company-statistics-mode
  :config (company-statistics-mode 1))

;; Nice but slows completions. We should invoke this only at the very end of configuring `company'.
(use-package company-fuzzy
  :ensure flx
  :ensure t
  :after company
  :diminish (company-fuzzy-mode global-company-fuzzy-mode)
  :commands (global-company-fuzzy-mode company-fuzzy-mode)
  :demand t
  :config
  (setq company-fuzzy-sorting-backend 'flx
        company-fuzzy-show-annotation nil ; The right-hand side gets cut off
        ;; We should not need this because the `flx' sorting accounts for the prefix
        company-fuzzy-prefix-on-top nil))

(use-package yasnippet
  :commands (yas-global-mode snippet-mode yas-hippie-try-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook ((text-mode prog-mode) . yas-global-mode)
  :diminish yas-minor-mode
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))
        yas-verbosity 0)
  (with-eval-after-load "hippie-expand"
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  (unbind-key "<tab>" yas-minor-mode-map))

(use-package yasnippet-snippets
  :after yasnippet
  :demand t
  :commands yasnippet-snippets-initialize
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
  :bind ("C-M-y" . ivy-yasnippet))

;; `amx-major-mode-commands' limits to commands that are relevant to the current major mode
;; `amx-show-unbound-commands' shows frequently used commands that have no key bindings
(use-package amx
  :commands amx-mode
  :hook (after-init . amx-mode)
  :config
  ;; Update the command list every n minutes
  (setq amx-auto-update-interval 10))

(use-package ivy
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
  (setq ivy-count-format "(%d/%d) " ; Helps identify wrap around
        ivy-extra-directories nil ; Hide . and ..
        ivy-fixed-height-minibuffer t ; Distracting if the height keeps changing
        ;; Make the height of the minibuffer proportionate to the screen
        ;; ivy-height-alist '((t
        ;;                      lambda (_caller)
        ;;                      (/ (frame-height) 2)))
        ;; We update `ivy-re-builders-alist' after loading `orderless'
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
  :ensure t
  :ensure amx ; `counsel' makes use of `amx' if installed
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
  (;; Counsel can use the sorting from `amx' or `smex' for `counsel-M-x'.
   ([remap execute-extended-command] . counsel-M-x)
   ([remap completion-at-point]      . counsel-company)
   ("C-M-i"                          . counsel-company)
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
   ("S-<f3>"                         . counsel-switch-buffer)
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
                                         "\\|.cache"
                                         "\\|.metadata"
                                         "\\|.recommenders"
                                         "\\|typings"
                                         "\\|__pycache__")
        counsel-mode-override-describe-bindings t
        counsel-preselect-current-file t
        counsel-switch-buffer-preview-virtual-buffers nil ; Removes recent files and bookmarks
        counsel-yank-pop-preselect-last t
        counsel-yank-pop-separator "\n------------------------------------------\n"))

(use-package ivy-hydra ; Additional keybindings for `ivy'
  :after (ivy hydra)
  :demand t
  :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body))

;; Ivy is not well supported, and we are using `company-fuzzy' for sorting completion frameworks
(use-package prescient
  :commands prescient-persist-mode
  :hook (after-init . prescient-persist-mode)
  :config (setq prescient-sort-full-matches-first t))

;; We are using `company-fuzzy' for sorting completion candidates
(use-package company-prescient
  :after company
  :demand t
  :commands company-prescient-mode
  :disabled t
  :config
  ;; We want `capf' sort for programming modes, not with recency. This breaks support for the
  ;; `:separate' keyword in `company'.
  ;; (setq company-prescient-sort-length-enable nil)
  (company-prescient-mode 1))

(use-package all-the-icons-ivy
  :after ivy
  :demand t
  :commands all-the-icons-ivy-setup
  :config (all-the-icons-ivy-setup))

(use-package orderless
  :after ivy
  :demand t
  :defines orderless-component-separator
  :functions sb/just-one-face
  :config
  ;; (defvar ivy-re-builders-alist)
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))
        completion-styles '(orderless)
        ;; completion-styles '(orderless initials basic partial-completion emacs22)
        orderless-matching-styles '(orderless-regexp))

  ;;  completion-category-defaults nil
  ;;  completion-category-overrides '((file (styles partial-completion))
  ;;                                  (minibuffer (initials))))
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
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC"     . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src"     . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  ;; Verbatim regions in org mode should not be ispelled:
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  ;; Properties block in org do not need to be ispelled
  (add-to-list 'ispell-skip-region-alist '("\:PROPERTIES\:$" . "\:END\:$"))
  ;; Footnoes in org that have http links that are line breaked should not be ispelled:
  (add-to-list 'ispell-skip-region-alist '("^http" . "\\]"))

  (add-to-list 'ispell-skip-region-alist '("`" "`"))

  ;; Skip some math environments
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{multline}" . "\\\\end{multline}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{equation}" . "\\\\end{equation}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{align}"    . "\\\\end{align}")))

(use-package flyspell
  :ensure nil
  :if (symbol-value 'sb/IS-LINUX)
  :commands (flyspell-overlay-p flyspell-correct-previous flyspell-correct-next flyspell-buffer)
  :diminish
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
  (setq flyspell-abbrev-p           t ; Add corrections to abbreviation table
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook
  (;; (before-save . flyspell-buffer) ; Saving files will be slow
   ;; Enabling `flyspell-prog-mode' does not seem to be very useful and highlights links and
   ;; language-specific words
   (prog-mode . flyspell-prog-mode)
   ;; `find-file-hook' will not work for buffers with no associated files
   (after-init . (lambda ()
                   (when (string= (buffer-name) "*scratch*")
                     (flyspell-mode 1))))
   (text-mode . flyspell-mode))
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   :map flyspell-mode-map
   ("C-;"     . nil)
   ("C-,"     . sb/flyspell-goto-previous-error)))

;; Flyspell popup is more efficient. Ivy-completion does not show the "Save" option in a few cases.
(use-package flyspell-popup
  :disabled t
  :bind ("C-;" . flyspell-popup-correct)
  :config (setq flyspell-popup-correct-delay 0.1))

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)))

;; As of Emacs 28, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.
(use-package spell-fu
  :defines spell-fu-directory
  :commands spell-fu-mode
  :config
  (setq spell-fu-directory (expand-file-name "spell-fu" no-littering-var-directory))
  :init
  (add-hook 'text-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(hl-line
                                             ;; `nxml-mode' is derived from `text-mode'
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
                                             hl-line
                                             pandoc-citation-key-face))
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

(use-package highlight-indentation
  :commands highlight-indentation-mode
  :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
  :hook ((yaml-mode python-mode) . highlight-indentation-mode))

;; Claims to be better than `electric-indent-mode'
(use-package aggressive-indent
  :commands aggressive-indent-mode
  :hook ((lisp-mode emacs-lisp-mode lisp-interaction-mode) . aggressive-indent-mode)
  :diminish
  :config
  (setq aggressive-indent-comments-too t
        ;; Never use `electric-indent-mode'
        aggressive-indent-dont-electric-modes t))

(use-package paren
  :ensure nil
  ;; :init (run-with-idle-timer 2 nil #'show-paren-mode)
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis ; `mixed' may lead to performance problems
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Enable autopairing
(use-package elec-pair
  :ensure nil
  :commands (electric-pair-mode)
  :disabled t
  ;; :init (run-with-idle-timer 2 nil #'electric-pair-mode)
  :hook (after-init . electric-pair-mode)
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

  ;; Avoid balancing parentheses since they can be both irritating and slow
  (setq electric-pair-preserve-balance nil)

  ;; Disable pairs when entering minibuffer
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (electric-pair-mode -1)))
  ;; Re-enable pairs when existing minibuffer
  (add-hook 'minibuffer-exit-hook (lambda ()
                                    (electric-pair-mode 1))))

;; `sp-cheat-sheet' will show you all the commands available, with examples. Seems to have
;; performance issue with `latex-mode', `markdown-mode', and large JSON files.
;; https://web.archive.org/web/20201109035847/http://ebzzry.io/en/emacs-pairs/
(use-package smartparens
  :commands (sp-pair sp-local-pair sp-raise-sexp sp-join-sexp sp-absorb-sexp
                     sp-transpose-sexp sp-absort-sexp sp-copy-sexp
                     sp-backward-kill-sexp sp-kill-sexp sp-change-inner
                     sp-change-enclosing sp-convolute-sexp sp-emit-sexp
                     sp-backward-down-sexp sp-backward-up-sexp
                     sp-backward-slurp-sexp sp-backward-barf-sexp
                     sp-forward-barf-sexp sp-forward-slurp-sexp sp-rewrap-sexp
                     sp-unwrap-sexp sp-backward-unwrap-sexp sp-wrap-round
                     sp-wrap-curly sp-wrap-square sp-split-sexp)
  :diminish
  :preface
  ;; https://web-mode.org/
  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  :hook
  ((after-init . (lambda ()
                   (require 'smartparens-config)
                   (smartparens-global-mode 1)
                   (show-smartparens-global-mode 1))))
  :config
  (setq sp-show-pair-from-inside t
        sp-autoskip-closing-pair 'always)

  (smartparens-strict-mode -1)

  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

  (sp-local-pair 'markdown-mode "<" ">")

  ;; Do not insert a parenthesis pair when the point is at the beginning of a word
  (sp-pair "("  nil :unless '(sp-point-before-word-p))
  (sp-pair "["  nil :unless '(sp-point-before-word-p))
  (sp-pair "{"  nil :unless '(sp-point-before-word-p))
  (sp-pair "\"" nil :unless '(sp-point-before-word-p sp-point-after-word-p))

  (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p))
  :bind
  (("C-M-a" . sp-beginning-of-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-e" . sp-end-of-sexp) ; "f_oo bar" -> "foo bar_"
   ("C-M-u" . sp-up-sexp) ; "f_oo bar" -> "foo bar"_
   ("C-M-w" . sp-down-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-f" . sp-forward-sexp) ; "foo ba_r" -> "foo bar"_
   ("C-M-b" . sp-backward-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-n" . sp-next-sexp) ; ))" -> ((foo) (bar))"
   ("C-M-p" . sp-previous-sexp) ; "(foo (b|ar baz))" -> "(foo| (bar baz))"
   ;; TODO: The keybindings are not properly supported yet in terminal Emacs
   ("C-S-b" . sp-backward-symbol) ; "foo bar| baz" -> "foo |bar baz"
   ("C-S-f" . sp-forward-symbol) ; "|foo bar baz" -> "foo| bar baz"
   ;; "(foo bar)" -> "foo bar"
   ("C-M-k" . sp-splice-sexp)))

;; v8.1: This seems a reasonable alternative to `projectile', but does not remember remote projects
;; yet.
(use-package project
  :commands (project-switch-project project-current
                                    project-find-file project-execute-extended-command
                                    project-known-project-roots
                                    project-remove-known-project
                                    project-remember-project
                                    project-kill-buffers
                                    project-switch-to-buffer
                                    project-search
                                    project-compile)
  :bind
  (:map project-prefix-map
        ("f" . project-find-file)
        ("F" . project-or-external-find-file)
        ("b" . project-switch-to-buffer)
        ("d" . project-dired)
        ("v" . project-vc-dir)
        ("c" . project-compile)
        ("k" . project-kill-buffers)
        ("p" . project-switch-project)
        ("g" . project-find-regexp)
        ("r" . project-query-replace-regexp)))

(use-package projectile
  :commands (projectile-project-p projectile-project-name
                                  projectile-expand-root
                                  projectile-project-root
                                  projectile-mode
                                  projectile-compile
                                  projectile-compile-project)
  :preface
  (defun sb/close-treemacs-with-projectile (orig-fun &rest args)
    (let ((res (apply orig-fun args)))
      (treemacs)
      res))
  :config
  (setq projectile-enable-caching nil ; Caching will not watch for file system changes
        projectile-file-exists-remote-cache-expire nil
        projectile-mode-line-prefix "" ; Save modeline space
        ;; Use only in desired directories, too much noise otherwise
        projectile-require-project-root t
        ;; The contents of ".projectile" are ignored when using the alien project indexing method
        projectile-indexing-method 'alien
        ;; No sorting should be faster, note that files are not sorted if
        ;; `projectile-indexing-method' is set to 'alien'.
        projectile-sort-order 'recently-active
        projectile-verbose nil)

  ;; See Git status after switching to a project
  ;; (setq projectile-switch-project-action #'magit-status)

  ;; https://github.com/MatthewZMD/.emacs.d
  (when (and (symbol-value 'sb/IS-WINDOWS)
             (executable-find "tr"))
    (setq projectile-indexing-method 'alien))

  ;; Disable computing the project type that is shown on the modeline
  (defun projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      ;; (format " [%s: %s]"
      ;;         projectile-mode-line-prefix
      ;;         (or project-name "-"))
      (format " [%s]" (or project-name "-"))))

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
                                        (expand-file-name "github"            sb/user-home)
                                        (expand-file-name "iitk-workspace"    sb/user-home)
                                        (expand-file-name "iitkgp-workspace"  sb/user-home)
                                        (expand-file-name "iss-workspace"     sb/user-home)
                                        (expand-file-name "plass-workspace"   sb/user-home)
                                        (expand-file-name "prospar-workspace" sb/user-home)
                                        ))

  (dolist (prjs (list
                 (expand-file-name sb/user-home) ; Do not consider $HOME as a project
                 "~/" ; Do not consider $HOME as a project
                 (expand-file-name "/tmp")
                 (expand-file-name "bitbucket/.metadata"       sb/user-home)
                 (expand-file-name "github/.metadata"          sb/user-home)
                 (expand-file-name "iitk-workspace/.metadata"  sb/user-home)
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
  ;; TODO: These do not achieve what I want.

  ;; (add-hook 'projectile-after-switch-project-hook
  ;;           (lambda ()
  ;;             (treemacs-add-and-display-current-project)
  ;;             (treemacs-display-current-project-exclusively)
  ;;             (other-window 1)))

  ;; (add-hook 'projectile-after-switch-project-hook
  ;;           (lambda ()
  ;;             (treemacs)
  ;;             (treemacs-display-current-project-exclusively)
  ;;             (other-window 1)))

  ;; (advice-add 'projectile-kill-buffers :around #'sb/close-treemacs-with-projectile)

  :bind-keymap ("C-c p" . projectile-command-map)
  ;; :init
  ;; Use idle timer in case we open a project file without enabling projectile via bind-keys
  ;; (run-with-idle-timer 2 nil #'projectile-mode)
  :hook (after-init . projectile-mode)
  :bind
  ;; Set these in case `counsel-projectile' is disabled
  (("<f5>" . projectile-switch-project)
   ("<f6>" . projectile-find-file)
   :map projectile-command-map
   ("A"    . projectile-add-known-project)))

;; I am unsure how does this package advances `projectile' in terms of usability.
(use-package counsel-projectile
  :disabled t
  :defines counsel-projectile-default-file
  :commands (counsel-projectile-switch-project-by-name counsel-projectile-mode)
  :preface
  (defun sb/counsel-projectile-switch-project-magit (project)
    "Open Magit for the PROJECT."
    (let ((projectile-switch-project-action 'magit-status))
      (counsel-projectile-switch-project-by-name project)))

  ;; Set a default landing file: https://github.com/ericdanan/counsel-projectile/issues/172
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

;; Enable before `ivy-rich-mode' for better performance. The new transformers (file permissions)
;; seem an overkill, and it hides long file names.
(use-package all-the-icons-ivy-rich
  :ensure t
  :ensure ivy-rich
  :commands all-the-icons-ivy-rich-mode
  :if (display-graphic-p)
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :config (setq all-the-icons-ivy-rich-icon-size 0.9))

(use-package ivy-rich
  :commands (ivy-rich-modify-column ivy-rich-set-columns ivy-rich-modify-columns)
  :after (ivy counsel) ; We do not enable `all-the-icons-ivy-rich' in TUI mode
  :preface
  ;; Adapted from
  ;; https://github.com/tshu-w/.emacs.d/blob/master/lisp/editor-completion.el
  (defun sb/ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))

  (defun sb/ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))
  :init (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (if (display-graphic-p)
      (ivy-rich-set-columns 'counsel-find-file
                            '((all-the-icons-ivy-rich-file-icon)
                              (ivy-rich-candidate    (:width 0.70))
                              (sb/ivy-rich-file-user (:width 15 :face font-lock-doc-face))
                              (sb/ivy-rich-file-size (:width 10 :align right
                                                             :face font-lock-doc-face))))
    (ivy-rich-set-columns 'counsel-find-file
                          '((ivy-rich-candidate    (:width 0.70))
                            (sb/ivy-rich-file-user (:width 15 :face font-lock-doc-face))
                            (sb/ivy-rich-file-size (:width 10 :align right
                                                           :face font-lock-doc-face)))))

  ;; Increase the width to see the major mode clearly
  (ivy-rich-modify-columns 'ivy-switch-buffer
                           '((ivy-rich-switch-buffer-size (:align right))
                             (ivy-rich-switch-buffer-major-mode (:width 18 :face error))
                             (ivy-rich-switch-buffer-project (:width 0.30 :face success))))

  (ivy-rich-set-columns 'counsel-recentf
                        '((file-name-nondirectory (:width 0.24))
                          (ivy-rich-candidate (:width 0.75)))))

(use-package counsel-fd
  :if (executable-find "fd")
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
  :hook (after-init . global-flycheck-mode)
  :config
  ;; Remove newline checks, since they would trigger an immediate check when we want the
  ;; `flycheck-idle-change-delay' to be in effect while editing.
  (setq flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change)
        flycheck-checker-error-threshold 1500
        flycheck-idle-buffer-switch-delay 10 ; Increase the time (s) to allow for quick transitions
        flycheck-idle-change-delay 10 ; Increase the time (s) to allow for edits
        flycheck-emacs-lisp-load-path 'inherit
        ;; Show error messages only if the error list is not already visible
        ;; flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        ;; There are no checkers for `csv-mode', and many program modes use lsp. `yaml-mode' is
        ;; derived from `text-mode'. `chktex' errors are often not very helpful.
        flycheck-global-modes '(not csv-mode))

  ;; We prefer not to use `textlint' and `proselint', `proselint' is not maintained.
  (dolist (checkers '(proselint textlint tex-chktex))
    (delq checkers flycheck-checkers))

  (when (eq sb/modeline-theme 'doom-modeline)
    (setq flycheck-mode-line nil))

  (setq-default flycheck-markdown-markdownlint-cli-config (expand-file-name ".markdownlint.json"
                                                                            sb/user-home)
                flycheck-chktexrc "chktexrc"
                flycheck-pylintrc '("setup.cfg" "pylintrc")
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
    "Variables inherited by the `org-lint' subprocess.")

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
    '(".git/" "elpa/" ".cache" ".clangd"))

  (defun sb/flycheck-may-check-automatically (&rest _conditions)
    (or (null buffer-file-name)
        (let ((bufname (file-truename buffer-file-name)))
          (not (seq-some (lambda (re) (string-match-p re bufname))
                         sb/excluded-directory-regexps)))))

  (advice-add 'flycheck-may-check-automatically
              :after-while #'sb/flycheck-may-check-automatically)

  ;; Chain flycheck checkers with lsp, we can also use per-project directory local variables
  ;; https://github.com/flycheck/flycheck/issues/1762

  (defvar-local sb/flycheck-local-checkers nil)

  (defun sb/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker sb/flycheck-local-checkers))
        (funcall fn checker property)))

  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get)

  ;; (add-hook 'lsp-managed-mode-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'python-mode)
  ;;               (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint)))))))
  ;;             ))
  )

;; Showing error messages  in the echo area are less intrusive.
(or
 (use-package flycheck-popup-tip ; Show error messages in popups
   :unless (display-graphic-p)
   :disabled t
   :hook (flycheck-mode . flycheck-popup-tip-mode))

 ;; Does not display popup under TTY, check possible workarounds at
 ;; https://github.com/flycheck/flycheck-popup-tip
 (use-package flycheck-pos-tip
   :disabled t
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
  ;; :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  :hook (markdown-mode . whitespace-mode)
  :config
  (setq show-trailing-whitespace t
        whitespace-line-column sb/fill-column
        whitespace-style '(face lines-tail trailing)))

;; This is different from `whitespace-cleanup-mode' since this is unconditional
(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (setq delete-trailing-lines t) ; `M-x delete-trailing-whitespace' deletes trailing lines
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

;; Call `whitespace-cleanup' only if the initial buffer was clean. This mode works on the entire
;; file unlike `ws-butler'. To enable the mode for an entire project, set `whitespace-cleanup-mode'
;; to `t' in the `.dir-locals.el' file.
(use-package whitespace-cleanup-mode
  :disabled t
  :diminish
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
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
  :diminish
  :commands (symbol-overlay-mode)
  :hook (prog-mode . symbol-overlay-mode)
  :bind
  (("M-p" . symbol-overlay-jump-prev)
   ("M-n" . symbol-overlay-jump-next))
  :config
  ;; Delay highlighting to allow for transient cursor placements
  (setq symbol-overlay-idle-time 2))

(use-package hl-todo
  :commands global-hl-todo-mode
  ;; :init (run-with-idle-timer 3 nil #'global-hl-todo-mode)
  :hook (after-init . global-hl-todo-mode)
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
  :hook ((prog-mode yaml-mode conf-mode css-mode html-mode) . highlight-numbers-mode))

(use-package page-break-lines ; Display ugly "^L" page breaks as tidy horizontal lines
  :diminish
  :commands (global-page-break-lines-mode page-break-lines-mode)
  ;; :init (run-with-idle-timer 3 nil #'global-page-break-lines-mode)
  :hook (after-init . global-page-break-lines-mode))

(use-package number-separator
  :ensure nil
  :load-path "extras"
  :commands number-separator-mode
  :disabled t
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
  :bind
  (("C-<"     . mc/mark-previous-like-this)
   ("C->"     . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; Edit remote file: "/method:user@host#port:filename". Shortcut "/ssh::" will connect to default
;; "user@host#port".
;; Edit local file with sudo: "C-x C-f /sudo::/etc/hosts".
;; Open a remote file with ssh + sudo: "C-x C-f /ssh:host|sudo:root:/etc/passwd".
;; Multihop syntax: C-x C-f /ssh:bird@bastion|ssh:you@remotehost:/path
;; Multihop with sudo: "C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file"
;; Multihop with sudo with custom user: "C-x C-f
;; /ssh:you@remotehost|sudo:them@remotehost:/path/to/file"

;; https://helpdeskheadesk.net/help-desk-head-desk/2021-05-19/
;; Use bookmarks to speed up remote file access: upon visiting a location with TRAMP, save it as a
;; bookmark with `bookmark-set' (C-x r m). To revisit that bookmark, use `bookmark-jump' (C-x r b)
;; or `bookmark-bmenu-list' (C-x r l). Rename the bookmarked location in `bookmark-bmenu-mode' with
;; `R'.
(use-package tramp
  :config
  (setq tramp-default-user user-login-name
        ;; Tramp uses SSH when connecting and when viewing a directory, but it will use SCP to copy
        ;; files which is faster than SSH.
        ;; tramp-default-method "ssh"
        ;; tramp-default-remote-shell "/bin/bash"
        remote-file-name-inhibit-cache nil ; Remote files are not updated outside of Tramp
        ;; Disable default options, reuse SSH connections by reading "~/.ssh/config" control master
        ;; settings
        ;; https://emacs.stackexchange.com/questions/22306/working-with-tramp-mode-on-slow-connection-emacs-does-network-trip-when-i-start
        ;; https://puppet.com/blog/speed-up-ssh-by-reusing-connections
        tramp-ssh-controlmaster-options ""
        tramp-verbose 1
        ;; Disable version control for remote files to improve performance
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp tramp-file-name-regexp))

  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)

  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; https://www.gnu.org/software/tramp/
  (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  :bind ("C-S-q" . tramp-cleanup-all-buffers))

;; (declare-function sb/sshlist "private")

;; (progn
;;   (defun sb/ivy-tramp ()
;;     "Invoke remote hosts with ivy and tramp."
;;     (interactive)
;;     (counsel-find-file (ivy-read "Remote Tramp targets: " (sb/sshlist))))

;;   (bind-key "C-c d t" #'sb/ivy-tramp))

(use-package counsel-tramp
  :bind ("C-c d t" . counsel-tramp))

;; TODO: SSH into Gcloud
;; https://gist.github.com/jackrusher/36c80a2fd6a8fe8ddf46bc7e408ae1f9
;; Make sure you've set your default project with:
;; gcloud config set project <project-name>
;; C-x C-f /gcssh:compute-instance:/path/to/filename.clj

;; LATER: Can we shorten long Tramp file names? This does not work with Tramp.
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/data/swarnendu/" . "/vindhya/data/swarnendu/"))
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/home/swarnendu/" . "/vindhya/home/swarnendu/"))

(use-package imenu
  :ensure nil
  :after (:any markdown-mode yaml-mode prog-mode)
  :config
  (setq imenu-auto-rescan t
        imenu-max-items 1000
        ;; `t' will use a popup menu rather than a minibuffer prompt, `on-mouse' might be useful
        ;; with mouse support enabled
        imenu-use-popup-menu nil
        ;; `nil' implies no sorting, and will list by position in the buffer
        imenu-sort-function nil))

(defvar tags-revert-without-query)

(setq large-file-warning-threshold (* 500 1024 1024) ; MB
      tags-add-tables nil
      tags-case-fold-search nil ; t=case-insensitive, nil=case-sensitive
      ;; Do not ask before rereading the `TAGS' files if they have changed
      tags-revert-without-query t)

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
  :demand t
  :config
  (setq xref-show-definitions-function #'ivy-xref-show-defs
        xref-show-xrefs-function       #'ivy-xref-show-xrefs))

(use-package counsel-etags
  :defines (counsel-etags-ignore-directories counsel-etags-ignore-filenames)
  :commands counsel-etags-virtual-update-tags
  :if (symbol-value 'sb/IS-LINUX)
  :bind
  (("M-]"     . counsel-etags-find-tag-at-point)
   ("C-c g s" . counsel-etags-find-symbol-at-point)
   ("C-c g f" . counsel-etags-find-tag)
   ("C-c g l" . counsel-etags-list-tag)
   ("C-c g c" . counsel-etags-scan-code))
  :config
  (defalias 'list-tags 'counsel-etags-list-tag-in-current-file)

  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'counsel-etags-virtual-update-tags 'append 'local)))

  (dolist (ignore-dirs '(".vscode" "build" ".metadata" ".recommenders" ".clangd" ".cache"))
    (add-to-list 'counsel-etags-ignore-directories ignore-dirs))

  (dolist (ignore-files '(".clang-format" ".clang-tidy" "*.json" "*.html" "*.xml"))
    (add-to-list 'counsel-etags-ignore-filenames ignore-files)))

(use-package dumb-jump
  :after xref
  :demand t
  :commands dumb-jump-xref-activate
  :config
  (setq dumb-jump-quiet t)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; The built-in `describe-function' includes both functions and macros. `helpful-function' is
;; functions only, so we use `helpful-callable' as a drop-in replacement.
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

(use-package vlf ; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
  :commands vlf
  :defines vlf-application
  :init
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

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(use-package popwin
  :commands popwin-mode
  :hook (after-init . popwin-mode)
  :config
  (defvar popwin:special-display-config-backup popwin:special-display-config)

  (push '("*Help*"              :noselect t)   popwin:special-display-config)
  (push '(compilation-mode      :noselect t)   popwin:special-display-config)
  (push '("*Compile-Log*"       :noselect t)   popwin:special-display-config)
  (push '("*manage-minor-mode*" :noselect t)   popwin:special-display-config)
  (push '("*Paradox Report*"    :noselect t)   popwin:special-display-config)
  (push '("*Selection Ring:")                  popwin:special-display-config)
  (push '("*Flycheck errors*"   :noselect nil) popwin:special-display-config)
  (push '("*Flycheck checkers*" :noselect nil) popwin:special-display-config)
  (push '("*ripgrep-search*"    :noselect nil) popwin:special-display-config)
  (push '("^\*magit:.+\*$"      :noselect nil) popwin:special-display-config)
  (push '("*xref*"              :noselect nil) popwin:special-display-config)
  (push '(helpful-mode          :noselect t)   popwin:special-display-config)
  (push "*Shell Command Output*"               popwin:special-display-config)
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
(add-to-list 'display-buffer-alist '("^\\*deadgrep*"            display-buffer-same-window))
;; Open shell in same window.
(add-to-list 'display-buffer-alist `(,(regexp-quote "*shell")   display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Compile-Log\\*"       display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Warnings\\*"          display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Backtrace\\*"         display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Async Shell Command*"    display-buffer-no-window))

;; ;; Do not popup the *Async Shell Command* buffer
;; (add-to-list 'display-buffer-alist
;;              (cons "\\*Async Shell Command\\*.*"
;;                    (cons #'display-buffer-no-window nil)))

(use-package expand-region ; Expand region by semantic units
  :bind
  (("C-="   . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package expand-line
  :diminish
  :bind ("M-i" . turn-on-expand-line-mode))

;; Restore point to the initial location with `C-g' after marking a region
(use-package smart-mark
  ;; :init (run-with-idle-timer 3 nil #'smart-mark-mode)
  :hook (after-init . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :commands (whole-line-or-region-local-mode whole-line-or-region-global-mode)
  :diminish (whole-line-or-region-local-mode)
  ;; :init (run-with-idle-timer 3 nil #'whole-line-or-region-global-mode)
  :hook (after-init . whole-line-or-region-global-mode))

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

;; The real beginning and end of buffers (i.e., `point-min' and `point-max') are accessible by
;; pressing the keys `M-<' and `M->' keys again.
(use-package beginend
  ;; :init (run-with-idle-timer 3 nil #'beginend-global-mode)
  :hook (after-init . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))

(use-package undo-tree
  :defines undo-tree-map
  :commands (global-undo-tree-mode undo-tree-redo)
  :diminish
  :config
  (setq undo-tree-auto-save-history              t
        undo-tree-visualizer-diff                t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-visualizer-timestamps          t)
  (global-undo-tree-mode 1)
  (unbind-key "C-/" undo-tree-map)
  :bind
  (("C-z"   . undo-tree-undo)
   ("C-x u" . undo-tree-visualize)))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :bind* ("C-." . iedit-mode))

;; Avoid the "Overwrite old session file (not loaded)?" warning by loading the `session' package
(use-package session
  :disabled t
  :commands (session-initialize)
  :hook (after-init . session-initialize))

(use-package immortal-scratch
  :commands immortal-scratch-mode
  ;; :init (run-with-idle-timer 2 nil #'immortal-scratch-mode)
  :hook (after-init . immortal-scratch-mode))

;; I use the *scratch* buffer for taking notes, this package helps to make the data persist
(use-package persistent-scratch
  :commands persistent-scratch-setup-default
  :hook (after-init . persistent-scratch-setup-default)
  :config
  (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

(use-package crux
  :bind
  (("C-c d i" . crux-ispell-word-then-abbrev)
   ("<f12>"   . crux-kill-other-buffers)
   ("C-c d s" . crux-sudo-edit)))

;; This package disables the mouse completely which is an extreme.
(use-package disable-mouse
  :if (display-mouse-p)
  :disabled t
  :commands global-disable-mouse-mode
  :diminish disable-mouse-global-mode
  :hook (after-init . global-disable-mouse-mode))

;; Move the cursor from the line of view
(use-package avoid
  :ensure nil
  :commands mouse-avoidance-mode
  :if (display-mouse-p)
  :init (mouse-avoidance-mode 'banish))

(use-package apt-sources-list
  :commands apt-sources-list-mode)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook ((prog-mode latex-mode LaTeX-mode org-src-mode) . rainbow-delimiters-mode))

(use-package ssh-config-mode
  :commands (ssh-config-mode ssh-known-hosts-mode ssh-authorized-keys-mode turn-on-font-lock)
  :hook (ssh-config-mode . turn-on-font-lock))

(use-package ace-window
  :bind ([remap other-window] . ace-window))

(use-package windmove ; "Shift + direction" arrows
  :ensure nil
  :commands windmove-default-keybindings
  :init (windmove-default-keybindings)
  :config
  ;; Wrap around at edges
  (setq windmove-wrap-around t))

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `after-save-hook' and leads to auto-formatters being invoked more frequently. We do not need this
;; given that we have `auto-save-visited-mode' enabled.
(use-package super-save
  :defines (super-save-remote-files super-save-triggers)
  :commands super-save-mode
  :disabled t
  :diminish
  ;; :init (run-with-idle-timer 3 nil #'super-save-mode)
  :hook (after-init . super-save-mode)
  :config
  (setq super-save-remote-files nil) ; Ignore remote files, can cause Emacs to hang
  (add-to-list 'super-save-triggers 'ace-window))

;; `avy-setup-default' will bind `avy-isearch' to `C-'' in `isearch-mode-map', so that you can
;; select one of the currently visible `isearch' candidates using `avy'.
(use-package avy
  :commands avy-setup-default
  :bind
  (("M-b"   . avy-goto-word-1)
   ("C-'"   . avy-goto-char-timer) ; Does not work with TUI, but works with Alacritty
   ("M-g c" . avy-goto-char-timer) ; TODO: Reuse the keybinding
   ("C-/"   . avy-goto-line) ; Does not work with TUI, but works with Alacritty
   ;; TODO: Reuse the keybinding
   ("M-g l" . avy-goto-line)))

(use-package ace-jump-buffer
  :bind ("C-b" . ace-jump-buffer)
  :config
  (setq ajb-max-window-height 30
        ajb-sort-function 'bs--sort-by-name))

;; This package adds a `C-'' binding to Ivy minibuffer that uses Avy
(use-package ivy-avy
  :after ivy
  :bind
  (:map ivy-minibuffer-map
        ("C-'"   . ivy-avy) ; Does not work with TUI, but works with Alacritty
        ("M-g l" . ivy-avy)))

(use-package bookmark
  :ensure nil)

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
    (add-hook 'after-save-hook        #'bm-buffer-save)
    (add-hook 'kill-buffer-hook       #'bm-buffer-save)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
    (add-hook 'after-revert-hook      #'bm-buffer-restore)
    (add-hook 'find-file-hook         #'bm-buffer-restore)
    (add-hook 'after-init-hook        #'bm-repository-load))
  :init
  ;; Must be set before `bm' is loaded
  (setq bm-restore-repository-on-load t)
  ;; We need to use a reasonable delay so that reading the saved bookmarks file does not affect
  ;; usability
  ;; (run-with-idle-timer 2 nil #'sb/bm-setup)
  :hook (after-init . sb/bm-setup)
  :config (setq-default bm-buffer-persistence t)
  :bind
  (("C-<f1>" . bm-toggle)
   ("C-<f2>" . bm-next)
   ("C-<f3>" . bm-previous)))

(use-package esup
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
  :commands (explain-pause-mode explain-pause-top)
  :diminish)

;; `text-mode' is the parent mode for `LaTeX-mode' and `org-mode', and so any hooks defined will
;; also get run for all modes derived from a basic mode such as `text-mode'.

;; Enabling `autofill-mode' makes it difficult to include long instructions verbatim, since they get
;; wrapped around automatically.
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Identify weasel words, passive voice, and duplicate words, `textlint' includes writegood. I
;; prefer `grammarly' and `lsp-ltex'. The module does not check grammar but checks the writing
;; style.
(use-package writegood-mode
  :disabled t
  :commands writegood-mode
  :diminish
  :hook (text-mode . writegood-mode))

(use-package wc-mode
  :commands wc-mode)

;; Gets the definition of word or phrase at point from https://wordnik.com/
(use-package define-word
  :commands (define-word define-word-at-point))

;; https://emacs.stackexchange.com/questions/19686/how-to-use-pdf-tools-pdf-view-mode-in-emacs
;; Use `isearch', `swiper' will not work
(use-package pdf-tools
  :defines pdf-annot-activate-created-annotations
  :commands (pdf-tools-install pdf-loader-install pdf-view-mode
                               pdf-annot-delete pdf-annot-add-highlight-markup-annotation
                               pdf-annot-add-text-annotation)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  ;; :init (run-with-idle-timer 3 nil #'require 'pdf-tools nil t) ; Expensive to load
  :hook (after-init . (lambda ()
                        (require 'pdf-tools nil t)))
  :config
  (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install :no-query)'

  (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable

  (setq pdf-annot-activate-created-annotations t  ; Automatically annotate highlights
        pdf-view-resize-factor 1.1) ; Fine-grained zoom factor of 10%

  ;; We do not enable `pdf-view-themed-minor-mode' since it can change plot colors
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  :bind
  (:map pdf-view-mode-map
        ("C-s" . isearch-forward)
        ("d"   . pdf-annot-delete)
        ("h"   . pdf-annot-add-highlight-markup-annotation)
        ("t"   . pdf-annot-add-text-annotation)
        ("M" . pdf-view-midnight-minor-mode)))

;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.
(use-package saveplace-pdf-view
  :after (pdf-tools saveplace)
  :demand t)

(use-package logview
  :commands logview-mode)

(use-package antlr-mode
  :ensure nil
  :mode "\\.g4\\'")

(use-package bison-mode
  :mode ("\\.bison\\'"))

(use-package llvm-mode
  :ensure nil
  :load-path "extras"
  :commands llvm-mode
  :mode "\\.ll\\'")

(use-package tablegen-mode
  :ensure nil
  :load-path "extras"
  :commands tablegen-mode
  :disabled t
  :mode "\\.td\\'")

(use-package autodisass-llvm-bitcode
  :commands autodisass-llvm-bitcode
  :mode "\\.bc\\'")

;; Enable live preview with "C-c C-c l" (`markdown-live-preview-mode'). The following page lists
;; more shortcuts.
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :commands (markdown-mode gfm-mode markdown-insert-bold
                           markdown-insert-italic
                           markdown-insert-blockquote
                           markdown-insert-pre
                           markdown-insert-code markdown-move-up
                           markdown-insert-link
                           markdown-insert-wiki-link
                           markdown-demote
                           markdown-move-down
                           markdown-insert-header-dwim
                           markdown-insert-reference-link-dwim
                           markdown-insert-header-atx-1
                           markdown-insert-header-atx-2
                           markdown-insert-header-atx-3
                           markdown-insert-header-atx-4
                           markdown-promote
                           markdown-insert-list-item
                           markdown-insert-uri
                           markdown-insert-footnote)
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
  :commands (markdown-toc-refresh-toc markdown-toc-generate-toc
                                      markdown-toc-generate-or-refresh-toc))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf
;; Convert `markdown' to `org': `pandoc -f markdown -t org -o output-file.org input-file.md'
(use-package pandoc-mode
  :commands (pandoc-load-default-settings pandoc-mode)
  :hook (markdown-mode . pandoc-mode)
  :config
  (pandoc-load-default-settings)
  ;; (unbind-key "C-c /" pandoc-mode-map) ; Binds `C-c /' to `pandoc-main-hydra/body'
  )

;; Open preview of markdown file in a browser
(use-package markdown-preview-mode
  :disabled t
  :commands markdown-preview-mode)

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
  :defines lsp-disabled-clients
  :commands csv-mode
  :hook
  (csv-mode . (lambda ()
                (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                (spell-fu-mode -1)
                (flyspell-mode -1)))
  :config
  (setq csv-separators '("," ";" "|" " ")))

(use-package highlight-doxygen
  :commands highlight-doxygen-global-mode
  :init (highlight-doxygen-global-mode))

(use-package make-mode
  :ensure nil
  :mode
  (("\\Makefile\\'"       . makefile-mode)
   ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
   ("makefile\\.rules\\'" . makefile-gmake-mode))
  :config (add-hook 'makefile-mode-hook (lambda()
                                          (setq-local indent-tabs-mode t))))

(use-package eldoc
  :ensure nil
  :if (symbol-value 'sb/IS-LINUX)
  :commands turn-on-eldoc-mode
  :diminish
  :hook (prog-mode . turn-on-eldoc-mode)
  ;; :config
  ;; The variable-height minibuffer and extra eldoc buffers are distracting. This variable limits
  ;; ElDoc messages to one line. This prevents the echo area from resizing itself unexpectedly when
  ;; point is on a variable with a multiline docstring, which is distracting, but then it cuts of
  ;; useful information.
  ;; (setq eldoc-echo-area-use-multiline-p nil)
  )

(use-package c-eldoc
  :disabled t ;; We use LSP
  :hook (c-mode-common . c-turn-on-eldoc-mode))

(use-package css-mode
  :commands css-mode
  :defines sb/flycheck-local-checkers
  :hook (css-mode . lsp-deferred)
  :config
  (setq css-indent-offset 2)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("css-languageserver" "--stdio"))
    :major-modes '(css-mode)
    :remote? t
    :server-id 'cssls-r)))

(use-package css-eldoc
  :disabled t ;; We use LSP
  :after css-mode
  :demand t
  :commands css-eldoc-enable
  :config (css-eldoc-enable))

;; `eldoc-box-hover-at-point-mode' blocks the view because it shows up at point.
(use-package eldoc-box
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :config
  (setq eldoc-box-clear-with-C-g t
        eldoc-box-fringe-use-same-bg nil)
  :diminish eldoc-box-hover-mode eldoc-box-hover-at-point-mode)

(use-package ini-mode
  :commands ini-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlight and allow to open http link at point in programming buffers.
            ;; `goto-address-prog-mode' only highlights links in strings and comments.
            (goto-address-prog-mode 1)
            ;; Native from Emacs 27+. Disable in TUI since the line characters also get copied.
            (when (display-graphic-p)
              (display-fill-column-indicator-mode 1))))

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
                                                                'emacs-lisp-checkdoc 'append)))))

(use-package yaml-mode
  :defines lsp-ltex-enabled lsp-disabled-clients
  :commands yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'" ".clang-format" ".clang-tidy")
  :hook
  (yaml-mode . (lambda ()
                 ;; `yaml-mode' is derived from `text-mode', so disable grammar and spell checking.
                 (make-local-variable 'lsp-disabled-clients)
                 (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                 (spell-fu-mode -1)
                 (flyspell-mode -1)
                 (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("yaml-language-server" "--stdio"))
    :major-modes '(yaml-mode)
    :remote? t
    :server-id 'yamlls-r)))

(use-package yaml-imenu
  :after yaml-mode
  :demand t
  :config (yaml-imenu-enable))

(declare-function ht-merge "ht")

;; TODO: Disable `flycheck-mode-line' per-buffer when `lsp-modeline-diagnostics-mode' is active.

;; Registering `lsp-format-buffer' makes sense only if the server is active. We may not always
;; want to format unrelated files and buffers (e.g., commented YAML files in out-of-project
;; locations).
(use-package lsp-mode
  :ensure spinner
  ;; :diminish "LSP"
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
            lsp-xml-jar-version
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
                                    lsp-signature-help
                                    lsp-enable-which-key-integration
                                    lsp-modeline-diagnostics-mode
                                    lsp-modeline-code-actions-mode
                                    lsp-symbol-highlight ht-merge
                                    lsp-completion--regex-fuz
                                    lsp-describe-thing-at-point
                                    lsp-find-type-definition)
  :hook
  ((lsp-mode . lsp-enable-which-key-integration)
   (lsp-mode . lsp-lens-mode))
  :custom-face
  ;; Reduce the height
  (lsp-headerline-breadcrumb-symbols-face ((t (:inherit
                                               font-lock-doc-face :weight bold :height 0.9))))
  (lsp-headerline-breadcrumb-prefix-face ((t (:inherit font-lock-string-face :height 0.9))))
  (lsp-headerline-breadcrumb-project-prefix-face ((t (:inherit font-lock-string-face
                                                               :weight bold :height 0.9))))
  :config
  ;; We can add "--compile-commands-dir=<build-dir>" option to indicate the directory where
  ;; "compile_commands.json" reside
  (setq lsp-clients-clangd-args '("-j=4"
                                  "--all-scopes-completion"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--cross-file-rename"
                                  "--fallback-style=LLVM"
                                  "--header-insertion=never"
                                  "--header-insertion-decorators"
                                  "--log=error"
                                  "--malloc-trim"
                                  ;; Increases memory usage but can improve performance
                                  "--pch-storage=memory"
                                  "--pretty")
        ;; lsp-clients-clangd-executable "clangd"
        ;; Enable integration of custom backends other than `capf' with `company'
        lsp-completion-provider :none
        lsp-completion-show-detail nil ; Disable completion metadata since they can be very long
        ;; lsp-completion-show-kind nil
        ;; lsp-eldoc-enable-hover nil
        lsp-enable-dap-auto-configure nil
        lsp-enable-on-type-formatting nil
        ;; lsp-semantic-tokens-enable t
        ;; lsp-enable-snippet t ; Autocomplete parentheses
        lsp-headerline-breadcrumb-enable nil ; Breadcrumb is not useful for all modes
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-html-format-wrap-line-length sb/fill-column
        lsp-html-format-end-with-newline t
        lsp-html-format-indent-inner-html t
        lsp-imenu-sort-methods '(position)
        ;; lsp-keep-workspace-alive nil
        lsp-log-io nil ; Increases memory usage because of JSON parsing if enabled
        ;; We already have `flycheck' error summary listed on the modeline, but the `lsp' server may
        ;; report additional errors. However, the modeline can get too congested.
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-diagnostics-scope :file ; Focus on the errors at hand
        ;; lsp-modeline-workspace-status-enable nil
        ;; Sudden changes in the height of the echo area causes the cursor to lose position,
        ;; manually request via `lsp-signature-activate'
        ;; lsp-signature-auto-activate nil
        ;; Disable showing function documentation with `eldoc'
        ;; lsp-signature-render-documentation nil
        ;; lsp-signature-function 'lsp-signature-posframe
        ;; Avoid annoying questions. We expect a server restart to succeed more often than not.
        lsp-restart 'auto-restart
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
        lsp-use-plists nil
        lsp-xml-logs-client nil
        lsp-yaml-print-width sb/fill-column)

  (defvar lsp-pylsp-configuration-sources)
  (defvar lsp-pylsp-plugins-autopep8-enable)
  (defvar lsp-pylsp-plugins-mccabe-enabled)
  (defvar lsp-pylsp-plugins-pycodestyle-enabled)
  (defvar lsp-pylsp-plugins-pycodestyle-max-line-length)
  (defvar lsp-pylsp-plugins-pydocstyle-convention)
  (defvar lsp-pylsp-plugins-pydocstyle-enabled)
  (defvar lsp-pylsp-plugins-pydocstyle-ignore)
  (defvar lsp-pylsp-plugins-pyflakes-enabled)
  (defvar lsp-pylsp-plugins-pylint-args)
  (defvar lsp-pylsp-plugins-pylint-enabled)
  (defvar lsp-pylsp-plugins-yapf-enabled)
  (defvar lsp-pyright-langserver-command-args)

  (when (eq sb/python-langserver 'pylsp)
    (setq lsp-pylsp-configuration-sources []
          lsp-pylsp-plugins-autopep8-enable nil
          ;; Do not turn on fuzzy completion with jedi, `lsp-mode' is fuzzy on the client side
          ;; lsp-pylsp-plugins-jedi-completion-fuzzy nil
          lsp-pylsp-plugins-mccabe-enabled nil
          ;; Set this per-project
          ;; lsp-pylsp-plugins-preload-modules ["numpy", "csv", "pandas", "statistics"]
          lsp-pylsp-plugins-pycodestyle-enabled nil
          lsp-pylsp-plugins-pycodestyle-max-line-length sb/fill-column
          lsp-pylsp-plugins-pydocstyle-convention "pep257"
          lsp-pylsp-plugins-pydocstyle-enabled nil
          lsp-pylsp-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213"))
          lsp-pylsp-plugins-pyflakes-enabled nil
          lsp-pylsp-plugins-pylint-args (vconcat
                                         (list "-j 2"
                                               (concat "--rcfile="
                                                       (expand-file-name ".config/pylintrc"
                                                                         sb/user-home))))
          lsp-pylsp-plugins-pylint-enabled t ; Pylint can be expensive
          lsp-pylsp-plugins-yapf-enabled t))

  ;; We have `flycheck' error summary listed on the modeline, but `lsp' may report additional errors
  ;; (lsp-modeline-diagnostics-mode -1)

  (dolist (ignore-dirs '("/build\\'"
                         "/\\.metadata\\'"
                         "/\\.recommenders\\'"
                         "/\\.clangd\\'"
                         "/\\.cache\\'"
                         "/__pycache__\\'"
                         "/\\.log\\'"
                         ))
    (add-to-list 'lsp-file-watch-ignored-directories ignore-dirs))

  (with-eval-after-load "lsp-lens"
    (diminish 'lsp-lens-mode))
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind
  ;; `lsp-imenu-create-categorised-index' - sorts the items by kind.
  ;; `lsp-imenu-create-uncategorized-index' - will have the items sorted by position.
  (("M-."     . lsp-find-definition)
   :map lsp-command-map
   ("=" . nil)
   ("w" . nil)
   ("g" . nil)
   ("G" . nil)
   ("a" . nil)
   ("F" . nil)
   ("L" . lsp)
   ("q" . lsp-disconnect)
   ("Q" . lsp-workspace-shutdown)
   ("H" . lsp-describe-session)
   ("R" . lsp-workspace-restart)
   ("d" . lsp-find-declaration)
   ("e" . lsp-find-definition)
   ("r" . lsp-find-references)
   ("i" . lsp-find-implementation)
   ("I" . lsp-goto-implementation)
   ("t" . lsp-goto-type-definition)
   ("r" . lsp-rename)
   ("h" . lsp-symbol-highlight)
   ("f" . lsp-format-buffer)
   ("x" . lsp-execute-code-action)
   ("c" . lsp-imenu-create-categorised-index)
   ("u" . lsp-imenu-create-uncategorised-index)
   ("a" . lsp-workspace-folders-add)
   ("v" . lsp-workspace-folders-remove)
   ("b" . lsp-workspace-blacklist-remove)))

(use-package lsp-ui
  :defines lsp-ui-modeline-code-actions-enable
  :commands (lsp-ui-doc-mode lsp-ui-mode lsp-ui-doc--hide-frame
                             lsp-ui-peek-find-implementation lsp-ui-imenu)
  :after lsp-mode
  :demand t
  :config
  (setq lsp-ui-doc-enable t ; Enable/disable on-hover dialogs
        ;; lsp-ui-doc-max-width 100
        ;; lsp-ui-doc-max-height 10
        lsp-ui-doc-include-signature t
        lsp-ui-imenu-auto-refresh 'after-save
        lsp-ui-imenu-window-width 16
        lsp-ui-sideline-enable t ; Enable/disable whole sideline
        lsp-ui-sideline-show-diagnostics t ; Show/hide diagnostics when typing
        lsp-ui-modeline-code-actions-enable t)

  (when (not (display-graphic-p))
    (setq lsp-ui-doc-enable nil
          lsp-ui-peek-enable nil))

  (lsp-ui-mode 1)

  ;; ;; https://github.com/emacs-lsp/lsp-ui/issues/578
  ;; (add-hook 'minibuffer-setup-hook
  ;;           (lambda ()
  ;;             (lsp-ui-doc--hide-frame)))

  ;; (add-hook 'lsp-managed-mode-hook
  ;;           (lambda ()
  ;;             ;; (eldoc-box-hover-mode -1)
  ;;             ;; (eldoc-box-hover-at-point-mode -1)
  ;;             (eldoc-mode -1)))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references]  . lsp-ui-peek-find-references)
        :map lsp-command-map
        ("D" . lsp-ui-doc-mode)))

;; Sync workspace folders and treemacs projects
(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list lsp-treemacs-sync-mode)
  :config (lsp-treemacs-sync-mode 1)
  :bind
  (:map lsp-command-map
        ("S" . lsp-treemacs-symbols)
        ("F" . lsp-treemacs-references)
        ("Y" . lsp-treemacs-sync-mode)
        ("C" . lsp-treemacs-call-hierarchy)
        ("T" . lsp-treemacs-type-hierarchy)
        ("E" . lsp-treemacs-errors-list)))

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :demand t
  :bind
  (:map lsp-command-map
        ("G" . lsp-ivy-global-workspace-symbol)
        ("W" . lsp-ivy-workspace-symbol)))

(use-package dap-mode
  :commands (dap-debug dap-hydra dap-mode dap-ui-mode))

(use-package docstr
  :diminish
  :hook ((c++-mode python-mode java-mode) . docstr-mode))

(use-package cc-mode
  :ensure nil
  :defines (c-electric-brace c-enable-auto-newline c-set-style)
  :commands (c-fill-paragraph c-end-of-defun c-beginning-of-defun c++-mode)
  :mode
  (("\\.h\\'" . c++-mode)
   ("\\.c\\'" . c++-mode))
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

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "clangd")
    :major-modes '(c-mode c++-mode)
    :remote? t
    :server-id 'clangd-r))
  :bind
  (:map c-mode-base-map
        ("C-c c a" . c-beginning-of-defun)
        ("C-c c e" . c-end-of-defun)
        ("M-q"     . c-fill-paragraph)))

;; FIXME: The package is not loading.
(use-package modern-cpp-font-lock
  ;; :after c++-mode
  ;; :demand t
  :commands modern-c++-font-lock-mode
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . (lambda ()
                      (modern-c++-font-lock-mode 1))))

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
  :hook
  (cmake-mode . (lambda ()
                  (make-local-variable 'lsp-disabled-clients)
                  (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                  (spell-fu-mode -1)
                  (flyspell-mode -1)
                  (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "cmake-language-server")
    :major-modes '(cmake-mode)
    :remote? t
    :server-id 'cmakels-r)))

(use-package cmake-font-lock
  :commands cmake-font-lock-activate
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package python
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :bind
  (:map python-mode-map
        ;; Assigning a keybinding such as "C-[" is involved, `[' is treated as `meta'
        ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c
        ("M-{"   . python-nav-backward-block)
        ("M-}"   . python-nav-forward-block)
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

  ;; (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint))))))

  (setq auto-mode-alist (append '(("SConstruct\\'" . python-mode)
                                  ("SConscript\\'" . python-mode))
                                auto-mode-alist)))

(use-package python-docstring
  :after python-mode
  :demand t
  :commands (python-docstring-mode python-docstring-install)
  ;; :diminish
  :config (python-docstring-install))

(use-package pip-requirements
  :commands pip-requirements-mode)

(use-package pyvenv
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

(use-package py-isort
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :commands py-isort-before-save
  :hook
  (python-mode . (lambda ()
                   (add-hook 'before-save-hook #'py-isort-before-save)))
  :config (setq py-isort-options '("-l 100")))

;; "pyright --createstub pandas"
(use-package lsp-pyright
  :if (and (eq sb/python-langserver 'pyright) (executable-find "pyright"))
  :commands (lsp-pyright-locate-python lsp-pyright-locate-venv)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)))
  :config
  (setq lsp-pyright-python-executable-cmd "python3")

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     (lambda ()
                       (cons "pyright-langserver"
                             lsp-pyright-langserver-command-args)))
    :major-modes '(python-mode)
    :remote? t
    :server-id 'pyright-r
    :multi-root lsp-pyright-multi-root
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
     ("pyright/beginProgress"  'lsp-pyright--begin-progress-callback)
     ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
     ("pyright/endProgress"    'lsp-pyright--end-progress-callback)))))

(use-package lsp-jedi
  :defines lsp-jedi-diagnostics-enable
  :if (and (eq sb/python-langserver 'jedi) (executable-find "jedi-language-server"))
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-jedi)))
  :config
  (setq lsp-jedi-diagnostics-enable t)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "jedi-language-server")
    :major-modes '(python-mode)
    :remote? t
    :server-id 'jedils-r)))

;; Yapfify works on the original file, so that any project settings supported by YAPF itself are
;; used.
(use-package yapfify
  ;; :diminish yapf-mode
  :if (and (eq sb/python-langserver 'pyright) (executable-find "yapf"))
  :commands yapf-mode
  :hook (python-mode . yapf-mode))

(use-package cperl-mode
  :ensure nil
  :mode ("latexmkrc\\'")
  :hook (cperl-mode . lsp-deferred)
  :config
  ;; Prefer CPerl mode to Perl mode
  (fset 'perl-mode 'cperl-mode)

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
    :server-id 'perlls-r)))

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
  :hook
  (java-mode . (lambda ()
                 (setq-default c-basic-offset 4
                               c-set-style "java")
                 (lsp-deferred)))
  :config
  (setq lsp-java-inhibit-message t
        ;; Requires Java 11+, Java 11 is the LTS
        lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java"
        lsp-java-save-actions-organize-imports t
        lsp-java-format-settings-profile "Swarnendu"
        lsp-java-format-settings-url (expand-file-name
                                      "github/dotfiles/java/eclipse-format-swarnendu.xml"
                                      sb/user-home)))

(use-package ant
  :commands (ant ant-clean ant-compile ant-test))

;; Can disassemble `.class' files from within jars
(use-package autodisass-java-bytecode
  :commands autodisass-java-bytecode
  :mode "\\.class\\'")

(use-package groovy-mode ; Syntax highlighting for Gradle files
  :commands groovy-mode
  :mode "\\.gradle\\'")

(use-package image-mode
  :ensure nil
  :mode "\\.svg$"
  :hook (image-mode . show-image-dimensions-in-mode-line))

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
        sh-indent-comment t)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("bash-language-server" "start"))
    :major-modes '(sh-mode)
    :remote? t
    :server-id 'bashls-r)))

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
  ;; :config
  ;; (setq transient-display-buffer-action '(display-buffer-below-selected))

  ;; Allow using `q' to quit out of popups, in addition to `C-g'
  ;; (transient-bind-q-to-quit)
  )

(use-package magit
  :commands magit-display-buffer-fullframe-status-v1
  :bind
  (("C-x g"   . magit-status)
   ("C-c M-g" . magit-file-dispatch)
   ("C-x M-g" . magit-dispatch))
  :config
  ;; Open the status buffer in a full frame
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
    (setq ;; magit-diff-refine-hunk  t
     magit-diff-highlight-trailing nil
     magit-diff-paint-whitespace   nil)))

(use-package git-modes
  :commands gitignore-mode gitattributes-mode gitconfig-mode)

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
  (setq git-gutter:added-sign      " "
        git-gutter:deleted-sign    " "
        git-gutter:modified-sign   " "
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
  (setq diff-hl-draw-borders nil) ; Highlight without a border looks nicer
  ;; Display margin since the fringe is unavailable in tty
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (dired-mode         . diff-hl-dired-mode-unless-remote)
   (diff-hl-mode       . diff-hl-flydiff-mode)
   (after-init         . global-diff-hl-mode)))

(use-package git-commit
  :commands git-commit-turn-on-flyspell
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :config
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

;; Use the minor mode `smerge-mode' to move between conflicts and resolve them
(use-package smerge-mode
  :ensure nil
  :after hydra
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
        ("M-g k u" . smerge-keep-upper)
        ("M-g k l" . smerge-keep-lower)
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
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ;; Split windows horizontally in ediff (instead of vertically)
        ediff-split-window-function #'split-window-horizontally)
  (ediff-set-diff-options 'ediff-diff-options "-w"))

(use-package bat-mode
  :ensure nil
  :commands bat-mode
  :mode
  (("\\.bat\\'" . bat-mode)
   ("\\.cmd\\'" . bat-mode)))

(use-package web-mode
  :commands web-mode
  :mode "\\.html?\\'"
  :hook (web-mode . lsp-deferred)
  :config
  (setq web-mode-enable-auto-closing              t
        web-mode-enable-auto-pairing              nil ; Prefer `smartparens'
        web-mode-enable-auto-quoting              t
        web-mode-enable-block-face                t
        web-mode-enable-css-colorization          t
        web-mode-enable-current-element-highlight t ; Highlight the element under the cursor
        web-mode-enable-current-column-highlight  t
        web-mode-markup-indent-offset             2 ; HTML
        web-mode-css-indent-offset                2 ; CSS
        web-mode-code-indent-offset               2 ; Script
        web-mode-style-padding                    2 ; For `<style>' tag
        ;; For `<script>' tag
        web-mode-script-padding                   2)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("html-languageserver" "--stdio"))
    :major-modes '(html-mode web-mode mhtml-mode)
    :remote? t
    :server-id 'htmlls-r)))

(use-package emmet-mode
  :defines emmet-move-cursor-between-quote
  :commands emmet-mode
  :hook ((web-mode css-mode html-mode) . emmet-mode)
  :config (setq emmet-move-cursor-between-quote t))

(use-package rainbow-mode
  :commands rainbow-mode
  :hook ((css-mode html-mode web-mode) . rainbow-mode))

(use-package nxml-mode
  :ensure nil
  :commands nxml-mode
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$")
  :hook
  (nxml-mode . (lambda ()
                 ;; `xml-mode' is derived from `text-mode', so disable grammar and spell checking.
                 (make-local-variable 'lsp-disabled-clients)
                 (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                 (spell-fu-mode -1)
                 (flyspell-mode -1)
                 (lsp-deferred)))
  :config
  (fset 'xml-mode 'nxml-mode)
  (setq nxml-auto-insert-xml-declaration-flag t
        nxml-slash-auto-complete-flag t)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("java" "-jar" lsp-xml-jar-file))
    :major-modes '(xml-mode nxml-mode)
    :remote? t
    :server-id 'xmlls-r)))

;; The advantage with `flycheck-grammarly' over `lsp-grammarly' is that you need not set up lsp
;; support, so you can use it anywhere. But `flycheck-grammarly' does not support a PRO Grammarly
;; account. We only need this package for checking text in *scratch* buffer.
(use-package flycheck-grammarly
  :after flycheck
  :defines flycheck-grammarly-check-time
  :demand t
  :config
  (setq flycheck-grammarly-check-time 3
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

;; Most likely, `org', `markdown', and `latex' files will be in directories that can use LSP
;; support. We only need to enable `flycheck-grammarly' support for the *scratch* buffer which is in
;; `text-mode'.

;; org -> grammarly -> languagetool
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (flycheck-select-checker 'org-lint)
;;             (when (featurep 'flycheck-grammarly)
;;               (flycheck-add-next-checker 'org-lint 'grammarly))
;;             (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
;;               (flycheck-add-next-checker 'grammarly 'languagetool))
;;             (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
;;               (flycheck-add-next-checker 'org-lint 'languagetool))))

;; We only limit to *scratch* buffer since we can use `grammarly' and `ltex' for directories.
(add-hook 'text-mode-hook
          (lambda ()
            (when (and (featurep 'flycheck-grammarly) (string= (buffer-name) "*scratch*"))
              (flycheck-select-checker 'grammarly))
            ;; (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
            ;;   (flycheck-add-next-checker 'grammarly 'languagetool))
            ;; (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
            ;;   (flycheck-select-checker 'languagetool))
            ))

;; `markdown-mode' is derived from `text-mode'
;; markdown-markdownlint-cli -> grammarly -> languagetool
;; (add-hook 'markdown-mode-hook
;;           (lambda()
;;             (flycheck-select-checker 'markdown-markdownlint-cli)
;;             (when (featurep 'flycheck-grammarly)
;;               ;; (make-local-variable 'flycheck-error-list-minimum-level)
;;               ;; (setq flycheck-error-list-minimum-level 'warning
;;               ;;       flycheck-navigation-minimum-level 'warning)
;;               ;; (flycheck-add-next-checker 'markdown-markdownlint-cli '(warning . grammarly) 'append)
;;               (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly))
;;             ;; (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
;;             ;;   (flycheck-add-next-checker 'grammarly 'languagetool))
;;             ;; (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
;;             ;;   (flycheck-add-next-checker 'markdown-markdownlint-cli 'languagetool))
;;             ))

;; (dolist (hook '(LaTex-mode-hook latex-mode-hook))
;;   (add-hook hook (lambda ()
;;                    (flycheck-select-checker 'tex-chktex)
;;                    (when (featurep 'flycheck-grammarly)
;;                      (flycheck-add-next-checker 'tex-chktex 'grammarly))
;;                    (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
;;                      (flycheck-add-next-checker 'grammarly 'languagetool))
;;                    (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
;;                      (flycheck-add-next-checker 'tex-chktex 'languagetool)))))

;; We need to enable lsp workspace to allow `lsp-grammarly' to work, which makes it ineffective for
;; temporary text files. However, `lsp-grammarly' supports PRO Grammarly accounts. If there are
;; failures, then try logging out of Grammarly and logging in again. But it does not solve the issue
;; for me, so I have disabled the package and I am again trying `flycheck-grammarly'.
(use-package lsp-grammarly
  :ensure keytar
  :ensure t
  :defines (lsp-grammarly-active-modes lsp-grammarly-user-words)
  :commands (lsp-grammarly--server-command lsp-grammarly--init
                                           lsp-grammarly--get-credentials lsp-grammarly--get-token
                                           lsp-grammarly--store-token lsp-grammarly--show-error
                                           lsp-grammarly--update-document-state)
  :hook
  ((text-mode markdown-mode org-mode latex-mode) . (lambda ()
                                                     (require 'lsp-grammarly)
                                                     (lsp-deferred)))
  :config
  ;; (setq lsp-grammarly-active-modes '(text-mode latex-mode
  ;;                                              LaTeX-mode org-mode markdown-mode gfm-mode)
  ;;       lsp-grammarly-user-words '(
  ;;                                  ))

  (defvar lsp-grammarly-active-modes)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection #'lsp-grammarly--server-command)
    :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-grammarly-active-modes))
    :priority -1
    :remote? t
    :add-on? t
    :server-id 'grammarly-r
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure 'grammarly-ls callback error-callback))
    :after-open-fn #'lsp-grammarly--init
    :async-request-handlers
    (ht ("$/getCredentials" #'lsp-grammarly--get-credentials)
        ("$/getToken" #'lsp-grammarly--get-token)
        ("$/storeToken" #'lsp-grammarly--store-token)
        ("$/showError" #'lsp-grammarly--show-error)
        ("$/updateDocumentState" #'lsp-grammarly--update-document-state))))

  )

(use-package lsp-ltex
  :defines (lsp-ltex-enabled lsp-ltex-check-frequency lsp-ltex-dictionary lsp-ltex-java-path)
  :commands (lsp-ltex--downloaded-extension-path lsp-ltex--execute)
  :hook
  ((text-mode markdown-mode org-mode latex-mode) . (lambda ()
                                                     (require 'lsp-ltex)
                                                     (lsp-deferred)))
  :init
  (setq lsp-ltex-check-frequency "save"
        ;; lsp-ltex-dictionary ("microbenchmarks")
        lsp-ltex-java-path "/usr/lib/jvm/java-11-openjdk-amd64"
        lsp-ltex-version "15.2.0")
  :config
  ;; https://github.com/ggbaker/doom-emacs-config/blob/f977ee6f33ef2d19b577e38a81b32af43ced6df5/config.el
  ;; Disable spell checking since we cannot get `lsp-ltex' to work with custom dict words
  (setq lsp-ltex-disabled-rules
        #s(hash-table size 30 data
                      ("en-US" ["MORFOLOGIK_RULE_EN_US"])
                      ("en-US" ["WHITESPACE_RULE"])))

  (defvar lsp-ltex-active-modes)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     "/home/swarnendu/.emacs.d/var/lsp/server/ltex-ls/latest/bin/ltex-ls")
    :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-ltex-active-modes))
    :priority -2
    :add-on? t
    :remote? t
    :server-id 'ltex-r
    :download-server-fn
    (lambda (_client _callback error-callback _update?)
      (lsp-package-ensure
       'ltex-ls
       (lambda ()
         (let ((dest (f-dirname (lsp-ltex--downloaded-extension-path))))
           (unless (lsp-ltex--execute "tar" "-xvzf" (lsp-ltex--downloaded-extension-path)
                                      "-C" dest)
             (error "Error during the unzip process: tar"))))
       error-callback))))
  )

;; `lsp-latex' provides better support for the `texlab' server compared to `lsp-tex'. On the other
;; hand, `lsp-tex' supports `digestif'. `lsp-latex' does not require `auctex'. However, the server
;; performance is very poor, so I continue to prefer `auctex'.

;; (use-package lsp-latex
;;   :disabled t
;;   :hook
;;   (latex-mode . (lambda()
;;                   (require 'lsp-latex)
;;                   (lsp-deferred)))
;;   :config
;;   (setq lsp-latex-bibtex-formatter             "latexindent"
;;         lsp-latex-latex-formatter              "latexindent"
;;         lsp-latex-bibtex-formatter-line-length sb/fill-column
;;         lsp-latex-chktex-on-open-and-save      t
;;         lsp-latex-build-is-continuous          t
;;         lsp-latex-build-on-save                t
;;         ;; Delay time in milliseconds before reporting diagnostics
;;         lsp-latex-diagnostics-delay            2000)

;;   (add-to-list 'lsp-latex-build-args "-c")
;;   (add-to-list 'lsp-latex-build-args "-pvc")

;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-tramp-connection "texlab")
;;     :major-modes '(tex-mode latex-mode LaTeX-mode bibtex-mode)
;;     :remote? t
;;     :server-id 'texlab-r)))

(use-package tex-site
  :ensure nil
  :commands tex-site)

;; Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides the tex
;; package.
(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :defines (tex-fontify-script font-latex-fontify-script
                               font-latex-fontify-sectioning
                               TeX-syntactic-comment
                               TeX-save-query LaTeX-item-indent
                               LaTeX-syntactic-comments
                               LaTeX-fill-break-at-separators)
  :functions (TeX-active-process)
  :commands (TeX-active-process TeX-save-document tex-site
                                LaTeX-mode LaTeX-math-mode
                                TeX-PDF-mode
                                TeX-source-correlate-mode
                                TeX-active-process
                                TeX-command-menu
                                TeX-revert-document-buffer
                                TeX-master-file
                                TeX-next-error)
  :hook
  (((latex-mode LaTeX-mode) . LaTeX-math-mode)
   ((latex-mode LaTeX-mode) . TeX-PDF-mode) ; Use `pdflatex'
   ((latex-mode LaTeX-mode) . TeX-source-correlate-mode)
   (LaTeX-mode . turn-on-auto-fill))
  :config
  (setq TeX-auto-save t ; Enable parse on save, stores parsed information in an `auto' directory
        TeX-auto-untabify t ; Remove all tabs before saving
        TeX-clean-confirm nil
        ;; Automatically insert braces after typing ^ and _ in math mode
        TeX-electric-sub-and-superscript t
        TeX-electric-math t ; Inserting $ completes the math mode and positions the cursor
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
  :bind
  ("C-c x q" . TeX-insert-quote))

(use-package bibtex
  :ensure nil
  :hook
  ((bibtex-mode . turn-on-auto-revert-mode)
   (bibtex-mode . lsp-deferred))
  :config
  (setq bibtex-align-at-equal-sign     t
        bibtex-maintain-sorted-entries t))

(use-package ivy-bibtex
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

;; Reftex is useful to view ToC even with LSP support
;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
(use-package reftex
  :ensure nil
  :commands (reftex-get-bibfile-list bibtex-parse-keys
                                     reftex-mode
                                     reftex-toc-rescan
                                     reftex-toc-Rescan
                                     reftex-default-bibliography)
  ;; :diminish
  :hook ((LaTeX-mode latex-mode) . reftex-mode)
  :bind
  (("C-c ["   . reftex-citation)
   ("C-c )"   . reftex-reference)
   ("C-c ("   . reftex-label)
   ("C-c ="   . reftex-toc)
   ("C-c C-j" . reftex-toc))
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
      Returns a string with text properties (as expected by read-file-name) or
empty string if no file can be found"
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
  :config
  (setq reftex-enable-partial-scans t
        reftex-highlight-selection 'both
        reftex-plug-into-AUCTeX t
        reftex-save-parse-info t
        reftex-toc-follow-mode t ; Other buffer follows the point in toc buffer
        reftex-use-multiple-selection-buffers t
        ;; Make the toc display with a vertical split, since it is easy to read long lines
        reftex-toc-split-windows-horizontally nil)

  (sb/reftex-try-add-all-bibitems-from-bibtex)
  ;; Rescan the entire document, not only the current file (`reftex-toc-rescan')
  (add-hook 'reftex-toc-mode-hook #'reftex-toc-Rescan))

;; (use-package bib-cite
;;   :ensure nil
;;   :disabled t
;;   :diminish bib-cite-minor-mode
;;   :commands bib-cite-minor-mode
;;   :hook ((LaTeX-mode latex-mode) . bib-cite-minor-mode )
;;   :config (setq bib-cite-use-reftex-view-crossref t)
;;   :bind (:map bib-cite-minor-mode-map
;;               ("C-c b"   . nil) ; We use `C-c b' for `comment-box'
;;               ("C-c l a" . bib-apropos)
;;               ("C-c l b" . bib-make-bibliography)
;;               ("C-c l d" . bib-display)
;;               ("C-c l t" . bib-etags)
;;               ("C-c l f" . bib-find)
;;               ("C-c l n" . bib-find-next)
;;               ("C-c l h" . bib-highlight-mouse)))

;; We can disable this once `lsp-latex-build' works well
(use-package auctex-latexmk
  :after tex-mode
  :demand t
  :commands (auctex-latexmk-setup auctex-latexmk)
  :config
  ;; Pass the `-pdf' flag when `TeX-PDF-mode' is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup)
  (setq TeX-command-default "LatexMk"))

;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
(declare-function TeX-active-process "tex.el")

(defun sb/save-buffer-and-run-latexmk ()
  "Save the current buffer and run LaTeXMk also."
  (interactive)
  (require 'tex)
  (require 'tex-buf)
  ;; Kill any active compilation process
  (let ((process (TeX-active-process)))
    (if process (delete-process process)))
  (let ((TeX-save-query nil))
    (TeX-save-document ""))
  (TeX-command-menu "LaTeXMk"))

(declare-function TeX-run-TeX "tex")

(defun sb/latex-compile-open-pdf ()
  "Save the current buffer, run LaTeXMk, and switch to the PDF
after a successful compilation."
  (interactive)
  (let ((TeX-save-query nil)
        (process (TeX-active-process))
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (if process (delete-process process))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk" "latexmk -pdf" master-file)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (TeX-next-error t)
      (progn
        (minibuffer-message "LaTeXMk done")
        (find-file (concat (file-name-directory (concat master-file ".tex"))
                           (concat master-file ".pdf")))))))

;; (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
;;   (add-hook hook
;;             (lambda ()
;;               (add-hook 'after-save-hook
;;                         (lambda ()
;;                           (sb/save-buffer-and-run-latexmk)) nil t))))

(with-eval-after-load "tex-mode"
  (defvar latex-mode-map)
  (bind-key "C-x C-s" #'sb/latex-compile-open-pdf latex-mode-map))

(with-eval-after-load "latex"
  (defvar LaTeX-mode-map)
  (bind-key "C-x C-s" #'sb/latex-compile-open-pdf LaTeX-mode-map))

(use-package math-preview
  :commands (math-preview-all math-preview-at-point math-preview-region)
  :config
  (setq math-preview-command (expand-file-name "node_modules/.bin/math-preview"
                                               sb/user-tmp)))

(use-package json-mode
  :ensure t
  :ensure json-reformat
  :ensure json-snatcher
  :commands (json-mode jsonc-mode json-mode-beautify)
  :mode
  (("\\.json\\'"                  . json-mode)
   ("pyrightconfig.json"          . jsonc-mode)
   (".*/vscode/settings.json$"    . jsonc-mode)
   (".*/\\.vscode/settings.json$" . jsonc-mode)
   ("User/settings.json$"         . jsonc-mode))
  :hook
  ((json-mode jsonc-mode) . (lambda ()
                              (make-local-variable 'js-indent-level)
                              (setq js-indent-level 2)
                              (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("vscode-json-languageserver" "--stdio"))
    :major-modes '(json-mode jsonc-mode)
    :remote? t
    :server-id 'jsonls-r)))

(use-package json-reformat
  :after (:any json-mode jsonc-mode)
  :demand t
  :config (setq json-reformat:indent-width 2
                js-indent-level 2))

;; (use-package json-snatcher
;;   :after (:any json-mode jsonc-mode)
;;   :demand t)

(use-package bazel
  :commands (bazel-mode bazelrc-mode)
  :hook (bazel-mode . flycheck-mode))

(use-package protobuf-mode
  :commands protobuf-mode
  :mode "\\.proto$"
  :hook (protobuf-mode . flycheck-mode))

(use-package mlir-mode
  :ensure nil
  :commands mlir-mode
  :load-path "extras"
  :mode "\\.mlir\\'")

(use-package clang-format
  :after (mlir-mode)
  :commands (clang-format clang-format-buffer clang-format-region)
  :config (setq clang-format-style "file"))

(use-package clang-format+
  :ensure clang-format
  :ensure t
  :defines clang-format+-always-enable
  :hook (mlir-mode . clang-format+-mode)
  :config (setq clang-format+-always-enable t))

;; Use for major modes which do not provide a formatter. `aphelia' allows for formatting via a
;; background process but does not support Tramp and supports fewer formatters.
(use-package format-all
  :commands (format-all-ensure-formatter format-all-buffer)
  :diminish
  :preface
  (defun sb/enable-format-all ()
    "Delay enabling format-all to avoid slowing down Emacs startup."
    (dolist (hook '(bazel-mode-hook LaTeX-mode-hook web-mode-hook markdown-mode-hook))
      (add-hook hook #'format-all-mode))
    (add-hook 'format-all-mode-hook #'format-all-ensure-formatter))
  ;; :init (run-with-idle-timer 2 nil #'sb/enable-format-all)
  :diminish
  :hook
  ((format-all-mode . format-all-ensure-formatter)
   ((bazel-mode LaTeX-mode web-mode markdown-mode) . format-all-mode)))

;; Tree-sitter provides advanced syntax highlighting features
(use-package tree-sitter
  :ensure tree-sitter-langs
  :functions tree-sitter-hl-mode
  :commands (global-tree-sitter-mode tree-sitter-hl-mode)
  :diminish tree-sitter-mode
  :preface
  (defun sb/enable-tree-sitter ()
    "Delay enabling tree-sitter to avoid slowing down Emacs startup."
    (dolist (hook '(sh-mode-hook c-mode-hook c++-mode-hook
                                 css-mode-hook html-mode-hook
                                 java-mode-hook json-mode-hook
                                 jsonc-mode-hook php-mode-hook
                                 python-mode-hook))
      (add-hook hook (lambda ()
                       (require 'tree-sitter-langs)
                       (global-tree-sitter-mode 1)))))
  ;; :init (run-with-idle-timer 2 nil #'sb/enable-tree-sitter)
  :hook (after-init . sb/enable-tree-sitter)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package editorconfig
  :if (executable-find "editorconfig")
  :commands editorconfig-mode)

;; Hooks into to `find-file-hook' to add all visited files and directories to `fasd'
(use-package fasd
  :defines fasd-enable-initial-prompt
  :if (executable-find "fasd")
  :commands (global-fasd-mode fasd-find-file)
  ;; :init (run-with-idle-timer 3 nil #'global-fasd-mode)
  :config (setq fasd-enable-initial-prompt nil)
  :bind* ("C-c /" . fasd-find-file))

(use-package dotenv-mode
  :mode "\\.env\\'")

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
  (add-hook 'compilation-filter-hook #'sanityinc/colourise-compilation-buffer))

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
;; `(setq company-backends '(company-xxx company-yyy company-zzz))'
;; Merge completions of all the backends
;; `(setq company-backends '((company-xxx company-yyy company-zzz)))'
;; Merge completions of all the backends, give priority to `company-xxx'
;; `(setq company-backends '((company-xxx :separate company-yyy company-zzz)))'
;; Company does not support grouping of entirely arbitrary backends, they need to be compatible in
;; what `prefix' returns.

;; If the group contains keyword `:with', the backends listed after this keyword are ignored for
;; the purpose of the `prefix' command. If the group contains keyword `:separate', the candidates
;; that come from different backends are sorted separately in the combined list.

;; LATER: I do not understand the difference between the following two, and the explanation.
;; `(add-to-list 'company-backends '(company-capf company-dabbrev))'
;; `(add-to-list 'company-backends '(company-capf :with company-dabbrev))'

(progn
  (defun sb/company-text-mode ()
    "Add backends for text completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    ;; Slightly larger value to have more precise matches and so that the popup does not block
    (setq-local company-minimum-prefix-length 3
                company-transformers '(delete-dups))
    (set (make-local-variable 'company-backends)
         '(company-files
           company-dabbrev
           company-ispell
           company-abbrev)))

  (dolist (hook '(text-mode-hook)) ; Extends to derived modes like `markdown-mode' and `org-mode'
    (add-hook hook (lambda ()
                     (sb/company-text-mode)
                     (company-fuzzy-mode 1)
                     (diminish 'company-fuzzy-mode)))))

(progn
  (defun sb/company-xml-mode ()
    "Add backends for completion with company."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)
    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev-code
                             company-dabbrev)))

  (dolist (hook '(nxml-mode-hook))
    (add-hook hook (lambda ()
                     (sb/company-xml-mode)
                     (company-fuzzy-mode 1)
                     (diminish 'company-fuzzy-mode)))))

(progn
  (defun sb/company-prog-mode ()
    "Add backends for program completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)

    ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
    (setq company-backends '(company-capf
                             company-yasnippet
                             company-files
                             company-dabbrev-code
                             company-dabbrev)))

  (add-hook 'prog-mode-hook (lambda ()
                              (sb/company-prog-mode)
                              (company-fuzzy-mode 1)
                              (diminish 'company-fuzzy-mode))))

;; (progn
;;   (defun sb/company-java-mode ()
;;     "Add backends for Java completion in company mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 3)
;;     (make-local-variable 'company-backends)
;;     (setq company-backends '((company-capf :with company-yasnippet)
;;                              (company-files : with company-yasnippet)
;;                              (company-dabbrev-code :with company-yasnippet)
;;                              company-dabbrev)))

;;   (add-hook 'java-mode-hook #'sb/company-java-mode))

;; https://emacs.stackexchange.com/questions/19072/company-completion-very-slow
;; `company-clang' is slow
;; (progn
;;   (defun sb/company-c-mode ()
;;     "Add backends for C/C++ completion in company mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 2)
;;     (make-local-variable 'company-backends)
;;     (setq company-backends '(company-capf
;;                              company-files
;;                              (company-dabbrev-code :with company-yasnippet)
;;                              (company-dabbrev
;;                               company-ispell))))

;;   (add-hook 'c-mode-common-hook #'sb/company-c-mode))

(progn
  (defun sb/company-sh-mode ()
    "Add backends for shell script completion in company mode."
    (use-package company-shell
      :defines company-shell-delete-duplictes
      :commands (company-shell company-shell-env company-fish-shell)
      :config (setq company-shell-delete-duplictes t))

    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    (setq company-backends '(company-capf
                             company-shell
                             company-shell-env
                             company-dabbrev-code
                             company-yasnippet
                             company-files
                             company-dabbrev)))

  (add-hook 'sh-mode-hook (lambda ()
                            (sb/company-sh-mode)
                            (company-fuzzy-mode 1)
                            (diminish 'company-fuzzy-mode)))

  (defun sb/company-fish-mode ()
    "Add backends for fish shell script completion in company mode."
    (use-package company-shell
      :defines company-shell-delete-duplictes
      :commands (company-shell company-shell-env company-fish-shell)
      :config (setq company-shell-delete-duplictes t))

    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)
    (setq company-backends '(company-capf
                             company-shell
                             company-shell-env
                             company-fish-shell
                             company-dabbrev-code
                             company-yasnippet
                             company-files
                             company-dabbrev)))

  (add-hook 'fish-mode-hook (lambda ()
                              (sb/company-fish-mode)
                              (company-fuzzy-mode 1)
                              (diminish 'company-fuzzy-mode))))

;; (progn
;;   (defun sb/company-elisp-mode ()
;;     "Set up company for elisp mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 2)
;;     (make-local-variable 'company-backends)
;;     (setq company-backends '(company-capf
;;                              company-yasnippet
;;                              company-files
;;                              company-dabbrev-code
;;                              company-dabbrev
;;                              company-ispell)))

;;   (add-hook 'emacs-lisp-mode-hook #'sb/company-elisp-mode))

;; (progn
;;   (defun sb/company-python-mode ()
;;     "Add backends for Python completion in company mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 3)
;;     (make-local-variable 'company-backends)
;;     ;; `company-dabbrev-code' is useful for variable names
;;     (setq company-backends '(company-capf
;;                              company-yasnippet
;;                              company-files
;;                              company-dabbrev-code
;;                              company-dabbrev)))

;;   (add-hook 'python-mode-hook #'sb/company-python-mode))

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
      :demand t
      :commands company-bibtex)

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)

    ;; `company-reftex' should be considerably more powerful than `company-auctex' backends for
    ;; labels and citations.

    (setq company-backends '(company-capf
                             company-files
                             company-reftex-citations
                             company-reftex-labels
                             company-auctex-environments
                             company-auctex-macros
                             company-latex-commands
                             company-math-symbols-latex
                             company-math-symbols-unicode
                             company-auctex-symbols
                             company-auctex-bibs
                             company-auctex-labels
                             company-bibtex
                             company-dabbrev
                             company-ispell)))

  (dolist (hook '(latex-mode-hook LaTeX-mode-hook TeX-mode-hook tex-mode-hook))
    (add-hook hook (lambda ()
                     (sb/company-latex-mode)
                     (company-fuzzy-mode 1)
                     (diminish 'company-fuzzy-mode)))))

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
    (add-hook hook (lambda ()
                     (sb/company-web-mode)
                     (company-fuzzy-mode 1)
                     (diminish 'company-fuzzy-mode)))))

;; Use "emacsclient -c -nw" to start a new frame.
(use-package server
  :disabled t
  :unless (string-equal "root" (getenv "USER")) ; Only start server if not root
  :commands server-running-p
  :init
  (unless (server-running-p)
    (server-start)))

;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(setenv "SHELL" shell-file-name) ; Recommended to connect with bash

;; `vterm' provides better performance than `eshell', `shell', and `(ansi-)term'. The advantage of
;; the later modules are they are built-in to Emacs. The package requires shell-side configuration.
;; Check https://github.com/akermu/emacs-libvterm.
(use-package vterm
  :config
  (setq vterm-always-compile-module t
        vterm-max-scrollback 5000
        vterm-term-environment-variable "xterm-24bit")

  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
              (buffer-face-mode t))))

(use-package vterm-toggle
  :commands vterm-toggle
  :bind ("C-`" . vterm-toggle))

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
  :type  '(repeat string)
  :group 'sb/emacs)

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
                                           compilation-mode
                                           flycheck-verify-mode
                                           ibuffer-mode)
  "List of major modes to skip over when calling `change-buffer'."
  :type  '(repeat string)
  :group 'sb/emacs)

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
  (interactive)
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

(defun sb/open-local-file-projectile (directory)
  "Open projectile file within DIRECTORY.
Specify by the keyword projectile-default-file define in `dir-locals-file'"
  (let ((default-file
          (f-join directory
                  (nth 1
                       (car (-tree-map (lambda (node)
                                         (when (eq (car node)
                                                   'dotemacs-projectile-default-file)
                                           (format "%s" (cdr node))))
                                       (dir-locals-get-class-variables (dir-locals-read-from-dir
                                                                        directory))))))))
    (if (f-exists? default-file)
        (counsel-find-file default-file)
      (message "The file %s doesn't exist in the select project" default-file))))

(defun sb/open-project-default-file1 (filepath)
  "Open projectile file with FILEPATH.
Specify by the keyword projectile-default-file define in `dir-locals-file'."
  (let ((liststring (with-temp-buffer
                      (insert-file-contents filepath)
                      (split-string (buffer-string) "\n"))))
    (mapcar (lambda (str)
              (when (cl-search "dotemacs-projectile-default-file" str)
                (let ((x (substring str (+
                                         13 (length "dotemacs-projectile-default-file")) (length
                                         str))))
                  (let ((default-file (expand-file-name (substring
                                                         x 1 -2) (projectile-project-root))))
                    (when (f-exists? default-file)
                      (let ((new-buffer (get-buffer-create default-file)))
                        (switch-to-buffer new-buffer)
                        (insert-file-contents default-file)))))))
            liststring)))
;; (sb/open-project-default-file1 "/home/swarnendu/.emacs.d/.dir-locals.el")

(defun sb/open-project-default-file2 ()
  "Open projectile file with FILEPATH.
Specify by the keyword projectile-default-file define in `dir-locals-file'."
  (interactive)
  (let ((mylist (dir-locals-get-class-variables (dir-locals-read-from-dir
                                                 (projectile-project-root)))))
    (mapcar (lambda (node)
              (when (eq (car node) nil)
                (let ((nillist (cdr node)))
                  (mapcar (lambda (elem)
                            (when (eq (car elem) 'dotemacs-projectile-default-file)
                              (let ((default-file (expand-file-name (cdr elem)
                                                                    (projectile-project-root))))
                                (when (f-exists? default-file)
                                  ;; (let ((new-buffer (get-buffer-create default-file)))
                                  ;;   (switch-to-buffer new-buffer)
                                  ;;   (insert-file-contents default-file))
                                  (find-file default-file)))))
                          nillist))))
            mylist)))

;; (sb/open-project-default-file2)

;; (with-eval-after-load "counsel-projectile"
;;   (add-to-list 'counsel-projectile-action '("d"
;;     sb/open-project-default-file2 "open default file") t))

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))

(bind-keys
 ("RET"       . newline-and-indent)
 ("C-l"       . goto-line)
 ("C-c z"     . repeat)
 ("C-z"       . undo)
 ;; Conflicts with Gnome window manager keybindings
 ;; ("<f11>"     . delete-other-windows)
 ("C-x k"     . kill-this-buffer)
 ("M-<left>"  . previous-buffer)
 ("C-S-<tab>" . previous-buffer)
 ("M-<right>" . next-buffer)
 ("C-<tab>"   . next-buffer)
 ("C-c d f"   . auto-fill-mode)
 ("M-c"       . capitalize-dwim)
 ("M-u"       . upcase-dwim)
 ("M-l"       . downcase-dwim)
 ("<f7>"      . previous-error)
 ("<f8>"      . next-error)
 ;; The default keybinding `C-S-backspace' does not work with TUI
 ("M-k"       . kill-whole-line))

;; In a line with comments, `C-u M-;' removes the comments altogether. That means deleting the
;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
(bind-keys*
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box)
 ("C-s"   . save-buffer)
 ("C-S-s" . sb/save-all-buffers))

(unbind-key "C-]") ; Bound to `abort-recursive-edit'

(unbind-key "C-x s") ; Bound to `save-some-buffers'
(bind-key   "C-x s" #'sb/switch-to-scratch)
(bind-key   "C-x j" #'sb/counsel-all-files-recursively)

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
  ;; :init (run-with-idle-timer 3 nil #'which-key-mode)
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)

  ;; Allow "C-h" to trigger `which-key' before it is done automatically
  (setq which-key-show-early-on-C-h t
        which-key-sort-order 'which-key-key-order-alpha))

(use-package which-key-posframe
  :commands which-key-posframe-mode
  :hook (which-key-mode . which-key-posframe-mode)
  :config
  ;; Modify the posframe background if it has a low contrast
  ;; (set-face-attribute 'which-key-posframe nil :background "floralwhite" :foreground "black")

  ;; Thicker border makes the posframe easier to distinguish
  (setq which-key-posframe-border-width 4)

  ;; Positioning the frame at the top obstructs the view to a lesser degree
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))

;; Hydras

;; https://github.com/abo-abo/hydra
;; `:exit nil' means the hydra state will continue, `:exit t' will quit the hydra. `:color red'
;; means continue the hydra on a valid key but stop when a foreign key has been pressed. `:color
;; blue' means exit.

(setq lv-use-separator t)

;; (declare-function spell-fu-goto-next-error "spell-fu")
;; (declare-function spell-fu-goto-previous-error "spell-fu")

(defhydra sb/hydra-spelling (:color amaranth)
  "
  ^Spell Check^          ^Errors^            ^Spell fu^
  ^────────^──────────^──────^────────────^───────^─────────────^
  _c_ ispell            _<_ previous         _p_ previous error
  _f_ flyspell          _>_ next             _n_ next error
  _q_ quit              ^^                   _a_ add word
  "
  ("c" ispell)
  ("f" flyspell-buffer)

  ("<" flyspell-correct-previous)
  (">" flyspell-correct-next)

  ("n" spell-fu-goto-next-error)
  ("p" spell-fu-goto-previous-error)
  ("a" spell-fu-word-add)

  ("q" nil "quit"))

(defhydra sb/hydra-text-scale-zoom (:color amaranth)
  "Zoom the text"
  ("i" default-text-scale-increase "in")
  ("o" default-text-scale-decrease "out")

  ("q" nil "quit"))

;; (defhydra sb/hydra-error (:color amaranth)
;;   "Navigate errors"
;;   ("h" first-error "first")
;;   ("j" next-error "next")
;;   ("k" previous-error "prev")
;;   ("v" recenter-top-bottom "recenter")
;;   ("q" nil "quit"))

;; https://github.com/abo-abo/hydra/wiki/avy
(defhydra sb/hydra-avy (:color red)
  "
  Line^^       Region^^        Goto
----------------------------------q------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"

  ("y" avy-copy-line)
  ("m" avy-move-line)
  ("k" avy-kill-whole-line)

  ("Y" avy-copy-region)
  ("M" avy-move-region)
  ("K" avy-kill-region)

  ("c" avy-goto-char-timer)
  ("w" avy-goto-word-1)
  ("l" avy-goto-line)

  ("C" avy-goto-char)
  ("W" avy-goto-word-0)
  ("L" avy-goto-end-of-line))

(defhydra sb/hydra-projectile (:color teal :hint nil global-map "C-c p")
  "
     PROJECTILE: %(projectile-project-root)

  Project                    Find File             Buffers                  Misc
-----------------------------------------------------------------------------------------------------------
  _p_: switch project        _f_: find file        _i_: ibuffer           _c_: invalidate cache
  _s_: switch project        _F_: find file dwim   _b_: switch to buffer  _z_: cache current
  _a_: add known project     _d_: find directory   _r_: recent file       _g_: find tag
  _x_: remove known project  _D_: find file cwd    _k_: kill all buffers  _o_: multi-occur
  _X_: clean known project   ^^                    ^^                     _m_: compile
"
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("a"   projectile-add-known-project)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)

  ("f"   projectile-find-file)
  ("F"   projectile-find-file-dwim)
  ("d"   projectile-find-dir)
  ("D"   projectile-find-file-in-directory)

  ("i"   projectile-ibuffer)
  ("b"   projectile-switch-to-buffer)
  ("r"   projectile-recentf)
  ("k"   projectile-kill-buffers)

  ("c"   projectile-invalidate-cache)
  ("z"   projectile-cache-current-file)
  ("g"   projectile-find-tag)
  ("o"   projectile-multi-occur)
  ("m"   projectile-compile)
  ("q"   nil "cancel"))

(defhydra sb/hydra-move-text ()
  "Move text"
  ("u" move-text-up "up")
  ("d" move-text-down "down")
  ("q"   nil "cancel"))

;; (declare-function flycheck-verify-setup "flycheck")
;; (declare-function flycheck-previous-error "flycheck")
;; (declare-function flycheck-next-error "flycheck")
;; (declare-function flycheck-list-errors "flycheck")
;; (declare-function flycheck-select-checker "flycheck")
;; (declare-function flycheck-describe-checker "flycheck")
;; (declare-function flycheck-disable-checker "flycheck")
;; (declare-function flycheck-buffer "flycheck")

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

(defhydra sb/hydra-python-indent ()
  "Adjust Python indentation."
  (">" python-indent-shift-right "right")
  ("<" python-indent-shift-left "left"))

(with-eval-after-load "python-mode"
  (bind-key "C-c" #'sb/hydra-python-indent/body python-mode-map))

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

(defhydra sb/hydra-multiple-cursors (:hint nil)
  "
                                              ^Up^            ^Down^        ^Other^
                                              ----------------------------------------------
                                              [_p_]   Next    [_n_]   Next    [_l_] Edit lines
                                              [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
                                              [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
                                              ^ ^             ^ ^             [_q_] Quit
                                              "
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))

(defhydra sb/hydra-smartparens (:hint nil)
  "
                                              Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
                                              ------------------------------------------------------------------------------------------------------------------------
                                              [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
                                              [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
                                              [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
                                              [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)

  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)

  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))

(defhydra sb/hydra-lsp (:exit t :hint nil)
  "
  Buffer^^               Server^^                   Symbol
  -----------------------------------------------------------------------------------------------------------
  [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
  [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
  [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"

  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown))

(defhydra sb/hydra-markdown-mode (:hint nil)
  "
  Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

  Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

  Lists             C-c C-x    _m_: insert item

  Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

  Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference

  "

  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue))

(bind-key "C-c h a" #'sb/hydra-avy/body)
(bind-key "C-c h d" #'sb/hydra-markdown-mode/body)
;; (bind-key "C-c h e" #'sb/hydra-error/body)
(bind-key "C-c h f" #'sb/hydra-flycheck/body)
(bind-key "C-c h g" #'sb/smerge-hydra/body)
(bind-key "C-c h j" #'sb/hydra-projectile/body)
(bind-key "C-c h l" #'sb/hydra-lsp/body)
(bind-key "C-c h m" #'sb/hydra-multiple-cursors/body)
(bind-key "C-c h p" #'sb/hydra-smartparens/body)
(bind-key "C-c h s" #'sb/hydra-spelling/body)
(bind-key "C-c h t" #'sb/hydra-move-text/body)
(bind-key "C-c h z" #'sb/hydra-text-scale-zoom/body)

;; Mark safe variables

;; (put 'bibtex-completion-bibliography          'safe-local-variable #'listp)
;; (put 'company-bibtex-bibliography             'safe-local-variable #'listp)
;; (put 'company-clang-arguments                 'safe-local-variable #'listp)
;; (put 'counsel-find-file-ignore-regexp         'safe-local-variable #'stringp)
;; (put 'flycheck-checker                        'safe-local-variable #'listp)
;; (put 'flycheck-clang-include-path             'safe-local-variable #'listp)
;; (put 'flycheck-gcc-include-path               'safe-local-variable #'listp)
;; (put 'flycheck-python-pylint-executable       'safe-local-variable #'stringp)
;; (put 'lsp-clients-clangd-args                 'safe-local-variable #'listp)
;; (put 'lsp-latex-root-directory                'safe-local-variable #'stringp)
;; (put 'lsp-pyright-extra-paths                 'safe-local-variable #'listp)
;; (put 'projectile-enable-caching               'safe-local-variable #'stringp)
;; (put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
;; (put 'projectile-project-root                 'safe-local-variable #'stringp)
;; (put 'pyvenv-activate                         'safe-local-variable #'stringp)
;; (put 'reftex-default-bibliography             'safe-local-variable #'listp)
;; (put 'tags-table-list                         'safe-local-variable #'listp)

;; https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs is ready in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

;;; init-use-package.el ends here
