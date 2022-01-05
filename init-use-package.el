;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; TODO:
;; Fix ordering of company completions, prefix match seems to be good

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
  'modus-operandi
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "leuven"          leuven)
           (const :tag "zenburn"         zenburn)
           (const :tag "doom-molokai"    doom-molokai)
           (const :tag "doom-gruvbox"    doom-gruvbox)
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
  (package-refresh-contents)
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
  :commands (f-exists? f-join))

(use-package s
  :commands s-starts-with? s-ends-with?)

(use-package dash
  :commands (-contains? -tree-map))

;; TODO: Learn how to use hydras better
(use-package hydra
  :commands (hydra-default-pre hydra-keyboard-quit defhydra
                               hydra-show-hint hydra-set-transient-map
                               hydra--call-interactively-remap-maybe))

(use-package no-littering
  :demand t)

(defcustom sb/custom-file
  (no-littering-expand-etc-file-name "custom.el")
  "File to write Emacs customizations."
  :type  'string
  :group 'sb/emacs)

(setq custom-file sb/custom-file)
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(defcustom sb/private-file
  (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type  'string
  :group 'sb/emacs)

(when (file-exists-p sb/private-file)
  (load sb/private-file 'noerror))

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
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system  'utf-8)

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
  :init (run-with-idle-timer 2 nil #'global-auto-revert-mode)
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
  :init (run-with-idle-timer 2 nil #'savehist-mode)
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
  :init (run-with-idle-timer 2 nil #'global-so-long-mode)
  :config (setq so-long-threshold 500))


;; Mark safe variables

(put 'bibtex-completion-bibliography          'safe-local-variable #'listp)
(put 'company-bibtex-bibliography             'safe-local-variable #'listp)
(put 'company-clang-arguments                 'safe-local-variable #'listp)
(put 'counsel-find-file-ignore-regexp         'safe-local-variable #'stringp)
(put 'flycheck-checker                        'safe-local-variable #'listp)
(put 'flycheck-clang-include-path             'safe-local-variable #'listp)
(put 'flycheck-gcc-include-path               'safe-local-variable #'listp)
(put 'flycheck-python-pylint-executable       'safe-local-variable #'stringp)
(put 'lsp-clients-clangd-args                 'safe-local-variable #'listp)
(put 'lsp-latex-root-directory                'safe-local-variable #'stringp)
(put 'lsp-pyright-extra-paths                 'safe-local-variable #'listp)
(put 'projectile-enable-caching               'safe-local-variable #'stringp)
(put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
(put 'projectile-project-root                 'safe-local-variable #'stringp)
(put 'pyvenv-activate                         'safe-local-variable #'stringp)
(put 'reftex-default-bibliography             'safe-local-variable #'listp)
(put 'tags-table-list                         'safe-local-variable #'listp)

;; https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs is ready in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

;;; init-use-package.el ends here
