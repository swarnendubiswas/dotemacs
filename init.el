;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;; To evaluate an Sexp, just go to the end of the sexp and type `C-x C-e', instead of evaluating the
;; whole buffer Use `C-M-x' to evaluate the current top-level s-expression. Use `M-:' to evaluate
;; any Emacs Lisp expression and print the result.

;; Init file should not ideally contain calls to `load' or `require', since they cause eager loading
;; and are expensive, a cheaper alternative is to use `autoload'.

;; Only an interactive function can be invoked with `M-x' or a key binding.

;; When defining a lambda expression that is to be used as an anonymous function, you can in
;; principle use any method to construct the list. But typically you should use the lambda macro, or
;; the function special form, or the #' read syntax which is a short-hand for using function.

;; Quoting a lambda form means the anonymous function is not byte-compiled. The following forms are
;; all equivalent: `(lambda (x) (* x x))' `(function (lambda (x) (* x x)))' `#'(lambda (x) (* x x))'

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
;; https://emacs.stackexchange.com/questions/27007/backward-quote-what-does-it-mean-in-elisp

;; Backquote constructs allow you to quote a list, but selectively evaluate elements of that list.
;; `(1 2 (3 ,(+ 4 5))) => (1 2 (3 9))

;; A local variable specification takes the following form:
;; -*- mode: MODENAME; VAR: VALUE; ... -*-

;; Hooks in the `:hook' section, run in reverse order. Example:
;; (use-package package-name
;;   :hook
;;   (x-mode . last)
;;   (x-mode . second)
;;   (x-mode . first))

;; `emacs-startup-hook' runs later than the `after-init-hook'

;; Good articles and reference configurations
;; https://protesilaos.com/dotemacs
;; https://github.com/CSRaghunandan/.emacs.d
;; https://github.com/purcell/emacs.d
;; https://github.com/MatthewZMD/.emacs.d
;; https://github.com/redguardtoo/emacs.d
;; https://github.com/jwiegley/dot-emacs
;; https://github.com/d12frosted/environment/tree/master/emacs
;; https://github.com/raxod502/radian/tree/develop/emacs
;; https://github.com/dholm/dotemacs
;; https://tychoish.com/post/towards-faster-emacs-start-times/

;;; Code:

;; GC may happen after this many bytes are allocated since last GC If you experience freezing,
;; decrease this. If you experience stuttering, increase this.
(defconst dotemacs-1MB (* 1 1000 1000))
(defconst dotemacs-4MB (* 4 1000 1000))
(defconst dotemacs-50MB (* 50 1000 1000))
(defconst dotemacs-64MB (* 64 1000 1000))
(defconst dotemacs-100MB (* 100 1000 1000))
(defconst dotemacs-128MB (* 128 1000 1000))
(defconst dotemacs-200MB (* 200 1000 1000))

;; Ideally, we would have reset `gc-cons-threshold' to its default value otherwise there can be
;; large pause times whenever GC eventually happens. But lsp suggests increasing the limit
;; permanently.

(defun sb/defer-garbage-collection ()
  "Defer garbage collection."
  (setq gc-cons-percentage 0.6
        gc-cons-threshold dotemacs-64MB))

(defun sb/restore-garbage-collection ()
  "Restore garbage collection."
  (setq gc-cons-percentage 0.1
        gc-cons-threshold dotemacs-4MB))

(add-hook 'emacs-startup-hook #'sb/restore-garbage-collection)
(add-hook 'minibuffer-setup-hook #'sb/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook #'sb/restore-garbage-collection)

(defgroup dotemacs nil
  "Personal configuration for dotemacs."
  :group 'local)

(defcustom dotemacs-extras-directory (expand-file-name "extras"
                                                       user-emacs-directory)
  "Path for third-party packages and files."
  :type 'string
  :group 'dotemacs)

;; (defcustom dotemacs-temp-directory (expand-file-name "tmp"
;;                                                      user-emacs-directory)
;;   "Storage location for various configuration files."
;;   :type 'string
;;   :group 'dotemacs)

;; (unless (file-exists-p dotemacs-temp-directory)
;;   (make-directory dotemacs-temp-directory))

(defcustom dotemacs-theme
  'modus-operandi
  "Specify which Emacs theme to use."
  :type '(radio
          (const :tag "eclipse" eclipse)
          (const :tag "leuven" leuven)
          (const :tag "solarized-light" solarized-light)
          (const :tag "spacemacs-light" spacemacs-light)
          (const :tag "solarized-dark" solarized-dark)
          (const :tag "tangotango" tangotango)
          (const :tag "zenburn" zenburn)
          (const :tag "doom-molokai" doom-molokai)
          (const :tag "monokai" monokai)
          (const :tag "modus-operandi" modus-operandi)
          (const :tag "modus-vivendi" modus-vivendi)
          (const :tag "customized" sb/default)
          (const :tag "none" none)
          )
  :group 'dotemacs)

(defcustom dotemacs-modeline-theme
  'doom-modeline
  "Specify the mode-line theme to use."
  :type '(radio
          (const :tag "powerline" powerline)
          (const :tag "smart-mode-line" sml)
          (const :tag "spaceline" spaceline)
          (const :tag "airline" airline)
          (const :tag "doom-modeline" doom-modeline)
          (const :tag "awesome-tray" awesome-tray)
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-window-split
  'horizontal
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type '(radio
          ;; Split into two windows one above the other
          (const :tag "vertical" vertical) ; split-window-below
          ;; Split into two side-by-side windows
          (const :tag "horizontal" horizontal)) ; split-window-right
  :group 'dotemacs)

;; Large values make reading difficult when the window is split
(defcustom dotemacs-fill-column 100
  "Column beyond which lines should not extend."
  :type 'number
  :group 'dotemacs)

(defcustom dotemacs-delete-trailing-whitespace-p
  nil
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.
Sometimes we do not want to unnecessarily add differences due to
whitespaces."
  :type 'boolean
  :group 'dotemacs)

(defcustom dotemacs-tags-scheme
  'ctags
  "Choose whether to use gtags or ctags."
  :type '(radio
          (const :tag "ctags" ctags)
          (const :tag "gtags" gtags)
          (const :tag "none" none))
  :group 'dotemacs)

(defcustom dotemacs-ctags-path
  "/usr/local/bin/ctags"
  "Absolute path to Universal Ctags executable."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-gtags-path
  "/usr/local/bin/gtags"
  "Absolute path to GNU Global executable."
  :type 'string
  :group 'dotemacs)

;; Keep enabled till the configuration is stable
(defcustom dotemacs-debug-init-file
  nil
  "Enable features to debug errors during Emacs initialization."
  :type 'boolean
  :group 'dotemacs)

(defconst dotemacs-user-home
  (getenv "HOME")
  "User HOME directory.")

(defconst dotemacs-user-tmp
  (expand-file-name "tmp" dotemacs-user-home)
  "User temp directory.")

(defcustom dotemacs-textlint-home
  (expand-file-name "textlint-workspace" dotemacs-user-tmp)
  "Absolute path to textlint workspace."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-python-langserver
  'pyright
  "Choose the Python Language Server implementation."
  :type '(radio
          (const :tag "pyls" pyls)
          (const :tag "mspyls" mspyls)
          (const :tag "pyright" pyright)
          (const :tag "jedi" jedi)
          (const :tag "none" none))
  :group 'dotemacs)

(defconst dotemacs-emacs27+ (> emacs-major-version 26))
(defconst dotemacs-emacs28+ (> emacs-major-version 27))
(defconst dotemacs-is-linux (eq system-type 'gnu/linux))
(defconst dotemacs-is-windows (eq system-type 'windows-nt))

;; Silence "assignment to free variable" warning
(defvar apropos-do-all)
(defvar c-electric-indent)
(defvar compilation-always-kill)
(defvar compilation-scroll-output)
(defvar grep-highlight-matches)
(defvar grep-scroll-output)
(defvar tags-revert-without-query)
(defvar use-package-always-defer)
(defvar use-package-compute-statistics)
(defvar use-package-enable-imenu-support)
(defvar use-package-expand-minimally)
(defvar use-package-verbose)
(defvar use-package-always-ensure)

(declare-function sb/sshlist "private" ())
;; FIXME: Fix smerge warnings
(declare-function smerge-next "smerge-mode" ())

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' before loading
(setq use-package-enable-imenu-support t)
(if (bound-and-true-p dotemacs-debug-init-file)
    (setq debug-on-error t
          use-package-always-ensure t
          use-package-compute-statistics t ; Use `M-x use-package-report' to see results
          use-package-expand-minimally nil
          use-package-verbose t)
  (setq use-package-always-defer t
        use-package-compute-statistics nil
        ;; Avoid printing errors and warnings since the configuration is known to work
        use-package-expand-minimally t
        use-package-verbose nil))
(eval-when-compile
  (require 'use-package))

;; Installation is one-time, so avoid the overhead of run-time checks
(use-package use-package-ensure-system-package
  :disabled t)

(use-package bind-key
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish)

(use-package dash
  :commands -tree-map)

(use-package f
  :commands (f-join f-exists?))

;; Put this early in the `user-init-file'
(use-package no-littering)

(defcustom dotemacs-custom-file
  (no-littering-expand-etc-file-name "custom.el")
  "File to write Emacs customizations."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-private-file
  (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type 'string
  :group 'dotemacs)

;; Use `C-c h' consistently for invoking a hydra
(use-package hydra
  :commands (hydra-default-pre hydra-keyboard-quit defhydra
                               hydra-show-hint hydra-set-transient-map
                               hydra--call-interactively-remap-maybe))

(setq custom-file dotemacs-custom-file)
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(when (file-exists-p dotemacs-private-file)
  (load dotemacs-private-file 'noerror))

(use-package paradox
  :commands (paradox-list-packages paradox-upgrade-packages)
  :bind
  (("C-c d l" . paradox-list-packages)
   ("C-c d u" . paradox-upgrade-packages))
  :custom
  (paradox-display-star-count nil)
  (paradox-execute-asynchronously t)
  (paradox-github-token t)
  :config (paradox-enable))

;; FIXME: Improve startup
;; Check the PATH with `(getenv "PATH")'
(use-package exec-path-from-shell
  :defines  exec-path-from-shell-check-startup-files
  :if (or (daemonp) (memq window-system '(x ns)))
  :init
  (setq exec-path-from-shell-arguments '("-l" "-i")
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "NODE_PATH" "JAVA_HOME" "PYTHONPATH"))
  (exec-path-from-shell-initialize))

(setq ad-redefinition-action 'accept ; Turn off warnings due to redefinitions
      auto-mode-case-fold nil
      ;; auto-save-list-file-prefix (expand-file-name "auto-save"
      ;;                                              dotemacs-temp-directory)
      auto-save-no-message t
      backup-inhibited t ; Disable backup for a per-file basis
      blink-matching-paren nil ; Distracting
      case-fold-search t ; Searches and matches should ignore case
      compilation-always-kill t
      compilation-ask-about-save nil
      ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first
      ;; error
      compilation-scroll-output 'first-error
      completion-ignore-case nil ; Consider case when completing
      confirm-kill-emacs nil
      confirm-kill-processes nil ; Prevent "Active processes exist" when you quit Emacs
      confirm-nonexistent-file-or-buffer t
      create-lockfiles nil
      cursor-in-non-selected-windows t ; Hide the cursor in inactive windows
      custom-safe-themes t
      delete-by-moving-to-trash t ; Use system trash to deal with mistakes
      enable-recursive-minibuffers t
      enable-remote-dir-locals t
      ;; Disable the warning "X and Y are the same file"
      find-file-suppress-same-file-warnings t
      find-file-visit-truename t ; Show true name, useful in case of symlinks
      ;; Avoid resizing the frame when your newly set font is larger (or smaller) than the system
      ;; default
      frame-inhibit-implied-resize t
      frame-title-format (list '(buffer-file-name "%f" "%b"))
      help-window-select t ; Makes it easy to close the window
      history-delete-duplicates t
      ;; Doom Emacs: Emacs updates its UI more often than it needs to, so we slow it down slightly
      ;; from 0.5s
      idle-update-delay 1.0
      indicate-buffer-boundaries nil
      inhibit-compacting-font-caches t ; Do not compact font caches during GC
      ;; Disable loading of `default.el' at startup, inhibits site default settings
      inhibit-default-init t
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t ; `inhibit-splash-screen' is an alias
      ;; *scratch* is in Lisp interaction mode by default, use text mode. `text-mode' is more
      ;; expensive, but I use *scratch* for composing emails.
      initial-major-mode 'text-mode
      initial-scratch-message nil
      kill-do-not-save-duplicates t
      kill-whole-line t
      ;; major-mode 'text-mode ; Major mode to use for files that do no specify a major mode
      make-backup-files nil ; Stop making backup `~' files
      mouse-drag-copy-region t
      mouse-yank-at-point t ; Yank at point instead of at click
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
      ;; FIXME: Adding a list seems to introduce nesting problems
      vc-handled-backends nil ; Disabling vc can improve performance
      view-read-only t ; View mode for read-only buffers
      visible-bell nil
      x-gtk-use-system-tooltips nil ; Do not use system tooltips
      x-underline-at-descent-line t ; Underline looks a bit better when drawn lower
      )

;; Changing buffer-local variables will only affect a single buffer. `setq-default' changes the
;; buffer-local variable's default value.
(setq-default fill-column dotemacs-fill-column
              indent-tabs-mode nil ; Spaces instead of tabs
              indicate-empty-lines nil
              ;; tab-always-indent 'complete
              tab-width 4
              truncate-lines nil)

;; https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
(setq-default bidi-display-reordering 'left-to-right
              bidi-inhibit-bpa t
              bidi-paragraph-direction 'left-to-right)

;; LSP mode generates lots of objects, which causes a problem with gcmh mode.
(use-package gcmh
  :diminish
  :hook
  ((after-init . gcmh-mode)
   (focus-out-hook . gcmh-idle-garbage-collect)))

;; Activate utf8
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Scrolling: from Doom Emacs
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the cursor more than N
      ;; lines past window edges (where N is the settings of `scroll-conservatively'). This is
      ;; especially slow in larger files during large-scale scrolling commands. If kept over 100,
      ;; the window is never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines.
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      ;; Do not accelerate scrolling
      mouse-wheel-progressive-speed nil
      mouse-avoidance-mode 'banish)

(fset 'display-startup-echo-area-message #'ignore)
(fset 'yes-or-no-p 'y-or-n-p) ; Type "y"/"n" instead of "yes"/"no"

;; SB: I do not use the following commands.

;; Do not disable narrowing commands
;; (put 'narrow-to-region 'disabled nil)
;; (put 'narrow-to-page 'disabled nil)
;; (put 'narrow-to-defun 'disabled nil)

;; Do not disable case-change functions
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)

;; Auto-refresh all buffers, does not work for remote files
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 5 "Faster would mean less likely to use stale data")
  (auto-revert-remote-files t)
  (auto-revert-verbose nil))

;; Revert pdf files without asking
(setq revert-without-query '("\\.pdf"))

(use-package saveplace ; Remember cursor position in files
  :ensure nil
  :hook (after-init . save-place-mode)
  ;; :custom
  ;; (save-place-file (expand-file-name "places"
  ;;                                    dotemacs-temp-directory))
  )

(use-package savehist ; Save minibuffer histories across sessions
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables '(
                                   extended-command-history
                                   kill-ring
                                   search-ring
                                   ))
  ;; (savehist-file (expand-file-name "savehist" dotemacs-temp-directory))
  (savehist-save-minibuffer-history t))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'forward)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "/")
  (uniquify-strip-common-suffix t))

(use-package hippie-exp ; Replace `dabbrev-exp'
  :ensure nil
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol))
  (hippie-expand-verbose nil)
  :bind ("M-/" . hippie-expand))

(use-package subword
  :ensure nil
  :diminish
  :hook (prog-mode . subword-mode))

;; horizontal - Split the selected window into two windows (e.g., `split-window-below'), one above
;; the other
;; vertical - Split the selected window into two side-by-side windows (e.g., `split-window-right')
(cond ((eq dotemacs-window-split 'horizontal) (setq split-width-threshold nil
                                                    split-height-threshold 0))
      ((eq dotemacs-window-split 'vertical) (setq split-height-threshold nil
                                                  split-width-threshold 0)))

;; Make use of wider screens
;; (when (string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
;;   (split-window-right))

;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun sb/auto-save-wrapper (save-fn &rest args)
  "Hide 'Auto-saving...done' messages by calling the method.
SAVE-FN with non-nil ARGS."
  (ignore args)
  (apply save-fn '(t)))
(advice-add 'do-auto-save :around #'sb/auto-save-wrapper)

(use-package abbrev
  :ensure nil
  :diminish
  :hook
  ;; ((text-mode org-mode markdown-mode latex-mode LaTeX-mode) . abbrev-mode)
  (text-mode . abbrev-mode)
  :custom
  (abbrev-file-name (expand-file-name "abbrev-defs" dotemacs-extras-directory))
  (save-abbrevs 'silently))

;; (when (display-graphic-p) ; `window-system' is deprecated
;;   (progn
;;     (menu-bar-mode -1)
;;     (scroll-bar-mode -1)
;;     (tool-bar-mode -1)))

;; Disable the following modes
(dolist (mode '(
                blink-cursor-mode ; Blinking cursor is distracting
                desktop-save-mode
                global-prettify-symbols-mode ; Makes it difficult to edit the buffer
                minibuffer-depth-indicate-mode
                size-indication-mode
                tooltip-mode
                ))
  (when (fboundp mode)
    (funcall mode -1)))

;; Enable the following modes
(dolist (mode '(
                auto-compression-mode
                column-number-mode
                ;; Typing with the mark active will overwrite the marked region
                delete-selection-mode
                global-visual-line-mode ; Wrap lines
                ;; Enable visual feedback on selections, mark follows the point
                transient-mark-mode
                global-hl-line-mode
                ))
  (when (fboundp mode)
    (funcall mode 1)))

;; Not a library/file, so `eval-after-load' does not work
(diminish 'auto-fill-function)
(diminish 'visual-line-mode)

;; (fringe-mode '(1 . 1))

;; Native from Emacs 27+
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; This puts the buffer in read-only mode and disables font locking
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode)
  :custom (so-long-threshold 800))

;; Maximize Emacs on startup. Moved to `early-init-file'.
;; I am not sure which one of the following is better or faster
;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
;; (add-hook 'emacs-startup-hook 'toggle-frame-maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package leuven-theme
  :if (eq dotemacs-theme 'leuven)
  :init (load-theme 'leuven t))

(use-package eclipse-theme
  :if (eq dotemacs-theme 'eclipse)
  :init
  (load-theme 'eclipse t)
  (set-background-color "white")
  (set-face-attribute 'region nil
                      :background "LemonChiffon"
                      :foreground "black")
  (set-face-attribute 'mode-line nil
                      :background "grey88"
                      :foreground "black"
                      :box nil))

(use-package spacemacs-common
  :ensure spacemacs-theme
  :if (eq dotemacs-theme 'spacemacs-light)
  :init
  (load-theme 'spacemacs-light t)
  ;; (add-to-list 'default-frame-alist
  ;;              '(background-color . "#fbf8ef"))
  )

(use-package zenburn-theme
  :if (eq dotemacs-theme 'zenburn)
  :init (load-theme 'zenburn t))

(use-package solarized-light-theme
  :ensure solarized-theme
  :if (eq dotemacs-theme 'solarized-light)
  :init
  (setq solarized-distinct-fringe-background t)
  (load-theme 'solarized-light t))

(use-package solarized-dark-theme
  :ensure solarized-theme
  :if (eq dotemacs-theme 'solarized-dark)
  :init (load-theme 'solarized-dark t))

(use-package doom-themes
  :if (eq dotemacs-theme 'doom-molokai)
  :init
  (load-theme 'doom-molokai t)
  (set-face-attribute 'font-lock-comment-face nil
                      ;; :foreground "#cccccc"
                      ;; :foreground "#b2b2b2"
                      :foreground "#999999"))

(use-package monokai-theme
  :if (eq dotemacs-theme 'monokai)
  :init (load-theme 'monokai t))

(use-package modus-operandi-theme
  :if (eq dotemacs-theme 'modus-operandi)
  :init
  (setq modus-operandi-theme-completions 'opinionated
        modus-operandi-theme-fringes 'subtle
        modus-operandi-theme-intense-hl-line t
        modus-operandi-theme-intense-standard-completions t
        modus-operandi-theme-mode-line nil
        modus-operandi-theme-scale-headings nil
        modus-operandi-theme-variable-pitch-headings nil)
  (load-theme 'modus-operandi t)
  :custom-face
  (mode-line ((t (:background "#d7d7d7" :foreground "#0a0a0a"
                              :box (:line-width 1 :color "#505050")
                              :height 0.8))))
  (mode-line-inactive ((t (:background "#efefef" :foreground "#404148"
                                       :box (:line-width 1 :color "#bcbcbc")
                                       :height 0.8)))))

(use-package modus-vivendi-theme
  :if (eq dotemacs-theme 'modus-vivendi)
  :init
  (setq modus-vivendi-theme-mode-line 'moody
        modus-vivendi-theme-variable-pitch-headings nil
        modus-vivendi-theme-scale-headings nil)
  (load-theme 'modus-vivendi t))

(when (and (eq dotemacs-theme 'sb/default) (display-graphic-p))
  (progn
    ;; (setq frame-background-mode 'light)
    ;; (set-background-color "#ffffff")
    (set-foreground-color "#333333")
    (set-face-attribute 'hl-line nil
                        :background "light yellow")
    (set-face-attribute 'region nil
                        :background "gainsboro")))

(use-package powerline
  :if (eq dotemacs-modeline-theme 'powerline)
  :init
  (setq powerline-display-buffer-size t
        powerline-display-hud nil
        powerline-display-mule-info nil
        powerline-gui-use-vcs-glyph t)
  (when (eq dotemacs-theme 'leuven)
    (set-face-attribute 'mode-line nil
                        :background "grey88"
                        :foreground "black")
    (set-face-attribute 'mode-line-buffer-id nil
                        :weight 'bold
                        :foreground "black"
                        :background "gray88")
    (powerline-default-theme)))

(use-package smart-mode-line
  :if (eq dotemacs-modeline-theme 'sml)
  :init
  (setq sml/theme 'light
        sml/mode-width 'full
        sml/no-confirm-load-theme t
        sml/shorten-modes t
        sml/shorten-directory t)
  (sml/setup))

(use-package spaceline
  :if (eq dotemacs-modeline-theme 'spaceline)
  :defines (spaceline-hud-p
            spaceline-selection-info-p
            spaceline-version-control-p
            spaceline-input-method-p
            spaceline-persp-name-p)
  :init
  (require 'spaceline-config)
  (setq spaceline-hud-p nil
        spaceline-input-method-p nil
        spaceline-persp-name-p nil
        spaceline-selection-info-p nil
        spaceline-version-control-p t)
  (spaceline-emacs-theme))

(use-package airline-themes
  :if (eq dotemacs-modeline-theme 'airline)
  :init
  (require 'airline-themes)
  (setq airline-eshell-colors nil
        airline-hide-eyebrowse-on-inactive-buffers t)
  (load-theme 'airline-cool t))

(use-package doom-modeline
  ;; Requires the fonts included with `all-the-icons', run `M-x all-the-icons-install-fonts'
  :ensure all-the-icons
  :if (eq dotemacs-modeline-theme 'doom-modeline)
  :init
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-indent-info t
        doom-modeline-minor-modes t)
  (doom-modeline-mode 1)
  :custom-face
  (doom-modeline-bar ((t (:inherit default :height 0.8)))))

(use-package awesome-tray
  :ensure nil
  :if (eq dotemacs-modeline-theme 'awesome-tray)
  :load-path "extras"
  :hook (after-init . awesome-tray-mode)
  :custom
  (awesome-tray-active-modules
   '("file-path" "buffer-name" "mode-name"
     "location" "git"))
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

(use-package auto-dim-other-buffers
  :hook (after-init . auto-dim-other-buffers-mode))

;; Value is in 1/10pt, so 100 will give you 10pt
;; (set-frame-font "DejaVu Sans Mono" nil t)
;; (set-frame-font "Roboto Mono")

;; https://github.com/larstvei/dot-emacs
;; (cond ((member "Inconsolata" (font-family-list))
;;        (set-face-attribute 'default nil :font "Inconsolata-18")))

(cond ((string= (system-name) "swarnendu-Inspiron-7572") (set-face-attribute
                                                          'default nil :height 135))
      ((string= (system-name) "cse-BM1AF-BP1AF-BM6AF") (set-face-attribute
                                                        'default nil :height 140)))
(set-face-attribute 'mode-line nil :height 110)
(set-face-attribute 'mode-line-inactive nil :height 110)

;; https://stackoverflow.com/questions/7869429/altering-the-font-size-for-the-emacs-minibuffer-separately-from-default-emacs
(defun sb/minibuffer-font-setup ()
  "Customize minibuffer font."
  (set (make-local-variable 'face-remapping-alist)
       '((default :height 0.95))))
(add-hook 'minibuffer-setup-hook #'sb/minibuffer-font-setup)

;; Changing height of the echo area is jarring, but limiting the height can make it difficult to see
;; useful information
;; (add-hook 'emacs-startup-hook (lambda ()
;;                                 (setq resize-mini-windows nil)))

(use-package solar
  :ensure nil
  :custom
  (calendar-latitude 26.50)
  (calendar-longitude 80.23))

(use-package circadian
  :disabled t
  :if (and (not (eq dotemacs-theme 'sb/default)) (not (eq dotemacs-theme 'none)))
  :after solar
  :custom
  (circadian-themes '((:sunrise . modus-operandi)
                      (:sunset  . modus-operandi)))
  :init (circadian-setup))

(use-package beacon
  :diminish
  :hook (after-init . beacon-mode))

(use-package ibuffer
  :ensure nil
  :custom
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-display-summary nil)
  (ibuffer-use-header-line t)
  :bind ("C-x C-b" . ibuffer)
  :config (defalias 'list-buffers 'ibuffer))

(use-package ibuf-ext
  :load-path "extras"
  :custom
  ;; Do not show filter groups if there are no buffers in that group
  (ibuffer-show-empty-filter-groups nil)
  :hook (ibuffer . ibuffer-auto-mode))

(use-package ibuffer-projectile ; Group buffers by projectile project
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

(use-package dired
  :ensure nil
  :commands dired-next-line
  :preface
  (defun sb/dired-go-home ()
    (interactive)
    (dired dotemacs-user-home))
  (defun sb/dired-jump-to-top ()
    (interactive)
    (goto-char (point-min)) ; Faster than `(beginning-of-buffer)'
    (dired-next-line 2))
  (defun sb/dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max)) ; Faster than `(end-of-buffer)'
    (dired-next-line -1))
  :bind (:map dired-mode-map
              ("M-<home>" . sb/dired-go-home)
              ("i" . find-file)
              ("M-<up>" . sb/dired-jump-to-top)
              ("M-<down>" . sb/dired-jump-to-bottom))
  :custom
  (dired-auto-revert-buffer t "Revert each dired buffer automatically when you revisit it")
  (dired-dwim-target t "Guess a default target directory")
  (dired-listing-switches "-ABhl --si --group-directories-first" "Check `ls' for additional options")
  (dired-ls-F-marks-symlinks t "-F marks links with @")
  (dired-recursive-copies 'always "Single prompt for all n directories")
  (dired-recursive-deletes 'always "Single prompt for all n directories"))

;; Auto refresh dired when files change
(add-hook 'dired-mode #'auto-revert-mode)

(use-package dired-x
  :ensure nil
  :custom
  (dired-cleanup-buffers-too t)
  (dired-omit-verbose nil "Do not show messages when omitting files")
  :hook (dired-mode . dired-omit-mode)
  :bind ("C-x C-j" . dired-jump)
  ;; :diminish dired-omit-mode ; This does not work
  :config
  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Remove 'Omit' from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map))

(use-package dired+
  :load-path "extras"
  :commands diredp-toggle-find-file-reuse-dir
  :custom
  (diredp-hide-details-initially-flag nil)
  (diredp-hide-details-propagate-flag nil)
  :config (unbind-key "r" dired-mode-map) ; Bound to `diredp-rename-this-file'
  :hook
  (dired-mode . (lambda ()
                  ;; Do not create multiple dired buffers
                  (diredp-toggle-find-file-reuse-dir 1))))

(use-package dired-efap
  :after dired+
  :defines dired-efap-initial-filename-selection
  :commands dired-efap
  :hook
  (dired-mode . (lambda ()
                  (require 'dired-efap)))
  :custom (dired-efap-initial-filename-selection nil)
  :bind* (:map dired-mode-map
               ("r" . dired-efap)))

(use-package dired-narrow ; Narrow dired to match filter
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package diredfl ; More detailed colors
  :hook (dired-mode . diredfl-mode))

(use-package dired-async
  :ensure async
  :diminish
  :hook (dired-mode . dired-async-mode))

;; (use-package dired-rsync
;;   :bind (:map dired-mode-map
;;               ("C-c C-r" . dired-rsync)))

;; (use-package dired-posframe
;;   :hook (dired-mode . dired-posframe-mode)
;;   :diminish)

(use-package treemacs
  :functions treemacs-git-mode
  :commands (treemacs treemacs-toggle)
  :hook
  ((projectile-mode . treemacs-filewatch-mode)
   (projectile-mode . treemacs-follow-mode)
   ;; (projectile-mode . treemacs-fringe-indicator-mode)
   )
  :custom
  (treemacs-collapse-dirs 3)
  (treemacs-follow-after-init t)
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-indentation 2)
  (treemacs-is-never-other-window nil "Prevents treemacs from being selected with `other-window`")
  (treemacs-lock-width t)
  ;; (treemacs-persist-file (expand-file-name "treemacs-persist" dotemacs-temp-directory))
  (treemacs-position 'right)
  (treemacs-project-follow-cleanup t)
  (treemacs-recenter-after-file-follow t)
  (treemacs-recenter-after-tag-follow  t)
  (treemacs-show-hidden-files nil)
  (treemacs-silent-filewatch t)
  (treemacs-silent-refresh t)
  ;; (treemacs-sorting 'alphabetic-desc)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1)
  (treemacs-width 24)
  :config
  ;; Effectively overrides treemacs-follow-mode, but is a bit noisy
  ;; (treemacs-tag-follow-mode 1)
  (treemacs-git-mode 'extended)
  (set-face-attribute 'treemacs-directory-collapsed-face nil :height 0.7)
  (set-face-attribute 'treemacs-directory-face nil :height 0.7)
  (set-face-attribute 'treemacs-file-face nil :height 0.7)
  (set-face-attribute 'treemacs-root-face nil :height 0.9)
  (set-face-attribute 'treemacs-tags-face nil :height 0.7)
  (set-face-attribute 'treemacs-git-ignored-face nil :height 0.7)
  (set-face-attribute 'treemacs-git-untracked-face nil :height 0.7)
  (set-face-attribute 'treemacs-git-modified-face nil :height 0.7)
  (set-face-attribute 'treemacs-git-unmodified-face nil :height 0.7)
  (treemacs-resize-icons 16)
  :bind* ("C-j" . treemacs))

(use-package treemacs-projectile
  :disabled t
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :disabled t
  :after treemacs
  :if (display-graphic-p)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :disabled t
  :after (treemacs magit))

;; Install fonts with `M-x all-the-icons-install-fonts'
(use-package all-the-icons
  :if (display-graphic-p)
  ;; :config (all-the-icons-install-fonts 'y)
  )

(use-package all-the-icons-ibuffer
  :if (display-graphic-p)
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :custom (all-the-icons-ibuffer-icon-size 0.8))

(use-package all-the-icons-dired
  :diminish
  :if (display-graphic-p)
  ;; :hook (dired-mode . (lambda ()
  ;;                       (interactive)
  ;;                       ;; Do not show icons over tramp
  ;;                       (unless (file-remote-p default-directory)
  ;;                         (all-the-icons-dired-mode))))
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package org
  :defines (org-hide-leading-stars-before-indent-mode
            org-src-strip-leading-and-trailing-blank-lines
            org-src-tabs-acts-natively)
  :hook (org-mode . visual-line-mode)
  :diminish org-indent-mode
  :custom
  (org-src-fontify-natively t "Code block fontification using the major-mode of the code")
  (org-startup-indented t)
  (org-startup-truncated nil)
  (org-src-preserve-indentation t)
  (org-src-tabs-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-fontify-done-headline t)
  (org-fontify-whole-heading-line t)
  (org-startup-folded 'showeverything)
  (org-hide-leading-stars t)
  (org-hide-leading-stars-before-indent-mode t)
  (org-support-shift-select t)
  ;; See `org-speed-commands-default' for a list of the keys and commands enabled at the beginning
  ;; of headlines. `org-babel-describe-bindings' will display a list of the code blocks commands and
  ;; their related keys.
  (org-use-speed-commands t)
  (org-src-strip-leading-and-trailing-blank-lines t)
  ;; Display entities like `\tilde' and `\alpha' in UTF-8 characters
  (org-pretty-entities t)
  ;; Render subscripts and superscripts in org buffers
  (org-pretty-entities-include-sub-superscripts t)
  :config
  (unbind-key "M-<up>" org-mode-map)
  (unbind-key "M-<down>" org-mode-map))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Use `C-'' in `isearch-mode-map' to use `avy-isearch' to select one of the currently visible
;; isearch candidates
(use-package isearch
  :ensure nil
  :custom (search-highlight t "Highlight incremental search")
  :bind
  (("C-s" . nil) ; Change the binding for `isearch-forward-regexp'
   ("C-f" . isearch-forward-regexp)
   :map isearch-mode-map
   ("C-s" . nil) ; Change the binding for `isearch-repeat-forward'
   ("C-f" . isearch-repeat-forward)))

(use-package isearch-symbol-at-point
  :after isearch
  :commands (isearch-symbol-at-point isearch-backward-symbol-at-point))

(use-package isearch-dabbrev
  :after isearch
  :bind (:map isearch-mode-map
              ("<tab>" . isearch-dabbrev-expand)))

(use-package anzu
  :after isearch
  :diminish anzu-mode
  :commands anzu-mode
  :custom
  (anzu-search-threshold 10000)
  (anzu-minimum-input-length 2)
  :config
  (when (eq dotemacs-modeline-theme 'spaceline)
    (setq anzu-cons-mode-line-p nil))
  (unless (eq dotemacs-theme 'leuven)
    (set-face-attribute 'anzu-mode-line nil
                        :foreground "blue"
                        :weight 'light))
  (global-anzu-mode 1))

(use-package swiper
  :bind
  (([remap swiper] . swiper-isearch)
   ("<f4>" . swiper-isearch))
  :custom (swiper-action-recenter t))

(with-eval-after-load 'grep
  (setq grep-command "grep -irHn "
        grep-highlight-matches t
        grep-scroll-output t))

(use-package wgrep ; Writable grep
  :commands (wgrep-change-to-wgrep-mode)
  :custom (wgrep-auto-save-buffer t))

(use-package deadgrep
  ;; :bind ("<f8>" . deadgrep)
  )

(use-package ripgrep
  :commands ripgrep-regexp)

(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-add-file
                          recentf-apply-filename-handlers)
  :hook (dired-mode . (lambda ()
                        (recentf-add-dired-directory)))
  :custom
  (recentf-auto-cleanup 'never "Do not stat remote files")
  ;; Check regex with `re-builder'
  (recentf-exclude '(
                     "[/\\]elpa/"
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
                     ;; "/ssh:"
                     "~$"
                     "/.autosaves/"
                     "/TAGS$"
                     "*.cache"
                     ))
  ;; https://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
  ;; (recentf-keep '(file-remote-p file-readable-p))
  (recentf-max-saved-items 100)
  (recentf-menu-filter 'recentf-sort-descending)
  ;; (recentf-save-file (expand-file-name "recentf" dotemacs-temp-directory))
  :config
  (run-at-time nil (* 5 60) 'recentf-save-list) ; seconds
  (add-to-list 'recentf-exclude (file-truename no-littering-etc-directory))
  (add-to-list 'recentf-exclude (file-truename no-littering-var-directory))
  :hook (after-init . recentf-mode))

(defun sb/inhibit-message-call-orig-fun (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN, forward ARGS."
  (let ((inhibit-message t))
    (apply orig-fun args)))

;; Hide the "Wrote to recentf" message which is irritating
(advice-add 'recentf-save-list :around #'sb/inhibit-message-call-orig-fun)

;; Hide the "Wrote ..." message which is irritating
(advice-add 'write-region :around #'sb/inhibit-message-call-orig-fun)

;; Use `M-x company-diag' or the modeline status to see the backend used
;; Try `M-x company-complete-common' when there are no completions
(use-package company
  :commands company-abort
  :hook (after-init . global-company-mode)
  :preface
  (defun sb/quit-company-save-buffer ()
    "Quit company popup and save the buffer."
    (interactive)
    (company-abort)
    (save-buffer))
  :custom
  (company-dabbrev-downcase nil "Do not downcase returned candidates")
  (company-dabbrev-ignore-case nil "Backend is case-sensitive")
  (company-dabbrev-other-buffers t "Search in other buffers with same major mode")
  (company-idle-delay 0.0 "Recommended by lsp")
  (company-ispell-available t)
  (company-ispell-dictionary (expand-file-name "wordlist"
                                               dotemacs-extras-directory))
  (company-minimum-prefix-length 2 "Small words are faster to type")
  (company-require-match nil "Allow input string that do not match candidates")
  (company-selection-wrap-around t)
  (company-show-numbers 'left "Speed up completion")
  (company-tooltip-align-annotations t)
  :config
  ;; SB: We set `company-backends' as a local variable
  ;; (dolist (backends '(company-semantic company-bbdb company-oddmuse company-cmake))
  ;;   (delq backends company-backends))

  ;; Ignore numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  (push (apply-partially #'cl-remove-if
                         (lambda (c)
                           (string-match-p "\\`[0-9]+\\'" c)))
        company-transformers)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-common-or-cycle)
              ("C-M-/" . company-other-backend) ;; Was bound to `dabbrev-completion'
              ("C-s" . sb/quit-company-save-buffer)
              ("<escape>" . company-abort)))

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)
(advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)
(advice-add 'ispell-lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; Posframes do not have unaligned rendering issues with variable `:height' unlike an overlay.
;; https://github.com/company-mode/company-mode/issues/1010
;; However, the width of the frame popup is often not enough.
(use-package company-posframe
  :diminish
  ;; :disabled t ; FIXME: Issue with no completion shown
  :custom
  (company-posframe-show-metadata nil)
  (company-posframe-show-indicator nil)
  :hook (company-mode . company-posframe-mode))

;; The package seems unmaintained and only works for `elisp-mode'
(use-package company-flx
  :disabled t
  :hook (company-mode . company-flx-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-box
  :disabled t ; FIXME: Issue with no completion shown
  :if (display-graphic-p)
  :diminish
  :hook (company-mode . company-box-mode)
  ;; :custom (company-box-show-single-candidate t)
  :config
  (set-face-background 'company-box-background "cornsilk")
  (set-face-background 'company-box-selection "light blue"))

(use-package company-ctags
  :disabled t
  :after company
  :custom
  (company-ctags-fuzzy-match-p t)
  (company-ctags-everywhere t))

;; Typing `TabNine::config' in any buffer should open the extension settings, deep local mode is
;; computationally expensive
(use-package company-tabnine
  :after company
  :disabled t)

(use-package company-fuzzy
  :after company
  :disabled t
  :diminish
  :init (global-company-fuzzy-mode 1))

(use-package flx)

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-global-mode snippet-mode)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (after-init . yas-global-mode)
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-verbosity 2)
  :config (unbind-key "<tab>" yas-minor-mode-map))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package amx
  :hook (after-init . amx-mode)
  ;; :custom (amx-save-file (expand-file-name "amx-items" dotemacs-temp-directory))
  )

(use-package ivy
  :commands ivy-completion-in-region
  :preface
  ;; https://github.com/abo-abo/swiper/wiki/Hiding-dired-buffers
  (defun sb/ignore-dired-buffers (str)
    "Return non-nil if STR names a Dired buffer.
  This function is intended for use with `ivy-ignore-buffers'."
    (let ((buf (get-buffer str)))
      (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))
  :custom
  (completion-in-region-function #'ivy-completion-in-region)
  (ivy-initial-inputs-alist nil "Do not start completion with `^'")
  (ivy-case-fold-search 'always "Always ignore case while searching")
  (ivy-count-format "(%d/%d) " "Help identify wrap around")
  (ivy-extra-directories nil "Hide . and ..")
  (ivy-fixed-height-minibuffer t "Distracting if the height keeps changing")
  ;; Make the height of the minibuffer proportionate to the screen
  (ivy-height-alist '((t
                       lambda (_caller)
                       (/ (frame-height) 2))))
  (ivy-re-builders-alist '(
                           (counsel-M-x . ivy--regex-fuzzy)
                           (counsel-find-file . ivy--regex-fuzzy)
                           ;; (swiper . ivy--regex-fuzzy)
                           ;; (swiper-isearch . ivy--regex-fuzzy)
                           ;; (counsel-rg . ivy--regex-ignore-order)
                           (t . ivy--regex-ignore-order)
                           ))
  (ivy-truncate-lines nil "`counsel-flycheck' output gets truncated")
  (ivy-wrap t)
  :hook (after-init . ivy-mode)
  :config
  (defalias 'wgrep-change-to-wgrep-mode 'ivy-wgrep-change-to-wgrep-mode)
  (defalias 'occur 'ivy-occur)
  :config
  (dolist (buffer '(
                    "TAGS" "magit-process" "*eldoc for use-package*" "^\\*Help\\*$"
                    "^\\*Compile-Log\\*$" "^\\*.+Completions\\*$" "^\\*Backtrace\\*$"
                    "*flycheck-posframe-buffer*" "^\\*Ibuffer\\*$" "*emacs*" "*Warnings*"
                    "^\\*prettier" "^\\*json*" "^\\*texlab*" "^\\*clangd*"
                    "^\\*shfmt*"
                    ))
    (add-to-list 'ivy-ignore-buffers buffer))
  (add-to-list 'ivy-ignore-buffers #'sb/ignore-dired-buffers)
  :diminish
  :bind
  (("C-c r" . ivy-resume)
   ("<f3>" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("C-'" . ivy-avy)
   ("<return>" . ivy-alt-done) ; Continue completion
   ("<left>" . ivy-previous-line)
   ("<right>" . ivy-next-line)))

(use-package counsel
  :ensure amx
  ;; :ensure-system-package fasd
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
  :init
  ;; Enabling preview can make switching buffers slow
  (setq counsel-switch-buffer-preview-virtual-buffers nil)
  :bind
  (([remap execute-extended-command] . counsel-M-x)
   ("<f1>" . counsel-M-x)
   ([remap completion-at-point] . counsel-company)
   ([remap describe-bindings] . counsel-descbinds)
   ([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap dired] . counsel-dired)
   ("C-x f" . counsel-file-jump) ; Jump to a file below the current directory
   ([remap find-file] . counsel-find-file)
   ("<f2>" . counsel-find-file)
   ;; `counsel-flycheck' shows less information than `flycheck-list-errors'
   ;; ([remap flycheck-list-errors] . counsel-flycheck)
   ("C-c s g" . counsel-git-grep)
   ("C-<f9>" . sb/counsel-goto-recent-directory)
   ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
   ([remap load-library] . counsel-load-library)
   ([remap load-theme] . counsel-load-theme)
   ([remap recentf-open-files] . counsel-recentf)
   ("<f9>" . counsel-recentf)
   ("C-c s r" . counsel-rg)
   ("C-c C-m" . counsel-mark-ring)
   ;; ("<f3>" . counsel-switch-buffer) ; Preview of buffers can be a bottleneck
   ([remap yank-pop] . counsel-yank-pop))
  :bind* ("C-c C-j" . counsel-semantic-or-imenu)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (concat
                                    "\\(?:\\`[#.]\\)" ; File names beginning with # or .
                                    "\\|\\(?:\\`.+?[#~]\\'\\)" ; File names ending with # or ~
                                    "\\|__pycache__"
                                    "\\|.aux$"
                                    "\\|.bbl$"
                                    "\\|.blg$"
                                    "\\|.cb$"
                                    "\\|.cb2$"
                                    "\\|.djvu$"
                                    "\\|.doc$"
                                    "\\|.docx$"
                                    "\\|.dvi$"
                                    "\\|.elc$"
                                    "\\|.fdb_latexmk$"
                                    "\\|.fls$"
                                    "\\|.jar$"
                                    "\\|.jpeg$"
                                    "\\|.jpg$"
                                    "\\|.lof$"
                                    "\\|.lot$"
                                    "\\|.o$"
                                    "\\|.out$"
                                    "\\|.png$"
                                    "\\|.ppt$"
                                    "\\|.pptx$"
                                    "\\|.pyc$"
                                    "\\|.rel$"
                                    "\\|.rip$"
                                    "\\|.so$"
                                    "\\|.synctex$"
                                    "\\|.synctex.gz$"
                                    "\\|.tar.gz$"
                                    "\\|.tar.xz$"
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
                                    ))
  (counsel-mode-override-describe-bindings t)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n-------------------------\n")
  :diminish
  :hook (ivy-mode . counsel-mode)
  :config
  ;; `counsel-flycheck' shows less information than `flycheck-list-errors', and there is an
  ;; argument error
  ;; (defalias 'flycheck-list-errors 'counsel-flycheck)
  ;; (defalias 'load-library 'counsel-load-library)
  ;; (defalias 'load-theme 'counsel-load-theme)
  ;; (defalias 'yank-pop 'counsel-yank-pop)

  ;; (add-to-list 'ivy-display-functions-alist
  ;;              '(counsel-company . ivy-display-function-overlay))
  )

(use-package ivy-hydra
  :after (ivy hydra)
  :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body))

(use-package ivy-posframe
  :disabled t
  :diminish
  :hook (ivy-mode . ivy-posframe-mode)
  :custom
  (ivy-display-function #'ivy-posframe-display-at-frame-center)
  (ivy-posframe-parameters '((left-fringe . 4)
                             (right-fringe . 4))))

(use-package prescient
  :hook (after-init . prescient-persist-mode)
  ;; :custom
  ;; (prescient-save-file (expand-file-name "prescient-save.el"
  ;;                                        dotemacs-temp-directory))
  )

;; https://github.com/raxod502/prescient.el/issues/65
(use-package ivy-prescient
  :disabled t ; No longer maintained
  :hook (counsel-mode . ivy-prescient-mode)
  :custom
  (ivy-prescient-enable-sorting nil "Allow prescient to override sorting logic")
  (ivy-prescient-sort-function '(not swiper swiper-isearch
                                     ivy-switch-buffer
                                     counsel-recentf counsel-grep
                                     flyspell-correct-ivy )))

;; https://www.reddit.com/r/emacs/comments/9o6inu/sort_ivys_counselrecentf_results_by_timestamp/e7ze1c8/
;; (with-eval-after-load 'ivy
;;   (add-to-list 'ivy-sort-functions-alist '(counsel-recentf . file-newer-than-file-p)))

;; (setq ivy-sort-matches-functions-alist
;;       '((t . ivy--prefix-sort)))
;; (add-to-list
;;  'ivy-sort-matches-functions-alist
;;  '(read-file-name-internal . ivy--sort-files-by-date))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(use-package all-the-icons-ivy
  :disabled t
  :hook (after-init . all-the-icons-ivy-setup))

(use-package ispell
  :ensure nil
  :if dotemacs-is-linux
  :custom
  (ispell-dictionary "en_US")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90"))
  (ispell-local-dictionary "en_US")
  (ispell-personal-dictionary (expand-file-name "spell" dotemacs-extras-directory))
  (ispell-silently-savep t "Save a new word to personal dictionary without asking"))

;; Hide the "Starting new Ispell process" message
(advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)

(use-package flyspell
  :ensure nil
  :if dotemacs-is-linux
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
              (message "No more miss-spelled word!")
              (setq arg 0))
          (forward-word)))))
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :hook
  ((prog-mode . flyspell-prog-mode)
   ;; (before-save-hook . flyspell-buffer) ; Saving will be slow
   (text-mode . flyspell-mode))
  :config
  ;; Skip regions in Org-mode
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src" . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))
  :diminish
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   ("C-c f w" . ispell-word)
   :map flyspell-mode-map
   ("C-;" . nil)
   ;; ("C-," . flyspell-auto-correct-previous-word)
   ("C-," . sb/flyspell-goto-previous-error)))

;; Flyspell popup is more efficient. Ivy-completion does not give the save option in a few cases.
(or (use-package flyspell-popup
      :bind ("C-;" . flyspell-popup-correct)
      :custom (flyspell-popup-correct-delay 0.2))

    (use-package flyspell-correct-ivy
      :ensure flyspell-correct
      :disabled t
      :functions flyspell-correct-ivy
      :custom (flyspell-correct-interface #'flyspell-correct-ivy)
      :bind ("C-;" . flyspell-correct-wrapper)))

(defhydra sb/hydra-spelling (:color blue)
  "
  ^
  ^Spelling^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^───────
  _q_ quit            _<_ previous        _c_ correction
  ^^                  _>_ next            _d_ dictionary
  ^^                  _f_ check           _m_ mode
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flyspell-correct-previous :color pink)
  (">" flyspell-correct-next :color pink)
  ("c" ispell)
  ("d" ispell-change-dictionary)
  ("f" flyspell-buffer)
  ("m" flyspell-mode))

;; As of Emacs 28, `flyspell' does not provide a way to automatically check all on-screen text,
;; and running `flyspell-buffer' on an entire buffer can be slow.
(use-package spell-fu
  :config (global-spell-fu-mode))

(or (use-package highlight-indentation
      :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
      :hook (python-mode . highlight-indentation-mode))

    (use-package highlight-indent-guides
      :disabled t
      :diminish
      :hook ((yaml-mode python-mode) . highlight-indent-guides-mode)
      :custom (highlight-indent-guides-method 'character)))

;; Claims to be better than electric-indent-mode
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-edit.el
(use-package aggressive-indent
  :diminish
  :hook ((lisp-mode emacs-lisp-mode) . aggressive-indent-mode)
  :custom
  (aggressive-indent-comments-too t)
  (aggressive-indent-dont-electric-modes t))

(electric-pair-mode 1) ; Enable autopairing, smartparens seems slow

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  ;; (show-paren-delay 0)
  (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; FIXME: Seems to have performance issue with latex-mode and markdown-mode.
;; `sp-cheat-sheet' will show you all the commands available, with examples.
(use-package smartparens
  :disabled t
  :diminish (smartparens-mode show-smartparens-mode)
  :commands sp-local-pair
  :hook
  ;; ((prog-mode . (lambda ()
  ;;                 (require 'smartparens-config)
  ;;                 (smartparens-global-mode 1)
  ;;                 (show-smartparens-global-mode 1)))
  ((prog-mode . smartparens-mode)
   ((latex-mode LaTeX-mode) . (lambda ()
                                (require 'smartparens-latex))))
  :custom
  (sp-show-pair-from-inside t)
  (sp-autoskip-closing-pair 'always)
  :config
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
  :commands (projectile-project-name projectile-project-root
                                     projectile-expand-root
                                     projectile-command-map
                                     projectile-default-mode-line
                                     projectile-mode
                                     projectile-locate-dominating-file
                                     projectile-switch-project
                                     projectile-ignored-projects
                                     projectile-globally-ignored-directories
                                     projectile-globally-ignored-files
                                     projectile-globally-ignored-file-suffixes)
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-auto-discover nil "Do not discover projects")
  ;; (projectile-cache-file (expand-file-name "projectile.cache" dotemacs-temp-directory))
  (projectile-completion-system 'ivy)
  ;; (projectile-dynamic-mode-line nil "")
  (projectile-enable-caching nil "Problematic if you create new files often")
  (projectile-file-exists-remote-cache-expire nil)
  ;; Contents of .projectile are ignored when using the alien or hybrid indexing method
  (projectile-indexing-method 'alien)
  ;; (projectile-known-projects-file (expand-file-name "projectile-known-projects.eld"
  ;;                                                   dotemacs-temp-directory))
  (projectile-mode-line-prefix "")
  (projectile-require-project-root t "Use only in desired directories, too much noise otherwise")
  (projectile-sort-order 'access-time)
  (projectile-verbose nil)
  :config
  ;; https://github.com/MatthewZMD/.emacs.d
  (when (and dotemacs-is-windows
             (executable-find "tr"))
    (setq projectile-indexing-method 'alien))

  (defun projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      (format " %s [%s] "
              projectile-mode-line-prefix
              (or project-name "-"))))
  (projectile-mode 1)

  ;; Avoid search when `projectile-mode' is enabled for faster startup
  ;; (setq projectile-project-search-path (list
  ;;                                       (concat `,(getenv "HOME") "/bitbucket")
  ;;                                       (expand-file-name "github" dotemacs-user-home)
  ;;                                       (expand-file-name "iitk-workspace" dotemacs-user-home)
  ;;                                       (expand-file-name "iitkgp-workspace" dotemacs-user-home)
  ;;                                       (expand-file-name "iss-workspace" dotemacs-user-home)
  ;;                                       (expand-file-name "plass-workspace" dotemacs-user-home)
  ;;                                       (expand-file-name "prospar-workspace" dotemacs-user-home)
  ;;                                       (expand-file-name "research" dotemacs-user-home)
  ;;                                       ))

  (dolist (prjs (list
                 ;; Do not consider $HOME as a project
                 (expand-file-name dotemacs-user-home)
                 (expand-file-name "/tmp")
                 (expand-file-name "bitbucket/.metadata" dotemacs-user-home)
                 (expand-file-name "github/.metadata" dotemacs-user-home)
                 (expand-file-name "iitk-workspace/.metadata" dotemacs-user-home)
                 (expand-file-name "plass-workspace/.metadata" dotemacs-user-home)
                 ))
    (add-to-list 'projectile-ignored-projects prjs))
  ;; Filtering does not work with `alien' indexing
  (dolist (dirs
           '(".cache" ".clangd" ".dropbox" ".git" ".hg" ".metadata" ".nx" ".recommenders" ".svn"
             ".vscode" "__pycache__" "auto" "elpa" "node_modules"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  (dolist (items
           '("GPATH" "GRTAGS" "GTAGS" "GSYMS" "TAGS" "tags" ".dir-locals.el" ".projectile"
             ".project" ".tags" "__init__.py"))
    (add-to-list 'projectile-globally-ignored-files items))
  (dolist (exts
           '(".a" ".aux" ".bak" ".blg" ".class" ".deb" ".djvu" ".doc" ".docx" ".elc" ".gif" ".jar"
             ".jpeg" ".jpg" ".o" ".odt" ".out" ".pdf" ".png" ".ppt" ".pptx" ".ps" ".pt" ".pyc"
             ".rel" ".rip" ".rpm" ".so" ".svg" ".tar.gz" ".tar.xz" ".xls" ".xlsx" ".zip" "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes exts)))

(use-package counsel-projectile
  :after counsel
  :defines counsel-projectile-default-file
  :commands counsel-projectile-switch-project-by-name
  :hook (counsel-mode . counsel-projectile-mode)
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
  :custom
  ;; (counsel-projectile-find-file-more-chars 3)
  (counsel-projectile-remove-current-buffer t)
  (counsel-projectile-sort-buffers t)
  (counsel-projectile-sort-directories t)
  (counsel-projectile-sort-files t)
  (counsel-projectile-sort-projects t)
  ;; :config
  ;; (counsel-projectile-modify-action
  ;;  'counsel-projectile-switch-project-action
  ;;  '((default sb/counsel-projectile-switch-project-action-default-file)))
  :bind
  (("<f5>" . counsel-projectile-switch-project)
   ("<f6>" . counsel-projectile-find-file)
   ;; ("<f7>" . counsel-projectile-rg)
   ([remap projectile-ag] . counsel-projectile-ag)
   ([remap projectile-find-dir] . counsel-projectile-find-dir)
   ([remap projectile-find-file] . counsel-projectile-find-file)
   ([remap projectile-grep] . counsel-projectile-grep)
   ([remap projectile-switch-project] . counsel-projectile-switch-project)
   ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)))

;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el
;; Enable before `ivy-rich-mode' for better performance
(use-package all-the-icons-ivy-rich
  :if (display-graphic-p)
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :init (setq all-the-icons-ivy-rich-icon-size 0.8))

(use-package ivy-rich
  :functions ivy-format-function-line
  :custom (ivy-rich-parse-remote-buffer nil)
  :config (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :hook (ivy-mode . ivy-rich-mode))

(use-package counsel-fd
  :if (executable-find "fd")
  :commands (counsel-fd-dired-jump counsel-fd-dired-jump))

;; FIXME: Exclude directories and files from being checked
;; https://github.com/flycheck/flycheck/issues/1745
(use-package flycheck
  :commands (flycheck-add-next-checker flycheck-next-checker
                                       flycheck-previous-error
                                       flycheck-describe-checker
                                       flycheck-buffer
                                       flycheck-list-errors
                                       flycheck-disabled-checkers
                                       flycheck-select-checker
                                       flycheck-verify-setup
                                       flycheck-next-error
                                       flycheck-disable-checker)
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change mode-enabled))
  ;; (flycheck-display-errors-delay 0.5)
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; (flycheck-idle-buffer-switch-delay 1)
  ;; (flycheck-idle-change-delay 1)
  ;; (flycheck-indication-mode 'left-margin)
  :init
  (when (or (eq dotemacs-modeline-theme 'spaceline) (eq
                                                     dotemacs-modeline-theme 'doom-modeline))
    (setq flycheck-mode-line nil))

  (setq-default
   ;; flycheck-disabled-checkers '(
   ;;                              ;; emacs-lisp-checkdoc
   ;;                              tex-lacheck
   ;;                              ;; tex-chktex
   ;;                              python-flake8
   ;;                              python-mypy
   ;; python-pycompile)
   ;; flycheck-emacs-lisp-executable "emacs"
   flycheck-markdown-markdownlint-cli-config (expand-file-name ".markdownlint.json"
                                                               dotemacs-user-home)
   flycheck-pylintrc (expand-file-name ".config/pylintrc"
                                       dotemacs-user-home)
   flycheck-python-pylint-executable "python3"
   flycheck-shellcheck-follow-sources nil
   flycheck-textlint-config (expand-file-name "textlintrc.json"
                                              dotemacs-textlint-home)
   flycheck-textlint-executable (expand-file-name "node_modules/.bin/textlint"
                                                  dotemacs-textlint-home))

  ;; Workaround for eslint loading slow
  ;; https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
  ;; (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

  ;; (dolist (hook '(js-mode js2-mode typescript-mode))
  ;;   (setq-local flycheck-checker 'javascript-eslint))
  :config
  ;; https://github.com/flycheck/flycheck/issues/1833
  (add-to-list 'flycheck-hooks-alist
               '(after-revert-hook . flycheck-buffer)))

(use-package flycheck-grammarly
  :defer 2 ; Expensive to load
  :config
  (require 'flycheck-grammarly)
  ;; Remove from the beginning of the list `flycheck-checkers' and append to the end
  (setq flycheck-checkers (delete 'grammarly-checker flycheck-checkers))
  (add-to-list 'flycheck-checkers 'grammarly-checker t)
  :custom (flycheck-grammarly-check-time 3))

(or (use-package flycheck-popup-tip ; Show error messages in popups
      :disabled t
      :hook (flycheck-mode . flycheck-popup-tip-mode))

    (use-package flycheck-pos-tip
      :disabled t
      :if (display-graphic-p)
      :hook (flycheck-mode . flycheck-pos-tip-mode))

    (use-package flycheck-posframe
      :disabled t
      :if (display-graphic-p)
      :hook (flycheck-mode . flycheck-posframe-mode)
      :custom (flycheck-posframe-position 'point-bottom-left-corner)
      :config (flycheck-posframe-configure-pretty-defaults))

    (use-package flycheck-inline
      :disabled t
      :hook (flycheck-mode . flycheck-inline-mode)))

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

(use-package whitespace
  :disabled t
  :commands (whitespace-mode global-whitespace-mode
                             whitespace-buffer whitespace-cleanup
                             whitespace-turn-off)
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  :hook (markdown-mode . whitespace-mode)
  :custom
  (show-trailing-whitespace t)
  (whitespace-line-column dotemacs-fill-column)
  (whitespace-style '(face lines-tail trailing)))

;; This is different from whitespace-cleanup since this is unconditional
(when (bound-and-true-p dotemacs-delete-trailing-whitespace-p)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package whitespace-cleanup-mode
  :diminish
  :commands whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode)
  :config (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode))

(use-package ws-butler ; Unobtrusively trim extraneous white-space *ONLY* in lines edited
  :if (not (bound-and-true-p dotemacs-delete-trailing-whitespace-p))
  :diminish
  :hook (prog-mode . ws-butler-mode))

(or
 (use-package highlight-symbol ; Highlight symbol under point
   :disabled t ; `symbol-overlay' is faster
   :diminish
   :hook (prog-mode . highlight-symbol-mode)
   :bind
   (("M-p" . highlight-symbol-prev)
    ("M-n" . highlight-symbol-next))
   :custom (highlight-symbol-on-navigation-p t))

 (use-package symbol-overlay ; Highlight symbol under point
   :commands (symbol-overlay-mode)
   :diminish
   :hook ((prog-mode html-mode yaml-mode) . symbol-overlay-mode)
   :bind
   (("M-p" . symbol-overlay-jump-prev)
    ("M-n" . symbol-overlay-jump-next))))

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :custom (hl-todo-highlight-punctuation ":")
  :config
  (add-to-list 'hl-todo-keyword-faces '("LATER" . "#d0bf8f"))
  (add-to-list 'hl-todo-keyword-faces '("ISSUE" . "#ff8c00"))
  (add-to-list 'hl-todo-keyword-faces '("DEBUG" . "#ff8c00"))
  (add-to-list 'hl-todo-keyword-faces '("TEST" . "tomato"))
  (add-to-list 'hl-todo-keyword-faces '("WARNING" . "#cc0000"))
  (add-to-list 'hl-todo-keyword-faces '("BEWARE" . "#aa0000"))
  (add-to-list 'hl-todo-keyword-faces '("DEPRECATED" . "#aa0000")))

(use-package highlight-numbers
  :hook ((prog-mode conf-mode css-mode html-mode) . highlight-numbers-mode))

(use-package number-separator
  :ensure nil
  :load-path "extras"
  :custom
  (number-separator ",")
  (number-separator-interval 3)
  (number-separator-ignore-threshold 4)
  (number-separator-decimal-char ".")
  :config (number-separator-mode 1))

(use-package volatile-highlights
  :diminish
  :commands volatile-highlights-mode
  :config (volatile-highlights-mode 1))

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

;; First mark the word, then add more cursors. Use `mc/edit-lines' to add a cursor to each line in
;; an active region that spans multiple lines. add a cursor to each line:
(use-package multiple-cursors
  :bind
  (("C-<" . mc/mark-previous-like-this)
   ("C->" . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

;; Edit remote file: `/method:user@host#port:filename'
;; Shortcut /ssh:: will connect to default user@host#port.
;; Edit local file with sudo: `C-x C-f /sudo::/etc/hosts'
;; Open a remote file with ssh + sudo: `C-x C-f /ssh:host|sudo:root:/etc/passwd'
(use-package tramp
  :defer 2
  :custom
  (tramp-default-method "ssh" "SSH is faster than the default SCP")
  ;; Auto-save to a local directory for better performance
  ;; (tramp-auto-save-directory (expand-file-name "tramp-auto-save"
  ;;                                              dotemacs-temp-directory))
  ;; (tramp-persistency-file-name (expand-file-name "tramp"
  ;;                                                dotemacs-temp-directory))
  (tramp-verbose 1)
  (remote-file-name-inhibit-cache nil "Remote files are not updated outside of Tramp")
  (tramp-completion-reread-directory-timeout nil)
  ;; Disable version control
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp
                                tramp-file-name-regexp))
  :config
  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
  (setenv "SHELL" "/bin/bash") ; Recommended to connect with bash
  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME"))))

;; Does not pick up other usernames, difficult to customize different default home directories
;; https://github.com/masasam/emacs-counsel-tramp/issues/4
;; https://github.com/masasam/emacs-counsel-tramp/issues/5
(use-package counsel-tramp
  :disabled t
  :bind ("C-c d t" . counsel-tramp)
  :config
  (defalias 'tramp 'counsel-tramp)
  ;; (add-hook 'counsel-tramp-pre-command-hook
  ;;           (lambda ()
  ;;             (global-aggressive-indent-mode -1)
  ;;             (projectile-mode -1)
  ;;             (counsel-projectile-mode -1)))
  ;; (add-hook 'counsel-tramp-quit-hook
  ;;           (lambda ()
  ;;             (global-aggressive-indent-mode 1)
  ;;             (projectile-mode 1)
  ;;             (counsel-projectile-mode 1)))
  )

(defun sb/counsel-tramp ()
  "Invoke remote hosts with ivy and tramp."
  (interactive)
  (counsel-find-file (ivy-read "Tramp: " (sb/sshlist))))
(bind-key "C-c d t" #'sb/counsel-tramp)

(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items 500)
  (imenu-max-item-length 100)
  (imenu-use-popup-menu nil)
  (imenu-sort-function nil))

;; (use-package imenu+
;;   :load-path "extras"
;;   :after imenu)

;; (use-package imenu-anywhere
;;   :after imenu)

;; (use-package popup-imenu
;;   :after imenu)

(use-package flimenu
  :after imenu
  :disabled t
  :commands flimenu-global-mode
  :config (flimenu-global-mode 1))

(setq large-file-warning-threshold (* 500 1024 1024) ; MB
      tags-add-tables nil
      tags-case-fold-search nil ; t=case-insensitive, nil=case-sensitive
      ;; Do not ask before rereading the `TAGS' files if they have changed
      tags-revert-without-query t)

;; FIXME: Remove support for gtags. It is less maintained than counsel-etags.
;; (use-package counsel-gtags
;;   :if (and (eq system-type 'gnu/linux) (eq dotemacs-tags-scheme 'gtags))
;;   :diminish
;;   ;; :ensure-system-package global
;;   ;; :init
;;   ;; (add-hook 'c-mode-common-hook
;;   ;;           (lambda ()
;;   ;;             (when (derived-mode-p 'c-mode 'c++-mode)
;;   ;;               (counsel-gtags-mode 1))))
;;   :hook ((prog-mode protobuf-mode latex-mode) . counsel-gtags-mode)
;;   :custom (counsel-gtags-auto-update t)
;;   :bind (:map counsel-gtags-mode-map
;;               ("M-'" . counsel-gtags-dwim)
;;               ("M-," . counsel-gtags-go-backward)
;;               ("M-?" . counsel-gtags-find-reference)
;;               ("C-c g s" . counsel-gtags-find-symbol)
;;               ("C-c g d" . counsel-gtags-find-definition)
;;               ("C-c g c" . counsel-gtags-create-tags)
;;               ("C-c g u" . counsel-gtags-update-tags))
;;   :config
;;   (use-package global-tags ; Make xref and gtags work together
;;     :if (eq dotemacs-tags-scheme 'gtags)
;;     :demand t
;;     :config (add-to-list 'xref-backend-functions 'global-tags-xref-backend)))

(use-package xref
  :ensure nil
  :if (eq dotemacs-tags-scheme 'ctags)
  :commands xref-etags-mode
  :config (xref-etags-mode)
  :bind
  (("M-'" . xref-find-definitions)
   ("M-?" . xref-find-references)
   ("C-M-." . xref-find-apropos)
   ("M-," . xref-pop-marker-stack)
   :map xref--xref-buffer-mode-map
   ("C-o" . xref-show-location-at-point)
   ("<tab>" . xref-quit-and-goto-xref)
   ("r" . xref-query-replace-in-results)))

(use-package ivy-xref
  :after (ivy xref)
  :commands (ivy-xref-show-defs ivy-xref-show-xrefs)
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel-etags
  :disabled t
  ;; :ensure-system-package (ctags . "snap install universal-ctags")
  :if (and (eq system-type 'gnu/linux) (eq dotemacs-tags-scheme 'ctags))
  :bind
  (("M-]" . counsel-etags-find-tag-at-point)
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
  :custom
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-quiet t)
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defhydra sb/dumb-jump-hydra (:color blue :columns 3)
  "Dumb Jump"
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("b" dumb-jump-back "Back"))

;; The built-in `describe-function' includes both functions and macros. `helpful-function'
;; is functions only, so `helpful-callable' as a drop-in replacement.
(use-package helpful
  :bind
  (([remap describe-variable] . helpful-variable)
   ("C-h v" . helpful-variable)
   ([remap describe-key] . helpful-key)
   ("C-h k" . helpful-key)
   ([remap describe-function] . helpful-callable)
   ("C-h f" . helpful-callable)
   ([remap describe-symbol] . helpful-symbol)
   ("C-h c" . helpful-command)
   ("C-h p" . helpful-at-point)
   :map helpful-mode-map
   ("q" . helpful-kill-buffers)))

(use-package vlf ; Speed up Emacs for large files: `M-x vlf <PATH-TO-FILE>'
  :commands vlf
  :custom (vlf-application 'dont-ask)
  :config (use-package vlf-setup))

(use-package hungry-delete ; Erase all consecutive white space characters in a given direction
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  ;; :config
  ;; (with-eval-after-load 'ivy
  ;;   (unbind-key "backspace" ivy-occur-grep-mode-map))
  )

(use-package move-text ; Move lines with `M-<up>' and `M-<down>'
  :commands (move-text-up move-text-down)
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
  :bind ("C-c d m" . manage-minor-mode))

(use-package jgraph-mode
  :mode "\\.jgr\\'")

(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

(use-package gnuplot
  :mode "\\.gp\\'"
  :interpreter ("gnuplot" . gnuplot-mode))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(use-package popwin
  :disabled t
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
  (add-to-list 'popwin:special-display-config '(flycheck-verify-mode))
  (add-to-list 'popwin:special-display-config '("*lsp session*")))

(use-package expand-region ; Expand region by semantic units
  :bind ("C-=" . er/expand-region))

(use-package expand-line
  :disabled t
  :defines expand-line-mode
  :bind ("M-i" . turn-on-expand-line-mode))

(use-package smart-mark ; Restore point with `C-g' after marking a region
  :config (smart-mark-mode 1))

(use-package whole-line-or-region
  :disabled t
  :diminish (whole-line-or-region-local-mode)
  :config (whole-line-or-region-global-mode 1))

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

(use-package beginend
  :commands beginend-global-mode
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode))
  (beginend-global-mode 1))

(use-package undo-tree
  :defines undo-tree-map
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-mode-lighter "")
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-relative-timestamps t)
  (undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode 1)
  (unbind-key "C-/" undo-tree-map)
  (defalias 'redo 'undo-tree-redo)
  :diminish
  :bind
  ("C-x u" . undo-tree-visualize))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :bind* ("C-." . iedit-mode))

(use-package session ; Avoid the "Overwrite old session file (not loaded)?" warning
  :disabled t ; FIXME: Problem unresolved
  ;; :init
  ;; (setq session-save-file (expand-file-name "session"
  ;;                                           dotemacs-temp-directory))
  :hook
  (after-init . (lambda()
                  (session-initialize))))

(use-package immortal-scratch
  :hook (after-init . immortal-scratch-mode))

;; SB: I use the *scratch* buffer for taking notes, it helps to make the data persist
(use-package persistent-scratch
  :commands persistent-scratch-setup-default
  :hook (after-init . persistent-scratch-setup-default)
  ;; :custom
  ;; (persistent-scratch-save-file (expand-file-name "persistent-scratch"
  ;;                                                 dotemacs-temp-directory))
  )

;; (use-package sudo-edit ; Edit file with sudo
;;   :bind ("M-s e" . sudo-edit))

(use-package crux
  :bind
  (("C-c d i" . crux-ispell-word-then-abbrev)
   ("C-c d s" . crux-sudo-edit)
   ("<f12>" . crux-kill-other-buffers)))

(use-package disable-mouse
  :diminish disable-mouse-global-mode
  :hook (after-init . global-disable-mouse-mode))

(mouse-avoidance-mode 'exile)

(use-package apt-sources-list
  :mode ("\\.list\\'" . apt-sources-list-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode latex-mode LaTeX-mode) . rainbow-delimiters-mode))

(use-package ssh-config-mode
  :mode ("/\\.ssh/config\\'" . ssh-config-mode)
  :mode ("/sshd?_config\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)
  :hook (ssh-config-mode . turn-on-font-lock))

(use-package ace-window
  :bind
  (([remap other-window] . ace-window)
   ("<f10>" . ace-window)))

;; `Shift + direction' arrows
(use-package windmove
  :ensure nil
  :commands windmove-default-keybindings
  :init (windmove-default-keybindings)
  :custom (windmove-wrap-around t "Wrap around at edges"))

;; Save buffers when Emacs loses focus. This causes additional saves which leads to auto-formatters
;; being invoked more frequently.
(use-package super-save
  :diminish
  :commands super-save-mode
  :custom (super-save-remote-files nil "Ignore remote files")
  :hook (find-file . super-save-mode)
  :config (add-to-list 'super-save-triggers 'ace-window))

;; It will bind, for example, `avy-isearch' to `C-'' in `isearch-mode-map', so that you can select
;; one of the currently visible isearch candidates using `avy'.
(use-package avy
  :bind
  (("M-b" . avy-goto-word-1)
   ("C-'" . avy-goto-char)
   ("C-/" . avy-goto-line))
  :init (setq avy-indent-line-overlay nil)
  :custom
  (avy-background t)
  (avy-highlight-first t)
  (avy-style 'at)
  :config (avy-setup-default))

(use-package bookmark
  :ensure nil
  :disabled t
  :custom
  ;; (bookmark-default-file (expand-file-name "bookmarks"
  ;;                                          dotemacs-temp-directory))
  )

(use-package bm
  :commands (bm-buffer-save-all bm-repository-save
                                bm-repository-load bm-buffer-save
                                bm-buffer-restore)
  :init (setq bm-restore-repository-on-load t)
  :config
  (setq-default bm-buffer-persistence t)
  ;; (setq bm-repository-file (expand-file-name "bm-bookmarks" dotemacs-temp-directory))
  :hook
  ((after-init . bm-repository-load)
   (kill-buffer . bm-buffer-save)
   (kill-emacs . (lambda ()
                   (bm-buffer-save-all)
                   (bm-repository-save)))
   (after-save . bm-buffer-save)
   (find-file . bm-buffer-restore)
   (after-revert . bm-buffer-restore))
  :bind
  (("C-<f1>" . bm-toggle)
   ("C-<f2>" . bm-next)
   ("C-<f3>" . bm-previous)))

(use-package esup
  :commands esup)

(use-package bug-hunter)

(use-package explain-pause-mode
  :ensure nil
  :load-path "extras"
  :commands (explain-pause-mode explain-pause-top)
  ;; :hook (after-init . explain-pause-mode)
  :diminish)

;; `text-mode' is a basic mode for `LaTeX-mode' and `org-mode', and so any hooks defined will also
;; get run for all modes derived from a basic mode such as `text-mode'.

;; https://www.emacswiki.org/emacs/AutoFillMode
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

(add-hook 'text-mode-hook
          (lambda ()
            ;; Add `proselint', then `textlint', then `grammarly-checker'
            (flycheck-add-next-checker 'proselint 'textlint)
            (flycheck-add-next-checker 'textlint 'grammarly-checker)))

;; Identify weasel words, passive voice, and duplicate words
(use-package writegood-mode
  :disabled t ; textlint includes writegood
  :diminish
  :hook (text-mode . writegood-mode))

(use-package langtool
  :after text-mode
  :commands langtool-check
  :hook
  (text-mode . (lambda()
                 (require 'langtool)))
  :custom
  (langtool-default-language "en")
  (langtool-language-tool-jar (expand-file-name "languagetool-5.1-commandline.jar"
                                                no-littering-etc-directory)))

(use-package olivetti
  :diminish
  :custom (olivetti-body-width 0.92 "Fraction of the window width")
  :hook (text-mode . olivetti-mode)
  ;; :config (remove-hook 'olivetti-mode-on-hook 'visual-line-mode)
  )

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; https://emacs.stackexchange.com/questions/19686/how-to-use-pdf-tools-pdf-view-mode-in-emacs
;; Use regular `isearch', `swiper' will not work
(use-package pdf-tools
  :defer 2 ; Expensive to load
  :commands (pdf-tools-install pdf-loader-install)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable
  ;; (add-hook 'pdf-view-mode-hook (lambda ()
  ;;                                 (setq header-line-format nil)))
  :custom
  (pdf-annot-activate-created-annotations t "Automatically annotate highlights")
  (pdf-view-resize-factor 1.1 "Fine-grained zoom factor of 10%"))

(use-package saveplace-pdf-view
  :after pdf-tools
  :after saveplace)

(use-package logview
  :mode ("\\.log\\'" . logview-mode)
  ;; :custom
  ;; (logview-cache-filename (expand-file-name "logview-cache.extmap"
  ;;                                           dotemacs-temp-directory))
  )

(use-package antlr-mode
  :ensure nil
  :mode "\\.g4\\'")

(use-package bison-mode
  :mode ("\\.y\\'" "\\.l\\'" "\\.bison\\'"))

(use-package llvm-mode
  :ensure nil
  :load-path "extras"
  :mode "\\.ll\\'")

(use-package tablegen-mode
  :ensure nil
  :load-path "extras"
  :mode "\\.td\\'")

(use-package autodisass-llvm-bitcode
  :mode "\\.bc\\'")

(use-package markdown-mode
  :mode
  ;; The order is important to associate "README.md" with `gfm-mode'
  (("\\.md\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode)
   ("\\.markdown\\'" . markdown-mode))
  ;; :init
  ;; Looks good, but hiding markup makes it difficult to be consistent while editing
  ;; (setq-default markdown-hide-markup t)
  :custom
  (markdown-command
   "pandoc -f markdown -s --mathjax --standalone --quiet --highlight-style=pygments")
  (markdown-enable-math t "Syntax highlight for LaTeX fragments")
  (markdown-enable-wiki-links t)
  ;; https://emacs.stackexchange.com/questions/13189/github-flavored-markdown-mode-syntax-highlight-code-blocks/33497
  (markdown-fontify-code-blocks-natively t)
  (markdown-indent-on-enter 'indent-and-new-item)
  ;; (markdown-make-gfm-checkboxes-buttons nil)
  (markdown-list-indent-width 2)
  (markdown-split-window-direction 'horizontal)
  :config
  (unbind-key "C-c C-j")
  ;; `markdown-mode' is derived from `text-mode'
  (flycheck-add-next-checker 'markdown-markdownlint-cli 'proselint))

(use-package markdown-mode+
  :after markdown-mode)

(use-package markdown-toc
  :disabled t
  :after markdown-mode
  :commands (markdown-toc-refresh-toc
             markdown-toc-generate-toc
             markdown-toc-generate-or-refresh-toc))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf
(use-package pandoc-mode
  ;; :ensure-system-package pandoc
  :commands (pandoc-load-default-settings)
  :diminish
  :config (pandoc-load-default-settings)
  :hook (markdown-mode . pandoc-mode))

(use-package grip-mode
  :if (executable-find "grip")
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(use-package add-node-modules-path
  :init
  (dolist (mode '(typescript-mode js-mode js2-mode coffee-mode))
    (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))

(use-package prettier
  :init (setenv "NODE_PATH" "/usr/local/lib/node_modules")
  ;; :hook
  ;; ((markdown-mode gfm-mode) . (lambda ()
  ;;                               (when (and buffer-file-name
  ;;                                          (not (file-remote-p buffer-file-name)))
  ;;                                 prettier-mode)))
  :hook ((markdown-mode gfm-mode) . prettier-mode))

(use-package csv-mode
  :commands csv-align-mode
  :mode "\\.csv\\'"
  :custom (csv-separators '("," ";" "|" " "))
  :config
  ;; FIXME: Aligning may be slow for large files
  (csv-align-mode 1))

(use-package highlight-doxygen
  :commands highlight-doxygen-mode
  :config (highlight-doxygen-global-mode 1))

(use-package rst
  :ensure nil
  :mode ("\\.rst\\'" . rst-mode))

(use-package boogie-friends
  :mode ("\\.smt\\'" . z3-smt2-mode))

(use-package z3-mode
  :commands z3-mode)

(use-package make-mode
  :ensure nil
  :mode
  (("\\Makefile\\'" . makefile-mode)
   ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
   ("makefile\\.rules\\'" . makefile-gmake-mode)))

;; The variable-height minibuffer and extra eldoc buffers are distracting
(use-package eldoc
  :ensure nil
  :if dotemacs-is-linux
  :diminish
  :hook ((emacs-lisp-mode lisp-interaction-mode) . turn-on-eldoc-mode))

(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook (css-mode . turn-on-css-eldoc))

(use-package octave
  :ensure nil
  :mode "\\.m\\'")

(use-package ess
  :custom
  (inferior-R-args "--quiet --no-restore-history --no-save")
  (ess-indent-offset 4)
  (ess-indent-from-lhs 4)
  :config
  (use-package ess-smart-underscore))

(use-package ini-mode
  :mode "\\.ini\\'")

(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))

(use-package pkgbuild-mode
  :mode ("PKGBUILD" . pkgbuild-mode))

(use-package elisp-mode
  :ensure nil
  :mode
  (("\\.el\\'" . emacs-lisp-mode)
   ("\\.elc\\'" . elisp-byte-code-mode))
  :hook
  ((lisp-mode emacs-lisp-mode) . (lambda ()
                                   (when buffer-file-name
                                     (add-hook 'after-save-hook #'check-parens nil t)
                                     ;; (flycheck-select-checker 'emacs-lisp)
                                     (flycheck-add-next-checker
                                      'emacs-lisp
                                      'emacs-lisp-checkdoc)))))

(use-package lsp-mode
  :defines (lsp-perl-language-server-path
            lsp-perl-language-server-port
            lsp-perl-language-server-client-version
            lsp-disabled-clients
            lsp-enabled-clients
            lsp-pyright-langserver-command-args)
  :commands (lsp-register-client lsp-tramp-connection
                                 make-lsp-client
                                 lsp-format-buffer
                                 lsp-configuration-section lsp
                                 lsp-deferred
                                 lsp--set-configuration
                                 lsp-package-ensure
                                 ht-merge)
  ;; https://justin.abrah.ms/dotfiles/emacs.html
  ;; :ensure-system-package
  ;; ((typescript-language-server . "npm install -g typescript-language-server")
  ;;  (javascript-typescript-langserver . "npm install -g javascript-typescript-langserver")
  ;;  (yaml-language-server . "npm install -g yaml-language-server")
  ;;  (tsc . "npm install -g typescript"))
  :hook
  (
   ((cperl-mode css-mode less-mode perl-mode sgml-mode
                typescript-mode) . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-managed-mode . lsp-modeline-diagnostics-mode)
   ((c++-mode python-mode java-mode) . lsp-headerline-breadcrumb-mode)
   (lsp-mode . lsp-modeline-code-actions-mode)
   ((c++-mode java-mode json-mode) . (lambda ()
                                       ;; (when buffer-file-name
                                       (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                                       ;; )
                                       ))
   )
  :custom
  (lsp-clients-clangd-args '("-j=2" "--background-index" "--clang-tidy" "--pch-storage=memory"
                             ;; "--suggest-missing-includes"
                             "--fallback-style=LLVM" "--log=error"))
  (lsp-completion-provider :none)
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-hook nil)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers nil) ; Could be a directory-local variable
  (lsp-enable-folding nil)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-semantic-highlighting t)
  (lsp-enable-snippet t) ; Autocomplete parentheses
  (lsp-enabled-clients '(clangd clangd-remote jsts-ls flow-ls
                                ts-ls eslint json-ls
                                jsonls-remote cmakels
                                cmakels-remote html-ls
                                htmlls-remote angular-ls texlab
                                texlab-remote jdtls bash-ls
                                bashls-remote typescript-remote
                                css-ls cssls-remote
                                intelephense-remote
                                perl-language-server
                                perlls-remote php-ls xmlls
                                xmlls-remote yamlls yamlls-remote))
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-html-format-wrap-line-length dotemacs-fill-column)
  (lsp-html-format-end-with-newline t)
  (lsp-html-format-indent-inner-html t)
  (lsp-html-format-max-preserve-new-lines nil 1)
  (lsp-imenu-sort-methods '(position))
  (lsp-keep-workspace-alive nil)
  (lsp-log-io nil "texlab communication is huge")
  (lsp-modeline-diagnostics-scope :project)
  (lsp-pyls-configuration-sources [])
  (lsp-pyls-plugins-autopep8-enabled nil)
  ;; Do not turn on fuzzy completion with jedi, lsp-mode is fuzzy on the client side
  (lsp-pyls-plugins-jedi-completion-fuzzy nil)
  (lsp-pyls-plugins-mccabe-enabled nil)
  (lsp-pyls-plugins-preload-modules ["numpy", "csv", "pandas"])
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-pycodestyle-max-line-length dotemacs-fill-column)
  (lsp-pyls-plugins-pydocstyle-convention "pep257")
  (lsp-pyls-plugins-pydocstyle-enabled nil)
  (lsp-pyls-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213")))
  (lsp-pyls-plugins-pyflakes-enabled nil)
  (lsp-pyls-plugins-pylint-args (vconcat
                                 (list "-j 2"
                                       (concat "--rcfile="
                                               (expand-file-name ".config/pylintrc"
                                                                 dotemacs-user-home)))))
  (lsp-pyls-plugins-pylint-enabled t "Pylint can be expensive")
  (lsp-pyls-plugins-yapf-enabled t)
  ;; (lsp-session-file (expand-file-name "lsp-session" dotemacs-temp-directory))
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-xml-logs-client nil)
  ;; https://github.com/eclipse/lemminx/archive/0.14.1.tar.gz
  (lsp-xml-jar-file (expand-file-name "org.eclipse.lemminx-0.14.1-uber.jar"
                                      dotemacs-extras-directory))
  (lsp-yaml-print-width dotemacs-fill-column)
  :custom-face
  (lsp-headerline-breadcrumb-symbols-face ((t (:inherit
                                               font-lock-doc-face
                                               :weight
                                               bold
                                               :height
                                               0.9))))
  (lsp-headerline-breadcrumb-prefix-face ((t (:inherit
                                              font-lock-string-face
                                              :height 0.9))))
  (lsp-headerline-breadcrumb-project-prefix-face ((t (:inherit
                                                      font-lock-string-face
                                                      :weight
                                                      bold
                                                      :height
                                                      0.9))))
  :config
  ;; Support lsp over tramp
  (when (eq dotemacs-python-langserver 'pyls)
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection "pyls")
      :major-modes '(python-mode)
      :remote? t
      :server-id 'pyls-remote)))

  (when (eq dotemacs-python-langserver 'mspyls)
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "mspyls")
                      :major-modes '(python-mode)
                      :remote? t
                      :server-id 'mspyls-remote)))

  (when (eq dotemacs-python-langserver 'pyright)
    (lsp-register-client
     (make-lsp-client :new-connection
                      (lsp-tramp-connection
                       (lambda ()
                         (cons "pyright-langserver"
                               lsp-pyright-langserver-command-args)))
                      :major-modes '(python-mode)
                      :remote? t
                      :server-id 'pyright-remote
                      :multi-root t
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

  (when (eq dotemacs-python-langserver 'jedi)
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "jedi-language-server")
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
   (make-lsp-client :new-connection (lsp-tramp-connection
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
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     '("typescript-language-server" "--stdio"))
                    :major-modes '(js-mode typescript-mode)
                    :remote? t
                    :server-id 'typescript-remote))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "texlab")
    :major-modes '(tex-mode latex-mode LaTeX-mode bibtex-mode)
    :remote? t
    :server-id 'texlab-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     '("vscode-json-languageserver" "--stdio"))
                    :major-modes '(json-mode jsonc-mode)
                    :remote? t
                    :server-id 'jsonls-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     '("css-languageserver" "--stdio"))
                    :major-modes '(css-mode less-mode sass-mode scss-mode)
                    :remote? t
                    :server-id 'cssls-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     '("html-languageserver" "--stdio"))
                    :major-modes '(html-mode web-mode mhtml-mode sgml-mode)
                    :remote? t
                    :server-id 'htmlls-remote))
  ;; FIXME: Get lsp to work for xml remote
  (lsp-register-client
   (make-lsp-client
    :new-connection
    (lsp-tramp-connection
     '("java" "-jar" (expand-file-name "org.eclipse.lemminx-0.14.1-uber.jar"
                                       dotemacs-extras-directory)))
    :major-modes '(xml-mode nxml-mode)
    :remote? t
    :server-id 'xmlls-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     '("yaml-language-server" "--stdio"))
                    :major-modes '(yaml-mode)
                    :remote? t
                    :server-id 'yamlls-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
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
  :bind
  (("M-." . lsp-find-definition)
   ("C-c l i" . lsp-goto-implementation)
   ("C-c l t" . lsp-goto-type-definition)
   ("C-c l r" . lsp-rename)
   ("C-c l h" . lsp-symbol-highlight)
   ("C-c l f" . lsp-format-buffer)
   ("C-c l r" . lsp-find-references)))

(when (or (eq dotemacs-python-langserver 'pyls) (eq dotemacs-python-langserver 'mspyls))
  (add-hook 'python-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  ;; Sync workspace folders and treemacs projects
  (lsp-treemacs-sync-mode 1))

;; SB: I do not use code folding
(use-package origami
  :disabled t
  :hook (after-init . global-origami-mode))

(use-package lsp-origami
  :disabled t
  :hook (origami-mode . lsp-origami-mode))

(use-package lsp-ivy
  :after lsp-mode
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
  :bind
  (("C-c l g" . lsp-ivy-global-workspace-symbol)
   ("C-c l w" . lsp-ivy-workspace-symbol)))

(use-package lsp-java
  :ensure request
  :hook (java-mode . lsp-deferred)
  :custom
  (lsp-java-inhibit-message t)
  (lsp-java-save-actions-organize-imports t))

(use-package request
  :after lsp-java
  :custom (request-storage-directory (expand-file-name "request" no-littering-var-directory)))

(use-package lsp-python-ms
  :if (eq dotemacs-python-langserver 'mspyls)
  :init (setq lsp-python-ms-auto-install-server t)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms)
                   (dolist (ls '(pyls pyls-remote pyright pyright-remote jedi jedils-remote))
                     (add-to-list 'lsp-disabled-clients ls))
                   (add-to-list 'lsp-enabled-clients 'mspyls)
                   (add-to-list 'lsp-enabled-clients 'mspyls-remote)))
  :custom
  (lsp-python-ms-python-executable-cmd "python3"))

;; `pyright --createstub pandas'
(use-package lsp-pyright
  :if (and (eq dotemacs-python-langserver 'pyright) (executable-find "pyright"))
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (dolist (ls '(pyls pyls-remote mspyls mspyls-remote jedi jedils-remote))
                     (add-to-list 'lsp-disabled-clients ls))
                   (add-to-list 'lsp-enabled-clients 'pyright)
                   (add-to-list 'lsp-enabled-clients 'pyright-remote)))
  :custom
  (lsp-pyright-python-executable-cmd "python3"))

(use-package lsp-jedi
  :if (and (eq dotemacs-python-langserver 'jedi) (executable-find "jedi-language-server"))
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-jedi)
                   (dolist (ls '(pyls pyls-remote mspyls mspyls-remote pyright pyright-remote))
                     (add-to-list 'lsp-disabled-clients ls))
                   (add-to-list 'lsp-enabled-clients 'jedi)
                   (add-to-list 'lsp-enabled-clients 'jedils-remote)))
  :custom
  (lsp-jedi-diagnostics-enable t))

;; Py-yapf works on a temporary file (placed in /tmp). Therefore it does not pick up on any project
;; specific YAPF styles. Yapfify works on the original file, so that any project settings supported
;; by YAPF itself are used
(use-package yapfify
  :diminish yapf-mode
  :if (eq dotemacs-python-langserver 'pyright)
  :hook (python-mode . yapf-mode))

;;  Call this in c-mode-common-hook:
;; (define-key (current-local-map) "}" (lambda () (interactive) (c-electric-brace 1)))
(use-package cc-mode
  :ensure nil
  :defines (c-electric-brace c-enable-auto-newline)
  :mode ("\\.h\\'" . c++-mode)
  :mode ("\\.c\\'" . c++-mode)
  :hook (c++-mode . lsp-deferred)
  :custom
  (c-set-style "cc-mode")
  (c-basic-offset 2)
  :config
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
  :bind (:map c-mode-base-map
              ("C-c c a" . c-beginning-of-defun)
              ("C-c c e" . c-end-of-defun)
              ("M-q" . c-fill-paragraph)))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-global-mode))

(use-package flycheck-clang-analyzer
  :after flycheck
  :commands flycheck-clang-analyzer-setup
  :config (flycheck-clang-analyzer-setup))

(use-package flycheck-clang-tidy
  :after flycheck
  :hook (flycheck-mode . flycheck-clang-tidy-setup))

(use-package cuda-mode
  :mode (("\\.cu\\'" . c++-mode)
         ("\\.cuh\\'" . c++-mode)))

(use-package opencl-mode
  :mode "\\.cl\\'")

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . lsp-deferred))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package python
  :ensure nil
  :init
  (setq python-shell-completion-native-enable nil) ; Disable readline based native completion
  (setenv "PYTHONPATH" "python3")
  :hook (python-mode . lsp-deferred)
  ;; :bind (:map python-mode-map
  ;;             ("M-[" . python-nav-backward-block)
  ;;             ("M-]" . python-nav-forward-block)
  ;;             ("C-[" . python-indent-shift-left)
  ;;             ("C-]" . python-indent-shift-right))
  :config
  (with-eval-after-load 'lsp-mode
    (when (eq dotemacs-python-langserver 'pyls)
      (progn
        (dolist (ls '(pyright pyright-remote mspyls mspyls-remote jedi jedils-remote))
          (add-to-list 'lsp-disabled-clients ls))
        (add-to-list 'lsp-enabled-clients 'pyls)
        (add-to-list 'lsp-enabled-clients 'pyls-remote))))
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-shell-exec-path "python3"
        python-shell-interpreter "python3"
        auto-mode-alist (append '(("SConstruct\\'" . python-mode)
                                  ("SConscript\\'" . python-mode))
                                auto-mode-alist))
  ;; FIXME: `lsp' is the first checker, chain the other checkers
  ;; (flycheck-add-next-checker 'lsp 'python-pylint)
  )

(use-package python-docstring
  :diminish
  :hook (python-mode . python-docstring-mode))

(use-package pip-requirements
  :hook (pip-requirements-mode . company-mode))

(use-package pyvenv
  :diminish
  :custom
  (pyvenv-mode-line-indicator
   '(pyvenv-virtual-env-name ("[venv:"
                              pyvenv-virtual-env-name "]")))
  :hook (python-mode . pyvenv-mode)
  :config
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat
                                                pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package py-isort
  :if (executable-find "isort")
  :hook
  (python-mode . (lambda ()
                   (add-hook 'before-save-hook #'py-isort-before-save))))

(use-package ein)

(setq auto-mode-alist (append '(("latexmkrc\\'" . cperl-mode))
                              auto-mode-alist))

(add-hook 'java-mode-hook
          (lambda ()
            (setq-default c-basic-offset 4
                          c-set-style "java")))

(use-package ant)

;; Can disassemble `.class' files from within jars
(use-package autodisass-java-bytecode
  :mode "\\.class\\'")

;; Syntax highlighting for Gradle files
(use-package groovy-mode
  :mode
  (("\\.groovy\\'" . groovy-mode)
   ("\\.gradle\\'" . groovy-mode)))

(use-package sh-script ; Shell script mode
  :ensure nil
  :mode
  (("\\.zsh\\'" . sh-mode)
   ("\\bashrc\\'" . sh-mode))
  :config
  (unbind-key "C-c C-d" sh-mode-map) ; Was bound to `sh-cd-here'
  ;; FIXME: Shellcheck is a resource hog for $HOME/.bash* files
  ;; FIXME: `lsp' is the first checker, chain the other checkers
  ;; (flycheck-add-next-checker 'sh-bash 'sh-shellcheck)
  :hook (sh-mode . lsp-deferred)
  :custom
  (sh-basic-offset 2)
  (sh-indent-comment t) ; Indent comments as a regular line
  (sh-indent-after-continuation 'always))

(use-package fish-mode
  :mode "\\.fish\\'"
  :interpreter "fish"
  :hook
  (fish-mode . (lambda ()
                 (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package shfmt
  :hook (sh-mode . shfmt-on-save-mode)
  :custom (shfmt-arguments '("-i" "4" "-p" "-ci")))

;; FIXME: Does this help?
(with-eval-after-load 'vc
  (remove-hook 'find-file-hook #'vc-find-file-hook)
  (remove-hook 'find-file-hook #'vc-refresh-state))

(use-package transient
  ;; :custom
  ;; (transient-history-file (expand-file-name "transient/history.el"
  ;;                                           dotemacs-temp-directory))
  ;; (transient-levels-file (expand-file-name "transient/levels.el"
  ;;                                          dotemacs-temp-directory))
  ;; (transient-values-file (expand-file-name "transient/values.el"
  ;;                                          dotemacs-temp-directory))
  :config (transient-bind-q-to-quit))

(use-package magit
  :commands (magit-display-buffer-fullframe-status-v1 magit-status)
  :bind
  (("C-x g" . magit-status)
   ("C-c M-g" . magit-file-dispatch)
   ("C-x M-g" . magit-dispatch))
  :custom
  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (magit-completing-read-function #'ivy-completing-read)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-save-repository-buffers t)
  ;; https://irreal.org/blog/?p=8877
  (magit-section-initial-visibility-alist '((stashes . show)
                                            (untracked . show)
                                            (unpushed . show))))

(use-package magit-diff
  :ensure nil
  :after magit
  :custom (magit-diff-refine-hunk t))

(use-package gitignore-mode
  :mode
  (("/\\.gitignore\\'"        . gitignore-mode)
   ("/\\.git/info/exclude\\'" . gitignore-mode)
   ("/git/ignore\\'"          . gitignore-mode)))

(use-package gitattributes-mode
  :mode
  (("/\\.gitattributes\\'"       . gitattributes-mode)
   ("/\\.git/info/attributes\\'" . gitattributes-mode)
   ("/git/attributes\\'"         . gitattributes-mode)))

(use-package gitconfig-mode
  :mode
  (("/\\.gitconfig\\'"  . gitconfig-mode)
   ("/\\.git/config\\'" . gitconfig-mode)
   ("/git/config\\'"    . gitconfig-mode)
   ("/\\.gitmodules\\'" . gitconfig-mode)))

(or
 (use-package git-gutter
   :diminish
   :bind
   (("C-x p" . git-gutter:previous-hunk)
    ("C-x n" . git-gutter:next-hunk))
   :hook (after-init . global-git-gutter-mode)
   :custom (git-gutter:update-interval 1)
   :config
   ;; https://github.com/syl20bnr/spacemacs/issues/10555
   ;; https://github.com/syohex/emacs-git-gutter/issues/24
   (setq git-gutter:disabled-modes '(fundamental-mode org-mode)))

 (use-package diff-hl ; Based on `vc'
   :disabled t ; This does not work for me
   :commands (diff-hl-magit-pre-refresh diff-hl-magit-post-refresh)
   :custom (diff-hl-draw-borders nil)
   :hook
   ((after-init . global-diff-hl-mode)
    (dired-mode . diff-hl-dired-mode))
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package git-commit
  :after magit
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :custom (git-commit-summary-max-length 50))

;; Use the minor mode `smerge-mode' to move between conflicts and resolve them
(use-package smerge-mode
  :ensure nil
  :commands smerge-mode
  :preface
  (defun sb/enable-smerge-maybe ()
    "Enable smerge automatically based on conflict markers."
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1)))))
  :hook
  ((buffer-list-update . sb/enable-smerge-maybe)
   (find-file . sb/enable-smerge-maybe))
  :bind-keymap ("\C-cv" . smerge-command-prefix))


(defhydra sb/hydra-smerge-mode
  (:color pink :hint nil :post (smerge-auto-leave))
  "
   ^Motions^      ^Actions^
---^^-------------^^-------
_n_: Next      _b_: Base
_p_: Prev      _u_: Upper
^^             _l_: Lower
^^             _a_: All
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("q" nil "cancel" :color blue))

(use-package yaml-mode
  :hook (yaml-mode . lsp-deferred))

(use-package yaml-imenu)

(use-package php-mode
  :hook (php-mode . lsp-deferred))

(use-package batch-mode
  :ensure nil
  :mode
  (("\\.bat\\'" . batch-mode)
         ("\\.cmd\\'" . batch-mode)))

(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.hb\\.html\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode))
  :hook (web-mode . lsp-deferred)
  :custom
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-quoting t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-block-face t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t))

(use-package rainbow-mode
  :diminish
  :hook ((css-mode html-mode sass-mode) . rainbow-mode))

(use-package prism
  :load-path "extras"
  :disabled t
  :hook
  (((emacs-lisp-mode lisp-mode) . prism-mode)
   (python-mode . prism-whitespace-mode)))

;; (use-package company-php
;;   :init
;;   (with-eval-after-load 'php-mode
;;     (add-to-list 'company-backends 'company-ac-php-backend)))

(use-package nxml-mode
  :ensure nil
  :commands nxml-mode
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'")
  :init (fset 'xml-mode 'nxml-mode)
  :hook (nxml-mode . lsp-deferred)
  :custom
  (nxml-slash-auto-complete-flag t)
  (nxml-auto-insert-xml-declaration-flag t))

(setq auto-mode-alist (append '((".classpath\\'" . xml-mode))
                              auto-mode-alist))

;; Autocompletion with LSP, LaTeX, and Company is not perfect, so we continue to use AucTeX support
(use-package lsp-latex
  :hook
  ((latex-mode LaTeX-mode) . (lambda()
                               (require 'lsp-latex)
                               (lsp-deferred)))
  :custom
  (lsp-latex-bibtex-formatting-formatter "latexindent")
  (lsp-latex-bibtex-formatting-line-length dotemacs-fill-column)
  (lsp-latex-build-on-save t)
  (lsp-latex-lint-on-save t)
  :config
  (add-to-list 'lsp-latex-build-args "-c")
  (add-to-list 'lsp-latex-build-args "-pvc"))

;; Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides the tex
;; package.
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode))

(use-package tex-buf
  :ensure nil
  :defines (tex-fontify-script font-latex-fontify-script
                               font-latex-fontify-sectioning
                               TeX-syntactic-comment)
  :commands (TeX-active-process TeX-save-document
                                TeX-command-menu
                                TeX-revert-document-buffer)
  :hook
  (((latex-mode LaTeX-mode) . LaTeX-math-mode)
   (LaTeX-mode . TeX-PDF-mode) ; Use `pdflatex'
   (LaTeX-mode . TeX-source-correlate-mode)
   ;; Enable rainbow mode after applying styles to the buffer
   (TeX-update-style . rainbow-delimiters-mode))
  :custom
  (TeX-auto-save t "Enable parse on save, stores parsed information in an `auto' directory")
  (TeX-auto-untabify t "Remove all tabs before saving")
  (TeX-clean-confirm nil)
  (TeX-electric-sub-and-superscript t "Automatically insert braces in math mode")
  (TeX-parse-self t "Parse documents")
  (TeX-quote-after-quote nil "Allow original LaTeX quotes")
  (TeX-save-query nil)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server nil "Do not start the emacs server when correlating sources")
  (TeX-syntactic-comment t)
  (LaTeX-item-indent 0 "Two spaces + Extra indentation")
  (LaTeX-syntactic-comments t)
  (LaTeX-fill-break-at-separators nil "Do not insert line-break at inline math")
  (tex-fontify-script nil "Avoid raising of superscripts and lowering of subscripts")
  (font-latex-fontify-script nil "Avoid superscripts and
  subscripts from being displayed in a different font size")
  (font-latex-fontify-sectioning 1.0 "Avoid emphasizing section headers")
  :config
  ;; Revert PDF buffer after TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq-default TeX-master nil) ; Query for master file
  ;; Disable "LaTeX-insert-item" in favor of imenu
  ;; (unbind-key "C-c C-d" LaTeX-mode-map)
  ;; Unset "C-c ;" since we want to bind it to 'comment-line
  ;; (unbind-key "C-c ;" LaTeX-mode-map)
  )

(use-package auctex-latexmk
  :after tex-mode
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t "Pass the `-pdf' flag when `TeX-PDF-mode' is active")
  (TeX-command-default "LatexMk")
  :config (auctex-latexmk-setup))

(use-package bibtex
  :ensure nil
  :hook
  ((bibtex-mode . turn-on-auto-revert-mode)
   (bibtex-mode . lsp-deferred))
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-maintain-sorted-entries t))

(use-package bibtex-utils
  :after bibtex)

(use-package ivy-bibtex
  :bind ("C-c x b" . ivy-bibtex)
  :custom (ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package bibtex-completion
  :ensure nil
  :after bibtex-mode
  :custom
  (bibtex-completion-cite-default-as-initial-input t)
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  (bibtex-completion-display-formats '((t . "${author:24} ${title:*} ${=key=:16} ${=type=:12}"))))

;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
(use-package reftex
  :ensure nil
  :commands (reftex-get-bibfile-list bibtex-parse-keys
                                     reftex-default-bibliography)
  ;; :diminish
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
  :custom
  (reftex-enable-partial-scans t)
  (reftex-highlight-selection 'both)
  (reftex-plug-into-AUCTeX t)
  (reftex-save-parse-info t)
  (reftex-toc-follow-mode t "Other buffer follows the point in toc buffer")
  (reftex-use-multiple-selection-buffers t)

  ;; :init (add-hook 'reftex-load-hook #'sb/reftex-add-all-bibitems-from-bibtex)
  :config
  (sb/reftex-try-add-all-bibitems-from-bibtex)
  ;;  (add-hook 'reftex-toc-mode-hook #'reftex-toc-rescan)
  )

(use-package bib-cite
  :ensure nil
  :diminish bib-cite-minor-mode
  :hook
  ((LaTeX-mode latex-mode) . (lambda ()
                               (bib-cite-minor-mode 1)))
  :custom (bib-cite-use-reftex-view-crossref t)
  :bind (:map bib-cite-minor-mode-map
              ("C-c b" . nil) ; We use `C-c b' for `comment-box'
              ("C-c l a" . bib-apropos)
              ("C-c l b" . bib-make-bibliography)
              ("C-c l d" . bib-display)
              ("C-c l t" . bib-etags)
              ("C-c l f" . bib-find)
              ("C-c l n" . bib-find-next)
              ("C-c l h" . bib-highlight-mouse)))

;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
(defun sb/save-buffer-and-run-latexmk ()
  "Save the current buffer and run LaTeXMk also."
  (interactive)
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

;; (bind-key "C-x C-s" #'sb/save-buffer-and-run-latexmk LaTeX-mode-map)
;; (bind-key "C-x C-s" #'sb/save-buffer-and-run-latexmk latex-mode-map)

(use-package texinfo
  :mode ("\\.texi\\'" . texinfo-mode))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook
  ((js2-mode . lsp-deferred)
   (js2-mode . js2-imenu-extras-mode))
  :custom
  (js2-basic-offset 2)
  (js-indent-level 2)
  :config (defalias 'javascript-mode 'js2-mode "`js2-mode' is aliased to `javascript' mode"))

(use-package xref-js2
  :after js2-mode
  :custom (xref-js2-search-program 'rg))

;; LATER: The Melpa package does not include support for `jsonc-mode'. A pull request is pending.
(use-package json-mode
  :ensure nil
  :load-path "extras"
  :mode
  (("\\.json\\'" . json-mode)
   (".*/\\.vscode/settings.json$" . jsonc-mode)
   ("User/settings.json$" . jsonc-mode))
  :hook
  ((json-mode jsonc-mode) . (lambda ()
                              (make-local-variable 'js-indent-level)
                              (setq js-indent-level 2)
                              (lsp-deferred))))

(use-package less-css-mode
  :mode "\\.less\\'"
  :hook (less-css-mode . lsp-deferred))

(use-package scss-mode
  :mode "\\.scss\\'"
  :hook (scss-mode . lsp-deferred))

(use-package sass-mode
  :mode "\\.sass\\'"
  :hook (sass-mode . lsp-deferred))

(use-package mlir-mode
  :ensure nil
  :load-path "extras"
  :mode "\\.mlir\\'")

(use-package bazel-mode
  :mode
  (("\\.bzl$" . bazel-mode)
   ("\\BUILD\\'" . bazel-mode)
   ("\\.bazelrc\\'" . bazelrc-mode))
  :hook (bazel-mode . flycheck-mode))

(use-package protobuf-mode
  :mode "\\.proto$"
  :hook (protobuf-mode . flycheck-mode))

(use-package clang-format+
  :ensure clang-format
  :hook (mlir-mode . clang-format+-mode))

(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode 1)
  :hook (tree-sitter-after-on . #'tree-sitter-hl-mode))

(use-package adoc-mode
  :mode "\\.adoc\\'")

(defun sb/company-text-mode ()
  "Add backends for text completion in company mode."
  (set (make-local-variable 'company-backends)
       '((
          company-files
          company-yasnippet ; Works everywhere
          company-dabbrev
          company-ispell ; Uses an English dictionary
          ))))
(dolist (hook '(text-mode-hook)) ; Should extend to `markdown-mode' and `org-mode'
  (add-hook hook (lambda ()
                   (sb/company-text-mode))))

(defun sb/company-prog-mode ()
  "Add backends for program completion in company mode."
  (setq-local company-minimum-prefix-length 2)
  (make-local-variable 'company-backends)
  (setq company-backends
        '(
          company-capf ; Disabled LSP mode's capf autoconfiguration
          (company-keywords :with
                            company-files
                            :with
                            company-yasnippet)
          (company-dabbrev-code
           company-dabbrev)
          )))
(add-hook 'prog-mode-hook #'sb/company-prog-mode)

(defun sb/company-c-mode ()
  "Add backends for C/C++ completion in company mode."
  (setq-local company-minimum-prefix-length 2)
  (make-local-variable 'company-backends)
  (setq company-backends
        '((
           company-capf ; Disabled LSP mode's capf autoconfiguration
           company-dabbrev-code
           company-keywords
           company-yasnippet
           company-files
           ;; https://emacs.stackexchange.com/questions/19072/company-completion-very-slow
           ;; company-clang ; This can be slow
           ;; company-dabbrev
           ))))
(add-hook 'c-mode-common-hook #'sb/company-c-mode)

(defun sb/company-sh-mode ()
  "Add backends for shell script completion in company mode."
  (use-package company-shell
    :custom (company-shell-delete-duplictes t))
  (setq-local company-minimum-prefix-length 2)
  (make-local-variable 'company-backends)
  (setq company-backends
        '((
           company-capf ; LSP mode autoconfigures capf
           (company-shell :with
                          company-shell-env :with
                          company-fish-shell :with
                          company-yasnippet)
           company-files
           company-dabbrev-code
           ;; company-dabbrev
           ))))
(add-hook 'sh-mode-hook #'sb/company-sh-mode)

(defun sb/company-elisp-mode ()
  "Set up company for elisp mode."
  (setq-local company-minimum-prefix-length 2)
  (set (make-local-variable 'company-backends)
       '((
          company-yasnippet
          company-elisp
          company-files
          company-capf
          company-dabbrev-code
          ;; company-dabbrev
          ))))
(add-hook 'emacs-lisp-mode-hook #'sb/company-elisp-mode)

(defun sb/company-python-mode ()
  "Add backends for Python completion in company mode."
  ;; Make sure to install virtualenv through pip, and not the distribution package manager. Run
  ;; `jedi:install-sever'.
  (use-package company-jedi)
  (setq-local company-minimum-prefix-length 2)
  (make-local-variable 'company-backends)
  (setq company-backends
        '(
          company-capf
          company-jedi
          company-keywords
          company-yasnippet
          ;; company-tabnine
          company-files
          company-dabbrev-code
          ;; company-dabbrev
          )))
(add-hook 'python-mode-hook #'sb/company-python-mode)

(defun sb/company-latex-mode ()
  "Add backends for latex completion in company mode."
  ;; (use-package company-auctex)
  ;; (use-package math-symbol-lists) ; Required by `ac-math' and `company-math'
  ;; (use-package company-math)
  (use-package company-reftex)
  (use-package company-bibtex)
  (make-local-variable 'company-backends)
  (setq company-backends
        '((
           company-yasnippet
           company-capf
           ;; company-tabnine
           company-bibtex
           ;; company-math-symbols-latex
           ;; company-latex-commands
           ;; company-math-symbols-unicode
           company-reftex-labels
           company-reftex-citations
           ;; company-auctex-labels
           ;; company-auctex-bibs
           company-files
           company-dabbrev
           company-ispell
           ))))
(dolist (hook '(latex-mode-hook LaTeX-mode-hook))
  (add-hook hook #'sb/company-latex-mode))

;; https://andreyorst.gitlab.io/posts/2020-06-29-using-single-emacs-instance-to-edit-files/
;; (use-package server
;;   :if (not (string-equal "root" (getenv "USER"))) ; Only start server mode if not root
;;   :config (unless (server-running-p) (server-start)))

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
   (format "%s -f TAGS -eR %s" dotemacs-ctags-path (directory-file-name dir-name))))

(defun sb/create-gtags (dir-name)
  "Create tags file with gtags in DIR-NAME."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -cv --gtagslabel=new-ctags %s" dotemacs-gtags-path (directory-file-name dir-name))))

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
(defcustom sb/skippable-buffers
  '(
    "TAGS" "*Backtrace*"
    ;; "*Messages*" "*scratch*" "*Help*" "*Packages*" "*prettier (local)*" "*emacs*" "*Warnings*"
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
                                           magit-diff-mode)
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

(global-set-key [remap next-buffer] 'sb/next-buffer)
(global-set-key [remap previous-buffer] 'sb/previous-buffer)

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
  (let ((modes   ())
        (parent  nil))
    (while (setq parent (get mode 'derived-mode-parent))
      (push parent modes)
      (setq mode parent))
    (setq modes  (nreverse modes))))

;; Generic keybindings, package-specific are usually in their own modules. Use `C-h b' to see
;; available bindings in a buffer. Use `M-x describe-personal-keybindings' to see modifications.

;; `bind-key*', `bind*' overrides all minor mode bindings. The `kbd` macro is not required with
;; `bind-key' variants. With `bind-key', you do not need an explicit `(kbd ...)'.
;; Other variants:
;; (global-set-key (kbd "RET") 'newline-and-indent)
;; (define-key global-map (kbd "RET") 'newline-and-indent)

(bind-keys
 ("RET" . newline-and-indent)
 ("C-l" . goto-line)
 ("C-c z" . repeat)
 ("C-z" . undo)
 ("C-S-z" . redo)
 ("<f11>" . delete-other-windows)
 ("C-x k" . kill-this-buffer)
 ("M-<left>" . previous-buffer)
 ("M-<right>" . next-buffer)
 ("C-c d f" . auto-fill-mode)
 ("M-c" . capitalize-dwim)
 ("M-u" . upcase-dwim)
 ("M-l" . downcase-dwim)
 ("<f7>" . previous-error)
 ("<f8>" . next-error))

;; In a line with comments, `C-u M-;' removes the comments altogether. That means deleting the
;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
(bind-keys*
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box)
 ("C-s" . save-buffer)
 ("C-S-s" . sb/save-all-buffers))
(unbind-key "C-x s") ; Bound to save-some-buffers
(bind-key "C-x s" #'sb/switch-to-scratch)
(bind-key "C-x j" #'sb/counsel-all-files-recursively)

(use-package default-text-scale
  :bind
  (("C-M-+" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(defhydra sb/hydra-text-scale-zoom ()
  "zoom"
  ("i" default-text-scale-increase "in")
  ("o" default-text-scale-decrease "out"))

(use-package which-key ; Show help popups for prefix keys
  :diminish
  :hook (after-init . which-key-mode)
  :config (which-key-setup-side-window-right-bottom))

(use-package which-key-posframe
  :hook (which-key-mode . which-key-posframe-mode))

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
(put 'lsp-python-ms-extra-paths 'safe-local-variable #'listp)
(put 'projectile-enable-caching 'safe-local-variable #'stringp)
(put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
(put 'projectile-project-root 'safe-local-variable #'stringp)
(put 'pyvenv-activate 'safe-local-variable #'stringp)
(put 'reftex-default-bibliography 'safe-local-variable #'listp)
(put 'tags-table-list 'safe-local-variable #'listp)

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

;; (with-eval-after-load "counsel-projectile" (add-to-list 'counsel-projectile-action '("d"
;;   sb/open-project-default-file2 "open default file") t))

;; https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

;;; init.el ends here
