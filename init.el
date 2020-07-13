;;; init.el --- Emacs customization -*- lexical-binding: t; no-byte-compile: nil; -*-
;; Swarnendu Biswas

;;; Commentary:

;; To evaluate an Sexp, just go to the end of the sexp and type "C-x C-e", instead of evaluating the
;; whole buffer Use C-M-x to evaluate the current top-level s-expression. Use M-: to evaluate any
;; Emacs Lisp expression and print the result.

;; Init file should not ideally contain calls to "load" or "require", since they cause eager loading
;; and are expensive, a cheaper alternative is to use "autoload".

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Anonymous-Functions.html

;; When defining a lambda expression that is to be used as an anonymous function, you can in
;; principle use any method to construct the list. But typically you should use the lambda macro, or
;; the function special form, or the #' read syntax which is a short-hand for using function.
;; Quoting a lambda form means the anonymous function is not byte-compiled. The following forms are
;; all equivalent: (lambda (x) (* x x)) (function (lambda (x) (* x x))) #'(lambda (x) (* x x))

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
;; https://emacs.stackexchange.com/questions/27007/backward-quote-what-does-it-mean-in-elisp

;; Backquote constructs allow you to quote a list, but selectively evaluate elements of that list.
;; `(1 2 (3 ,(+ 4 5))) => (1 2 (3 9))

;; A local variable specification takes the following form:
;; -*- mode: MODENAME; VAR: VALUE; ... -*-

;;; Code:

(setq debug-on-error nil
      load-prefer-newer t
      user-full-name "Swarnendu Biswas")

(defgroup dotemacs nil
  "Personal configuration for dotemacs."
  :group 'local)

(defcustom dotemacs-temp-directory (concat user-emacs-directory "tmp/")
  "Storage location for various configuration files."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-extras-directory (concat user-emacs-directory "extras/")
  "Path for third-party packages and files."
  :type 'string
  :group 'dotemacs)

(unless (file-exists-p dotemacs-temp-directory)
  (make-directory dotemacs-temp-directory))

(defcustom dotemacs-emacs-custom-file (concat dotemacs-temp-directory "custom.el")
  "File to write Emacs customizations."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-theme
  'default
  "Specify which Emacs theme to use."
  :type '(radio
          (const :tag "eclipse" eclipse)
          (const :tag "leuven" leuven)
          (const :tag "professional" professional)
          (const :tag "solarized-light" solarized-light)
          (const :tag "spacemacs-light" spacemacs-light)
          (const :tag "solarized-dark" solarized-dark)
          (const :tag "tangotango" tangotango)
          (const :tag "zenburn" zenburn)
          (const :tag "doom-themes" doom-themes)
          (const :tag "monokai" monokai)
          (const :tag "default" default))
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
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-window-split
  'vertical
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type '(radio
          ;; Split into two windows one above the other
          (const :tag "vertical" vertical) ; split-window-below
          ;; Split into two side-by-side windows
          (const :tag "horizontal" horizontal)) ; split-window-right
  :group 'dotemacs)

(defconst dotemacs-fill-column 100
  "Column beyond which lines should not extend.")

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
          ;; (const :tag "gtags" gtags)
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

(defconst dotemacs-user-home
  (getenv "HOME")
  "User HOME directory.")

(eval-when-compile
  (require 'package)
  (setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
        ;; Avoid loading packages twice
        package-enable-at-startup nil)
  (package-initialize t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; (eval-after-load 'gnutls
;;   '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(defvar use-package-enable-imenu-support) ; Silence "assignment to free variable" warning
(setq use-package-enable-imenu-support t) ; Need to set before loading use-package
(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer t
      use-package-compute-statistics nil ; Use "M-x use-package-report" to see results
      ;; Avoid printing error and warning code, use if the configuration is known to work
      use-package-expand-minimally nil)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package bind-key
  :ensure t
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish
  :ensure t)

(use-package paradox
  :ensure t
  :bind (("C-c d l" . paradox-list-packages)
         ("C-c d u" . paradox-upgrade-packages))
  :custom
  (paradox-display-star-count nil)
  (paradox-execute-asynchronously t)
  (paradox-github-token t)
  :config (paradox-enable))

(use-package cus-edit
  :custom (custom-file dotemacs-emacs-custom-file)
  :config
  (when (file-exists-p custom-file)
    (load custom-file :noerror)))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(x ns))
  :custom (exec-path-from-shell-check-startup-files nil) ; Ignore definition check
  :init (exec-path-from-shell-initialize))

(setq ad-redefinition-action 'accept ; Turn off warnings due to functions being redefined
      auto-mode-case-fold nil ; Disable a second case insensitive pass
      auto-save-list-file-prefix (expand-file-name "auto-save" dotemacs-temp-directory)
      backup-inhibited t ; Disable backup for a per-file basis, not to be used by major modes
      blink-matching-paren nil ; Distracting
      case-fold-search t ; Searches and matches should ignore case
      completion-ignore-case t ; Ignore case when completing
      confirm-kill-emacs nil
      confirm-kill-processes nil ;; Prevent "Active processes exist" when you quit Emacs
      confirm-nonexistent-file-or-buffer t
      create-lockfiles nil
      custom-safe-themes t
      delete-by-moving-to-trash t ; Use system trash to deal with mistakes
      enable-recursive-minibuffers t
      find-file-visit-truename t ; Show true name, useful in case of symlinks
      frame-title-format (list '(buffer-file-name "%f" "%b"))
      gc-cons-percentage 0.5 ; Portion of heap used for allocation
      ;; GC may happen after this many bytes are allocated since last GC
      gc-cons-threshold (* 200 1024 1024)
      history-delete-duplicates t
      indicate-buffer-boundaries nil
      inhibit-compacting-font-caches t ; Do not compact font caches during GC
      ;; Disable loading of "default.el" at startup, inhibits site default settings
      inhibit-default-init t
      ;; *scratch* is in Lisp interaction mode by default, use text mode instead
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t ; inhibit-splash-screen is an alias
      initial-major-mode 'text-mode
      initial-scratch-message nil
      kill-do-not-save-duplicates t
      kill-whole-line t
      major-mode 'text-mode ; Major mode to use for files that do no specify a major mode
      make-backup-files nil ; Stop making backup ~ files
      mouse-drag-copy-region t
      mouse-yank-at-point t ; Yank at point instead of at click
      pop-up-frames nil ; Avoid making separate frames
      ;; pop-up-windows nil ; Disallow creating new windows
      read-buffer-completion-ignore-case t ; Ignore case when reading a buffer name
      read-file-name-completion-ignore-case t ; Ignore case when reading a file name completion
      read-process-output-max (* 1024 1024) ; 1 MB
      require-final-newline t ; Always end a file with a newline.
      ring-bell-function 'ignore ; Disable beeping sound
      save-interprogram-paste-before-kill t
      select-enable-clipboard t ; Enable use of system clipboard across Emacs and other applications
      sentence-end-double-space nil
      set-mark-command-repeat-pop t
      shift-select-mode t ; Use shift-select for marking
      suggest-key-bindings t
      switch-to-buffer-preserve-window-point t
      truncate-lines nil
      truncate-partial-width-windows nil
      use-dialog-box nil
      use-file-dialog nil
      vc-handled-backends nil
      visible-bell nil
      ;; Do not use system tooltips
      x-gtk-use-system-tooltips nil)

(setq-default fill-column dotemacs-fill-column
              indent-tabs-mode nil ; Spaces instead of tabs by default
              indicate-empty-lines t
              standard-indent 2
              tab-always-indent 'complete
              tab-width 4
              ;; Doom Emacs: Disable bidirectional text rendering for a modest performance boost
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Ideally, we would have reset 'gc-cons-threshold' to its default value otherwise there can be
;; large pause times whenever GC eventually happens. But lsp suggests increasing the limit
;; permanently.
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold 800000)))

(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(fset 'display-startup-echo-area-message #'ignore)
(fset 'yes-or-no-p 'y-or-n-p) ; Type "y"/"n" instead of "yes"/"no"

(column-number-mode 1)
(diminish 'auto-fill-function) ; Not a library/file, so eval-after-load does not work
(global-prettify-symbols-mode -1) ; Makes it difficult to edit the buffer
(minibuffer-depth-indicate-mode 1)
(transient-mark-mode 1) ; Enable visual feedback on selections, default since v23

(use-package autorevert ; Auto-refresh all buffers, does not work for remote files
  :diminish auto-revert-mode
  :hook ((after-init . global-auto-revert-mode)
         (dired-mode . auto-revert-mode)) ; Auto refresh dired when files change
  :custom
  (auto-revert-verbose nil)
  ;; Enable auto revert on other buffers like dired
  (global-auto-revert-non-file-buffers t))

(use-package delsel ; Typing with the mark active will overwrite the marked region
  :hook (after-init . delete-selection-mode))

(use-package saveplace ; Remember cursor position in files
  :unless noninteractive
  :hook (after-init . save-place-mode)
  :custom (save-place-file (expand-file-name "places" dotemacs-temp-directory)))

(use-package savehist ; Save minibuffer histories across sessions
  :unless noninteractive
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history
                                   file-name-history))
  (savehist-autosave-interval 300)
  (savehist-file (expand-file-name "savehist" dotemacs-temp-directory))
  (savehist-save-minibuffer-history t))

;; (use-package uniquify
;; :custom
;; (uniquify-after-kill-buffer-p t)
;; (uniquify-buffer-name-style 'forward)
;; (uniquify-ignore-buffers-re "^\\*")
;; (uniquify-separator "/")
;; (uniquify-strip-common-suffix t))

;; https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el
(use-package hippie-exp
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-expand-all-abbrevs
                                      try-complete-file-name
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol))
  :bind ("M-/" . hippie-expand))

(use-package subword
  :diminish
  :hook (after-init . global-subword-mode))

;; vertical - Split the selected window into two windows, one above the other (split-window-below)
;; horizontal - Split the selected window into two side-by-side windows (split-window-right)
(cond ((eq dotemacs-window-split 'vertical) (setq split-width-threshold nil
                                                  split-height-threshold 0))
      ((eq dotemacs-window-split 'horizontal) (setq split-height-threshold nil
                                                    split-width-threshold 0)))
;; Magit is creating new frames
;; (setq split-height-threshold (* (window-height) 10)
;;       split-width-threshold (* (window-width) 10))

;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun my-auto-save-wrapper (save-fn &rest args)
  "Hide 'Auto-saving...done' messages by calling the method SAVE-FN with non-nil ARGS."
  (ignore args)
  (apply save-fn '(t)))
(advice-add 'do-auto-save :around #'my-auto-save-wrapper)

(use-package abbrev
  :diminish
  :hook ((text-mode prog-mode) . abbrev-mode)
  :custom
  (abbrev-file-name (expand-file-name "abbrev-defs" dotemacs-extras-directory))
  (save-abbrevs 'silently))

(when (display-graphic-p) ; window-system is deprecated
  (progn
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)))

;; (add-hook 'after-make-frame-functions
;;           (lambda ()
;;             (when (display-graphic-p)
;;               (tool-bar-mode -1))))

;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)

(tooltip-mode -1)
(blink-cursor-mode -1) ; Blinking cursor is distracting
(global-visual-line-mode 1)
(diminish 'visual-line-mode)
(size-indication-mode -1)
(toggle-frame-maximized) ; Maximize Emacs on startup
(set-frame-parameter nil 'unsplittable t)
(fringe-mode '(0 . 0))

(cond ((eq dotemacs-theme 'leuven) (use-package leuven-theme
                                     :ensure t
                                     :init (load-theme 'leuven t)))

      ((eq dotemacs-theme 'professional) (use-package professional-theme
                                           :ensure t
                                           :init (load-theme 'professional t)))

      ((eq dotemacs-theme 'eclipse) (use-package eclipse-theme
                                      :ensure t
                                      :init
                                      (load-theme 'eclipse t)
                                      (set-background-color "white")
                                      (set-face-attribute 'region nil
                                                          :background "LemonChiffon"
                                                          :foreground "black")
                                      (set-face-attribute 'mode-line nil
                                                          :background "grey88"
                                                          :foreground "black"
                                                          :box nil)))

      ((eq dotemacs-theme 'spacemacs-light) (use-package spacemacs-common
                                              :ensure spacemacs-theme
                                              :init
                                              (load-theme 'spacemacs-light t)
                                              ;; (add-to-list 'default-frame-alist '(background-color . "#fbf8ef"))
                                              ))

      ((eq dotemacs-theme 'zenburn) (use-package zenburn-theme
                                      :ensure t
                                      :init (load-theme 'zenburn t)))

      ((eq dotemacs-theme 'solarized-light) (use-package solarized-light-theme
                                              :ensure solarized-theme
                                              :init
                                              (load-theme 'solarized-light t)
                                              (setq solarized-distinct-fringe-background t)))

      ((eq dotemacs-theme 'solarized-dark) (use-package solarized-dark-theme
                                             :ensure solarized-theme
                                             :init (load-theme 'solarized-dark t)))

      ((eq dotemacs-theme 'tangotango) (use-package tangotango-theme
                                         :ensure t
                                         :init (load-theme 'tangotango t)))

      ((eq dotemacs-theme 'doom-themes) (use-package doom-themes
                                          :ensure t
                                          :init (load-theme 'doom-monokai-classic t)
                                          :config
                                          (set-face-attribute 'font-lock-comment-face nil
                                                              ;; :foreground "#cccccc"
                                                              ;; :foreground "#b2b2b2"
                                                              :foreground "#999999")))

      ((eq dotemacs-theme 'monokai) (use-package monokai-theme
                                      :ensure t
                                      :init (load-theme 'monokai t)))

      ((eq dotemacs-theme 'default) (progn
                                      (setq frame-background-mode 'light)
                                      (set-background-color "#ffffff")
                                      (set-foreground-color "#666666")
                                      (set-face-attribute 'region nil
                                                          :background "light sky blue"
                                                          :foreground "white"))))

(cond ((eq dotemacs-modeline-theme 'powerline) (use-package powerline
                                                 :ensure t
                                                 :init
                                                 (setq powerline-display-mule-info nil
                                                       powerline-display-buffer-size t
                                                       powerline-display-hud nil
                                                       powerline-gui-use-vcs-glyph t)
                                                 (set-face-attribute 'powerline-inactive1 nil
                                                                     :background "gray40"
                                                                     :foreground "white"
                                                                     :weight 'light)
                                                 (set-face-attribute 'powerline-inactive2 nil
                                                                     :background "grey50"
                                                                     :foreground "white")
                                                 (when (eq dotemacs-theme 'leuven)
                                                   (set-face-attribute 'mode-line nil
                                                                       :background "grey88"
                                                                       :foreground "black")
                                                   (set-face-attribute 'mode-line-buffer-id nil
                                                                       :weight 'bold
                                                                       :foreground "black"
                                                                       :background "gray88")
                                                   (powerline-default-theme))))

      ((eq dotemacs-modeline-theme 'sml) (use-package smart-mode-line
                                           :ensure t
                                           :init
                                           (setq sml/theme 'light
                                                 sml/no-confirm-load-theme t
                                                 sml/mode-width 'full
                                                 sml/shorten-modes t
                                                 sml/shorten-directory t)
                                           (sml/setup)))

      ((eq dotemacs-modeline-theme 'spaceline) (use-package spaceline
                                                 :ensure t
                                                 :defines (spaceline-hud-p spaceline-selection-info-p spaceline-version-control-p spaceline-input-method-p spaceline-persp-name-p)
                                                 :init
                                                 (require 'spaceline-config)
                                                 (setq spaceline-hud-p nil
                                                       spaceline-selection-info-p nil
                                                       spaceline-version-control-p t
                                                       spaceline-input-method-p nil
                                                       spaceline-persp-name-p nil)
                                                 (set-face-attribute 'powerline-inactive1 nil
                                                                     :background "gray40"
                                                                     :foreground "white"
                                                                     :weight 'light)
                                                 (set-face-attribute 'powerline-inactive2 nil
                                                                     :background "grey50"
                                                                     :foreground "white")
                                                 (when (eq dotemacs-theme 'leuven)
                                                   (set-face-attribute 'powerline-active1 nil
                                                                       :background "gray22"
                                                                       :foreground "white"
                                                                       :weight 'light)
                                                   (set-face-attribute 'mode-line-inactive nil
                                                                       :background "grey88"
                                                                       :foreground "black"))
                                                 (spaceline-emacs-theme)))

      ((eq dotemacs-modeline-theme 'airline) (use-package airline-themes
                                               :ensure t
                                               :init
                                               (require 'airline-themes)
                                               (setq airline-hide-eyebrowse-on-inactive-buffers t
                                                     airline-eshell-colors nil)
                                               (load-theme 'airline-cool t)))

      ((eq dotemacs-modeline-theme 'doom-modeline) (use-package doom-modeline
                                                     :ensure t
                                                     :init (doom-modeline-mode 1)
                                                     :custom
                                                     (doom-modeline-buffer-encoding nil)
                                                     (doom-modeline-indent-info t)
                                                     (doom-modeline-minor-modes t)))

      ((eq dotemacs-modeline-theme 'default)))

(use-package awesome-tray
  :if (eq dotemacs-modeline-theme 'default)
  :load-path "extras"
  :hook (after-init . awesome-tray-mode)
  :custom (awesome-tray-active-modules '("buffer-name" "location" "file-path" "mode-name" "git")))

(use-package auto-dim-other-buffers
  :ensure t
  :hook (after-init . auto-dim-other-buffers-mode))

;; Value is in 1/10pt, so 100 will give you 10pt
;; (set-frame-font "DejaVu Sans Mono" nil t)
(set-frame-font "Roboto Mono")
(set-face-attribute 'default nil :height 140)
(set-face-attribute 'mode-line nil :height 110)

(use-package ibuffer
  :custom
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-display-summary nil)
  (ibuffer-use-header-line t)
  :config (defalias 'list-buffers 'ibuffer))

(use-package ibuf-ext ; Don't show filter groups if there are no buffers in that group
  :load-path "extras"
  :custom (ibuffer-show-empty-filter-groups nil)
  :hook (ibuffer . ibuffer-auto-mode))

(use-package ibuffer-projectile ; Group buffers by projectile project
  :ensure t
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

(use-package dired
  :preface
  (defun dired-go-home ()
    (interactive)
    (dired dotemacs-user-home))
  (defun dired-jump-to-top ()
    (interactive)
    (goto-char (point-min)) ; Faster than (beginning-of-buffer)
    (dired-next-line 2))
  (defun dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max)) ; Faster than (end-of-buffer)
    (dired-next-line -1))
  :bind (:map dired-mode-map
              ("M-<home>" . dired-go-home)
              ("i" . find-file)
              ("M-<up>" . dired-jump-to-top)
              ("M-<down>" . dired-jump-to-bottom))
  :custom
  (dired-auto-revert-buffer t) ; Revert each dired buffer automatically when you "revisit" it
  (dired-dwim-target t) ; Guess a default target directory for copy, rename, etc.
  ;; Check "ls" for additional options
  (dired-listing-switches "-ABhl --si --group-directories-first")
  (dired-ls-F-marks-symlinks t) ; -F marks links with @
  ;; Single prompt for all n directories
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

(use-package dired-x
  :custom
  (dired-omit-verbose nil) ; Do not show messages when omitting files
  :hook (dired-mode . dired-omit-mode)
  ;; :config
  ;; ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  ;; (defadvice dired-omit-startup (after diminish-dired-omit activate)
  ;;   "Make sure to remove \"Omit\" from the modeline."
  ;;   (diminish 'dired-omit-mode) dired-mode-map)
  :bind ("C-x C-j" . dired-jump))

(use-package dired+ ; Do not create multiple dired buffers
  :load-path "extras"
  :after dired
  :init
  (setq diredp-hide-details-initially-flag nil
        diredp-hide-details-propagate-flag nil)
  :config (diredp-toggle-find-file-reuse-dir 1))

(use-package dired-efap
  :ensure t
  :custom (dired-efap-initial-filename-selection nil)
  :bind (:map dired-mode-map
              ("r" . dired-efap)))

(use-package dired-narrow ; Narrow dired to match filter
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package treemacs
  :ensure t
  :commands (treemacs treemacs-toggle)
  :hook ((projectile-mode . treemacs-follow-mode)
         (projectile-mode . treemacs-filewatch-mode)
         ;; (projectile-mode . treemacs-fringe-indicator-mode)
         )
  :custom (treemacs-persist-file (concat dotemacs-temp-directory "treemacs-persist"))
  :config
  (setq treemacs-follow-after-init t
        treemacs-width 20
        treemacs-lock-width t
        treemacs-indentation 2
        treemacs-position 'right
        treemacs-collapse-dirs 3
        ;; treemacs-sorting 'alphabetic-desc
        treemacs-show-hidden-files nil
        treemacs-project-follow-cleanup t
        ;; Prevents treemacs from being selected with `other-window`
        treemacs-is-never-other-window nil
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-recenter-after-file-follow t
        treemacs-recenter-after-tag-follow  t
        ;; Do not log messages
        treemacs-silent-filewatch t
        treemacs-silent-refresh t
        treemacs-tag-follow-delay 1
        treemacs-tag-follow-cleanup t)

  ;; Effectively overrides treemacs-follow-mode, but is a bit noisy
  ;; (treemacs-tag-follow-mode 1)
  (treemacs-git-mode 'extended)

  ;; Decrease the font size
  (set-face-attribute 'treemacs-directory-collapsed-face nil
                      :height 0.7)
  (set-face-attribute 'treemacs-directory-face nil
                      :height 0.7)
  (set-face-attribute 'treemacs-file-face nil
                      :height 0.7)
  (set-face-attribute 'treemacs-root-face nil
                      :height 0.9)
  (set-face-attribute 'treemacs-tags-face nil
                      :height 0.7)
  (set-face-attribute 'treemacs-git-ignored-face nil
                      :height 0.7)
  (set-face-attribute 'treemacs-git-untracked-face nil
                      :height 0.7)

  (treemacs-resize-icons 16)
  :bind* ("C-j" . treemacs))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package all-the-icons ; Install fonts with `M-x all-the-icons-install-fonts`
  :ensure t)

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :custom (all-the-icons-ibuffer-icon-size 0.8))

(use-package all-the-icons-dired
  :ensure t
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode))

;; Use "C-'" in isearch-mode-map to use avy-isearch to select one of the currently visible isearch
;; candidates.
(use-package isearch
  :custom (search-highlight t) ; Highlight incremental search
  :bind (("C-s" . nil) ; isearch-forward-regexp
         ("C-f" . isearch-forward-regexp)
         :map isearch-mode-map
         ("C-s" . nil) ; isearch-repeat-forward
         ("C-f" . isearch-repeat-forward)))

(use-package isearch-symbol-at-point
  :ensure t)

(use-package isearch-dabbrev
  :ensure t
  :bind (:map isearch-mode-map
              ("<tab>" . isearch-dabbrev-expand)))

(use-package swiper
  :ensure t
  :custom (swiper-action-recenter t))

(use-package wgrep ; Writable grep
  :ensure t
  :custom (wgrep-auto-save-buffer t))

(use-package deadgrep
  :ensure t
  :bind ("<f8>" . deadgrep))

;; We use a different command/keybinding to lookup recent directories.
(use-package recentf
  :custom
  (recentf-save-file (expand-file-name "recentf" dotemacs-temp-directory))
  (recentf-auto-cleanup 'never) ; Do not stat remote files
  (recentf-menu-filter 'recentf-sort-descending)
  ;; Check regex with re-builder
  (recentf-exclude '("[/\\]elpa/"
                     "[/\\]\\.git/"
                     ".*\\.gz\\'"
                     ".*-autoloads.el\\'"
                     "[/\\]archive-contents\\'"
                     "[/\\]\\.loaddefs\\.el\\'"
                     "[/\\]tmp/.*"
                     ".*/recentf\\'"
                     "~$"
                     "/.autosaves/"
                     ".*-loaddefs.el"
                     "/TAGS$"))
  :config (run-at-time nil 300 'recentf-save-list) ; Save every 300 s
  :hook (after-init . recentf-mode))

;; Hide the "Wrote to recentf" message which is irritating
(defun sb/recentf-save-list (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN."
  (let ((inhibit-message t))
    (apply orig-fun args)))
(advice-add 'recentf-save-list :around #'sb/recentf-save-list)

;; Use "M-x company-diag" or the modeline status to see the backend used
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil) ; Do not downcase returned candidates
  (company-dabbrev-ignore-case nil)
  (company-idle-delay 0.0) ; Recommended by lsp
  (company-ispell-available t)
  (company-ispell-dictionary (expand-file-name "wordlist" dotemacs-extras-directory))
  (company-minimum-prefix-length 2) ; Small words are faster to type
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-show-numbers t) ; Speed up completion
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("<tab>" . company-complete-common-or-cycle)))

(use-package company-flx
  :ensure t
  :hook (global-company-mode . company-flx-mode))

(use-package company-box
  :ensure t
  :disabled t
  :diminish
  :defines company-box-icons-all-the-icons
  :hook (global-company-mode . company-box-mode)
  :custom
  ;; (company-box-backends-colors nil)
  (company-box-show-single-candidate t)
  (company-box-max-candidates 50)
  (company-box-doc-delay 0.2))

;; (dolist (hook '(text-mode-hook markdown-mode-hook))
;;   (add-hook hook
;;             (lambda ()
;;               (make-local-variable 'company-backends)
;;               (setq company-backends '(company-capf
;;                                        company-files
;;                                        company-dabbrev
;;                                        company-ispell)))))

;; (dolist (hook '(latex-mode-hook LaTeX-mode-hook plain-tex-mode-hook))
;;   (add-hook hook
;;             (lambda ()
;;               (use-package company-bibtex
;;                 :ensure t
;;                 :demand t)

;;               (set (make-local-variable 'company-backends) '((company-capf
;;                                                               :with company-bibtex
;;                                                               company-dabbrev :separate
;;                                                               company-files))))))

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (make-local-variable 'company-backends)
;;             (setq company-backends '(company-capf
;;                                      (company-dabbrev-code
;;                                       company-clang
;;                                       company-keywords)
;;                                      company-dabbrev
;;                                      company-files))))

;; (add-hook 'sh-mode-hook
;;           (lambda ()
;;             (progn
;;               (use-package company-shell
;;                 :ensure t
;;                 :custom (company-shell-delete-duplicates t))

;;               (make-local-variable 'company-backends)
;;               (setq company-backends '(company-capf
;;                                        (company-shell
;;                                         company-shell-env
;;                                         company-fish-shell)
;;                                        company-dabbrev-code
;;                                        company-dabbrev
;;                                        company-files
;;                                        company-keywords)))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (after-init . yas-global-mode)
  :custom (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (unbind-key "<tab>" yas-minor-mode-map)
  (use-package yasnippet-snippets))

(use-package amx
  :ensure t
  :hook (after-init . amx-mode)
  :custom
  (amx-history-length 20)
  (amx-save-file (expand-file-name "amx-items" dotemacs-temp-directory)))

(use-package ivy
  :ensure t
  :custom
  (ivy-case-fold-search 'always) ; Always ignore case while searching
  (ivy-count-format "(%d/%d) ") ; This is beneficial to identify wrap around
  (ivy-extra-directories nil) ; Hide "." and ".."
  (ivy-fixed-height-minibuffer t) ; It is distracting if the mini-buffer height keeps changing
  ;; Make the height of the minibuffer proportionate to the screen
  (ivy-height-alist '((t
                       lambda (_caller)
                       (/ (frame-height) 2))))
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-wrap t)
  (completion-in-region-function #'ivy-completion-in-region)
  :hook (after-init . ivy-mode)
  :config
  (defalias 'wgrep-change-to-wgrep-mode 'ivy-wgrep-change-to-wgrep-mode)
  (defalias 'occur 'ivy-occur)
  :config
  (dolist (buffer '("TAGS" "magit-process"))
    (add-to-list 'ivy-ignore-buffers buffer))
  :bind
  (("C-c r" . ivy-resume)
   ([remap switch-to-buffer] . ivy-switch-buffer)
   ("<f3>" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("C-'" . ivy-avy)
   ("<return>" . ivy-alt-done) ; Continue completion
   ("<left>" . ivy-previous-line)
   ("<right>" . ivy-next-line))
  :diminish)

(use-package counsel
  :ensure t
  :ensure amx
  :ensure-system-package fasd
  :preface
  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
  (defun sb/counsel-goto-recent-directory ()
    "Open recent directories with dired."
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
   ("<f1>" . counsel-M-x)
   ([remap completion-at-point] . counsel-company)
   ([remap describe-bindings] . counsel-descbinds)
   ([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap dired] . counsel-dired)
   ("C-x f" . counsel-file-jump) ; Jump to a file below the current directory
   ([remap find-file] . counsel-find-file)
   ("<f2>" . counsel-find-file)
   ([remap flycheck-list-errors] . counsel-flycheck)
   ;; Shows only the first 200 results, use "C-c C-o" to save all the matches to a buffer
   ("C-c s g" . counsel-git-grep)
   ("C-<f9>" . sb/counsel-goto-recent-directory)
   ([remap swiper] . counsel-grep-or-swiper)
   ("<f4>" . counsel-grep-or-swiper)
   ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
   ([remap load-library] . counsel-load-library)
   ([remap load-theme] . counsel-load-theme)
   ([remap recentf-open-files] . counsel-recentf)
   ("<f9>" . counsel-recentf)
   ("C-c s r" . counsel-rg)
   ("C-c C-m" . counsel-mark-ring)
   ("C-c C-j" . counsel-semantic-or-imenu)
   ([remap yank-pop] . counsel-yank-pop))
  :custom
  (counsel-find-file-ignore-regexp (concat
                                    "\\(?:\\`[#.]\\)" ; File names beginning with # or .
                                    "\\|\\(?:\\`.+?[#~]\\'\\)" ; File names ending with # or ~
                                    "\\|__pycache__"
                                    "\\|.aux$"
                                    "\\|.bbl$"
                                    "\\|.blg$"
                                    "\\|.cb$"
                                    "\\|.cb2$"
                                    "\\|.docx$"
                                    "\\|.dvi$"
                                    "\\|.elc$"
                                    "\\|.fdb_latexmk$"
                                    "\\|.fls$"
                                    "\\|.jar$"
                                    "\\|.lof$"
                                    "\\|.lot$"
                                    "\\|.o$"
                                    "\\|.out$"
                                    "\\|.pdf$"
                                    "\\|.pptx$"
                                    "\\|.pyc$"
                                    "\\|.rel$"
                                    "\\|.rip$"
                                    "\\|.so$"
                                    "\\|.synctex$"
                                    "\\|.synctex.gz$"
                                    "\\|.tar.gz$"
                                    "\\|.toc$"
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
  (counsel-yank-pop-separator "\n-------------------------\n")
  :hook (ivy-mode . counsel-mode)
  :config (defalias 'flycheck-list-errors 'counsel-flycheck)
  :diminish)

(use-package prescient
  :ensure t
  :hook (counsel-mode . prescient-persist-mode)
  :custom (prescient-save-file (expand-file-name "prescient-save.el" dotemacs-temp-directory)))

(use-package ivy-prescient
  :ensure t
  :hook (counsel-mode . ivy-prescient-mode))

(use-package company-prescient
  :ensure t
  :hook (company-mode . company-prescient-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :custom (all-the-icons-ivy-rich-icon-size 0.8))

;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ivy.el#L449
(use-package ivy-rich
  :ensure t
  :custom
  (ivy-format-function #'ivy-format-function-line)
  (ivy-rich-parse-remote-buffer nil)
  :hook (ivy-mode . ivy-rich-mode))

(use-package flyspell
  :if (eq system-type 'gnu/linux)
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
  (ispell-local-dictionary "en_US")
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary (expand-file-name "spell" dotemacs-extras-directory))
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90"))
  (ispell-silently-savep t) ; Save a new word to personal dictionary without asking
  (flyspell-issue-message-flag nil)
  :hook ((prog-mode . flyspell-prog-mode)
         (before-save-hook . flyspell-buffer)
         ((text-mode find-file-hooks) . flyspell-mode))
  :diminish
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   ("C-c f w" . ispell-word)
   :map flyspell-mode-map
   ("C-;" . nil)
   ("C-," . sb/flyspell-goto-previous-error)))

(use-package flyspell-popup
  :ensure t
  :bind ("C-;" . flyspell-popup-correct)
  :custom (flyspell-popup-correct-delay 0.5))

(use-package highlight-indentation
  :ensure t
  :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
  :hook (python-mode . highlight-indentation-mode))

;; Claims to be better than electric-indent-mode
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-edit.el
(use-package aggressive-indent
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode) . aggressive-indent-mode)
  :custom (aggressive-indent-dont-electric-modes t)
  :diminish)

(electric-pair-mode 1) ; Enable autopairing, smartparens seems slow

(use-package paren
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed) ; Options: 'expression, 'parenthesis, 'mixed
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; FIXME: Seems to have performance issue with latex-mode and markdown-mode.
;; "sp-cheat-sheet" will show you all the commands available, with examples.
(use-package smartparens
  :ensure t
  :disabled t
  :hook ((after-init . smartparens-global-mode)
         (after-init . show-smartparens-global-mode))
  :custom
  (sp-show-pair-from-inside t)
  (sp-autoskip-closing-pair 'always)
  :config
  (require 'smartparens-config)
  (dolist (hook '(latex-mode-hook LaTeX-mode-hook plain-tex-mode-hook))
    (add-hook hook
              (lambda ()
                (require 'smartparens-latex))))
  :bind (("C-M-a" . sp-beginning-of-sexp) ; "foo ba_r" -> "_foo bar"
         ("C-M-e" . sp-end-of-sexp) ; "f_oo bar" -> "foo bar_"
         ("C-M-u" . sp-up-sexp) ; "f_oo bar" -> "foo bar"_
         ("C-M-w" . sp-down-sexp) ; "foo ba_r" -> "_foo bar"
         ("C-M-f" . sp-forward-sexp) ; "foo ba_r" -> "foo bar"_
         ("C-M-b" . sp-backward-sexp) ; "foo ba_r" -> "_foo bar"
         ("C-M-n" . sp-next-sexp) ; ((fo|o) (bar)) -> ((foo) |(bar))"
         ("C-M-p" . sp-previous-sexp) ; (foo (b|ar baz)) -> (foo| (bar baz))"
         ("C-S-b" . sp-backward-symbol) ; foo bar| baz -> foo |bar baz
         ("C-S-f" . sp-forward-symbol) ; |foo bar baz -> foo| bar baz
         ;; (foo bar) -> foo bar
         ("C-M-k" . sp-splice-sexp))
  :diminish)

(use-package projectile
  :ensure t
  :ensure-system-package fd
  :custom
  (projectile-auto-discover nil)
  (projectile-cache-file (expand-file-name "projectile.cache" dotemacs-temp-directory))
  (projectile-completion-system 'ivy)
  ;; (projectile-enable-caching t) ; Problematic since I often create new files
  (projectile-file-exists-remote-cache-expire nil)
  ;; Contents of .projectile are ignored when using the alien or hybrid indexing method
  (projectile-indexing-method 'hybrid)
  (projectile-known-projects-file (expand-file-name "projectile-known-projects.eld" dotemacs-temp-directory))
  (projectile-mode-line-prefix "")
  ;; Use projectile only in desired directories, too much noise otherwise
  (projectile-require-project-root t)
  ;; (projectile-sort-order 'recently-active)
  (projectile-verbose nil)
  :config
  (defun projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      (format "%s [%s]"
              projectile-mode-line-prefix
              (or project-name "-"))))
  (projectile-mode 1)
  ;; Avoid search when projectile-mode is enabled for faster startup
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
                 (expand-file-name dotemacs-user-home) ; Do not consider the HOME as a project
                 (expand-file-name "bitbucket/.metadata" dotemacs-user-home)
                 (expand-file-name "github/.metadata" dotemacs-user-home)
                 (expand-file-name "iitk-workspace/.metadata" dotemacs-user-home)
                 (expand-file-name "plass-workspace/.metadata" dotemacs-user-home)
                 ))
    (add-to-list 'projectile-ignored-projects prjs))
  (dolist (dirs '(".cache" ".clangd" ".dropbox" ".git" ".hg" ".metadata" ".nx" ".recommenders" ".svn"
                  ".vscode" "__pycache__" "auto" "elpa"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  (dolist (items '("GPATH" "GRTAGS" "GTAGS" "GSYMS"  "TAGS" "tags" ".dir-locals.el" ".projectile"
                   ".project" ".tags" "__init__.py"))
    (add-to-list 'projectile-globally-ignored-files items))
  (dolist (exts '(".a" ".aux" ".bak" ".blg" ".class" ".elc" ".jar" ".o" ".out" ".pdf" ".pt" ".pyc"
                  ".rel" ".rip" ".tar.gz" "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes exts))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :hook (counsel-mode . counsel-projectile-mode)
  :custom (projectile-switch-project-action 'counsel-projectile-find-file)
  :bind (("<f5>" . counsel-projectile-switch-project)
         ("<f6>" . counsel-projectile-find-file)
         ("<f7>" . counsel-projectile-rg)))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'left-margin)
  :config
  (when (or (eq dotemacs-modeline-theme 'spaceline) (eq dotemacs-modeline-theme 'doom-modeline))
    (setq flycheck-mode-line nil))
  (setq-default flycheck-disabled-checkers '(tex-lacheck python-flake8 emacs-lisp-checkdoc))
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local flycheck-checker 'python-pylint
                          flycheck-python-pylint-executable "python3"
                          flycheck-pylintrc (concat dotemacs-user-home "/.config/pylintrc"))))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local flycheck-checker
                          'markdown-markdownlint-cli
                          flycheck-markdown-markdownlint-cli-config (concat dotemacs-user-home
                                                                            "/.markdownlint.json"))))
  (add-hook 'sh-mode-hook
            (lambda ()
              (setq-local flycheck-checker 'sh-shellcheck))))

;; Binds avy-flycheck-goto-error to C-c ! g
(use-package avy-flycheck
  :ensure t
  :after flycheck
  :config (avy-flycheck-setup))

(use-package flycheck-popup-tip ; Show error messages in popups
  :ensure t
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package whitespace
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  :custom
  (show-trailing-whitespace nil)
  (whitespace-line-column dotemacs-fill-column))

;; This is different from whitespace-cleanup since this is unconditional
(when (bound-and-true-p dotemacs-delete-trailing-whitespace-p)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package ws-butler ; Unobtrusively trim extraneous white-space *ONLY* in lines edited
  :ensure t
  :if (not (bound-and-true-p dotemacs-delete-trailing-whitespace-p))
  :diminish
  :hook (prog-mode . ws-butler-mode))

(use-package highlight-symbol ; Highlight symbol under point
  :ensure t
  :hook (prog-mode . highlight-symbol-mode)
  :bind (("M-p" . highlight-symbol-prev)
         ("M-n" . highlight-symbol-next))
  :custom (highlight-symbol-on-navigation-p t)
  :diminish)

(use-package hl-todo
  :ensure t
  :hook (after-init . global-hl-todo-mode))

;; Edit remote file: /method:user@host#port:filename.
;; Shortcut /ssh:: will connect to default user@host#port.
;; Edit local file with sudo: C-x C-f /sudo::/etc/hosts
;; Open a remote file with ssh + sudo: C-x C-f /ssh:host|sudo:root:/etc/passwd
(use-package tramp
  :custom
  (tramp-default-method "ssh") ; ssh is faster than the default scp
  ;; Auto-save to a local directory for better performance
  (tramp-auto-save-directory (expand-file-name "tramp-auto-save" dotemacs-temp-directory))
  (tramp-persistency-file-name (expand-file-name "tramp" dotemacs-temp-directory))
  (tramp-verbose 1) ; Default is 3
  (remote-file-name-inhibit-cache nil) ; Remote files are not updated outside of Tramp
  (tramp-completion-reread-directory-timeout nil)
  ;; Disable version control
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp
                                tramp-file-name-regexp))
  :config
  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
  (add-to-list 'tramp-default-method-alist '("172.27.15.105" "swarnendu" "ssh"))
  (setenv "SHELL" "/bin/bash") ; Recommended to connect with bash
  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

;; FIXME: Does not pick up other usernames.
(use-package counsel-tramp
  :ensure t
  :bind ("C-c d t" . counsel-tramp)
  :config
  (defalias 'tramp 'counsel-tramp)
  (add-hook 'counsel-tramp-pre-command-hook
            (lambda ()
              ;; (global-aggressive-indent-mode -1)
              (projectile-mode -1)
              (counsel-projectile-mode -1)))
  (add-hook 'counsel-tramp-quit-hook
            (lambda ()
              ;; (global-aggressive-indent-mode 1)
              (projectile-mode 1)
              (counsel-projectile-mode 1))))

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items 500)
  (imenu-max-item-length 100))

(setq tags-revert-without-query t
      large-file-warning-threshold (* 500 1024 1024)
      tags-add-tables nil)

(use-package counsel-gtags
  :ensure t
  :if (and (eq system-type 'gnu/linux) (eq dotemacs-tags-scheme 'gtags))
  :diminish
  :ensure-system-package global
  ;; :init
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'c-mode 'c++-mode)
  ;;               (counsel-gtags-mode 1))))
  :hook ((prog-mode protobuf-mode latex-mode) . counsel-gtags-mode)
  :custom (counsel-gtags-auto-update t)
  :bind (:map counsel-gtags-mode-map
              ("M-'" . counsel-gtags-dwim)
              ("M-," . counsel-gtags-go-backward)
              ("M-?" . counsel-gtags-find-reference)
              ("C-c g s" . counsel-gtags-find-symbol)
              ("C-c g d" . counsel-gtags-find-definition)
              ("C-c g c" . counsel-gtags-create-tags)
              ("C-c g u" . counsel-gtags-update-tags))
  :config
  (use-package global-tags ; Make xref and gtags work together
    :ensure t
    :if (eq dotemacs-tags-scheme 'gtags)
    :demand t
    :config (add-to-list 'xref-backend-functions 'global-tags-xref-backend)))

(use-package xref
  :if (eq dotemacs-tags-scheme 'ctags)
  :config (xref-etags-mode)
  :bind (("M-'" . xref-find-definitions)
         ("M-?" . xref-find-references)
         ("C-M-." . xref-find-apropos)
         ("M-," . xref-pop-marker-stack)
         :map xref--xref-buffer-mode-map
         ("C-o" . xref-show-location-at-point)
         ("<tab>" . xref-quit-and-goto-xref)
         ("r" . xref-query-replace-in-results))
  :config
  (use-package ivy-xref
    :ensure t
    :demand t ; Load once xref is invoked
    :config
    (setq xref-show-definitions-function #'ivy-xref-show-defs
          xref-show-xrefs-function #'ivy-xref-show-xrefs)))

(use-package counsel-etags
  :ensure t
  :ensure-system-package (ctags . "snap install universal-ctags")
  :if (and (eq system-type 'gnu/linux) (eq dotemacs-tags-scheme 'ctags))
  :bind (("M-]" . counsel-etags-find-tag-at-point)
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
  (dolist (ignore-dirs '(".vscode" "build" ".metadata" ".recommenders"))
    (add-to-list 'counsel-etags-ignore-directories ignore-dirs))
  (dolist (ignore-files '(".clang-format" "*.json" "*.html" "*.xml"))
    (add-to-list 'counsel-etags-ignore-filenames ignore-files)))

(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-selector 'ivy)
  (dumb-jump-prefer-searcher 'rg))

(use-package helpful
  :ensure t
  :bind (("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h f" . helpful-function)
         ("C-h c" . helpful-command)
         ("C-h p" . helpful-at-point)
         :map helpful-mode-map
         ("q" . helpful-kill-buffers)))

(use-package vlf ; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
  :ensure t
  :custom (vlf-application 'dont-ask)
  :config (use-package vlf-setup))

(use-package hungry-delete ; Erase 'all' consecutive white space characters in a given direction
  :ensure t
  :diminish
  :hook (after-init . global-hungry-delete-mode ))

(use-package move-text ; Move lines with "M-<up>" and "M-<down>"
  :ensure t
  :init (move-text-default-bindings))

(use-package duplicate-thing
  :ensure t
  :bind* ("C-c C-d" . duplicate-thing))

(use-package discover-my-major ; Discover key bindings and their meaning for the current Emacs major mode
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package manage-minor-mode ; Manage your minor-mode on the dedicated interface buffer
  :ensure t
  :bind ("C-c d m" . manage-minor-mode))

(use-package jgraph-mode
  :ensure t
  :mode "\\.jgr\\'")

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'")

(use-package gnuplot
  :ensure t
  :mode "\\.gp\\'"
  :interpreter ("gnuplot" . gnuplot-mode))

(use-package goto-last-change
  :ensure t
  :bind ("C-x C-\\" . goto-last-change))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(use-package popwin
  :ensure t
  :hook (after-init . popwin-mode)
  :config
  (defvar popwin:special-display-config-backup popwin:special-display-config)
  (push '("*Help*" :noselect t) popwin:special-display-config)
  (push '(compilation-mode :noselect t) popwin:special-display-config)
  (push '("*Compile-Log*" :noselect t) popwin:special-display-config)
  (push '("*manage-minor-mode*" :noselect t) popwin:special-display-config)
  (push '("*Paradox Report*" :regexp t :noselect t) popwin:special-display-config)
  (push '("*Selection Ring:") popwin:special-display-config)
  (push '("*Flycheck errors*" :noselect nil) popwin:special-display-config)
  (push '("*ripgrep-search*" :noselect nil) popwin:special-display-config)
  (push '("^\*magit:.+\*$" :noselect nil) popwin:special-display-config)
  (push '("*xref*" :noselect nil) popwin:special-display-config)
  (push '("*helpful\*" :noselect nil) popwin:special-display-config)
  (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
  (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
  (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  (add-to-list 'popwin:special-display-config '("*Apropos*"))
  (add-to-list 'popwin:special-display-config '("*Warnings*")))

(use-package expand-region ; Expand region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package expand-line
  :ensure t
  :bind ("M-i" . turn-on-expand-line-mode))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :ensure t
  :bind* ("C-." . iedit-mode))

(use-package persistent-scratch
  :ensure t
  :hook (after-init . persistent-scratch-setup-default)
  :custom (persistent-scratch-save-file (expand-file-name
                                         (concat
                                          dotemacs-temp-directory "persistent-scratch"))))

(use-package crux
  :ensure t
  :bind (("C-c d i" . crux-ispell-word-then-abbrev)
         ("C-c d s" . crux-sudo-edit)
         ("<f12>" . crux-kill-other-buffers)))

(use-package apt-sources-list
  :ensure t
  :mode ("\\.list\\'" . apt-sources-list-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ssh-config-mode
  :ensure t
  :mode ("/\\.ssh/config\\'" . ssh-config-mode)
  :mode ("/sshd?_config\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)
  :config (add-hook 'ssh-config-mode-hook #'turn-on-font-lock))

(use-package ace-window
  :ensure t
  :bind (([remap other-window] . ace-window)
         ("<f10>" . ace-window)))

;; This causes additional saves which leads to auto-formatters being invoked more frequently
(use-package super-save ; Save buffers when Emacs loses focus
  :ensure t
  :diminish
  :custom
  (super-save-remote-files nil) ; Ignore remote files
  (super-save-auto-save-when-idle t)
  :hook (after-init . super-save-mode)
  :config (add-to-list 'super-save-triggers 'ace-window))

(use-package avy
  :ensure t
  :bind (("M-b" . avy-goto-word-1)
         ("C-'" . avy-goto-char)
         ("C-/" . avy-goto-line))
  :custom
  ;; (avy-background t)
  (avy-highlight-first t)
  (avy-style 'at)
  :config
  ;; It will bind, for example, avy-isearch to C-' in isearch-mode-map, so that you can select one
  ;; of the currently visible isearch candidates using avy.
  (avy-setup-default))

(use-package bookmark
  :custom (bookmark-default-file (expand-file-name "bookmarks" dotemacs-temp-directory)))

(use-package bm
  :ensure t
  :bind (("C-<f1>" . bm-toggle)
         ("C-<f2>" . bm-next)
         ("C-<f3>" . bm-previous)))

(use-package esup
  :ensure t
  :commands (esup))

(use-package explain-pause-mode
  :load-path "extras")

;; text-mode is a basic mode for LaTeX-mode and org-mode, and so any hooks defined will also get run
;; for all modes derived from a basic mode such as text-mode.

(use-package writegood-mode ; Identify weasel words, passive voice, and duplicate words
  :ensure t
  :diminish
  :hook (text-mode . writegood-mode))

(use-package flycheck-grammarly
  :ensure t)

(use-package logview
  :ensure t
  :custom (logview-cache-filename (expand-file-name
                                   (concat
                                    dotemacs-temp-directory "logview-cache.extmap"))))

(use-package bison-mode
  :ensure t
  :mode ("\\.y\\'" "\\.l\\'" "\\.bison\\'"))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package markdown-mode
  :ensure t
  :diminish gfm-mode
  :ensure-system-package pandoc
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  ;; :bind ("C-c C-d" . nil)
  ;; Looks good, but hiding markup makes it difficult to be consistent while editing
  ;; :init (setq-default markdown-hide-markup t)
  :custom
  ;; (mardown-indent-on-enter 'indent-and-new-item)
  (markdown-enable-math t)
  ;; (markdown-make-gfm-checkboxes-buttons nil)
  (markdown-list-indent-width 2)
  (markdown-command "pandoc -f markdown -s")
  :config
  (use-package markdown-mode+
    :ensure t
    :demand t))

(use-package pandoc-mode
  :ensure t
  :diminish
  :config (pandoc-load-default-settings)
  :hook (markdown-mode . pandoc-mode))

(use-package prettier-js
  :ensure t
  :disabled t ;; Seems like there are bugs/inconsistencies in indenting lists
  :init
  (dolist (hook '(markdown-mode-hook gfm-mode-hook))
    (add-hook hook #'prettier-js-mode))
  :custom (prettier-js-args (list "--config" (concat dotemacs-user-home "/.prettierrc"))))

(use-package prettier
  :ensure t
  :init (setenv "NODE_PATH" "/usr/local/lib/node_modules")
  :hook ((markdown-mode gfm-mode) . prettier-mode))

(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package boogie-friends
  :ensure t
  :mode ("\\.smt\\'" . z3-smt2-mode))

(use-package ivy-bibtex
  :ensure t
  :bind ("C-c x b" . ivy-bibtex)
  :config
  (use-package bibtex-completion
    :custom
    (bibtex-completion-cite-prompt-for-optional-arguments nil)
    (bibtex-completion-cite-default-as-initial-input t)
    (bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:10}"))))
  :custom (ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

;; ;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
;; (defun sb/save-buffer-and-run-latexmk ()
;;   "Save the current buffer and run LaTeXMk also."
;;   (interactive)
;;   (require 'tex-buf)
;;   (let ((process (TeX-active-process))) (if process (delete-process process)))
;;   (let ((TeX-save-query nil)) (TeX-save-document ""))
;;   (TeX-command-menu "LaTeXMk"))
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c x c") #'sb/save-buffer-and-run-latexmk)))
;; (add-hook 'latex-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c x c") #'sb/save-buffer-and-run-latexmk)))

(use-package make-mode
  :mode (("\\Makefile\\'" . makefile-mode)
         ("makefile\\.rules\\'" . makefile-gmake-mode)))

(use-package eldoc
  :if (eq system-type 'gnu/linux)
  :diminish
  :config (global-eldoc-mode -1))

(use-package matlab-mode
  :ensure t)

(use-package ini-mode
  :ensure t
  :mode "\\.ini\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(dolist (hooks '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hooks
            (lambda ()
              (when buffer-file-name
                (add-hook 'after-save-hook #'check-parens nil t)))))

;;  Call this in c-mode-common-hook:
;; (define-key (current-local-map) "}" (lambda () (interactive) (c-electric-brace 1)))
(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)
  :mode ("\\.c\\'" . c++-mode)
  :custom
  (c-set-style "cc-mode")
  (c-basic-offset 2)
  :config
  ;; Disable electric indentation and on-type formatting
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq-local c-electric-flag nil
                          c-electric-indent nil
                          c-electric-brace nil
                          c-syntactic-indentation nil
                          c-enable-auto-newline nil
                          c-auto-newline nil)))
  (unbind-key "C-M-a" c-mode-map)
  :bind (:map c-mode-base-map
              ("C-c c a" . c-beginning-of-defun)
              ("C-c c e" . c-end-of-defun)
              ("M-q" . c-fill-paragraph)))

(use-package cuda-mode
  :ensure t
  :mode ("\\.cu\\'"	. c++-mode))

(use-package opencl-mode
  :ensure t
  :mode ("\\.cl\\'" . opencl-mode))

(use-package cmake-mode
  :ensure t
  ;; FIXME: Is this syntax correct?
  :mode ("CMakeLists.txt" "\\.cmake\\'")
  :config
  (use-package cmake-font-lock
    :ensure t
    :hook (cmake-mode . cmake-font-lock-activate)))

(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package pyvenv
  :ensure t
  :diminish
  :custom (pyvenv-mode-line-indicator
           '(pyvenv-virtual-env-name ("[venv:"
                                      pyvenv-virtual-env-name "]")))
  :hook (python-mode . pyvenv-mode))

(use-package ein
  :ensure t)

(add-hook 'java-mode-hook
          (lambda ()
            (setq-default c-basic-offset 4
                          c-set-style "java")))

(use-package ant
  :ensure t)

(use-package autodisass-java-bytecode ; Can disassemble .class files from within jars
  :ensure t)

(use-package sh-script ; Shell script mode
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\bashrc\\'" . sh-mode))
  :config (unbind-key "C-c C-d" sh-mode-map) ;; Was bound to sh-cd-here
  :custom
  (sh-basic-offset 2)
  (sh-indent-comment t) ; Indent comments as a regular line
  (sh-indent-after-continuation 'always))

(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'"
  :interpreter "fish"
  :hook (fish.mode . (lambda ()
                       (add-hook 'before-save-hook #'fish_indent-before-save))))

;; https://github.com/amake/shfmt.el
;; LATER: Could possibly switch to https://github.com/purcell/emacs-shfmt
(use-package shfmt
  :ensure reformatter
  :ensure-system-package (shfmt . "snap install shfmt")
  :load-path "extras/shfmt"
  :diminish shfmt-on-save-mode
  ;; :hook (sh-mode . shfmt-on-save-mode)
  :custom (shfmt-arguments "-i 4 -p -ci"))

(use-package flycheck-shfmt
  :ensure reformatter
  :after flycheck
  :load-path "extras/shfmt"
  :config (flycheck-shfmt-setup))

(use-package fish-completion
  :ensure t
  :if (when (executable-find "fish"))
  :config (global-fish-completion-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-save-repository-buffers t)
  (transient-history-file (expand-file-name "transient/history.el" dotemacs-temp-directory))
  (transient-levels-file (expand-file-name "transient/levels.el" dotemacs-temp-directory))
  (transient-values-file (expand-file-name "transient/values.el" dotemacs-temp-directory))
  ;; https://irreal.org/blog/?p=8877
  (magit-section-initial-visibility-alist '((stashes . hide) (untracked . hide) (unpushed . show))))

(use-package gitignore-mode
  :ensure t)

(use-package gitattributes-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package git-gutter
  :ensure t
  :diminish
  :bind (("C-x p" . git-gutter:previous-hunk)
         ("C-x n" . git-gutter:next-hunk))
  :hook (after-init . global-git-gutter-mode))

;; https://emacs.stackexchange.com/questions/16469/how-to-merge-git-conflicts-in-emacs
(defun sb/enable-smerge-maybe ()
  (when (and buffer-file-name (vc-backend buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1)))))
(add-hook 'buffer-list-update-hook #'sb/enable-smerge-maybe)

(use-package yaml-mode
  :ensure t)

(use-package php-mode
  :ensure t)

(use-package web-mode
  :ensure t
  ;; :mode
  ;; (("\\.html?\\'" . web-mode)
  ;;  ("\\.djhtml\\'" . web-mode)
  ;;  ("\\.phtml\\'" . web-mode)
  ;;  ("\\.hb\\.html\\'" . web-mode)
  ;;  ("\\.tpl\\.php\\'" . web-mode)
  ;;  ("\\.[agj]sp\\'" . web-mode)
  ;;  ("\\.as[cp]x\\'" . web-mode)
  ;;  ("\\.erb\\'" . web-mode))
  ;; :custom
  ;; (web-mode-enable-auto-pairing t)
  ;; (web-mode-enable-auto-closing t)
  ;; (web-mode-enable-auto-quoting t)
  ;; (web-mode-enable-css-colorization t)
  ;; (web-mode-enable-block-face t)
  ;; (web-mode-enable-current-element-highlight t)
  ;; (web-mode-enable-current-column-highlight t)
  )

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (((cmake-mode css-mode html-mode javascript-mode js-mode js2-mode json-mode jsonc-mode less-mode less-css-mode nxml-mode php-mode python-mode sass-mode scss-mode sh-mode typescript-mode web-mode yaml-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-diagnostics-modeline-mode))
  :custom
  (lsp-clients-clangd-args '("-j=2" "--background-index" "--clang-tidy" "--log=error" "--pretty"))
  (lsp-eldoc-enable-hover nil)
  (lsp-eldoc-hook nil)
  (lsp-enable-file-watchers nil) ; Could be a directory-local variable
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet t) ; Autocomplete parentheses
  (lsp-html-format-wrap-line-length 100)
  (lsp-html-format-indent-inner-html t)
  (lsp-imenu-sort-methods '(position))
  (lsp-keep-workspace-alive nil)
  (lsp-log-io t) ; Disable after a bit of testing
  (lsp-prefer-capf t)
  (lsp-pyls-configuration-sources [])
  (lsp-pyls-plugins-autopep8-enabled nil)
  (lsp-pyls-plugins-jedi-completion-fuzzy t)
  (lsp-pyls-plugins-mccabe-enabled nil)
  (lsp-pyls-plugins-preload-modules ["numpy"])
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-pycodestyle-max-line-length 100)
  (lsp-pyls-plugins-pydocstyle-convention "pep257")
  (lsp-pyls-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213")))
  (lsp-pyls-plugins-pyflakes-enabled nil)
  (lsp-pyls-plugins-pylint-args (vconcat (list "-j 2" (concat "--rcfile=" dotemacs-user-home "/.config/pylintrc"))))
  (lsp-pyls-plugins-yapf-enabled t)
  (lsp-session-file (expand-file-name ".lsp-session-v1" dotemacs-temp-directory))
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-xml-logs-client nil)
  (lsp-xml-jar-file (expand-file-name
                     (locate-user-emacs-file
                      "org.eclipse.lemminx-0.11.1-uber.jar")))
  (lsp-yaml-print-width 100)
  :config
  (advice-add #'lsp--auto-configure :override #'ignore)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("bash-language-server" "start"))
                    :major-modes '(sh-mode)
                    :remote? t
                    :server-id 'bash-lsp-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "intelephense")
                    :major-modes '(php-mode)
                    :remote? t
                    :server-id 'intelephense-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "texlab")
                    :major-modes '(plain-tex-mode tex-mode latex-mode)
                    :remote? t
                    :server-id 'texlab-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "cmake-language-server")
                    :major-modes '(cmake-mode)
                    :remote? t
                    :server-id 'cmakels-remote))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("typescript-language-server" "--stdio"))
                    :major-modes '(js-mode)
                    :remote? t
                    :server-id 'typescript-remote))
  :bind (("M-." . lsp-find-definition)
         ;; ("M-," . pop-tag-mark)
         ("C-c l i" . lsp-goto-implementation)
         ("C-c l t" . lsp-goto-type-definition)
         ("C-c l r" . lsp-rename)
         ("C-c l h" . lsp-symbol-highlight)
         ("C-c l f" . lsp-format-buffer)
         ("C-c l r" . lsp-find-references)))

;; FIXME: Why moving this to lsp::config does not work?
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (lsp-format-buffer)) nil t)))
(add-hook 'c++-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (lsp-format-buffer)) nil t)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-sideline-enable nil))

(use-package lsp-ivy
  :ensure t
  :after lsp
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
  :bind ("C-c l s" . lsp-ivy-global-workspace-symbol))

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp)
  :custom
  (lsp-java-inhibit-message t)
  (lsp-java-save-actions-organize-imports t)
  :config (add-hook 'java-mode-hook
                    (lambda ()
                      (add-hook 'before-save-hook
                                (lambda ()
                                  (lsp-format-buffer)) nil t))))

(use-package lsp-python-ms
  :disabled t
  :load-path "extras"
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))

(use-package lsp-latex
  :ensure t
  :hook ((latex-mode plain-tex-mode tex-mode) . lsp)
  :custom (lsp-latex-build-on-save t))


(use-package mlir-mode
  :load-path "extras"
  :mode ("\\.mlir\\'"))

(use-package bazel-mode
  :ensure t
  :mode (("\\.bzl$" . bazel-mode)
         ("\\BUILD\\'" . bazel-mode)
         ("\\.bazelrc\\'" . bazelrc-mode)))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto$")

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
            (format-time-string "%Y-%m-%d"))))

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
  "Create tags file with ctags."
  (interactive "Directory: ")
  (shell-command
   (format "%s -f TAGS -eR %s" dotemacs-ctags-path (directory-file-name dir-name))))

(defun sb/create-gtags (dir-name)
  "Create tags file with gtags."
  (interactive "Directory: ")
  (shell-command
   (format "%s -cv --gtagslabel=new-ctags %s" dotemacs-gtags-path (directory-file-name dir-name))))

;; https://emacs.stackexchange.com/questions/33332/recursively-list-all-files-and-sub-directories
(defun sb/counsel-all-files-recursively (dir-name)
  "List all files recursively."
  (interactive "Directory: ")
  (let* ((cands (split-string
                 (shell-command-to-string (format "find %s -type f" dir-name)) "\n" t)))
    (ivy-read "File: " cands
              :action #'find-file
              :caller 'sb/counsel-all-files-recursively)))

;; Generic keybindings, package-specific are usually in their own modules. Use `C-h b' to see
;; available bindings in a buffer. Use `M-x describe-personal-keybindings` to see modifications.

;; bind-key*, bind* overrides all minor mode bindings. The kbd macro is not required with bind-key
;; variants. With bind-key, you do not need an explicit "(kbd ...)". Other variants: (global-set-key
;; (kbd "RET") 'newline-and-indent) (define-key global-map (kbd "RET") 'newline-and-indent)

(bind-keys
 ("RET" . newline-and-indent)
 ("C-l" . goto-line)
 ("C-c z" . repeat)
 ("C-z" . undo)
 ("<f11>" . delete-other-windows)
 ("C-x k" . kill-this-buffer)
 ("M-<left>" . previous-buffer)
 ("M-<right>" . next-buffer)
 ("C-c d f" . auto-fill-mode)
 ("M-c" . capitalize-dwim)
 ("M-u" . upcase-dwim)
 ("M-l" . downcase-dwim))

;; In a line with comments, "C-u M-;" removes the comments altogether. That means deleting the
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
  :ensure t
  :bind (("C-M-+" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

(use-package which-key ; Show help popups for prefix keys
  :ensure t
  :hook (after-init . which-key-mode)
  :config (which-key-setup-side-window-right-bottom)
  :diminish)

;; ;; https://andreyorst.gitlab.io/posts/2020-06-29-using-single-emacs-instance-to-edit-files/
;; (use-package server
;;   :if (not (string-equal "root" (getenv "USER"))) ; Only start server mode if not root
;;   :config (unless (server-running-p) (server-start)))

;; Mark safe variables

(put 'bibtex-completion-bibliography 'safe-local-variable #'listp)
(put 'company-bibtex-bibliography 'safe-local-variable #'listp)
(put 'company-clang-arguments 'safe-local-variable #'listp)
(put 'counsel-etags-project-root 'safe-local-variable #'stringp)
(put 'counsel-find-file-ignore-regexp 'safe-local-variable #'stringp)
(put 'flycheck-clang-include-path 'safe-local-variable #'listp)
(put 'flycheck-gcc-include-path 'safe-local-variable #'listp)
(put 'flycheck-python-pylint-executable 'safe-local-variable #'stringp)
(put 'projectile-default-file 'safe-local-variable #'stringp)
(put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
(put 'projectile-project-root 'safe-local-variable #'stringp)
(put 'pyvenv-activate 'safe-local-variable #'stringp)
(put 'reftex-default-bibliography 'safe-local-variable #'listp)
(put 'tags-table-list 'safe-local-variable #'listp)

(defun open-local-file-projectile (directory)
  "Helm action function, open projectile file within DIRECTORY
specify by the keyword projectile-default-file define in `dir-locals-file'"
  (let ((default-file (f-join directory (nth 1
                                             (car (-tree-map (lambda (node)
                                                               (when (eq (car node) 'projectile-default-file)
                                                                 (format "%s" (cdr node))))
                                                             (dir-locals-get-class-variables (dir-locals-read-from-dir directory))))))))
    (if (f-exists? default-file)
        (counsel-find-file default-file)
      (message "The file %s doesn't exist in the select project" default-file)
      )
    )
  )

(with-eval-after-load "counsel-projectile"
  (add-to-list 'counsel-projectile-action '("d" open-local-file-projectile "open default file") t))

;;; init.el ends here
