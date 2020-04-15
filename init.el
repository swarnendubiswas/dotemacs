;;; init.el --- Emacs customization -*- lexical-binding: t; no-byte-compile: nil; -*-
;; Swarnendu Biswas

;;; Commentary:

;; Notes: To evaluate an Sexp, just go to the end of the sexp and type "C-x C-e", instead of
;; evaluating the whole buffer Use C-M-x to evaluate the current top-level s-expression. Use M-: to
;; evaluate any Emacs Lisp expression and print the result.

;; Init file should not ideally contain calls to "load" or "require", since they cause eager loading
;; and are expensive, a cheaper alternative is to use "autoload".

;;; Code:

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Anonymous-Functions.html

;; When defining a lambda expression that is to be used as an anonymous function, you can in
;; principle use any method to construct the list. But typically you should use the lambda macro, or
;; the function special form, or the #' read ;; syntax which is a short-hand for using function.
;; Quoting a lambda form means the anonymous function is not ;; byte-compiled. The following forms
;; are all equivalent: (lambda (x) (* x x)) (function (lambda (x) (* x x))) #'(lambda (x) (* x x))

(setq debug-on-error t
      load-prefer-newer t
      user-full-name "Swarnendu Biswas")

(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'local)

(defcustom dotemacs-temp-directory (concat user-emacs-directory "tmp/")
  "Storage location for various configuration files."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-extras-directory (concat user-emacs-directory "extras/")
  "Path for third-party packages and files."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-modules-directory (concat user-emacs-directory "modules/")
  "Path containing setup files for customized configuration."
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
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-modeline-theme
  'default
  "Specify the mode-line theme to use."
  :type '(radio
          (const :tag "powerline" powerline)
          (const :tag "smart-mode-line" sml)
          (const :tag "spaceline" spaceline)
          (const :tag "default" default))
  :group 'dotemacs)

(defcustom dotemacs-window-split
  'vertical ; Direction of the divider, Emacs calls this horizontal
  "Specify the direction in which the windows should be split. This depends on the orientation of the display."
  :type '(radio
          (const :tag "vertical" vertical)
          (const :tag "horizontal" horizontal))
  :group 'dotemacs)

(defconst dotemacs-fill-column 100
  "Column beyond which lines should not extend.")

(defcustom dotemacs-delete-trailing-whitespace-p
  t
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.  Sometimes we do not want to unnecessarily add
differences due to whitespaces."
  :type 'boolean
  :group 'dotemacs)

(defcustom dotemacs-tags-scheme
  'gtags
  "Choose whether to use gtags or ctags."
  :type '(radio
          (const :tag "ctags" ctags)
          (const :tag "gtags" gtags)
          (const :tag "none" none))
  :group 'dotemacs)

(defcustom dotemacs-ctags-path
  "/usr/local/bin/ctags"
  "Absolute path to Universal CTags executable."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-gtags-path
  "/usr/local/bin/gtags"
  "Absolute path to GNU Global executable."
  :type 'string
  :group 'dotemacs)

(eval-when-compile
  (require 'package)
  (setq package-user-dir (expand-file-name (concat user-emacs-directory "elpa"))
        ;; Avoid loading packages twice
        package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'package-archives '("melpa"
                                   . "https://melpa.org/packages/") t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer t
      use-package-compute-statistics t ; Use "M-x use-package-report" to see results
      ;; Disable after testing
      use-package-verbose t)

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
  :custom (exec-path-from-shell-check-startup-files nil)
  :init (exec-path-from-shell-initialize))

(setq inhibit-startup-screen t ; inhibit-splash-screen is an alias
      ;; Disable loading of "default.el" at startup, inhibits site default settings
      inhibit-default-init t
      inhibit-startup-echo-area-message t
      ;; *scratch* is in Lisp interaction mode by default, use text mode instead
      initial-major-mode 'text-mode
      initial-scratch-message nil
      create-lockfiles nil
      ;; Turn off alarms completely: https://www.emacswiki.org/emacs/AlarmBell
      ;; ring-bell-function 'ignore
      gc-cons-threshold (* 200 1024 1024) ; Increase gc threshold to 200 MB
      read-process-output-max (* 1024 1024) ; 1 MB
      use-dialog-box nil
      use-file-dialog nil
      delete-by-moving-to-trash t
      ;; completion-ignore-case t ; Ignore case when completing
      read-file-name-completion-ignore-case t ; Ignore case when reading a file name completion
      read-buffer-completion-ignore-case t
      switch-to-buffer-preserve-window-point t
      x-stretch-cursor t ; Make cursor the width of the character it is under
      auto-save-list-file-prefix (concat dotemacs-temp-directory "auto-save")
      select-enable-clipboard t ; Enable use of system clipboard across Emacs and other applications
      require-final-newline t ; Always end a file with a newline.
      make-backup-files nil ; Stop making backup ~ files
      backup-inhibited t ; Disable backup for a per-file basis, not to be used by major modes
      auto-save-timeout 180 ; Seconds
      auto-save-interval 600
      confirm-kill-emacs nil
      save-interprogram-paste-before-kill t
      kill-whole-line t
      suggest-key-bindings t
      shift-select-mode t ; Use shift-select for marking
      blink-matching-paren t
      kill-ring-max 10
      kill-do-not-save-duplicates t
      set-mark-command-repeat-pop t
      confirm-nonexistent-file-or-buffer t
      ad-redefinition-action 'accept ; Turn off warnings due to functions being redefined
      vc-handled-backends nil
      major-mode 'text-mode ; Major mode to use for files that do no specify a major mode
      sentence-end-double-space nil
      truncate-lines nil
      truncate-partial-width-windows nil
      history-delete-duplicates t)

;; Activate utf8 mode
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(fset 'yes-or-no-p 'y-or-n-p) ; Type "y"/"n" instead of "yes"/"no"
(fset 'display-startup-echo-area-message #'ignore)

(transient-mark-mode 1) ; Enable visual feedback on selections, default since v23
(column-number-mode 1)
(diminish 'auto-fill-function) ; This is not a library/file, so eval-after-load does not work

(use-package autorevert ; Auto-refresh all buffers, does not work for remote files
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t))

;; Typing with the mark active will overwrite the marked region, pending-delete-mode is an alias
(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package saveplace ; Remember cursor position in files
  :unless noninteractive
  :hook (after-init . save-place-mode)
  :custom (save-place-file (concat dotemacs-temp-directory "places")))

(use-package savehist ; Save minibuffer histories across sessions
  :unless noninteractive
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history
                                   file-name-history
                                   command-history))
  (savehist-autosave-interval 300)
  (savehist-file (concat dotemacs-temp-directory "savehist"))
  (savehist-save-minibuffer-history t))

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(use-package uniquify
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'forward)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "/")
  (uniquify-strip-common-suffix t))

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

;; Set Emacs split to horizontal or vertical
;; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
;; https://www.emacswiki.org/emacs/HorizontalSplitting
(if (eq dotemacs-window-split 'vertical) ; Direction of the divider is vertical
    (setq split-width-threshold 999)
  (setq split-width-threshold 0))

;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun my-auto-save-wrapper (save-fn &rest args)
  "Hide 'Auto-saving...done' messages by calling the method SAVE-FN with non-nil ARGS."
  (ignore args)
  (apply save-fn '(t)))
(advice-add 'do-auto-save :around #'my-auto-save-wrapper)

(use-package abbrev
  :diminish
  :hook ((text-mode prog-mode) . abbrev-mode)
  ;; :init
  ;; (dolist (hook '(text-mode-hook prog-mode-hook))
  ;;   (add-hook hook #'abbrev-mode ))
  :custom
  (abbrev-file-name (concat dotemacs-extras-directory "abbrev_defs"))
  ;; Do not ask to save new abbrevs when quitting
  (save-abbrevs 'silently))

(setq custom-safe-themes t
      frame-title-format (list '(buffer-file-name "%f" "%b")) ; Better frame title
      indicate-buffer-boundaries 'right
      indicate-empty-lines t)

(if window-system
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)))

(blink-cursor-mode -1) ; Blinking cursor is distracting
(toggle-frame-maximized) ; Maximize Emacs on startup

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

      ((eq dotemacs-theme 'zenburn) (use-package zenburn
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

      ((eq dotemacs-theme 'default) (progn
                                      (set-face-attribute 'region nil
                                                          :background "light sky blue"
                                                          :foreground "white"))))

;; Set font face independent of the color theme, value is in 1/10pt, so 100 will give you 10pt.
(set-frame-font "DejaVu Sans Mono" nil t)
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 130)

;; https://stackoverflow.com/questions/3281581/how-to-word-wrap-in-emacs
(global-visual-line-mode 1)
(diminish 'visual-line-mode)

(use-package all-the-icons ; Install fonts with `M-x all-the-icons-install-fonts`
  :ensure t)

(size-indication-mode -1)

(cond ((eq dotemacs-modeline-theme 'powerline) (use-package powerline
                                                 :ensure t
                                                 :init
                                                 (setq powerline-display-mule-info nil
                                                       powerline-display-buffer-size t
                                                       powerline-display-hud nil
                                                       powerline-gui-use-vcs-glyph t
                                                       powerline-default-separator 'slant)
                                                 (powerline-default-theme)
                                                 (set-face-attribute 'powerline-active1 nil
                                                                     :background "gray22"
                                                                     :foreground "white"
                                                                     :weight 'light)
                                                 (set-face-attribute 'powerline-active2 nil
                                                                     :background "grey88"
                                                                     :foreground "black")
                                                 (when (eq dotemacs-theme 'leuven)
                                                   (set-face-attribute 'mode-line nil
                                                                       :background "grey88"
                                                                       :foreground "black")
                                                   (set-face-attribute 'mode-line-buffer-id nil
                                                                       :weight 'bold
                                                                       :foreground "black"
                                                                       :background "gray88"))))

      ((eq dotemacs-modeline-theme 'sml) (use-package smart-mode-line
                                           :ensure t
                                           :config
                                           (setq sml/theme 'light
                                                 sml/no-confirm-load-theme t
                                                 ;; Everything after the minor-modes will be right-indented
                                                 sml/mode-width 'full
                                                 sml/shorten-modes t
                                                 sml/shorten-directory t)
                                           (sml/setup)))

      ((eq dotemacs-modeline-theme 'spaceline) (use-package spaceline
                                                 :ensure t
                                                 :init
                                                 (require 'spaceline-config)
                                                 (setq powerline-default-separator 'slant
                                                       ;; spaceline-anzu-p t
                                                       spaceline-hud-p nil
                                                       ;; spaceline-buffer-modified-p t
                                                       ;; spaceline-buffer-position-p t
                                                       spaceline-projectile-root-p t
                                                       spaceline-paradox-menu-p t)
                                                 (spaceline-emacs-theme)
                                                 (when (eq dotemacs-theme 'leuven)
                                                   (set-face-attribute 'powerline-active1 nil
                                                                       :background "gray22"
                                                                       :foreground "white"
                                                                       :weight 'light)
                                                   (set-face-attribute 'mode-line-inactive nil
                                                                       :background "grey88"
                                                                       :foreground "black"))))

      ((eq dotemacs-modeline-theme 'default) ))

(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer) ; Turn on ibuffer by default
  (setq ibuffer-always-show-last-buffer nil
        ibuffer-case-fold-search t ; Ignore case when searching
        ibuffer-default-sorting-mode 'alphabetic ; Options: major-mode
        ibuffer-display-summary nil
        ibuffer-expert t
        ibuffer-use-header-line t)
  (add-hook 'ibuffer-hook #'ibuffer-auto-mode))

;; Don't show filter groups if there are no buffers in that group
(use-package ibuf-ext
  :load-path "extras"
  :custom (ibuffer-show-empty-filter-groups nil))

(use-package ibuffer-projectile ; Group buffers by projectile project
  :ensure t
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))

(use-package dired
  :preface
  (defun dired-go-home ()
    (interactive)
    (dired "~/"))
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
  (dired-recursive-deletes 'always) ; Single prompt for all n directories
  (dired-recursive-copies 'always)
  ;; Check `ls' for additional options
  (dired-listing-switches "-ABhl --si --group-directories-first")
  (dired-ls-F-marks-symlinks t) ; -F marks links with @
  (dired-dwim-target t)
  :config
  ;; Auto refresh dired when files change
  (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package dired-x
  :custom
  (dired-bind-jump t)
  (dired-omit-verbose nil) ; Do not show messages when omitting files
  :config
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)
  :bind ("C-x C-j" . dired-jump))

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
  :disabled t
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
        treemacs-silent-filewatch t
        treemacs-silent-refresh t
        treemacs-tag-follow-delay 1
        treemacs-tag-follow-cleanup t)
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

;; (use-package treemacs-magit
;;   :ensure t
;;   :after (treemacs magit)
;;   ;; :commands treemacs-magit--schedule-update
;;   ;; :hook ((magit-post-commit
;;   ;;         git-commit-post-finish
;;   ;;         magit-post-stage
;;   ;;         magit-post-unstage)
;;   ;;        . treemacs-magit--schedule-update)
;;   )

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(setq case-fold-search t) ; Make search ignore case

;; Use "C-'" in isearch-mode-map to use avy-isearch to select one of the currently visible isearch
;; candidates.
(use-package isearch
  :custom
  (search-highlight t) ; Highlight incremental search
  (isearch-allow-scroll t)
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

(use-package swiper ; Performs poorly if there are a large number of matches
  :ensure t
  :custom
  (swiper-use-visual-line t)
  (swiper-action-recenter t))

(use-package wgrep
  :ensure t
  :custom (wgrep-auto-save-buffer t))

(use-package deadgrep
  :ensure t
  :bind (("C-c s d" . deadgrep)
         ("<f8>" . deadgrep)))

;; Adding directories to the list of recent files decreases the number of entries of recent files.
;; Therefore, we use a different command/keybinding to lookup recent directories.
;; https://www.emacswiki.org/emacs/RecentFiles
(use-package recentf
  :custom
  (recentf-save-file (concat dotemacs-temp-directory "recentf"))
  (recentf-max-saved-items 50)
  ;; Disable this so that recentf does not attempt to stat remote files
  (recentf-auto-cleanup 'never)
  (recentf-menu-filter 'recentf-sort-descending)
  ;; Check regex with re-builder
  (recentf-exclude '("[/\\]\\.elpa/"
                     "[/\\]\\.ido\\.last\\'"
                     "[/\\]\\.git/"
                     ".*\\.gz\\'"
                     ".*-autoloads.el\\'"
                     "[/\\]archive-contents\\'"
                     "[/\\]\\.loaddefs\\.el\\'"
                     "url/cookies"
                     "[/\\]tmp/.*"
                     ".*/recentf\\'"
                     "~$"
                     "/.autosaves/"
                     ".*-loaddefs.el"
                     "/TAGS$"
                     "/ssh:"
                     "/sudo:"
                     "/company-statistics-cache.el$"))
  :config (run-at-time nil (* 10 60) 'recentf-save-list)
  :hook (after-init . recentf-mode))

;; Hide the "wrote to recentf" message which is irritating
(defun sb/recentf-save-list (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN."
  (let ((inhibit-message t))
    (apply orig-fun args)))
(advice-add 'recentf-save-list :around #'sb/recentf-save-list)

(use-package company
  :ensure t
  :diminish
  :hook (after-init . global-company-mode)
  :custom
  (company-dabbrev-downcase nil) ; Do not downcase returned candidates
  (company-idle-delay 0.0)
  (company-global-modes t) ; Turn on company-mode for all major modes
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

(use-package company-flx
  :ensure t
  :hook (global-company-mode . company-flx-mode))

(use-package company-dict
  :ensure t
  :custom
  (company-dict-dir (concat user-emacs-directory "dict/"))
  (company-dict-enable-fuzzy t)
  (company-dict-enable-yasnippet nil)
  :config (add-to-list 'company-backends 'company-dict))

(use-package company-ctags
  :ensure t
  :config (company-ctags-auto-setup)
  :custom
  (company-ctags-fuzzy-match-p t)
  (company-ctags-everywhere t))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (after-init . yas-global-mode)
  :custom
  (yas-snippet-dirs (list (expand-file-name (concat user-emacs-directory "snippets"))))
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  :config
  (unbind-key "<tab>" yas-minor-mode-map)
  (use-package yasnippet-snippets))

(use-package amx
  :ensure t
  :hook (after-init . amx-mode)
  :custom (amx-save-file (concat dotemacs-temp-directory "amx-items")))

(use-package ivy
  :ensure t
  :custom
  (ivy-auto-select-single-candidate t)
  (ivy-case-fold-search 'always) ; Always ignore case while searching
  (ivy-count-format "(%d/%d) ") ; This is beneficial to identify wrap around
  (ivy-extra-directories nil) ; Hide "." and ".."
  (ivy-fixed-height-minibuffer t) ; It is distracting if the mini-buffer height keeps changing
  ;; (ivy-re-builders-alist '((counsel-find-file . ivy--regex-ignore-order)
  ;;                          (swiper . ivy--regex-ignore-order)
  ;;                          (counsel-rg . ivy--regex-ignore-order)
  ;;                          (counsel-grep-or-swiper . ivy--regex-ignore-order)
  ;;                          (ivy-switch-buffer . ivy--regex-plus)
  ;;                          (t . ivy--regex-fuzzy)))
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  ;; (ivy-sort-matches-functions-alist
  ;;  '((t)
  ;;    (ivy-switch-buffer . ivy-sort-function-buffer)
  ;;    (counsel-find-file . ivy-sort-function-buffer)))
  (ivy-use-ignore-default 'always) ; Always ignore buffers set in ivy-ignore-buffers
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-height-alist '((t
                       lambda (_caller)
                       (/ (frame-height) 2))))
  (completion-in-region-function #'ivy-completion-in-region)
  :config
  (dolist (buffer '(
                    ;; "^\\*Backtrace\\*$"
                    ;; "^\\*Compile-Log\\*$"
                    ;; "^\\*.+Completions\\*$"
                    "^\\*Help\\*$"
                    "^\\*Ibuffer\\*$"
                    ;; "company-statistics-cache.el"
                    ;; "^\\*lsp-log\\*$"
                    ;; "^\\*pyls\\*$"
                    ;; "^\\*pyls::stderr\\*$"
                    ))
    (add-to-list 'ivy-ignore-buffers buffer))
  :hook (after-init . ivy-mode)
  :bind
  (("C-c r" . ivy-resume)
   ("C-'" . ivy-avy)
   ([remap switch-to-buffer] . ivy-switch-buffer)
   ("<f3>" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<return>" . ivy-alt-done) ; Continue completion
   ("C-j" . ivy-immediate-done) ; View the current directory
   ("<left>" . ivy-previous-line)
   ("<right>" . ivy-next-line)
   ("M-y" . ivy-next-line))
  :diminish)

(use-package counsel
  :ensure t
  :ensure amx
  :preface
  (defun sb/counsel-recentf ()
    "Find a file on `recentf-list' and abbreviate the home directory."
    (interactive)
    (ivy-read "Recentf: " (mapcar #'abbreviate-file-name recentf-list)
              :action
              (lambda (f)
                (with-ivy-window
                  (find-file f)))
              :caller 'counsel-recentf))
  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
  (defun sb/counsel-goto-recent-directory ()
    "Open recent directories with dired."
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
            (append (mapcar 'file-name-directory recentf-list)
                    ;; fasd history
                    (if (executable-find "fasd")
                        (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "Directories:" collection :action 'dired)))
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap yank-pop] . counsel-yank-pop)
   ([remap describe-bindings] . counsel-descbinds)
   ([remap execute-extended-command] . counsel-M-x)
   ("<f1>" . counsel-M-x)
   ([remap find-file] . counsel-find-file)
   ("<f2>" . counsel-find-file)
   ([remap load-theme] . counsel-load-theme)
   ([remap load-library] . counsel-load-library)
   ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
   ([remap completion-at-point] . counsel-company)
   ("<f9>" . counsel-recentf)
   ("C-<f9>" . sb/counsel-goto-recent-directory)
   ("C-c s g" . counsel-git-grep) ; Shows only the first 200 results, use "C-c C-o" to save all the matches to a buffer.
   ("C-c s r" . counsel-rg)
   ("<f4>" . counsel-grep-or-swiper)
   ("C-c C-m" . counsel-mark-ring)
   ("C-c C-j" . counsel-semantic-or-imenu))
  :custom
  (counsel-mode-override-describe-bindings t)
  (counsel-grep-swiper-limit 1000000) ; Number of characters in the buffer
  (counsel-find-file-at-point nil)
  (counsel-yank-pop-separator "\n-----------------\n")
  (counsel-find-file-ignore-regexp (concat
                                    "\\(?:\\`[#.]\\)" ; File names beginning with # or .
                                    "\\|\\(?:\\`.+?[#~]\\'\\)" ; File names ending with # or ~
                                    "\\|__pycache__"
                                    "\\|.aux$"
                                    "\\|.bbl$"
                                    "\\|.blg$"
                                    "\\|.cb$"
                                    "\\|.cb2$"
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
                                    "\\|.pyc$"
                                    "\\|.rel$"
                                    "\\|.rip$"
                                    "\\|.synctex$"
                                    "\\|.synctex.gz$"
                                    "\\|.tar.gz$"
                                    "\\|.toc$"
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
  :hook (ivy-mode . counsel-mode)
  :diminish)

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1)
  :custom (all-the-icons-ivy-rich-icon-size 0.8))

(use-package ivy-rich
  :ensure t
  :custom
  (ivy-rich-path-style 'relative)
  (ivy-format-function #'ivy-format-function-line)
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
  (ispell-dictionary "english")
  (ispell-personal-dictionary (concat dotemacs-extras-directory "spell"))
  ;; Aspell speed: ultra | fast | normal | bad-spellers
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  (ispell-silently-savep t) ; Save a new word to personal dictionary without asking
  (flyspell-sort-corrections nil)
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
  :bind ("C-;" . flyspell-popup-correct))

(setq-default fill-column dotemacs-fill-column
              standard-indent 2
              tab-width 2
              tab-always-indent 'complete
              ;; Spaces instead of tabs by default
              indent-tabs-mode nil)

;; Claims to be better than electric-indent-mode
(use-package aggressive-indent
  :ensure t
  :hook (prog-mode . global-aggressive-indent-mode)
  :diminish)

;; ;; This apparently interferes with lsp formatting where lsp is enabled.
;; (use-package electric
;;   :hook (emacs-lisp-mode . electric-indent-mode))

(use-package highlight-indentation
  :ensure t
  :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
  :hook (python-mode . highlight-indentation-mode)
  ;; :config (set-face-background 'highlight-indentation-face "WhiteSmoke")
  )

(use-package paren
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed) ; Options: 'expression, 'parenthesis, 'mixed
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; "sp-cheat-sheet" will show you all the commands available, with examples.
;; https://ebzzry.github.io/emacs-pairs.html
(use-package smartparens
  :ensure t
  :hook (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-from-inside t
        sp-autoskip-closing-pair 'always)
  :bind (("C-M-a" . sp-beginning-of-sexp) ; "foo ba_r" -> "_foo bar"
         ("C-M-e" . sp-end-of-sexp) ; "f_oo bar" -> "foo bar_"
         ("C-M-u" . sp-up-sexp) ; "f_oo bar" -> "foo bar"_
         ("C-M-w" . sp-down-sexp) ; "foo ba_r" -> "_foo bar"
         ;; The following two are the more commonly required use cases.
         ("C-M-f" . sp-forward-sexp) ; "foo ba_r" -> "foo bar"_
         ("C-M-b" . sp-backward-sexp) ; "foo ba_r" -> "_foo bar"
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("C-S-b" . sp-backward-symbol)
         ("C-S-f" . sp-forward-symbol)
         ;; (foo bar) -> foo bar
         ("C-M-k" . sp-splice-sexp))
  :diminish)

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(use-package projectile
  :ensure t
  :custom
  (projectile-cache-file (concat dotemacs-temp-directory "projectile.cache"))
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-file-exists-remote-cache-expire nil)
  ;; Contents of .projectile are ignored when using the alien or hybrid indexing method
  (projectile-indexing-method 'alien)
  (projectile-known-projects-file (concat dotemacs-temp-directory "projectile-known-projects.eld"))
  (projectile-mode-line-prefix "")
  ;; Use projectile only in desired directories, too much noise otherwise
  (projectile-require-project-root t)
  (projectile-switch-project-action 'projectile-find-file) ; Use projectile-dired to view in dired
  (projectile-verbose nil)
  :config
  (defun projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      (format "%s[%s]"
	            projectile-mode-line-prefix
	            (or project-name "-"))))
  (projectile-mode 1) ; Otherwise keybindings not bound explicitly with bind* will not be respected
  ;; https://emacs.stackexchange.com/questions/27007/backward-quote-what-does-it-mean-in-elisp
  (setq projectile-project-search-path (list
                                        (concat `,(getenv "HOME") "/bitbucket")
                                        (concat `,(getenv "HOME") "/github")
                                        (concat `,(getenv "HOME") "/iitk-workspace")
                                        (concat `,(getenv "HOME") "/iitkgp-workspace")
                                        (concat `,(getenv "HOME") "/iss-workspace")
                                        (concat `,(getenv "HOME") "/plass-workspace")
                                        (concat `,(getenv "HOME") "/prospar-workspace")
                                        (concat `,(getenv "HOME") "/research")
                                        ))
  (add-to-list 'projectile-ignored-projects `,(concat `,(getenv "HOME") "/")) ; Do not consider the HOME as a project
  (dolist (dirs '(".cache"
                  ".clangd"
                  ".dropbox"
                  ".git"
                  ".hg"
                  ".metadata"
                  ".nx"
                  ".recommenders"
                  ".svn"
                  ".vscode"
                  "__pycache__"
                  "auto"
                  "elpa"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  (dolist (item '("GPATH"
                  "GRTAGS"
                  "GTAGS"
                  "GSYMS"
                  "TAGS"
                  "tags"
                  ".dir-locals.el"
                  ".projectile"
                  ".project"
                  ".tags"
                  "__init__.py"))
    (add-to-list 'projectile-globally-ignored-files item))
  (dolist (list '(".aux"
                  ".bak"
                  ".elc"
                  ".jar"
                  ".out"
                  ".pdf"
                  ".pt"
                  ".pyc"
                  ".rel"
                  ".rip"
                  ".tar.gz"
                  "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes list))
  ;; ;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
  ;; (defadvice projectile-project-root (around ignore-remote first activate)
  ;;   (unless (file-remote-p default-directory) ad-do-it))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :after ivy
  :hook (counsel-mode . counsel-projectile-mode)
  ;; :init
  ;; ;; Sort projects from newest to oldest
  ;; (add-to-list 'ivy-sort-functions-alist
  ;;              '(counsel-projectile-switch-project . file-newer-than-file-p))
  :bind (("<f5>" . counsel-projectile-switch-project)
         ("<f6>" . counsel-projectile)
         ("<f7>" . counsel-projectile-rg)))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :custom (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (setq-default flycheck-disabled-checkers '(tex-lacheck python-flake8 emacs-lisp-checkdoc))
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local flycheck-checker 'python-pylint
                          flycheck-python-pylint-executable "python3"
                          flycheck-pylintrc (concat `,(getenv "HOME") "/.config/pylintrc"))))
  ;; ;; C/C++
  ;; (add-hook 'c++-mode-hook
  ;;           (lambda ()
  ;;             (setq flycheck-clang-args ""
  ;;                   flycheck-clang-include-path "" ; Directories
  ;;                   flycheck-clang-includes "" ; Files
  ;;                   flycheck-clang-language-standard "c++11"
  ;;                   flycheck-gcc-args ""
  ;;                   flycheck-gcc-include-path "" ; Directories
  ;;                   flycheck-gcc-includes "" ; Files
  ;;                   flycheck-gcc-language-standard "c++11")))
  ;; ;; CUDA
  ;; (setq-local flycheck-cuda-language-standard "c++11")
  ;; (setq-local flycheck-cuda-includes "")
  ;; (setq-local flycheck-cuda-include-path "") ; Directories
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local flycheck-checker 'markdown-markdownlint-cli
                          flycheck-markdown-markdownlint-cli-config (concat
                                                                     `,(getenv "HOME") "/.config/.markdownlint.json"))))
  (add-hook 'sh-mode-hook
            (lambda ()
              (setq-local flycheck-checker 'sh-shellcheck))))

;; Binds avy-flycheck-goto-error to C-c ! g
(use-package avy-flycheck
  :ensure t
  :ensure avy
  :config (avy-flycheck-setup))

(use-package flycheck-popup-tip ; Show error messages in popups
  :ensure t
  :hook (flycheck-mode . flycheck-popup-tip-mode))

;; This is different from whitespace-cleanup since this is unconditional
(when (bound-and-true-p dotemacs-delete-trailing-whitespace-p)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package whitespace
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  :hook (after-init . global-whitespace-mode)
  :custom
  (show-trailing-whitespace nil)
  (whitespace-auto-cleanup t)
  (whitespace-style nil)
  (whitespace-line-column dotemacs-fill-column))

(use-package ws-butler ; Unobtrusively trim extraneous white-space *ONLY* in lines edited
  :ensure t
  :if (not (bound-and-true-p dotemacs-delete-trailing-whitespace-p))
  :diminish
  :hook (prog-mode . ws-butler-mode))

(use-package highlight-symbol ; Highlight symbol under point
  :ensure t
  :preface
  ;; http://www.wilfred.me.uk/.emacs.d/init.html
  (defun sb/highlight-symbol-first ()
    "Jump to the first location of symbol at point."
    (interactive)
    (push-mark)
    (eval
     `(progn
        (goto-char (point-min))
        (let ((case-fold-search nil))
          (search-forward-regexp
           (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
           nil t))
        (beginning-of-thing 'symbol))))
  (defun sb/highlight-symbol-last ()
    "Jump to the last location of symbol at point."
    (interactive)
    (push-mark)
    (eval
     `(progn
        (goto-char (point-max))
        (let ((case-fold-search nil))
          (search-backward-regexp
           (rx symbol-start ,(thing-at-point 'symbol) symbol-end)
           nil t)))))
  :hook (prog-mode . highlight-symbol-mode)
  :bind (("M-p" . highlight-symbol-prev)
         ("M-n" . highlight-symbol-next))
  :custom
  (highlight-symbol-idle-delay 1)
  (highlight-symbol-on-navigation-p t)
  (highlight-symbol-highlight-single-occurrence nil)
  :diminish)

(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-keyword-faces '(("TODO" . hl-todo)
                           ("NOTE" . hl-todo)
                           ("XXX" . hl-todo)
                           ("LATER" . hl-todo)
                           ("IMP" . hl-todo)
                           ("FIXME" . hl-todo)))
  :hook (after-init . global-hl-todo-mode))

;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
;; Edit remote file: /method:user@host#port:filename.
;; Shortcut /ssh:: will connect to default user@host#port.
;; Edit local file with sudo: C-x C-f /sudo::/etc/hosts
;; Open a file with ssh + sudo: C-x C-f /ssh:host|sudo:root:/etc/passwd
(use-package tramp
  :custom
  (tramp-default-method "ssh") ; ssh is faster than the default scp
  (tramp-default-user "swarnendu")
  (tramp-default-host "172.27.15.105")
  ;; Auto-save to a local directory for better performance
  (tramp-auto-save-directory (concat dotemacs-temp-directory "tramp-auto-save"))
  (tramp-persistency-file-name (concat dotemacs-temp-directory "tramp"))
  (tramp-verbose 1) ; Default is 3
  (remote-file-name-inhibit-cache nil) ; Remote files are not updated outside of Tramp
  (tramp-completion-reread-directory-timeout nil)
  ;; Disable version control
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp
                                tramp-file-name-regexp))
  :config
  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
  ;; (add-to-list 'tramp-default-method-alist '("" "swarnendu" "ssh"))
  ;; (add-to-list 'tramp-default-method-alist
  ;;              '("\\`localhost\\'" "\\`root\\'" "su"))

  ;; If the shell of the server is not bash, then it is recommended to connect with bash
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  ;; Disable backup
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

(use-package counsel-tramp
  :ensure t
  :bind ("C-c d t" . counsel-tramp)
  ;; :config
  ;; (add-hook 'counsel-tramp-pre-command-hook
  ;;           (lambda ()
  ;;             (global-aggressive-indent-mode -1)
  ;;             (projectile-mode -1)))
  ;; (add-hook 'counsel-tramp-quit-hook
  ;;           (lambda ()
  ;;             (global-aggressive-indent-mode 1)
  ;;             (projectile-mode 1)))
  )

(use-package imenu
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items 500)
  (imenu-max-item-length 100)
  :config
  (use-package imenu-anywhere
    :ensure t)
  (use-package popup-imenu
    :ensure t)
  :bind ("C-c C-j" . imenu-anywhere))

(setq tags-revert-without-query t ; Don't ask before rereading the TAGS files if they have changed
      ;; ; Warn when opening files bigger than 200MB, the size is chosen because of large TAGS files
      large-file-warning-threshold (* 250 1024 1024)
      tags-add-tables nil)

;; (use-package xref
;;   :commands xref-etags-mode
;;   :hook (prog-mode . xref-etags-mode)
;;   :bind (("M-'" . xref-find-definitions)
;;          ("C-M-." . xref-find-apropos)
;;          ("M-," . xref-pop-marker-stack)))

;; (use-package ivy-xref
;;   :ensure t
;;   :custom
;;   (xref-show-xrefs-function       #'ivy-xref-show-xrefs)
;;   (xref-show-definitions-function #'ivy-xref-show-defs))

(use-package counsel-gtags
  :ensure t
  :if (and (eq system-type 'gnu/linux) (eq dotemacs-tags-scheme 'gtags))
  :diminish
  ;; :commands (counsel-gtags-find-definition
  ;;            counsel-gtags-find-reference
  ;;            counsel-gtags-find-symbol
  ;;            counsel-gtags-find-file
  ;;            counsel-gtags-create-tags
  ;;            counsel-gtags-update-tags
  ;;            counsel-gtags-dwim)
  :hook ((c-mode c++-mode) . counsel-gtags-mode)
  :custom
  (counsel-gtags-ignore-case nil)
  (counsel-gtags-auto-update t)
  :bind (:map counsel-gtags-mode-map
              ("M-'" . counsel-gtags-dwim)
              ("M-," . counsel-gtags-go-backward)
              ("C-c g s" . counsel-gtags-find-symbol)
              ("C-c g d" . counsel-gtags-find-definition)
              ("C-c g r" . counsel-gtags-find-reference)
              ("C-c g c" . counsel-gtags-create-tags)
              ("C-c g u" . counsel-gtags-update-tags)))

(use-package counsel-etags
  :ensure t
  :if (and (eq system-type 'gnu/linux) (eq dotemacs-tags-scheme 'ctags))
  :bind(("M-'" . counsel-etags-find-tag-at-point)
        ("C-c g t" . counsel-etags-find-tag-at-point)
        ("C-c g s" . counsel-etags-find-symbol-at-point)
        ("C-c g g" . counsel-etags-grep-symbol-at-point)
        ("C-c g f" . counsel-etags-find-tag)
        ("C-c g l" .  counsel-etags-list-tag))
  :init
  (add-hook 'c++-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))
  :custom
  (counsel-etags-update-interval 180) ; How many seconds to wait before rerunning tags for auto-update
  ;; (imenu-create-index-function 'counsel-etags-imenu-default-create-index-function)
  :config
  ;; (setq imenu-create-index-function
  ;;       'counsel-etags-imenu-default-create-index-function)
  (add-to-list 'counsel-etags-ignore-directories ".vscode")
  (add-to-list 'counsel-etags-ignore-directories "build")
  (add-to-list 'counsel-etags-ignore-filenames ".clang-format")
  (add-to-list 'counsel-etags-ignore-filenames "*.json")
  (add-to-list 'counsel-etags-ignore-filenames "*.html")
  (add-to-list 'counsel-etags-ignore-filenames "*.xml")
  )

(use-package helpful
  :ensure t
  :bind
  (("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h f" . helpful-function)))

;; M-x vlf <PATH-TO-FILE>
(use-package vlf ; Speed up Emacs for large files
  :ensure t
  :custom (vlf-application 'dont-ask)
  :config (use-package vlf-setup))

(use-package hungry-delete ; Erase 'all' consecutive white space characters in a given direction
  :ensure t
  :diminish
  :hook (after-init . global-hungry-delete-mode ))

(use-package move-text ; Move text with M-<up> and M-<down> like Eclipse
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
  :mode "\\.dot\\'"
  :custom (graphviz-dot-indent-width 4))

(use-package gnuplot
  :ensure t
  :mode "\\.gp\\'"
  :interpreter ("gnuplot" . gnuplot-mode))

(use-package goto-last-change
  :ensure t
  :bind ("C-x C-\\" . goto-last-change))

(use-package popup
  :ensure t
  :custom (popup-use-optimized-column-computation nil))

(use-package posframe
  :ensure t)

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(use-package popwin
  :ensure t
  :hook (after-init . popwin-mode)
  :config
  ;; (defvar popwin:special-display-config-backup popwin:special-display-config)
  ;; (setq popwin:popup-window-height 20
  ;;       popwin:close-popup-window-timer-interval 0.5)
  ;; ;; Helm buffers include the "help" string
  ;; (push '("*Help*" :noselect t) popwin:special-display-config)
  ;; ;; (push '(dired-mode :position top) popwin:special-display-config)
  ;; (push '(compilation-mode :noselect t) popwin:special-display-config)
  ;; (push '("*Compile-Log*" :noselect t) popwin:special-display-config)
  ;; (push '(svn-info-mode :noselect t) popwin:special-display-config)
  ;; (push '(svn-status-mode) popwin:special-display-config)
  ;; (push '("^\*svn-.+\*$" :regexp t) popwin:special-display-config)
  (push '("*manage-minor-mode*" :noselect t) popwin:special-display-config)
  (push '("*Paradox Report*" :regexp t :noselect t) popwin:special-display-config)
  ;; (push '("*undo-tree\*" :width 0.3 :position right) popwin:special-display-config)
  ;; (push '("*Kill Ring*" :noselect nil) popwin:special-display-config)
  ;; (push '("*Selection Ring:") popwin:special-display-config)
  ;; (push '("*ag search*" :noselect nil) popwin:special-display-config)
  ;; (push '("*ggtags-global*" :stick t :noselect nil :height 30) popwin:special-display-config)
  (push '("*Flycheck errors*" :noselect nil) popwin:special-display-config)
  ;; (push '("*ripgrep-search*" :noselect nil) popwin:special-display-config)
  (push '("^\*magit:.+\*$" :noselect nil) popwin:special-display-config)
  ;; (push '("*xref*" :noselect nil) popwin:special-display-config)
  (push '("*helpful\*" :noselect nil) popwin:special-display-config)
  ;; (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
  ;; (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
  ;; (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  ;; (add-to-list 'popwin:special-display-config '("*Apropos*"))
  ;; (add-to-list 'popwin:special-display-config '("*Warnings*"))
  )

(setq pop-up-frames nil) ; Don't allow Emacs to popup new frames

(use-package sudo-edit ; Edit file with sudo
  :ensure t
  :bind ("M-s e" . sudo-edit))

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
  :disabled t
  :hook (after-init . persistent-scratch-setup-default)
  :custom (persistent-scratch-save-file (concat dotemacs-temp-directory "persistent-scratch"))
  ;; :config
  ;; ;; Enable both autosave and restore on startup
  ;; (ignore-errors (persistent-scratch-setup-default))
  )

(use-package crux
  :ensure t
  :bind ("C-c i" . crux-ispell-word-then-abbrev))

(use-package origami
  :ensure t)

;; (add-hook 'c++-mode-hook (lambda () (interactive)
;;                            (origami-mode 1)
;;                            (call-interactively
;;                             'origami-close-all-nodes)) t)

(use-package apt-sources-list
  :ensure t
  :mode ("\\.list\\'" . apt-sources-list-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook ((text-mode prog-mode) . rainbow-delimiters-mode))

(use-package ssh-config-mode
  :ensure t
  :mode ("/\\.ssh/config\\'" . ssh-config-mode)
  :mode ("/sshd?_config\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)
  :config (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(use-package ace-window
  :ensure t
  :bind (("C-c w" . ace-window)
         ([remap other-window] . ace-window)))

(use-package super-save ; Save buffers when Emacs loses focus
  :ensure t
  :diminish
  :custom (super-save-auto-save-when-idle t)
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode 1))

(use-package avy
  :ensure t
  :bind (("M-b" . avy-goto-word-1)
         ("C-'" . avy-goto-char)
         ("C-/" . avy-goto-line))
  :custom
  (avy-background t)
  (avy-highlight-first t)
  (avy-all-windows nil)
  ;; Option pre is a bit distracting because of all the movement while highlighting selection keys.
  ;; This causes the eyes to lose focus.
  (avy-style 'at)
  :config
  ;; It will bind, for example, avy-isearch to C-' in isearch-mode-map, so that you can select one
  ;; of the currently visible isearch candidates using avy.
  (avy-setup-default))

(use-package bookmark
  :custom (bookmark-default-file (concat dotemacs-temp-directory "bookmarks")))

(use-package bm
  :ensure t
  :bind (("C-<f1>" . bm-toggle)
         ("C-<f2>" . bm-next)
         ("C-<f3>" . bm-previous)))

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
  :custom (logview-cache-filename (concat dotemacs-temp-directory "logview-cache.extmap")))

(use-package markdown-mode
  :ensure t
  :diminish gfm-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  ;; :bind ("C-c C-d" . nil)
  :custom
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-enable-math t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-list-indent-width 2)
  (markdown-command "pandoc -f markdown -s "))

(use-package markdown-mode+
  :ensure t)

(use-package pandoc-mode
  :ensure t
  :diminish
  :config (add-hook 'pandoc-mode-hook #'pandoc-load-default-settings)
  :hook (markdown-mode . pandoc-mode))

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package boogie-friends
  :ensure t
  :mode ("\\.smt\\'" . z3-smt2-mode))

(use-package company-math
  :ensure t
  :ensure math-symbol-lists ; Required by ac-math and company-math
  :preface
  (defun company-math-setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands company-math-symbols-unicode))
                        company-backends)))
  :config (add-hook 'TeX-mode-hook #'company-math-setup))

(use-package bibtex-completion
  :custom
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  (bibtex-completion-cite-default-as-initial-input t)
  (bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:10}"))))

(use-package ivy-bibtex
  :ensure t
  :bind ("C-c l x" . ivy-bibtex)
  :custom (ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package company-bibtex
  :ensure t
  :config (add-to-list 'company-backends 'company-bibtex))

(use-package company-reftex
  :ensure t
  :config
  (add-to-list 'company-backends 'company-reftex-labels)
  (add-to-list 'company-backends 'company-reftex-citations))

;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
(defun sb/save-buffer-and-run-latexmk ()
  "Save the current buffer and run LaTeXMk also."
  (interactive)
  (let ((process (TeX-active-process))) (if process (delete-process process)))
  (let ((TeX-save-query nil)) (TeX-save-document ""))
  (TeX-command-menu "LaTeXMk"))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-s") #'sb/save-buffer-and-run-latexmk)))

(global-prettify-symbols-mode -1)

(use-package make-mode
  :mode (("\\Makefile\\'" . makefile-mode)
         ("makefile\\.rules\\'" . makefile-gmake-mode)))

(use-package electric
  :disabled t
  :hook (prog-mode . electric-layout-mode))

;; https://emacs.stackexchange.com/questions/31414/how-to-globally-disable-eldoc
(use-package eldoc
  :if (eq system-type 'gnu/linux)
  :diminish
  :config (global-eldoc-mode -1))

(use-package octave
  :mode "\\.m\\'")

(use-package ini-mode
  :ensure t
  :mode "\\.ini\\'")

;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (or (string-equal major-mode "lisp-mode") (string-equal major-mode "emacs-lisp-mode"))
;;               (check-parens))))

(add-hook 'lisp-mode-hook
          (lambda ()
            (when buffer-file-name
              (add-hook 'after-save-hook 'check-parens nil t))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (when buffer-file-name
              (add-hook 'after-save-hook 'check-parens nil t))))

;; Available C style: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Built_002din-Styles
;; gnu: The default style for GNU projects
;; k&r: What Kernighan and Ritchie, the authors of C used in their book
;; bsd: What BSD developers use, aka "Allman style" after Eric Allman.
;; whitesmith: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; stroustrup: What Stroustrup, the author of C++ used in his book
;; ellemtel: Popular C++ coding standards as defined by "Programming in C++, Rules and
;; Recommendations," Erik Nyquist and Mats Henricson, Ellemtel
;; linux: What the Linux developers use for kernel development
;; python: What Python developers use for extension modules
;; java: The default style for java-mode (see below)
;; user: When you want to define your own style

(setq-default c-default-style '((java-mode . "java")
                                (c-mode . "k&r")
                                (c++-mode . "stroustrup")
                                (other . "gnu/linux")
                                (awk-mode . "awk")))

(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)
  :mode ("\\.c\\'" . c++-mode)
  :custom
  (c-set-style "cc-mode")
  (c-basic-offset 2)
  (c-auto-newline 1)
  (c-electric-flag nil)
  :config
  (c-toggle-electric-state -1)
  (c-toggle-syntactic-indentation -1)
  (unbind-key "C-M-a" c-mode-map)
  :bind (:map c-mode-base-map
              ("C-c c a" . c-beginning-of-defun)
              ("C-c c e" . c-end-of-defun)
              ("M-q" . c-fill-paragraph)))

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (dolist (paths '(
                   "/usr/include"
                   "/usr/include/clang/7"
                   "/usr/include/boost"
                   "/usr/include/linux"
                   "/usr/include/c++/7"
                   "/usr/include/c++/7/tr1"
                   "/usr/local/include"))
    (add-to-list 'company-c-headers-path-system paths)))

(use-package cuda-mode
  :ensure t
  :mode ("\\.cu\\'"	. c++-mode))

(use-package opencl-mode
  :ensure t
  :mode ("\\.cl\\'" . opencl-mode))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" "\\.cmake\\'")
  :config
  (use-package cmake-font-lock
    :ensure t
    :hook (cmake-mode . cmake-font-lock-activate)))

(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(setq python-shell-interpreter "python3")

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

(use-package autodisass-java-bytecode ; Can disassemble .class files from within jars as well
  :ensure t)

(use-package sh-script ; Shell script mode
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\bashrc\\'" . sh-mode))
  :custom
  (sh-basic-offset 4)
  (sh-indent-comment t)
  (sh-indent-after-continuation 'always)
  :config
  ;; Was bound to sh-cd-here
  (unbind-key "C-c C-d" sh-mode-map))

;; (use-package company-shell
;;   :ensure t
;;   :config
;;   (setq company-shell-delete-duplicates t)
;;   (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'")

(use-package shfmt
  :ensure reformatter
  :load-path "extras/shfmt"
  :ensure-system-package shfmt
  :custom (shfmt-arguments "-i 4 -p -ci")
  :hook (sh-mode . shfmt-enable-on-save))

(use-package flycheck-shfmt
  :ensure reformatter
  :after flycheck
  :load-path "extras/shfmt"
  :config (flycheck-shfmt-setup))

(use-package fish-completion
  :ensure t
  :if (when (executable-find "fish"))
  :config (global-fish-completion-mode))

;; (defun sb/company-sh-backends ()
;;   "Add backends for C/C++ completion in company mode."
;;   (make-local-variable 'company-backends)
;;   (setq company-backends
;;         '((;; Generic backends
;;            company-files
;;            company-keywords
;;            company-capf
;;            company-dabbrev
;;            company-dabbrev-code
;;            ;; Mode-specific
;;            company-shell
;;            company-fish-shell))))
;; (add-hook 'sh-mode-hook 'sb/company-sh-backends)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (transient-levels-file (concat dotemacs-temp-directory "transient/levels.el"))
  (transient-values-file (concat dotemacs-temp-directory "transient/values.el"))
  (transient-history-file (concat dotemacs-temp-directory "transient/history.el"))
  (magit-save-repository-buffers t)
  (magit-completing-read-function 'ivy-completing-read)
  ;; (magit-display-buffer-function 'switch-to-buffer)
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; (magit-post-display-buffer-hook #'(lambda ()
  ;;                                     (when (derived-mode-p 'magit-status-mode)
  ;;                                       (delete-other-windows))))
  )

(use-package magit-popup
  :after magit)

(use-package git-modes
  :after magit)

(use-package git-gutter
  :ensure t
  :after magit
  :diminish
  :hook (after-init . global-git-gutter-mode))

;; https://emacs.stackexchange.com/questions/16469/how-to-merge-git-conflicts-in-emacs
(defun sb/enable-smerge-maybe ()
  (when (and buffer-file-name (vc-backend buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1)))))
(add-hook 'buffer-list-update-hook #'sb/enable-smerge-maybe)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred lsp-format-buffer)
  :hook (((python-mode sh-mode html-mode javascript-mode latex-mode plain-tex-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-clients-clangd-args '("-j=4" "-background-index" "--clang-tidy" "-log=error"))
  (lsp-enable-file-watchers nil) ; Could be a directory-local variable
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-semantic-highlighting nil) ; Options: nil, immediate, deferred
  (lsp-flycheck-live-reporting t)
  (lsp-html-format-wrap-line-length 100)
  (lsp-html-format-indent-inner-html t)
  (lsp-idle-delay 0.5)
  (lsp-imenu-sort-methods '(position))
  (lsp-log-io t) ; Disable after a bit of testing
  (lsp-prefer-flymake nil)
  (lsp-prefer-capf nil)
  ;; (lsp-pyls-configuration-sources ["pylint" "yapf"])
  (lsp-pyls-plugins-autopep8-enabled nil)
  (lsp-pyls-plugins-mccabe-enabled nil)
  (lsp-pyls-plugins-preload-modules ["numpy"])
  (lsp-pyls-plugins-pycodestyle-enabled nil)
  (lsp-pyls-plugins-pycodestyle-max-line-length 100)
  (lsp-pyls-plugins-pydocstyle-convention "pep257")
  (lsp-pyls-plugins-pydocstyle-ignore ["D100", "D101","D103","D213"])
  (lsp-pyls-plugins-pyflakes-enabled nil)
  (lsp-pyls-plugins-pylint-enabled t)
  (lsp-pyls-plugins-pylint-args ["-j 2"])
  (lsp-pyls-plugins-yapf-enabled t)
  (lsp-session-file (concat dotemacs-temp-directory ".lsp-session-v1"))
  (lsp-xml-logs-client nil)
  (lsp-xml-jar-file (expand-file-name
                     (locate-user-emacs-file
                      "org.eclipse.lemminx-0.11.1-uber.jar")))
  (lsp-yaml-print-width 100)
  :config
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

  ;; (lsp-register-client
  ;; (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))
  ;; (add-to-list 'lsp-language-id-configuration '(plain-tex-mode . "plaintex"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "~/.cargo/bin/texlab")
                    :major-modes '(plain-tex-mode latex-mode)
                    :remote? t
                    :server-id 'texlab-remote))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "cmake-language-server")
                    :major-modes '(cmake-mode)
                    :remote? t
                    :server-id 'cmakels-remote))

  (use-package lsp-pyls
    :config
    (add-hook 'python-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (lsp-format-buffer)) nil t))))


  (add-hook 'c++-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda ()
                          (lsp-format-buffer)) nil t)))

  (add-hook 'sh-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        (lambda () (lsp-format-buffer))
                        nil t)))

  :bind (("M-." . lsp-find-definition)
         ("C-c l i" . lsp-goto-implementation)
         ("C-c l t" . lsp-goto-type-definition)
         ("C-c l r" . lsp-rename)
         ("C-c l h" . lsp-symbol-highlight)
         ("C-c l f" . lsp-format-buffer)
         ("C-c l r" . lsp-find-references)
         ("C-c l R" . lsp-find-definition)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  ;; :custom
  ;; (lsp-ui-doc-enable nil)
  ;; (lsp-ui-doc-header nil)
  ;; (lsp-ui-doc-include-signature t)
  ;; (lsp-ui-doc-position 'at-point)
  ;; (lsp-ui-doc-use-childframe nil)
  ;; (lsp-ui-flycheck-enable nil)
  ;; (lsp-ui-flycheck-list-position 'right)
  ;; (lsp-ui-imenu-enable t)
  ;; (lsp-ui-peek-enable t)
  ;; (lsp-ui-peek-list-width 60)
  ;; (lsp-ui-peek-peek-height 25)
  ;; (lsp-ui-sideline-enable nil)
  ;; (lsp-ui-sideline-ignore-duplicate t)
  ;; (lsp-ui-sideline-show-hover t)
  ;; (lsp-ui-sideline-show-symbol nil)
  ;; :config
  ;; (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ;; (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )

(use-package lsp-origami
  :ensure t
  :ensure origami
  :hook (origami-mode . lsp-origami-mode))

(use-package company-lsp
  :ensure t
  :disabled t
  :commands company-lsp
  :custom (company-lsp-cache-candidates 'auto)
  :config (push 'company-lsp company-backends))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp)
  :custom (lsp-java-inhibit-message t)
  :config (add-hook 'java-mode-hook
                    (lambda ()
                      (add-hook 'before-save-hook
                                (lambda () (lsp-format-buffer))
                                nil t))))

(use-package lsp-python-ms
  :disabled t
  :load-path "extras")

(use-package lsp-treemacs
  :ensure t
  :after (lsp treemacs)
  :commands lsp-treemacs-errors-list)

(use-package lsp-java-treemacs
  :after treemacs)

(use-package bazel-mode
  :ensure t)

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto$")

;; Function definitions

;; http://stackoverflow.com/questions/15254414/how-to-silently-save-all-buffers-in-emacs
(defun sb/save-all-buffers ()
  "Save all modified buffers without prompting."
  (interactive)
  (save-some-buffers t))

(defun sb/kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

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
   (format "%s -f TAGS -e -R %s" dotemacs-ctags-path (directory-file-name dir-name))))

(defun sb/create-gtags (dir-name)
  "Create tags file with gtags."
  (interactive "Directory: ")
  (shell-command
   (format "%s %s" dotemacs-gtags-path (directory-file-name dir-name))))

;; Generic keybindings, package-specific are usually in their own modules. Use `M-x describe-personal-keybindings` to see modifications.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bind-key*, bind* overrides all minor mode bindings. The kbd macro is not required with bind-key
;; variants. With bind-key, you do not need an explicit "(kbd ...)". Other variants: (global-set-key
;; (kbd "RET") 'newline-and-indent) (define-key global-map (kbd "RET") 'newline-and-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bind-keys
 ("RET" . newline-and-indent)
 ("C-l" . goto-line)
 ("C-c z" . repeat)
 ("C-z" . undo))

(bind-keys
 ("<f10>" . other-window) ; Switch to the other buffer
 ("<f11>" . delete-other-windows)
 ("C-x k" . kill-this-buffer)
 ("<f12>" . sb/kill-other-buffers)
 ("M-<left>" . previous-buffer)
 ("M-<right>" . next-buffer)
 ("C-c d f" . #'auto-fill-mode))

;; In a line with comments, "C-u M-;" removes the comments altogether. That means deleting the
;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
(bind-keys*
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box))

(bind-keys*
 ("C-s" . save-buffer)
 ("C-S-s" . sb/save-all-buffers))
(unbind-key "C-x s") ; Bound to save-some-buffers
(bind-key "C-x s" #'sb/switch-to-scratch)

(use-package default-text-scale
  :ensure t
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))

(use-package which-key ; Show help popups for prefix keys
  :ensure t
  :hook (after-init . which-key-mode)
  :config (which-key-setup-side-window-right-bottom)
  :diminish)

;; Mark safe variables

(put 'company-clang-arguments 'safe-local-variable #'listp)
(put 'flycheck-clang-include-path 'safe-local-variable #'listp)
(put 'flycheck-gcc-include-path 'safe-local-variable #'listp)
(put 'reftex-default-bibliography 'safe-local-variable #'listp)
(put 'company-bibtex-bibliography 'safe-local-variable #'listp)
(put 'bibtex-completion-bibliography 'safe-local-variable #'listp)
(put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
(put 'projectile-project-root 'safe-local-variable #'stringp)
(put 'counsel-find-file-ignore-regexp 'safe-local-variable #'stringp)
(put 'counsel-etags-project-root 'safe-local-variable #'stringp)
(put 'tags-table-list 'safe-local-variable #'listp)
(put 'pyvenv-activate 'safe-local-variable #'stringp)
(put 'flycheck-python-pylint-executable 'safe-local-variable #'stringp)

;;; init.el ends here
