;;; init.el --- Emacs customization  -*- lexical-binding: t; no-byte-compile: nil; -*-
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

(setq debug-on-error nil
      user-full-name "Swarnendu Biswas")


;; Setup configuration variables

(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'local)

(defcustom dotemacs-temp-directory (concat user-emacs-directory "tmp/")
  "Storage location for various configuration files."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-extras-directory (concat user-emacs-directory "extras/")
  "Directory listing third-party packages and files."
  :type 'string
  :group 'dotemacs)

(defcustom dotemacs-modules-directory (concat user-emacs-directory "modules/")
  "Directory containing setup files for customized configuration."
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
          (const :tag "leuven" leuven)
          (const :tag "professional" professional)
          (const :tag "eclipse" eclipse)
          (const :tag "spacemacs-light" spacemacs-light)
          (const :tag "zenburn" zenburn)
          (const :tag "solarized-light" solarized-light)
          (const :tag "solarized-dark" solarized-dark)
          (const :tag "tangotango" tangotango)
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

;; (defcustom dotemacs-window-split
;;   'vertical
;;   "Specify the direction in which the windows should be split. This depends on the orientation of the display."
;;   :type '(radio
;;           (const :tag "vertical" vertical)
;;           (const :tag "horizontal" horizontal))
;;   :group 'dotemacs)

(defconst dotemacs-fill-column 100
  "Column beyond which lines should not extend.")

(defcustom dotemacs-delete-trailing-whitespace-p
  t
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.  Sometimes we do not want to unnecessarily add
differences due to whitespaces."
  :type 'boolean
  :group 'dotemacs)

;; (defcustom dotemacs-use-ignoramus-p
;;   nil
;;   "Should the ignoramus package be used?
;; The package controls ignoring boring file expressions."
;;   :type 'boolean
;;   :group 'dotemacs)

;; (defcustom dotemacs-use-ecb
;;   nil
;;   "Should the ECB package be activated?
;; If yes, then we disable some other packages, like popwin and which-key."
;;   :type 'boolean
;;   :group 'dotemacs)

;; FIXME: Use this as a conditional
(defcustom dotemacs-tags
  'gtags
  "Choose whether to use gtags or ctags."
  :type '(radio
          (const :tag "gtags" gtags)
          (const :tag "ctags" ctags)
          (const :tag "none" none))
  :group 'dotemacs)


;; Setup the package system

(setq load-prefer-newer t)

(eval-when-compile
  (require 'package)
  (setq package-user-dir (expand-file-name (concat user-emacs-directory "elpa"))
        ;; Avoid loading packages twice
        package-enable-at-startup nil)
  (package-initialize)

  ;; (add-to-list 'package-archives
  ;;              '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;;              '("elpy" . "https://jorgenschaefer.github.io/packages/") t)
  )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq use-package-check-before-init t
      use-package-always-ensure nil
      use-package-always-defer t
      use-package-verbose t
      ;; Set this to true once the configuration is stable
      use-package-expand-minimally nil)

(use-package use-package-ensure-system-package
  :ensure t)

;; ;; https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
;; (defun package--save-selected-packages (&rest opt) nil)

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
  (paradox-github-token t)
  (paradox-execute-asynchronously t)
  :config
  ;; (use-package async
  ;;   :ensure t)
  ;; (setq paradox-execute-asynchronously t
  ;;       paradox-spinner-type 'random)
  ;; (defalias 'upgrade-packages 'paradox-upgrade-packages)
  (paradox-enable))

;; www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/
(use-package cus-edit
  :custom (custom-file dotemacs-emacs-custom-file)
  ;; :config
  ;; There seems to be not much benefit in loading the contents of the custom file
  ;; (when (file-exists-p custom-file)
  ;;   (load custom-file :noerror))
  )

(use-package exec-path-from-shell
  :ensure t
  :custom (exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(x))
    (exec-path-from-shell-initialize)))


;; Configure GNU Emacs defaults

(setq inhibit-default-init t ; Disable loading of "default.el" at startup, inhibits site default settings
      inhibit-startup-screen t ; inhibit-splash-screen is an alias
      inhibit-startup-echo-area-message t
      initial-major-mode 'text-mode ; *scratch* is in Lisp interaction mode by default, use text mode instead
      initial-scratch-message nil
      create-lockfiles nil
      message-log-max 500
      ;; line-number-display-limit 2000000
      ring-bell-function 'ignore ; Turn off alarms completely: https://www.emacswiki.org/emacs/AlarmBell
      x-underline-at-descent-line t ; Draw underline lower
      gc-cons-threshold (* 10 1024 1024) ; Increase gc threshold
      use-dialog-box nil
      use-file-dialog nil
      delete-by-moving-to-trash t
      scroll-margin 0 ; Drag the point along while scrolling
      scroll-conservatively 1000 ; Never recenter the screen while scrolling
      scroll-error-top-bottom t ; Move to begin/end of buffer before signalling an error
      scroll-preserve-screen-position t
      completion-ignore-case t ; Ignore case when completing
      read-file-name-completion-ignore-case t ; Ignore case when reading a file name completion
      read-buffer-completion-ignore-case t
      switch-to-buffer-preserve-window-point t
      x-stretch-cursor t ; Make cursor the width of the character it is under i.e. full width of a TAB
      ;; auto-save-list-file-prefix (concat dotemacs-temp-directory "auto-save")
      auto-save-list-file-prefix nil
      select-enable-clipboard t ; Enable use of system clipboard across Emacs and other applications
      require-final-newline t ; Always end a file with a newline.
      make-backup-files nil ; Stop making backup ~ files
      backup-inhibited t ; Disable backup for a per-file basis, not to be used by major modes.
      auto-save-default t
      confirm-kill-emacs nil
      ;; idle-update-delay 2
      ;; We need to paste something from another program, but sometimes we do real paste after some
      ;; kill action, that will erase the clipboard, so we need to save it to kill ring. Paste it
      ;; using "C-y M-y".
      save-interprogram-paste-before-kill t
      kill-whole-line t
      suggest-key-bindings t
      shift-select-mode t ; Use shift-select for marking
      blink-matching-paren t
      kill-ring-max 20
      kill-do-not-save-duplicates t
      set-mark-command-repeat-pop t
      confirm-nonexistent-file-or-buffer t
      vc-handled-backends nil)

(setq-default major-mode 'text-mode ; Major mode to use for files that do no specify a major mode,
                                        ; default value is fundamental-mode
              sentence-end-double-space nil
              truncate-lines nil
              truncate-partial-width-windows nil
              history-length 50
              history-delete-duplicates t ; Delete duplicate (identical and old) elements in the minibuffer history
              ;; Disabling this is one way to speed up Emacs with buffers with long lines
              bidi-display-reordering nil)

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
  :diminish ;auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        ;; Auto-refresh dired buffers
        global-auto-revert-non-file-buffers t))

;; Typing with the mark active will overwrite the marked region, pending-delete-mode is an alias
(use-package delsel
  :hook (after-init . delete-selection-mode))

;; (use-package advice
;;   :config
;; Turn off warnings due to functions being redefined
(setq ad-redefinition-action 'accept)
;; )

(use-package saveplace ; Remember cursor position in files
  :unless noninteractive
  :hook (after-init . save-place-mode)
  :config (setq save-place-file (concat dotemacs-temp-directory "places")))

(use-package savehist ; Save minibuffer histories across sessions
  :unless noninteractive
  :hook (after-init . savehist-mode)
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-file (concat dotemacs-temp-directory "savehist"))
  (savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history
                                   file-name-history
                                   command-history))
  (savehist-autosave-interval 300))

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-after-kill-buffer-p t)
  (uniquify-strip-common-suffix t))

(use-package hippie-exp
  :init
  (use-package hippie-exp-ext
    :ensure t)
  :custom
  ;; https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el
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
  :diminish subword-mode
  :hook (after-init . global-subword-mode))

;; ;; Set Emacs split to horizontal or vertical
;; ;; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
;; (if (eq dotemacs-window-split 'horizontal)
;;     (setq split-height-threshold nil
;;           split-width-threshold 10)
;;   (setq split-height-threshold 10
;;         split-width-threshold nil))

;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun my-auto-save-wrapper (save-fn &rest args)
  "Hide 'Auto-saving...done' messages by calling the method SAVE-FN with non-nil ARGS."
  (apply save-fn '(t)))
(advice-add 'do-auto-save :around #'my-auto-save-wrapper)

;; (use-package warnings
;;   :config (add-to-list 'warning-suppress-types '(undo discard-info)))

(use-package abbrev
  :diminish abbrev-mode
  :hook ((text-mode prog-mode) . abbrev-mode)
  ;; :init
  ;; (dolist (hook '(text-mode-hook prog-mode-hook))
  ;;   (add-hook hook #'abbrev-mode ))
  :custom
  (abbrev-file-name (concat dotemacs-extras-directory "abbrev_defs"))
  ;; Do not ask to save new abbrevs when quitting
  (save-abbrevs 'silently))


;; Configure GNU Emacs appearance

(setq frame-title-format (list '(buffer-file-name "%f" "%b")) ; Better frame title
      indicate-empty-lines t
      custom-safe-themes t)

(setq-default indicate-buffer-boundaries 'right)

;; https://ladicle.com/post/config/#configuration
(if window-system
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)))

;; (use-package tool-bar
;;   :if (fboundp 'tool-bar-mode)
;;   :config (tool-bar-mode -1))

;; (use-package menu-bar
;;   :if (fboundp 'menu-bar-mode)
;;   :config (menu-bar-mode -1))

;; (use-package scroll-bar
;;   :if (fboundp 'scroll-bar-mode)
;;   :config
;;   ;; Maximize the space for displaying the buffer
;;   (scroll-bar-mode -1))

;; (use-package frame
;;   :config
;; ;; Start with Emacs window maximized:
;; ;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
;; ;; Only the frame that Emacs creates at startup, but will not touch any subsequent frames you create.
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; ;; It will maximize all frames: both the first one and any others you create. Options: fullheight, fullboth
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Blinking cursor is distracting
(blink-cursor-mode -1)

;; )

(toggle-frame-maximized) ; Maximize Emacs on startup

;; ;; Not the most useful information within Emacs
;; (use-package time ; Display the time and date in the mode line
;;   :disabled t
;;   :config
;;   (setq display-time-day-and-date t
;;         display-time-24hr-format nil
;;         display-time-default-load-average nil)
;;   (display-time))

;; ;; linum-mode can slow down Emacs for large files:
;; ;; http://blog.binchen.org/posts/turn-off-linum-mode-when-file-is-too-big.html
;; ;; Display line numbers in the margin
;; (or (use-package linum
;;       :disabled t
;;       :config (global-linum-mode 1))

;;     (use-package nlinum ; Might improve performance with jit font locking.
;;       :ensure t
;;       :disabled t ;; Does not work with emacsclient, since the frame is created later.
;;       :config (global-nlinum-mode 1)))

;; (use-package hlinum ; Extension to linum-mode to highlight current line number in the margin
;;   :ensure t
;;   :disabled t
;;   :config (hlinum-activate))

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
                                                          :box nil)
                                      ;; Org-mode customizations inspired from leuven theme
                                      (with-eval-after-load "org"
                                        (set-face-attribute 'org-level-1 nil
                                                            :height 1.2
                                                            :overline "#A7A7A7"
                                                            :foreground "#3C3C3C"
                                                            :background "#F5F5F5")
                                        (set-face-attribute 'org-level-2 nil
                                                            :height 1.1
                                                            :overline "#123555"
                                                            :foreground "#123555"
                                                            :background "#E5F4FB"))))

      ((eq dotemacs-theme 'spacemacs-light) (use-package spacemacs-common
                                              :ensure spacemacs-theme
                                              :init
                                              (load-theme 'spacemacs-light t)
                                              (add-to-list 'default-frame-alist '(background-color . "#fbf8ef"))))

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

;; https://gist.github.com/3demax/1264635/91ccb6c423effd811dbdb1412b70c15e95fa700d
;; https://emacs.stackexchange.com/questions/984/what-is-the-right-way-to-install-tab-bar
;; https://www.emacswiki.org/emacs/TabBarMode
(use-package tabbar
  :ensure t
  :preface
  (defun sb/tabbar-modification-state-change ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  (defun sb/tabbar-on-buffer-modification ()
    (set-buffer-modified-p t)
    (sb/tabbar-modification-state-change))

  (defun sb/tabbar-on-buffer-revert ()
    (set-buffer-modified-p nil)
    (sb/tabbar-modification-state-change))
  :hook (after-init . tabbar-mode)
  :config
  (setq tabbar-use-images nil ; Speed up by not using images
        tabbar-auto-scroll-flag t
        ;; tabbar-background-color nil
        ;; tabbar-separator '(0.2)
        )

  (add-hook 'after-save-hook #'sb/tabbar-modification-state-change)
  (add-hook 'first-change-hook #'sb/tabbar-on-buffer-modification)
  (add-hook 'after-revert-hook #'sb/tabbar-on-buffer-revert)

  ;; Add a buffer modification state indicator in the tab label, and place a space around the label
  ;; to make it look less crowded.
  (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
    (setq ad-return-value
          (if (and (buffer-modified-p (tabbar-tab-value tab))
                   (buffer-file-name (tabbar-tab-value tab)))
              (concat " * " (concat ad-return-value " "))
            (concat " " (concat ad-return-value " ")))))

  ;; (if (eq dotemacs-theme 'spacemacs-light)
  ;;     (progn
  ;;       (set-face-attribute 'tabbar-unselected nil
  ;;                           :inherit 'tabbar-unselected
  ;;                           ;; :background "gray90"
  ;;                           ;; :height 0.9
  ;;                           )
  ;;       (set-face-attribute 'tabbar-selected nil
  ;;                           :inherit 'tabbar-default
  ;;                           :height 1
  ;;                           ;; :bold t
  ;;                           ;; :underline nil
  ;;                           )
  ;;       ;; (set-face-attribute 'tabbar-separator nil
  ;;       ;;                     :inherit 'tabbar-separator
  ;;       ;;                     :height 1.0)
  ;;       ;; (set-face-attribute 'tabbar-modified nil
  ;;       ;;                     :inherit 'tabbar-modified
  ;;       ;;                     ;; :foreground "red"
  ;;       ;;                     :height 0.9)
  ;;       ;; (set-face-attribute 'tabbar-selected-modified nil
  ;;       ;;                     :inherit 'tabbar-selected-modified
  ;;       ;;                     ;; :foreground "dark green"
  ;;       ;;                     :height 1.1
  ;;       ;;                     :bold t)
  ;;       )
  ;;   (progn
  ;;     ;; (set-face-attribute 'tabbar-default nil
  ;;     ;;                     :inherit nil
  ;;     ;;                     :height 0.9
  ;;     ;;                     :weight 'normal
  ;;     ;;                     :width 'normal
  ;;     ;;                     :slant 'normal
  ;;     ;;                     :underline nil
  ;;     ;;                     :strike-through nil
  ;;     ;;                     :stipple nil
  ;;     ;;                     :background "gray80"
  ;;     ;;                     :foreground "black"
  ;;     ;;                     ;; :box '(:line-width 2 :color "white" :style nil)
  ;;     ;;                     :box nil
  ;;     ;;                     ;; :family "Lucida Grande"
  ;;     ;;                     ;;:family "helvetica"
  ;;     ;;                     )

  ;;     ;; (set-face-attribute 'tabbar-default nil
  ;;     ;;                     :background "gray80")

  ;;     (set-face-attribute 'tabbar-selected nil
  ;;                         :inherit 'tabbar-default
  ;;                         :background "gray95"
  ;;                         :foreground "gray20"
  ;;                         :height 0.95
  ;;                         :box '(:line-width 3 :color "grey95" :style nil))

  ;;     ;; (set-face-attribute 'tabbar-selected nil
  ;;     ;;                 :inherit 'tabbar-default
  ;;     ;;                 :background "#f2f2f6"
  ;;     ;;                 :foreground "black"
  ;;     ;;                 ;; :box '(:line-width 1 :color "black" :style pressed-button)
  ;;     ;;                 :height 1.2
  ;;     ;;                 :bold t
  ;;     ;;                 :underline nil)

  ;;     ;; (set-face-attribute 'tabbar-unselected nil
  ;;     ;;                     :inherit 'tabbar-default
  ;;     ;;                     :background "gray80"
  ;;     ;;                     :box '(:line-width 3 :color "grey80" :style nil))

  ;;     ;; (set-face-attribute 'tabbar-unselected nil
  ;;     ;;                     :background "gray88"
  ;;     ;;                     :foreground "gray30"
  ;;     ;;                     :height 0.9)

  ;;     ;; (set-face-attribute 'tabbar-button nil
  ;;     ;;                     :inherit 'tabbar-default
  ;;     ;;                     :box nil)

  ;;     ;; (set-face-attribute 'tabbar-separator nil
  ;;     ;;                     :background "grey50"
  ;;     ;;                     :foreground "grey50"
  ;;     ;;                     :height 1.0)

  ;;     ;; (set-face-attribute 'tabbar-separator nil
  ;;     ;;                     :height 1.0)

  ;;     ;; (set-face-attribute 'tabbar-highlight nil
  ;;     ;;                     :underline t
  ;;     ;;                     :background "lemon chiffon")

  ;;     ;; (set-face-attribute 'tabbar-button nil
  ;;     ;;                     ;; :box '(:line-width 1 :color "gray72" :style released-button)
  ;;     ;;                     )

  ;;     ;; (set-face-attribute 'tabbar-modified nil
  ;;     ;;                     :background "gray88"
  ;;     ;;                     :foreground "red"
  ;;     ;;                     ;; :box '(:line-width 1 :color "black" :style sunken)
  ;;     ;;                     )

  ;;     ;; (set-face-attribute 'tabbar-selected-modified nil
  ;;     ;;                     :background "#f2f2f6"
  ;;     ;;                     :foreground "dark green"
  ;;     ;;                     ;; :box '(:line-width 1 :color "black" :style sunken)
  ;;     ;;                     :box '(:style pressed-button)
  ;;     ;;                     :height 1
  ;;     ;;                     :bold t
  ;;     ;;                     :underline nil)

  ;;     ))

  :bind (:map tabbar-mode-map
              ("M-<left>" . tabbar-backward-tab)
              ("M-<right>" . tabbar-forward-tab)))

;; Set font face independent of the color theme, value is in 1/10pt, so 100 will give you 10pt.

;; (if (eq system-type 'windows-nt)
;;     (set-face-attribute 'default nil
;;                         :family "Consolas"
;;                         :height 120)
;;   (progn
;;     (cond ((string-equal (system-name) "consensus.ices.utexas.edu") (set-face-attribute
;;                                                                      'default nil
;;                                                                      :height 135))
;;           ((string-equal (system-name) "swarnendu") (set-face-attribute 'default nil
;;                                                                         ;; :family "Fira Code"
;;                                                                         :height 160))
;;           (t (set-face-attribute 'default nil
;;                                  ;; :family "Droid Sans Mono"
;;                                  :height 120)))))

(set-frame-font "DejaVu Sans Mono" nil t)
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 130)

;; FIXME: This is not working.
(use-package minimap
  :ensure t
  :diminish minimap-mode
  :custom
  (minimap-major-modes '(prog-mode))
  (minimap-window-location 'right)
  (minimap-minimum-width 10)
  (minimap-width-fraction 0.08)
  (minimap-update-delay 0.2)
  (minimap-automatically-delete-window nil)
  :config
  ;; (add-to-list 'minimap-major-modes 'latex-mode)
  (minimap-mode 1)
  :bind ("C-x m" . minimap-mode))

;; The newline wrap-around symbols in the fringes are ugly.

;; https://stackoverflow.com/questions/27845980/how-do-i-remove-newline-symbols-inside-emacs-vertical-border
;; (setf (cdr (assq 'continuation fringe-indicator-alist))
;;       ;; '(nil nil) ;; no continuation indicators
;;       '(nil right-curly-arrow) ;; right indicator only
;;       ;; '(left-curly-arrow nil) ;; left indicator only
;;       ;; '(left-curly-arrow right-curly-arrow) ;; default
;;       )

;; https://stackoverflow.com/questions/3281581/how-to-word-wrap-in-emacs
(global-visual-line-mode 1)
(diminish 'visual-line-mode)

;; Install fonts with `M-x all-the-icons-install-fonts`
(use-package all-the-icons
  :ensure t)


;; Configure GNU Emacs modeline

;; (defvar spaceline-anzu-p)
;; (defvar spaceline-hud-p)
;; (defvar spaceline-buffer-position-p)
;; (defvar spaceline-projectile-root-p)

(size-indication-mode -1)

(cond ((eq dotemacs-modeline-theme 'powerline) (use-package powerline
                                                 :ensure t
                                                 :config
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
                                                 :defer 5
                                                 :config
                                                 (require 'spaceline-config)
                                                 (setq powerline-height 20
                                                       powerline-default-separator 'slant
                                                       spaceline-anzu-p t
                                                       spaceline-hud-p nil
                                                       spaceline-buffer-modified-p t
                                                       spaceline-buffer-position-p t
                                                       spaceline-projectile-root-p t
                                                       spaceline-paradox-menu-p t)
                                                 ;; Adapted from https://github.com/lunaryorn/.emacs.d/blob/master/init.el
                                                 ;; (spaceline-compile
                                                 ;;   'compact
                                                 ;;   ;; Left side of the mode line
                                                 ;;   '(((buffer-modified buffer-size) :face highlight-face)
                                                 ;;     anzu
                                                 ;;     '(buffer-id remote-host)
                                                 ;;     major-mode
                                                 ;;     (process :when active)
                                                 ;;     ((flycheck-error flycheck-warning flycheck-info) :when active)
                                                 ;;     (paradox-menu :when active)
                                                 ;;     (minor-modes :when active)
                                                 ;;     ((nyan-cat buffer-position) :separator " | "))
                                                 ;;   ;; Right segment
                                                 ;;   '(;;((which-function projectile-root) :separator " | ")
                                                 ;;     (which-function :when active)
                                                 ;;     (projectile-root :when active)
                                                 ;;     (version-control :when active)
                                                 ;;     (battery :when active)
                                                 ;;     selection-info
                                                 ;;     input-method
                                                 ;;     ((point-position line-column) :separator " | ")
                                                 ;;     (global :when active)
                                                 ;;     ,@additional-segments
                                                 ;;     hud))
                                                 ;; (setq-default mode-line-format '("%e" (:eval (spaceline-ml-compact))))

                                                 (spaceline-emacs-theme)
                                                 (spaceline-info-mode)

                                                 (when (eq dotemacs-theme 'spacemacs-light)
                                                   (set-face-attribute 'powerline-active1 nil
                                                                       :background "gray22"
                                                                       :foreground "white"
                                                                       :weight 'light))

                                                 (when (eq dotemacs-theme 'leuven)
                                                   (set-face-attribute 'powerline-active1 nil
                                                                       :background "gray22"
                                                                       :foreground "white"
                                                                       :weight 'light)
                                                   ;;   ;; (set-face-attribute 'powerline-active2 nil
                                                   ;;   ;;                     :background "#1A4B7"
                                                   ;;   ;;                     :foreground "white")
                                                   ;;   (set-face-attribute 'mode-line nil
                                                   ;;                       :background "grey88"
                                                   ;;                       :foreground "black")
                                                   (set-face-attribute 'mode-line-inactive nil
                                                                       :background "grey88"
                                                                       :foreground "black")
                                                   ;;   (set-face-attribute 'mode-line-buffer-id nil
                                                   ;;                       :weight 'bold
                                                   ;;                       :foreground "black"
                                                   ;;                       :inherit 'mode-line)
                                                   )

                                                 ;; (when (eq dotemacs-theme 'default)
                                                 ;;   ;; (set-face-attribute 'spaceline-highlight-face nil
                                                 ;;   ;;                     :background "#1A4B77"
                                                 ;;   ;;                     :foreground "white")
                                                 ;;   (set-face-attribute 'powerline-active1 nil
                                                 ;;                       :background "gray22"
                                                 ;;                       :foreground "white"
                                                 ;;                       :weight 'light)
                                                 ;;   (set-face-attribute 'powerline-inactive1 nil
                                                 ;;                       :background "grey11"
                                                 ;;                       :foreground "white"))

                                                 ))

      ((eq dotemacs-modeline-theme 'default) ))

;; (use-package nyan-mode
;;   :ensure t
;;   :disabled t
;;   :preface
;;   ;; https://github.com/cemerick/.emacs.d#nyan-mode
;;   (defun sb/toggle-nyan-mode (&optional frame)
;;     "Enable/disable nyan mode."
;;     (if (display-graphic-p frame)
;;         (progn
;;           (nyan-mode 1)
;;           (nyan-start-animation)
;;           (setq-default nyan-wavy-trail nil
;;                         nyan-animate-nyancat t
;;                         nyan-bar-length 16
;;                         nyan-cat-face-number 5))
;;       (nyan-mode -1)))
;;   :config
;;   (add-hook 'after-make-frame-functions 'sb/toggle-nyan-mode)
;;   (add-hook 'after-init-hook 'sb/toggle-nyan-mode))


;; Configure ibuffer

(use-package ibuffer
  ;; :commands ibuffer
  ;; :bind ([remap list-buffers] . ibuffer)
  :config
  (defalias 'list-buffers 'ibuffer) ; Turn on ibuffer by default
  (setq ibuffer-expert t
        ibuffer-always-show-last-buffer nil
        ibuffer-default-sorting-mode 'alphabetic ; Options: major-mode
        ibuffer-use-header-line t
        ;; ibuffer-display-summary t
        ;; Ignore case when searching
        ibuffer-case-fold-search t
        ibuffer-show-empty-filter-groups nil
        ibuffer-formats
        '((mark modified read-only " "
                (name 30 30 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  (add-hook 'ibuffer-hook #'ibuffer-auto-mode))

(use-package ibuffer-projectile ; Group buffers by projectile project
  :ensure t
  ;; :after ibuffer
  :init (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups))


;; Configure dired

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
  :config
  (setq dired-auto-revert-buffer t ; Revert each dired buffer automatically when you "revisit" it
        dired-recursive-deletes 'always ; Single prompt for all n directories
        dired-recursive-copies 'always
        dired-listing-switches "-ABhl --si --group-directories-first" ; Check `ls' for additional options
        dired-ls-F-marks-symlinks t ; -F marks links with @
        dired-dwim-target t)
  ;; Auto refresh dired when files change
  (add-hook 'dired-mode-hook 'auto-revert-mode))

(use-package dired-x
  ;; :commands dired-jump
  :config
  (setq dired-bind-jump t
        ;; Do not show messages when omitting files
        dired-omit-verbose nil)
  (unless (bound-and-true-p dotemacs-use-ignoramus-p)
    (add-hook 'dired-mode-hook #'dired-omit-mode))
  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)
  :bind ("C-x C-j" . dired-jump))

;; (use-package dired+
;;   :disabled t
;;   ;; :after dired
;;   :load-path "extras"
;;   :init
;;   ;; Set this flag before dired+ is loaded: http://irreal.org/blog/?p=3341
;;   (setq-default diredp-hide-details-initially-flag nil
;;                 diredp-hide-details-propagate-flag nil)
;;   :config
;;   ;;(diredp-toggle-find-file-reuse-dir 1)
;;   (toggle-diredp-find-file-reuse-dir 1))

(use-package dired-efap
  :ensure t
  :after dired
  :config (setq dired-efap-initial-filename-selection nil)
  :bind (:map dired-mode-map
              ("r" . dired-efap )))

(use-package dired-narrow ; Narrow dired to match filter
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;; (use-package ecb
;;   :ensure t
;;   :disabled t
;;   :if (bound-and-true-p dotemacs-use-ecb)
;;   :config
;;   (ecb-layout-define "swarnendu" left nil
;;                      (ecb-split-ver 0.5 t)
;;                      (if (fboundp (quote ecb-set-sources-buffer)) (ecb-set-sources-buffer) (ecb-set-default-ecb-buffer))
;;                      (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
;;                      (if (fboundp (quote ecb-set-methods-buffer)) (ecb-set-methods-buffer) (ecb-set-default-ecb-buffer))
;;                      (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
;;                      (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
;;                      )
;;   (setq ecb-examples-bufferinfo-buffer-name nil
;;         ecb-create-layout-file (concat dotemacs-temp-directory "ecb-user-layouts.el")
;;         ecb-tip-of-the-day nil
;;         ecb-tree-buffer-style 'ascii-guides
;;         ecb-show-sources-in-directories-buffer 'always
;;         ecb-layout-name "swarna1"
;;         ecb-compile-window-height nil)
;;   (ecb-activate)
;;   (add-hook 'compilation-finish-functions
;;             (lambda (buf strg) (kill-buffer buf))))

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

(or
 ;; (use-package treemacs-icons-dired
 ;;   :ensure t
 ;;   :disabled t
 ;;   :after (treemacs dired)
 ;;   :config (treemacs-icons-dired-mode))

 (use-package all-the-icons-dired
   :ensure t
   :hook (dired-mode . all-the-icons-dired-mode)))


;; Configure search

(setq case-fold-search t) ; Make search ignore case

;; Use "C-'" in isearch-mode-map to use avy-isearch to select one of the currently visible isearch candidates.
(use-package isearch
  ;; :commands (isearch-forward isearch-forward-regexp isearch-repeat-forward)
  :preface
  ;; http://endlessparentheses.com/leave-the-cursor-at-start-of-match-after-isearch.html?source=rss
  (defun sb/isearch-exit-other-end ()
    "Exit isearch, at the opposite end of the string."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
  :config
  (setq search-highlight t ; Highlight incremental search
        isearch-allow-scroll t)
  (use-package isearch-symbol-at-point
    :ensure t)
  :bind (("C-s" . nil) ; isearch-forward-regexp
         ("C-f" . isearch-forward-regexp)
         :map isearch-mode-map
         ("C-s" . nil) ; isearch-repeat-forward
         ("C-f" . isearch-repeat-forward)
         ;; ("C-<return>" . sb/isearch-exit-other-end)
         ))

(use-package isearch-dabbrev
  :ensure t
  :bind (:map isearch-mode-map
              ("<tab>" . isearch-dabbrev-expand)))

(use-package anzu
  :ensure t
  :after isearch
  :diminish anzu-mode
  :hook (after-init . global-anzu-mode)
  :config
  (setq anzu-search-threshold 10000
        anzu-minimum-input-length 2)
  (when (eq dotemacs-modeline-theme 'spaceline)
    (setq anzu-cons-mode-line-p nil))
  (unless (eq dotemacs-theme 'leuven)
    (set-face-attribute 'anzu-mode-line nil
                        :foreground "blue"
                        :weight 'light)))

(use-package swiper ; Performs poorly if there are a large number of matches
  :ensure t
  :config
  (setq swiper-use-visual-line t
        swiper-action-recenter t))

(use-package wgrep
  :ensure t
  :init (setq wgrep-auto-save-buffer t))

(use-package deadgrep
  :ensure t
  :bind ("C-c s r" . deadgrep))


;; Adding directories to the list of recent files decreases the number of entries of recent files.
;; Therefore, we use a different command/keybinding to lookup recent directories.
(use-package recentf
  :config
  (setq recentf-max-menu-items 10
        ;; Set this first so that recentf can load content from this
        recentf-save-file (concat dotemacs-temp-directory "recentf")
        recentf-max-saved-items 200
        ;; Disable this so that recentf does not attempt to stat remote files:
        ;; https://www.emacswiki.org/emacs/RecentFiles
        recentf-auto-cleanup 'never
        recentf-menu-filter 'recentf-sort-descending
        ;; Check regex with re-builder
        recentf-exclude '("[/\\]\\.elpa/"
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

;; Hide the "wrote to recentf" message, which can be irritating.
(defun sb/recentf-save-list (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN."
  (let ((inhibit-message t))
    (apply orig-fun args)))
(advice-add 'recentf-save-list :around #'sb/recentf-save-list)


;; Configure Company as the in-buffer autocompletion framework

(use-package company
  :ensure t
  :diminish company-mode
  ;; :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :preface
  (defun sb/quit-company-save-buffer ()
    "Quit company popup and save the buffer."
    (company-abort)
    (save-buffer))
  :hook (after-init . global-company-mode)
  :config
  (setq company-global-modes t ; Turn on company-mode for all major modes
        company-show-numbers t ; Quick-access numbers for the first ten candidates
        company-minimum-prefix-length 3
        company-idle-delay 0.1
        ;; Invert the navigation direction if the completion popup is displayed on top
        company-tooltip-flip-when-above nil
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-selection-wrap-around t
        company-dabbrev-downcase nil ; Do not downcase the returned candidates
        company-dabbrev-ignore-case nil
        company-dabbrev-code-everywhere t ; Offer completions in comments and strings
        company-dabbrev-other-buffers t ; Search other buffers with the same mode
        company-dabbrev-code-modes t ; Use company-dabbrev-code in all modes
        ;; company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
        ;;                     company-preview-frontend
        ;;                     company-echo-metadata-frontend)
        ;; Allow typing keys that do not match any candidates
        company-require-match nil)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ;; ("C-s" . sb/quit-company-save-buffer)
              ))

;; ;; https://github.com/sebastiencs/company-box/issues/38
;; (use-package company-box
;;   :ensure t
;;   :disabled t
;;   :hook (company-mode . company-box-mode)
;;   :diminish
;;   :config
;;   (setq company-box-backends-colors nil
;;         company-box-show-single-candidate t
;;         company-box-max-candidates 50
;;         company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package company-flx
  :ensure t
  :after company
  :hook (global-company-mode . company-flx-mode)
  :custom (company-flx-limit 20))

;; Use prescient instead

;; (use-package company-statistics
;;   :ensure t
;;   :after company
;;   :hook (global-company-mode . company-statistics-mode)
;;   :init (setq company-statistics-file (concat dotemacs-temp-directory "company-statistics-cache.el")))

(use-package company-quickhelp
  :ensure t
  :after company
  :hook (global-company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay 0.5)
  (company-quickhelp-max-lines 60))

(use-package company-dict
  :ensure t
  :after company
  :config
  (setq company-dict-dir (concat user-emacs-directory "dict/")
        company-dict-enable-fuzzy t
        company-dict-enable-yasnippet nil)
  (add-to-list 'company-backends 'company-dict))

;; (with-eval-after-load "counsel"
;; (bind-key [remap complete-symbol] #'counsel-company company-mode-map)
;; (bind-key [remap completion-at-point] #'counsel-company company-mode-map)
;; (bind-key "C-:" #'counsel-company company-mode-map)
;; (bind-key "C-:" #'counsel-company company-active-map)
;; )

(use-package company-prescient
  :ensure t
  :after (company prescient)
  :hook (global-company-mode . company-prescient-mode))

(use-package yasnippet
  :ensure t
  ;; :commands (yas-expand yas-minor-mode)
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  ;; :init (yas-global-mode 1)
  ;; :hook ((LaTeX-mode prog-mode) . yas-minor-mode)
  :hook (after-init . yas-global-mode)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  :config
  (unbind-key "<tab>" yas-minor-mode-map)
  (use-package yasnippet-snippets))


;; Configure ivy as the generic completion framework

;; (use-package historian
;;   :ensure t
;;   :config (setq historian-save-file (concat dotemacs-temp-directory "historian"))
;;   (historian-mode 1))

;; (use-package ivy-historian
;;   :ensure t
;;   :config (ivy-historian-mode 1))

(use-package ivy
  :ensure t
  ;; :preface
  ;; ;; https://github.com/abo-abo/swiper/wiki/Sort-files-by-mtime
  ;; (defun eh-ivy-return-recentf-index (dir)
  ;;   (when (and (boundp 'recentf-list)
  ;;              recentf-list)
  ;;     (let ((files-list
  ;;            (cl-subseq recentf-list
  ;;                       0 (min (- (length recentf-list) 1) 20)))
  ;;           (index 0))
  ;;       (while files-list
  ;;         (if (string-match-p dir (car files-list))
  ;;             (setq files-list nil)
  ;;           (setq index (+ index 1))
  ;;           (setq files-list (cdr files-list))))
  ;;       index)))

  ;; (defun eh-ivy-sort-file-function (x y)
  ;;   (let* ((x (concat ivy--directory x))
  ;;          (y (concat ivy--directory y))
  ;;          (x-mtime (nth 5 (file-attributes x)))
  ;;          (y-mtime (nth 5 (file-attributes y))))
  ;;     (if (file-directory-p x)
  ;;         (if (file-directory-p y)
  ;;             (let ((x-recentf-index (eh-ivy-return-recentf-index x))
  ;;                   (y-recentf-index (eh-ivy-return-recentf-index y)))
  ;;               (if (and x-recentf-index y-recentf-index)
  ;;                   ;; Directories is sorted by `recentf-list' index
  ;;                   (< x-recentf-index y-recentf-index)
  ;;                 (string< x y)))
  ;;           t)
  ;;       (if (file-directory-p y)
  ;;           nil
  ;;         ;; File is sorted by mtime
  ;;         (time-less-p y-mtime x-mtime)))))
  :config
  (setq ivy-virtual-abbreviate 'abbreviate
        ivy-wrap t ; Useful to be able to wrap around boundary items
        ivy-action-wrap t
        ivy-case-fold-search 'always ; Always ignore case while searching
        ivy-height 20 ; This seems a good number to see several options at a time without cluttering the view
        ivy-fixed-height-minibuffer t ; It is distracting if the mini-buffer height keeps changing
        ivy-extra-directories nil ; Hide "." and ".."
        ivy-count-format "(%d/%d) " ; This is beneficial to identify wrap arounds
        ;; ivy-re-builders-alist '((counsel-find-file . ivy--regex-fuzzy)
        ;;                         (swiper . ivy--regex-plus)
        ;;                         (counsel-rg . ivy--regex-plus)
        ;;                         (counsel-grep-or-swiper . ivy--regex-plus)
        ;;                         (ivy-switch-buffer . ivy--regex-plus)
        ;;                         (t . ivy--regex-fuzzy))
        ivy-flx-limit 100
        ivy-use-ignore-default 'always ; Always ignore buffers set in ivy-ignore-buffers
        ivy-use-selectable-prompt nil
        ivy-auto-select-single-candidate t)
  (dolist (buffer '("^\\*Backtrace\\*$"
                    "^\\*Compile-Log\\*$"
                    "^\\*.+Completions\\*$"
                    "^\\*Help\\*$"
                    "^\\*Ibuffer\\*$"
                    "company-statistics-cache.el"
                    "^\\*lsp-log\\*$"
                    "^\\*pyls\\*$"
                    "^\\*pyls::stderr\\*$"))
    (add-to-list 'ivy-ignore-buffers buffer))

  (setq ivy-sort-matches-functions-alist
        '((t)
          (ivy-switch-buffer . ivy-sort-function-buffer)
          (counsel-find-file . ivy-sort-function-buffer)))

  ;; (add-to-list 'ivy-sort-functions-alist
  ;;              '(read-file-name-internal . eh-ivy-sort-file-function))

  ;; https://oremacs.com/2017/04/09/ivy-0.9.0/
  ;; (setq ivy-switch-buffer-faces-alist
  ;;       '((emacs-lisp-mode . swiper-match-face-1)
  ;;         (dired-mode . ivy-subdir)
  ;;         (org-mode . org-level-4)))

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
   ;; http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/
   ("M-y" . ivy-next-line))
  :diminish ivy-mode)

(use-package counsel
  :ensure t
  :after company
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
   ("C-c s a" . counsel-ag)
   ("C-c s g" . counsel-git-grep) ; Shows only the first 200 results, use "C-c C-o" to save all the matches to a buffer.
   ("C-c s r" . counsel-rg)
   ("<f4>" . counsel-grep-or-swiper)
   ("C-c C-m" . counsel-mark-ring)
   ("C-c C-j" . counsel-semantic-or-imenu))
  :config
  (setq counsel-mode-override-describe-bindings t
        counsel-grep-swiper-limit 1000000 ; Number of characters in the buffer
        counsel-find-file-at-point nil
        counsel-yank-pop-separator "\n-----------------\n"
        counsel-find-file-ignore-regexp (concat
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
                                         "\\|.tar.gz"
                                         "\\|.toc$"
                                         "TAGS"
                                         "GPATH"
                                         "GRTAGS"
                                         "GTAGS"
                                         "tramp"
                                         ))
  :hook (ivy-mode . counsel-mode)
  :diminish counsel-mode)

;; (use-package ivy-rich
;;   :ensure t
;;   :disabled t ;; Try all-the-icons-ivy
;;   :after (ivy company)
;;   :init
;;   (setq ivy-rich-path-style 'relative
;;         ivy-format-function #'ivy-format-function-line)
;;   (ivy-rich-mode 1))

(use-package ivy-prescient
  :ensure t
  :disabled t
  :after prescient
  :hook (ivy-mode . ivy-prescient-mode))

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :init (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-dired-jump counsel-recentf counsel-find-library counsel-projectile-find-file counsel-projectile-find-dir)))


;; Configure automatic spell check

(use-package flyspell
  :if (and (eq system-type 'gnu/linux) (executable-find "aspell"))
  :preface
  ;; Move point to previous error, based on code by hatschipuh at http://emacs.stackexchange.com/a/14912/2017
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
  :config
  (setq-default ispell-program-name (executable-find "aspell"))
  (setq ispell-local-dictionary "en_US"
        ispell-dictionary "english"
        ispell-personal-dictionary (concat dotemacs-extras-directory "spell")
        ;; Aspell speed: ultra | fast | normal | bad-spellers
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")
        ;; Save a new word to personal dictionary without asking
        ispell-silently-savep t)
  :custom
  (flyspell-sort-corrections nil)
  (flyspell-issue-message-flag nil)

  ;; ;; This is to turn on spell check in *scratch* buffer, which is in text-mode.
  ;; (dolist (hook '(text-mode-hook find-file-hooks))
  ;;   (add-hook hook #'turn-on-flyspell))

  ;; (dolist (hook '(text-mode-hook))
  ;;   (add-hook hook #'turn-on-flyspell))

  ;; ;; Turn on flyspell-mode for comments and strings.
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  :hook ((prog-mode . flyspell-prog-mode)
         (before-save-hook . flyspell-buffer)
         ((text-mode find-file-hooks) . flyspell-mode))

  :diminish flyspell-mode
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   ("C-c f w" . ispell-word)
   :map flyspell-mode-map
   ("C-;" . nil)
   ("C-," . sb/flyspell-goto-previous-error)))

(or
 (use-package flyspell-popup
   :ensure t
   :after flyspell
   :bind ("C-;" . flyspell-popup-correct))

 ;; (use-package flyspell-correct
 ;;   :ensure t
 ;;   :ensure flyspell-correct-ivy
 ;;   ;; :after flyspell
 ;;   :bind ("C-;" . flyspell-correct-wrapper))
 )


;; Configure indentation

(setq-default fill-column dotemacs-fill-column
              standard-indent 2
              tab-width 2
              tab-always-indent 'complete
              ;; Spaces instead of tabs by default
              indent-tabs-mode nil)

(use-package aggressive-indent
  :ensure t
  :hook (emacs-lisp-mode . aggressive-indent-mode)
  :diminish aggressive-indent-mode)

(use-package electric
  :hook (prog-mode . electric-indent-mode))

(use-package highlight-indentation
  :ensure t
  :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
  :init
  (add-hook 'python-mode-hook #'highlight-indentation-mode)
  (add-hook 'python-mode-hook #'highlight-indentation-current-column-mode)
  :config
  (set-face-background 'highlight-indentation-face "WhiteSmoke")
  (set-face-background 'highlight-indentation-current-column-face "wheat"))


;; Configure highlighting of matching parentheses and braces

(use-package paren
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-style 'mixed) ; Options: 'expression, 'parenthesis, 'mixed
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  ;; (when (fboundp 'show-paren-mode)
  ;;   (make-variable-buffer-local 'show-paren-mode))
  )

;; "sp-cheat-sheet" will show you all the commands available, with examples.
;; https://ebzzry.github.io/emacs-pairs.html
(use-package smartparens
  :ensure t
  :hook (after-init . smartparens-global-mode)
  ;; (show-smartparens-global-mode 1)
  :config
  (require 'smartparens-config)
  (setq sp-show-pair-from-inside t
        sp-autoskip-closing-pair 'always)

  ;; TODO: Try these.
  ;; (sp-pair "=" "=" :actions '(wrap))
  ;; (sp-pair "+" "+" :actions '(wrap))
  ;; (sp-pair "<" ">" :actions '(wrap))
  ;; (sp-pair "$" "$" :actions '(wrap))

  ;; ;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
  ;; (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  ;;                (sp-local-tag "i" "\"<" "\">"))

  ;; (sp-with-modes '(c++-mode)
  ;;                (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
  ;; (sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
  ;; ("* ||\n[i]" "RET")))

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
  :diminish smartparens-mode)

(use-package elec-pair
  :hook (after-init . electric-pair-mode))


;; Projectile

(use-package projectile
  :ensure t
  :init
  (setq projectile-known-projects-file (concat dotemacs-temp-directory "projectile-known-projects.eld")
        projectile-cache-file (concat dotemacs-temp-directory "projectile.cache"))
  :config
  (projectile-mode 1) ; Otherwise keybindings not bound explicitly with bind* will not be respected
  (setq projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil
        projectile-verbose nil
        projectile-require-project-root t ; Use projectile only in desired directories, too much noise otherwise
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'projectile-find-file ; Use projectile-dired to view in dired
        ;; projectile-mode-line nil
        projectile-completion-system 'ivy
        ;; The contents of .projectile are ignored when using the alien project indexing method
        projectile-indexing-method 'hybrid
        projectile-enable-idle-timer t ; Runs "regenerate ctags" by default
        projectile-idle-timer-seconds 120
        projectile-project-search-path '("/home/swarnendu/github"
                                         "/home/swarnendu/bitbucket"
                                         "/home/swarnendu/plass-workspace"
                                         "/home/swarnendu/iss-workspace"
                                         "/home/swarnendu/iitk-workspace"
                                         "/home/swarnendu/prospar-workspace"))

  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/")) ; Do not consider the home dir as a project

  (dolist (dirs '(".cache"
                  ".dropbox"
                  ".git"
                  ".hg"
                  ".svn"
                  ".nx"
                  "elpa"
                  "auto"
                  "__pycache__"
                  ".vscode"))
    (add-to-list 'projectile-globally-ignored-directories dirs))

  (dolist (item '("GPATH"
                  "GRTAGS"
                  "GTAGS"
                  "GSYMS"
                  "TAGS"
                  ".dir-locals.el"
                  ".projectile"
                  ".project"
                  ".tags"
                  "__init__.py"))
    (add-to-list 'projectile-globally-ignored-files item))

  (dolist (list '(".out"
                  ".pdf"
                  ".pyc"
                  ".elc"
                  ".rel"
                  ".rip"
                  ".tar.gz"
                  ".bak"
                  ".pt"
                  "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes list))

  ;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  ;; (add-hook 'projectile-after-switch-project-hook
  ;;           (lambda ()
  ;;             (unless (bound-and-true-p treemacs-mode)
  ;;               (treemacs)
  ;;               (other-window 1))))

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; (bind-keys ("<f5>" . projectile-switch-project)
  ;;            ("<f6>" . projectile-find-file))
  ;; :bind ("<f5>" . projectile-switch-project)
  )

(use-package counsel-projectile
  :ensure t
  :after ivy
  :hook (counsel-mode . counsel-projectile-mode)
  :init
  ;; ;; Sort projects from newest to oldest
  ;; (add-to-list 'ivy-sort-functions-alist
  ;;              '(counsel-projectile-switch-project . file-newer-than-file-p))

  ;; These methods seem too slow
  ;; :bind (("<f5>" . counsel-projectile-switch-project)
  ;;        ("<f6>" . counsel-projectile))

  ;; (bind-keys ("<f7>" . counsel-projectile-rg))
  :bind (("<f5>" . counsel-projectile-switch-project)
         ("<f6>" . counsel-projectile)
         ("<f7>" . counsel-projectile-rg))
  )

;; Flycheck

(use-package flycheck
  :ensure t
  :hook (after-init . flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-display-errors-delay 0.5
        ;; Faster than the default
        flycheck-highlighting-mode 'lines
        flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch)
        flycheck-pylintrc "/home/swarnendu/.config/pylintrc")
  (setq-local flycheck-python-pylint-executable "python3")
  (setq-default flycheck-disabled-checkers '(tex-lacheck python-flake8 emacs-lisp-checkdoc))

  (add-hook 'python-mode-hook
            (lambda ()
              (setq flycheck-checker 'python-pylint)))

  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c++11")
              (setq flycheck-gcc-language-standard "c++11")))

  ;; (when (eq dotemacs-modeline-theme 'spaceline)
  ;;   (setq flycheck-mode-line nil))
  )

(use-package avy-flycheck
  :ensure t
  :ensure avy
  :after (avy flycheck)
  :config
  ;; Binds avy-flycheck-goto-error to C-c ! g
  (avy-flycheck-setup))

(or
 ;; (use-package flycheck-popup-tip ; Show error messages in popups
 ;;   :ensure t
 ;;   :disabled t
 ;;   :hook (flycheck-mode . flycheck-popup-tip-mode))

 ;; (use-package flycheck-pos-tip
 ;;   :ensure t
 ;;   :disabled t
 ;;   :hook (flycheck-mode . flycheck-pos-tip-mode))

 (use-package flycheck-inline
   :ensure t
   :hook (flycheck-mode . flycheck-inline-mode)))


;; Whitespace

;; This is different from whitespace-cleanup since this is unconditional
(when (bound-and-true-p dotemacs-delete-trailing-whitespace-p)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package whitespace
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  ;; :commands (whitespace-cleanup whitespace-mode)
  :hook (after-init . global-whitespace-mode)
  :config
  (setq-default show-trailing-whitespace nil
                whitespace-auto-cleanup t
                whitespace-style nil
                whitespace-line-column dotemacs-fill-column))

(use-package ws-butler ; Unobtrusively trim extraneous white-space *ONLY* in lines edited
  :ensure t
  :diminish ws-butler-mode
  :hook (prog-mode . ws-butler-mode))


;; Highlight

;; ;; This is useful, but does not work well with in the terminal mode. Checking for
;; ;; (display-graphics-p) and using hooks do not seem to help. Furthermore, this is a performance
;; ;; bottleneck for large files.
;; (use-package hl-line
;;   :ensure t
;;   :disabled t
;;   :hook (after-init . global-hl-line-mode)
;;   :config
;;   (setq hl-line-sticky-flag nil)
;;   (unless (eq dotemacs-theme 'solarized-dark)
;;     (set-face-attribute 'hl-line nil
;;                         :background "old lace")))

;; (use-package hl-line+ ; Highlight only when idle
;;   :ensure t
;;   :disabled t
;;   :after hl-line
;;   :config
;;   (global-hl-line-mode -1)
;;   (toggle-hl-line-when-idle 1))

;; (use-package highlight-numbers
;;   :ensure t
;;   :disabled t
;;   :hook (prog-mode . highlight-numbers-mode))

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

  ;; :init
  ;; (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  ;; ;; Navigate occurrences of the symbol under point with M-n and M-p
  ;; (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)

  :hook (prog-mode . highlight-symbol-mode)
  :bind (("M-p" . highlight-symbol-prev)
         ("M-n" . highlight-symbol-next))
  :config
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t
        highlight-symbol-highlight-single-occurrence nil)
  :diminish highlight-symbol-mode)

(use-package fic-mode ; Highlight certain words
  :ensure t
  :commands fic-mode
  :diminish fic-mode
  :hook ((text-mode prog-mode nxml-mode) . fic-mode)
  :config
  (add-to-list 'fic-highlighted-words '("XXX"
                                        "LATER"
                                        "IMP"
                                        "NOTE"
                                        "NOTES"
                                        "TODOs"
                                        ))
  ;; (add-to-list 'fic-highlighted-words ')
  ;; (add-to-list 'fic-highlighted-words ')
  ;; (add-to-list 'fic-highlighted-words ')
  ;; (add-to-list 'fic-highlighted-words '"NOTES")
  )

(use-package beacon ; Highlight cursor position in buffer after scrolling
  :ensure t
  :hook (after-init . beacon-mode)
  :diminish beacon-mode)


;; Tramp

;; Hacks are from
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
;; /method:user@host#port:filename. Shortcut /ssh:: will connect to default user@host#port. Open a
;; file with ssh + sudo: C-x C-f /ssh:host|sudo:root:/etc/passwd
(use-package tramp
  :config
  (setq tramp-default-method "ssh" ; ssh is faster than the default scp
        tramp-default-user "swarnendu"
        tramp-default-host "172.27.15.105"
        ;; Auto-save to a local directory for better performance
        tramp-auto-save-directory (concat dotemacs-temp-directory "tramp-auto-save")
        tramp-persistency-file-name (concat dotemacs-temp-directory "tramp")
        tramp-verbose 1 ; Default is 3
        remote-file-name-inhibit-cache nil ; Remote files are not updated outside of Tramp
        tramp-completion-reread-directory-timeout nil)
  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
  ;; (add-to-list 'tramp-default-method-alist '("" "biswas.38" "ssh"))
  ;; (add-to-list 'tramp-default-method-alist '("" "sbiswas" "ssh"))
  (add-to-list 'tramp-default-method-alist '("" "swarnendu" "ssh"))
  ;; (add-to-list 'tramp-default-method-alist
  ;;              '("\\`localhost\\'" "\\`root\\'" "su"))

  ;; If the shell of the server is not bash, then it is recommended to connect with bash
  (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

  ;; Disable backup
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))

  ;; Disable version control. If you access remote files which are not under version control, a lot
  ;; of check operations can be avoided by disabling VC.
  (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp
                                     tramp-file-name-regexp)))
(use-package counsel-tramp
  :ensure t
  :after tramp
  :config
  (add-hook 'counsel-tramp-pre-command-hook
            (lambda ()
              (global-aggressive-indent-mode 0)
              (projectile-mode 0)))
  (add-hook 'counsel-tramp-quit-hook
            (lambda ()
              (global-aggressive-indent-mode 1)
              (projectile-mode 1))))


;; Imenu

(use-package imenu
  :config
  (setq imenu-auto-rescan t)
  (use-package imenu-anywhere
    :ensure t)
  (use-package popup-imenu
    :ensure t)
  :bind ("C-c C-j" . imenu-anywhere))

;; (use-package imenu-list
;;   :ensure t
;;   ;; :after imenu
;;   :config
;;   (setq imenu-list-auto-resize nil
;;         imenu-list-focus-after-activation t)
;;   (if (string-equal (system-name) "sbiswas-Dell-System-XPS-L502X")
;;       (setq imenu-list-size 0.12)
;;     (setq imenu-list-size 0.10))
;;   ;; (add-hook 'python-mode-hook #'imenu-list-minor-mode)
;;   ;; (add-hook 'c-mode-common-hook #'imenu-list-minor-mode)
;;   )


;; Tags

(use-package counsel-gtags
  :ensure t
  :disabled t
  :if (eq system-type 'gnu/linux)
  :diminish counsel-gtags-mode
  :commands (counsel-gtags-find-definition
             counsel-gtags-find-reference
             counsel-gtags-find-symbol
             counsel-gtags-find-file
             counsel-gtags-create-tags
             counsel-gtags-update-tags
             counsel-gtags-dwim)
  ;; :init
  ;; (add-hook 'java-mode-hook #'counsel-gtags-mode)
  ;; (add-hook 'python-mode-hook #'counsel-gtags-mode)
  ;; (when (eq dotemacs-tags 'gtags)
  ;;   (add-hook 'c-mode-common-hook
  ;;             (lambda ()
  ;;               (when (derived-mode-p 'c-mode 'c++-mode)
  ;;                 (counsel-gtags-mode 1)))))
  :hook ((python-mode java-mode c-mode c++-mode) . counsel-gtags-mode)

  :config
  (setq counsel-gtags-ignore-case nil
        counsel-gtags-auto-update t)
  :bind (:map counsel-gtags-mode-map
              ("M-." . counsel-gtags-dwim)
              ("M-," . counsel-gtags-go-backward)
              ("C-c g s" . counsel-gtags-find-symbol)
              ("C-c g r" . counsel-gtags-find-reference)
              ("C-c g c" . counsel-gtags-create-tags)
              ("C-c g u" . counsel-gtags-update-tags)))

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)

(use-package counsel-etags
  :ensure t
  ;; :bind(("M-." . counsel-etags-find-tag-at-point)
  ;;       ;; ("M-t" . counsel-etags-grep-symbol-at-point)
  ;;       ;; ("M-s" . counsel-etags-find-tag)
  ;;       )
  :config
  (add-to-list 'counsel-etags-ignore-directories ".vscode")
  (add-to-list 'counsel-etags-ignore-filenames ".clang-format")
  (add-to-list 'counsel-etags-ignore-filenames "*.json")
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 180))


;; Miscellaneous packages

(use-package dashboard
  :ensure t
  :hook (after-init . dashboard-setup-startup-hook)
  :config
  (use-package page-break-lines
    :ensure t
    :diminish)
  :custom
  (dashboard-items '((projects . 10)
                     (recents  . 10)
                     (bookmarks . 0)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t))

;; (use-package helpful
;;   :ensure t
;;   :bind
;;   (("C-h v" . helpful-variable)
;;    ("C-h k" . helpful-key)
;;    ("C-h f" . helpful-function)))

;; M-x vlf <PATH-TO-FILE>
(use-package vlf ; Speed up Emacs for large files
  :ensure t
  :config (setq large-file-warning-threshold (* 50 1024 1024) ; Warn when opening files bigger than 50MB
                vlf-application 'dont-ask)
  (use-package vlf-setup))

(use-package hungry-delete ; Erase 'all' consecutive white space characters in a given direction
  :ensure t
  :diminish hungry-delete-mode
  :hook (after-init . global-hungry-delete-mode ))

(use-package move-text ; Move text with M-<up> and M-<down> like Eclipse
  :ensure t
  :init (move-text-default-bindings))

(use-package duplicate-thing
  :ensure t
  :bind ("C-c C-d" . duplicate-thing))

(use-package discover-my-major ; Discover key bindings and their meaning for the current Emacs major mode
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package manage-minor-mode ; Manage your minor-mode on the dedicated interface buffer
  :ensure t
  :bind ("C-c d m" . manage-minor-mode))

(use-package jgraph-mode
  :ensure t
  ;; :mode ("\\.jgr\\'" . jgraph-mode)
  :mode "\\.jgr\\'")

(use-package graphviz-dot-mode
  :ensure t
  ;; :mode ("\\.dot\\'" . graphviz-dot-mode)
  :mode "\\.dot\\'"
  :config (setq graphviz-dot-indent-width 4))

(use-package gnuplot
  :ensure t
  ;; :mode ("\\.gp\\'" . gnuplot-mode)
  :mode "\\.gp\\'"
  :interpreter ("gnuplot" . gnuplot-mode))

(use-package goto-last-change
  :ensure t
  :bind ("C-x C-\\" . goto-last-change))

;; ;; http://stackoverflow.com/questions/13242165/emacs-auto-complete-popup-menu-broken
;; (use-package popup
;;   :ensure t
;;   :config (setq popup-use-optimized-column-computation nil))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(use-package popwin
  :ensure t
  ;; popwin does not support ecb or neotree. Only direx seems to be supported. Too bad, would loved to have both popwin and ecb enabled.
  ;; https://github.com/m2ym/popwin-el/issues/9
  ;; :if (not (bound-and-true-p dotemacs-use-ecb))
  ;; :demand t
  ;; :config (popwin-mode 1)

  :hook (after-init . popwin-mode)

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
  ;; (push '("*manage-minor-mode*" :noselect t) popwin:special-display-config)
  ;; (push '("*Paradox Report*" :regexp t :noselect t) popwin:special-display-config)
  ;; (push '("*undo-tree\*" :width 0.3 :position right) popwin:special-display-config)
  ;; (push '("*Kill Ring*" :noselect nil) popwin:special-display-config)
  ;; (push '("*Selection Ring:") popwin:special-display-config)
  ;; (push '("*ag search*" :noselect nil) popwin:special-display-config)
  ;; (push '("*ggtags-global*" :stick t :noselect nil :height 30) popwin:special-display-config)
  ;; (push '("*Flycheck errors*" :noselect nil) popwin:special-display-config)
  ;; (push '("*ripgrep-search*" :noselect nil) popwin:special-display-config)
  ;; (push '("^\*magit:.+\*$" :noselect nil) popwin:special-display-config)
  ;; (push '("*xref*" :noselect nil) popwin:special-display-config)
  ;; (push '("*helpful\*" :noselect nil) popwin:special-display-config)

  ;; (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
  ;; (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
  ;; (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  ;; (add-to-list 'popwin:special-display-config '("*Apropos*"))
  ;; (add-to-list 'popwin:special-display-config '("*Warnings*"))

  )

(setq pop-up-frames nil) ; Don't allow Emacs to popup new frames

;; (use-package window-purpose
;;   :ensure t
;;   :disabled t
;;   :init (purpose-mode)
;;   :config
;;   ;; give help buffers the 'popup-frame purpose
;;   (add-to-list 'purpose-user-mode-purposes
;;                '(help-mode . popup-frame))
;;   (purpose-compile-user-configuration)
;;   ;; new rules for buffers with the 'popup-frame purpose
;;   (add-to-list 'purpose-special-action-sequences
;;                '(popup-frame
;;                  purpose-display-reuse-window-buffer
;;                  purpose-display-reuse-window-purpose
;;                  purpose-display-pop-up-frame))

;;   (require 'window-purpose-x)
;;   (purpose-x-magit-single-on))

;; (use-package ivy-purpose
;;   :ensure t
;;   :ensure ivy
;;   :ensure window-purpose
;;   :after window-purpose
;;   :disabled t
;;   :config (ivy-purpose-setup))

(use-package sudo-edit ; Edit file with sudo
  :ensure t
  :bind ("M-s e" . sudo-edit))

(use-package expand-region ; Expand region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package expand-line
  :ensure t
  :bind ("M-i" . turn-on-expand-line-mode))

;; (use-package ignoramus ; Ignore backups, build files, etc
;;   :ensure t
;;   :disabled t
;;   :if (bound-and-true-p dotemacs-use-ignoramus-p)
;;   :config
;;   (require 'dired-x)
;;   (dolist (ext '(".cb"
;;                  ".cb2"
;;                  ".dvi"
;;                  ".fls"
;;                  ".idx"
;;                  ".o"
;;                  ".out"
;;                  ".pdf"
;;                  "-pkg.el"
;;                  ".rel"
;;                  ".rip"
;;                  ".toc"))
;;     (add-to-list 'ignoramus-file-basename-endings ext))
;;   (dolist (filenames '("GPATH"
;;                        "GRTAGS"
;;                        "GSYMS"
;;                        "GTAGS"
;;                        "TAGS"
;;                        "__init__.py"))
;;     (add-to-list 'ignoramus-file-basename-exact-names filenames))
;;   (add-to-list 'ignoramus-file-basename-regexps "\\`\\.")
;;   (dolist (dir '("\\`\\."
;;                  "__pycache__"
;;                  "auto"))
;;     (add-to-list 'ignoramus-file-basename-exact-names dir))
;;   (ignoramus-setup))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :ensure t
  :bind ("C-." . iedit-mode))

(use-package persistent-scratch
  :ensure t
  :hook (after-init . persistent-scratch-setup-default)
  :config
  (setq persistent-scratch-save-file (concat dotemacs-temp-directory "persistent-scratch"))
  ;; ;; Enable both autosave and restore on startup
  ;; (ignore-errors (persistent-scratch-setup-default))
  )

(use-package crux
  :ensure t
  :bind ("C-c i" . crux-ispell-word-then-abbrev))

(use-package apt-sources-list
  :ensure t
  :mode ("\\.list\\'" . apt-sources-list-mode))

(use-package rainbow-delimiters
  :ensure t
  ;; :init
  ;; (dolist (hook '(text-mode-hook prog-mode-hook))
  ;;   (add-hook hook #'rainbow-delimiters-mode))
  :hook ((text-mode prog-mode) . rainbow-delimiters-mode))

(use-package ssh-config-mode
  :ensure t
  :mode ("/\\.ssh/config\\'" . ssh-config-mode)
  :mode ("/sshd?_config\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)
  :config (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(use-package super-save
  :ensure t
  :diminish super-save-mode
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode 1))

(use-package prescient
  :ensure t
  :disabled t
  :custom (prescient-save-file (concat dotemacs-temp-directory "prescient-save.el"))
  :hook (after-init . prescient-persist-mode))

(use-package ace-window
  :ensure t
  :bind (("C-c w" . ace-window)
         ([remap other-window] . ace-window))
  :hook (after-init . ace-window-display-mode)
  ;; :config (setq aw-background nil)
  )

(use-package avy
  :ensure t
  :bind (("M-b" . avy-goto-word-1)
         ("C-'" . avy-goto-char)
         ("C-/" . avy-goto-line))
  :config
  ;; It will bind, for example, avy-isearch to C-' in isearch-mode-map, so that you can select one
  ;; of the currently visible isearch candidates using avy.
  (avy-setup-default)
  (setq avy-background t
        avy-highlight-first t
        avy-all-windows nil
        ;; Option pre is a bit distracting because of all the movement while highlighting selection
        ;; keys. This causes the eyes to lose focus.
        avy-style 'at))

(use-package bookmark
  :config (setq bookmark-default-file (concat dotemacs-temp-directory "bookmarks")))

(use-package bm
  :ensure t
  :bind (("C-<f1>" . bm-toggle)
         ("C-<f2>" . bm-next)
         ("C-<f3>" . bm-previous)))

(use-package amx
  :ensure t
  :hook (after-init . amx-mode)
  :custom (amx-save-file (concat dotemacs-temp-directory "amx-items")))


;; Configure individual major modes

;; Text mode

;; text-mode is a basic mode for LaTeX-mode and org-mode, and so any hooks defined here will also
;; get run for all modes derived from a basic mode such as text-mode.

(use-package writegood-mode ; Identify weasel words, passive voice, and duplicate words
  :ensure t
  :diminish writegood-mode
  :hook (text-mode . writegood-mode))

(defun sb/company-text-backends ()
  "Add backends for text completion in company mode."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((;; Generic backends
           company-files
           company-keywords
           company-capf
           company-dict
           company-dabbrev))))
;; (add-hook 'text-mode-hook #'sb/company-text-backends)

(use-package markdown-mode
  :ensure t
  ;; :commands (markdown-mode gfm-mode)
  :diminish gfm-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  ;; :bind ("C-c C-d" . nil)
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-enable-math t
        markdown-make-gfm-checkboxes-buttons t
        markdown-list-indent-width 2
        markdown-command "pandoc -f markdown -s "))

(use-package markdown-mode+
  :ensure t
  ;; :after markdown-mode
  )

;; (use-package pandoc
;;   :ensure t
;;   ;; :after markdown-mode
;;   )

(use-package pandoc-mode
  :ensure t
  ;; :after markdown-mode
  :diminish pandoc-mode
  :config (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  :hook (markdown-mode . pandoc-mode))

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'json-mode-beautify t t))))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")


;; LaTeX mode

;; AUCTeX's LaTeX mode is called LaTeX-mode, while latex-mode is the Emacs default.

;; (put 'TeX-narrow-to-group 'disabled nil)
;; (put 'LaTeX-narrow-to-environment 'disabled nil)

(use-package tex-site ; Initialize auctex
  :ensure auctex ; once installed, auctex overrides the tex package
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq TeX-auto-save t ; Enable parse on save, stores parsed information in an "auto" directory
        TeX-parse-self t ; Parse documents
        TeX-clean-confirm nil
        TeX-quote-after-quote nil ; Allow original LaTeX quotes
        TeX-electric-sub-and-superscript t ; Automatically insert braces in math mode
        TeX-auto-untabify t ; Remove all tabs before saving
        TeX-save-query nil
        LaTeX-item-indent 0
        LaTeX-syntactic-comments t)

  (setq-default TeX-master nil) ; Query for master file

  ;; Provide forward "C-c C-v" (TeX-view) and inverse (C-Mouse-1, Ctrl + "Left Click") search with SyncTeX
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server 'ask)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (TeX-source-correlate-mode)
              (LaTeX-math-mode)
              ;; prettify-symbol-mode is distracting while editing, and is buffer-local.
              (global-prettify-symbols-mode -1)
              (prettify-symbols-mode -1)))

  ;; (add-to-list 'TeX-command-list
  ;;              '("PDFLaTeX" "%'pdflatex%(mode)%' %t" TeX-run-TeX nil t
  ;;                (plain-tex-mode tex-mode TeX-mode LaTeX-mode TeX-latex-mode docTeX-mode)
  ;;                :help "Run PDFLaTeX"))
  ;; (add-to-list 'TeX-command-list
  ;;              '("View" "%V" TeX-run-discard nil t))

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-TeX-LaTeX-command-finished-hook
            #'TeX-revert-document-buffer)

  ;; to use pdfview with auctex
  (add-hook 'LaTeX-mode-hook 'pdf-tools-install)

  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))

  ;; (unbind-key "C-c C-d" LaTeX-mode-map)
  ;; ;; Unset "C-c ;" since we want to bind it to 'comment-line
  ;; (unbind-key "C-c ;" LaTeX-mode-map)

  ;; :bind
  ;; ;; Disable "LaTeX-insert-item" in favor of imenu
  ;; ("C-c C-j" . nil)

  )

(setq font-latex-fontify-script nil)

(use-package auctex-latexmk
  :ensure t
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-command-default "LaTeXMk"))))

(use-package company-auctex
  :ensure t
  ;; :after (company auctex)
  :config (company-auctex-init))

(use-package company-math
  :ensure t
  :ensure math-symbol-lists ; Required by ac-math and company-math
  :preface
  (defun company-math-setup ()
    (setq-local company-backends
                (append '((company-math-symbols-latex company-latex-commands company-math-symbols-unicode))
                        company-backends)))
  :config (add-hook 'TeX-mode-hook #'company-math-setup))

;; (use-package tex-smart-umlauts
;;   :ensure t
;;   :hook (LaTeX-mode-hook . tex-smart-umlauts-mode))

(use-package tex-mode
  :diminish latex-electric-env-pair-mode
  :init
  (setq latex-run-command "latexmk")
  (add-hook 'TeX-mode-hook
            (lambda()
              (latex-electric-env-pair-mode 1))))

(use-package reftex
  :diminish reftex-mode
  ;; :commands (reftex-citation)
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-insert-label-flags '(t t)
        reftex-cite-format 'abbrv
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-auto-update-selection-buffers t
        reftex-enable-partial-scans t
        reftex-allow-automatic-rescan t
        reftex-idle-time 0.5
        reftex-toc-follow-mode t
        reftex-use-fonts t
        reftex-cite-prompt-optional-args t ; Prompt for empty optional arguments in cite
        reftex-highlight-selection 'both)
  (use-package reftex-cite
    :preface
    ;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
    (defun get-bibtex-keys (file)
      (with-current-buffer (find-file-noselect file)
        (mapcar 'car (bibtex-parse-keys))))

    (defun find-bibliography-file ()
      "Try to find a bibliography file using RefTeX."
      ;; Returns a string with text properties (as expected by read-file-name) or empty string if no file can be found
      (interactive)
      (let ((bibfile-list nil))
        (condition-case nil
            (setq bibfile-list (reftex-get-bibfile-list))
          (error (ignore-errors
                   (setq bibfile-list (reftex-default-bibliography)))))
        (if bibfile-list
            (car bibfile-list) "")))

    (defun reftex-add-all-bibitems-from-bibtex ()
      (interactive)
      (mapc 'LaTeX-add-bibitems
            (apply 'append
                   (mapcar 'get-bibtex-keys (reftex-get-bibfile-list)))))
    :config (add-hook 'reftex-load-hook #'reftex-add-all-bibitems-from-bibtex)))

;; (use-package parsebib
;;   :ensure t)

;; (use-package bibtex
;;   :init (add-hook 'bibtex-mode-hook #'turn-on-auto-revert-mode)
;;   :config
;;   (setq bibtex-maintain-sorted-entries t)
;;   (use-package bibtex-utils
;;     :ensure t))

;; (use-package bib-cite
;;   :disabled t
;;   :diminish bib-cite-minor-mode
;;   :init  (add-hook 'LaTeX-mode-hook #'bib-cite-minor-mode)
;;   :config (setq bib-cite-use-reftex-view-crossref t)
;;   :bind
;;   (:map bib-cite-minor-mode-map
;;         ("C-c b" . nil) ; We use "C-c b" for comment-box
;;         ("C-c l a" . bib-apropos)
;;         ("C-c l b" . bib-make-bibliography)
;;         ("C-c l d" . bib-display)
;;         ("C-c l t" . bib-etags)
;;         ("C-c l f" . bib-find)
;;         ("C-c l n" . bib-find-next)
;;         ("C-c l h" . bib-highlight-mouse)))

(use-package ivy-bibtex
  :ensure t
  :bind ("C-c l x" . ivy-bibtex)
  :config
  (use-package bibtex-completion
    :config
    (setq bibtex-completion-cite-prompt-for-optional-arguments nil
          bibtex-completion-cite-default-as-initial-input t
          bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:10}"))))
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package company-bibtex
  :ensure t
  :config (add-to-list 'company-backends 'company-bibtex))

;; (use-package pdf-tools
;;   :ensure t
;;   :commands pdf-sync-forward-search
;;   :mode ("\\.pdf\\'" . pdf-tools-install)
;;   ;; :bind ("C-c C-g" . pdf-sync-forward-search)
;;   :config
;;   (setq mouse-wheel-follow-mouse t)
;;   (setq pdf-view-resize-factor 1.10))

;; ;; https://rtime.felk.cvut.cz/~sojka/blog/compile-on-save/
;; ;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (string= major-mode "latex-mode")
;;               (TeX-command-menu "LaTeXMk")
;;               (revert-buffer :ignore-auto :noconfirm)
;;               (find-alternate-file (current-buffer)))))

;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (string= major-mode "latex-mode")
;;               (let ((process (TeX-active-process))) (if process (delete-process process)))
;;               (revert-buffer :ignore-auto :noconfirm)
;;               (find-alternate-file (current-buffer))
;;               (TeX-command-menu "LaTeXMk"))))

;; (defun run-latexmk ()
;;   (when (string= major-mode "latex-mode")
;;     (let ((TeX-save-query nil)
;;           (TeX-process-asynchronous t)
;;           (master-file (TeX-master-file)))
;;       (TeX-save-document "")
;;       (TeX-run-TeX "LaTexmk"
;;                    (TeX-command-expand "latexmk -pdf %t" 'TeX-master-file)
;;                    master-file)
;;       (if (plist-get TeX-error-report-switches (intern master-file))
;;           (TeX-next-error t)
;;         (minibuffer-message "LaTeXMk done")))))
;; (add-hook 'after-save-hook #'run-latexmk)

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

;; ;; https://github.com/expez/.emacs.d/blob/master/lisp/init-latex.el
;; (defadvice LaTeX-insert-item (after remove-whitespace-first-item activate)
;;   "This advice is meant to fix the issue where an extra blank
;; line is naively added by `LaTeX-insert-item' when not already on
;; an item line."
;;   (check-item-entry))

;; (defun sb/company-LaTeX-backends ()
;;   "Add backends for LaTeX completion in company mode."
;;   (make-local-variable 'company-backends)
;;   (setq company-backends
;;         '((;; Generic backends
;;            company-dabbrev
;;            company-files
;;            company-keywords
;;            company-capf
;;            company-dict
;;            ;; company-gtags
;;            ;; LaTeX specific backends
;;            company-auctex-labels
;;            company-auctex-bibs
;;            company-auctex-macros
;;            company-auctex-symbols
;;            company-auctex-environments
;;            company-bibtex
;;            company-math-symbols-latex
;;            company-latex-commands
;;            company-math-symbols-unicode))))
;; (add-hook 'LaTeX-mode-hook #'sb/company-LaTeX-backends)
;; (add-hook 'latex-mode-hook #'sb/company-LaTeX-backends)

(require 'smartparens-latex)


;; PROG mode

;; (use-package semantic
;;   :disabled t
;;   :init
;;   (setq semanticdb-default-save-directory (concat dotemacs-temp-directory "semanticdb"))
;;   (add-hook 'prog-mode-hook #'semantic-mode)
;;   :config
;;   ;; (semantic-mode 1)
;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-summary-mode 1)
;;   ;;https://emacs.stackexchange.com/questions/32268/can-semantic-and-company-coexist
;;   ;; (global-semantic-idle-completions-mode 1)
;;   (global-semantic-highlight-func-mode 1))

(global-prettify-symbols-mode 1)

;; (use-package prog-mode
;;   :config
;;   (when (>= emacs-major-version 25)
;;     (setq prettify-symbols-unprettify-at-point 'right-edge)
;;     (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)))

(use-package make-mode
  :mode (("\\Makefile\\'" . makefile-mode)
         ("makefile\\.rules\\'" . makefile-gmake-mode)))

(use-package web-mode ; http://web-mode.org/
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.hb\\.html\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-indent-style 4
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t
        web-mode-enable-css-colorization t
        web-mode-enable-block-face t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t))

(use-package ac-html-angular ; Required by ac-html and company-web
  :ensure t
  :config (ac-html-angular+))

(use-package company-web
  :ensure t
  :preface
  (defun sb/company-web--setup ()
    (setq-local company-backends
                (append '(company-web-html)
                        company-backends)))
  :config (add-hook 'web-mode-hook #'sb/company-web--setup))

(use-package web-beautify
  :ensure t
  :init
  (with-eval-after-load "js2-mode"
    (add-hook 'js2-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-js-buffer t t))))
  (with-eval-after-load "js"
    (add-hook 'js-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-js-buffer t t))))
  (with-eval-after-load "json-mode"
    (add-hook 'json-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-js-buffer t t))))
  (with-eval-after-load "web-mode"
    (add-hook 'web-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-html-buffer t t))))
  (with-eval-after-load "sgml-mode"
    (add-hook 'html-mode-hook
              (lambda ()
                (add-hook 'before-save-hook #'web-beautify-html-buffer t t)))
    (with-eval-after-load "css-mode"
      (add-hook 'css-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook #'web-beautify-css-buffer t t))))))

(use-package nxml-mode
  :init (defalias 'xml-mode 'nxml-mode)
  :config (setq nxml-slash-auto-complete-flag t
                nxml-auto-insert-xml-declaration-flag t)
  (add-hook 'nxml-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-nxml))))

;; (use-package which-func ; Show the name of the function in the modeline
;;   :after prog-mode
;;   :disabled t
;;   :hook (c-mode-common . which-function-mode)
;;   ;; :init (setq which-func-modes '(java-mode c++-mode python-mode emacs-lisp-mode lisp-mode))
;;   :config
;;   ;; (when (eq dotemacs-modeline-theme 'sml)
;;   ;;   (set-face-attribute 'which-func nil
;;   ;;                       :foreground "black"
;;   ;;                       :weight 'light))
;;   ;; (when (or (eq dotemacs-modeline-theme 'powerline) (eq dotemacs-modeline-theme 'spaceline))
;;   ;;   (set-face-attribute 'which-func nil
;;   ;;                       ;; :foreground "white"
;;   ;;                       :weight 'light))
;;   )

(use-package electric
  :hook (prog-mode . electric-layout-mode))

(use-package eldoc
  :after prog-mode
  :if (eq system-type 'gnu/linux)
  ;; :disabled t
  :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode python-mode) . eldoc-mode)
  :diminish eldoc-mode)

;; (use-package eldoc-overlay
;;   :ensure t
;;   :after eldoc
;;   :disabled t ; Too intrusive
;;   :diminish eldoc-overlay-mode
;;   :config (eldoc-overlay-mode 1))

(use-package octave
  ;; :mode ("\\.m\\'" . octave-mode)
  :mode "\\.m\\'"
  )

;; (use-package ess
;;   :ensure t
;;   :config
;;   (setq inferior-R-args "--quiet --no-restore-history --no-save"
;;         ess-indent-offset 4
;;         ess-indent-from-lhs 4)
;;   (use-package ess-smart-underscore
;;     :ensure t))

(use-package ini-mode
  :ensure t
  ;; :mode ("\\.ini\\'" . ini-mode)
  :mode "\\.ini\\'")

(add-hook 'after-save-hook
          (lambda ()
            (when (or (string-equal major-mode "lisp-mode") (string-equal major-mode "emacs-lisp-mode"))
              (check-parens))))


;; C/C++ mode

;; Available C style: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Built_002din-Styles
;; gnu: The default style for GNU projects
;; k&r: What Kernighan and Ritchie, the authors of C used in their book
;; bsd: What BSD developers use, aka "Allman style" after Eric Allman.
;; whitesmith: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; stroustrup: What Stroustrup, the author of C++ used in his book
;; ellemtel: Popular C++ coding standards as defined by "Programming in C++, Rules and Recommendations," Erik Nyquist
;;  and Mats Henricson, Ellemtel
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
  :config
  (setq c-set-style "cc-mode"
        c-basic-offset 2
        c-auto-newline 1)
  (c-toggle-electric-state 1)
  (c-toggle-syntactic-indentation 1)
  (unbind-key "C-M-a" c-mode-map)
  :bind (:map c-mode-base-map
              ("C-c c a" . c-beginning-of-defun)
              ("C-c c e" . c-end-of-defun)
              ("M-q" . c-fill-paragraph)))

(use-package c-eldoc
  :ensure t
  :after (eldoc cc-mode)
  :if (eq system-type 'gnu/linux)
  :init
  (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook #'c-turn-on-eldoc-mode))

;; (use-package function-args
;;   :ensure t
;;   :disabled t
;;   :after cc-mode
;;   :diminish function-args-mode
;;   :config (fa-config-default)
;;   :bind (:map function-args-mode-map
;;               ("C-M-k" . nil)
;;               ("C-M-j" . nil)
;;               :map c++-mode-map
;;               ("M-u" . nil)   ;; This overrides M-u
;;               ("C-c c s" . fa-show)
;;               ("C-c c b" . fa-jump)
;;               ("C-c c c" . moo-complete)
;;               ("C-c c l" . moo-jump-local)
;;               ("C-c c d" . moo-jump-directory)))

(use-package company-c-headers
  :ensure t
  :after (company cc-mode)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (dolist (paths '(
                   "/usr/include"
                   "/usr/include/clang/6"
                   "/usr/include/boost"
                   "/usr/include/linux"
                   "/usr/include/c++/7"
                   "/usr/include/c++/7/tr1"
                   "/usr/local/include"))
    (add-to-list 'company-c-headers-path-system paths)))

;; ;; Install irony-server on consensus: cmake -DLIBCLANG_INCLUDE_DIR=/workspace/sbiswas/software/llvm/clang+llvm-3.9.1-x86_64-linux-gnu-debian8/include -DLIBCLANG_LIBRARY=/usr/lib64/llvm/libclang.so -DCMAKE_INSTALL_PREFIX=/h2/sbiswas/.emacs.d/irony/ /h2/sbiswas/.emacs.d/elpa/irony-20170523.618/server && cmake --build . --use-stderr --config Release --target install
;; (use-package irony
;;   :ensure t
;;   :diminish irony-mode
;;   :disabled t
;;   :init
;;   (add-hook 'c++-mode-hook #'irony-mode)
;;   (add-hook 'c-mode-hook #'irony-mode)
;;   :config
;;   (setq irony-server-install-prefix (concat dotemacs-temp-directory "irony"))
;;   (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
;;   ;; Use compilation database first, clang_complete as fallback.
;;   (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
;;                                                   irony-cdb-clang-complete))
;;   ;; Replace the `completion-at-point' and `complete-symbol' bindings in irony-mode's buffers by irony-mode's function
;;   (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async)

;;   (use-package company-irony
;;     :ensure t
;;     :after company
;;     :init
;;     (use-package company-irony-c-headers
;;       :ensure t
;;       :after irony)
;;     :config
;;     ;; http://emacs.stackexchange.com/questions/801/how-to-get-intelligent-auto-completion-in-c
;;     ;; http://tuhdo.github.io/c-ide.html
;;     (setq company-backends (delete 'company-semantic company-backends))
;;     (add-to-list 'company-backends 'company-irony))

;;   (use-package flycheck-irony
;;     :ensure t
;;     :ensure irony
;;     :ensure flycheck
;;     :disabled t
;;     :after flycheck
;;     :commands flycheck-irony-setup
;;     :init (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;   (use-package irony-eldoc
;;     :ensure t
;;     :disabled t
;;     :commands irony-eldoc
;;     :init (add-hook 'irony-mode-hook #'irony-eldoc)))

(use-package clang-format
  :ensure t
  :after cc-mode
  :init
  (setq-default clang-format-style "{BasedOnStyle: LLVM, IndentWidth: 2, ColumnLimit: 100}")
  (setq clang-format-executable "/usr/bin/clang-format"))

;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (when (string-equal major-mode "c++-mode")
;;               (clang-format-buffer))))

(use-package cuda-mode
  :ensure t
  :mode ("\\.cu\\'"	. c++-mode))

(use-package opencl-mode
  :ensure t
  :mode ("\\.cl\\'" . opencl-mode))

;; (defun sb/company-cc-backends ()
;;   "Add backends for C/C++ completion in company mode."
;;   (make-local-variable 'company-backends)
;;   (setq company-backends
;;         '((
;;            ;; C++ specific backends
;;            company-clang
;;            company-irony
;;            company-c-headers
;;            company-irony-c-headers
;;            company-semantic
;;            company-gtags ; FIXME: Should we add this after gtags is loaded?

;;            ;; Generic backends
;;            company-files
;;            company-keywords
;;            ;; company-dabbrev
;;            company-dabbrev-code
;;            company-capf
;;            company-semantic
;;            ))))
;; (add-hook 'c++-mode-hook #'sb/company-cc-backends)

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists.txt" "\\.cmake\\'")
  ;; :mode (("CMakeLists\\.txt\\'" . cmake-mode)
  ;;        ("\\.cmake\\'" . cmake-mode))
  :config
  (use-package cmake-font-lock
    :ensure t
    :hook (cmake-mode . cmake-font-lock-activate)))

;; (use-package cmake-ide
;;   :ensure t
;;   :config
;;   (setq cmake-ide-flags-c++ (append '("-std=c++11")))
;;   (cmake-ide-setup))

(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))


;; Python mode

;; (defun sb/python-setup ()
;;   "Helper function for configuring python mode."
;;   (setq-default python-indent-offset 4
;;                 python-indent-guess-indent-offset t)
;;   (setq python-shell-completion-native-enable nil)
;;   (setq python-shell-interpreter "python3"
;;         python-shell-unbuffered nil))

;; (use-package elpy
;;   :ensure t
;;   :ensure find-file-in-project
;;   :diminish elpy-mode
;;   :disabled t
;;   ;; :defer t
;;   :preface
;;   (defun sb/elpy-setup ()
;;     "Setup elpy and python configurations."
;;     (sb/python-setup)
;;     (setq elpy-modules '(elpy-module-company
;;                          elpy-module-eldoc
;;                          elpy-module-pyvenv
;;                          elpy-module-highlight-indentation
;;                          elpy-module-yasnippet
;;                          elpy-module-sane-defaults)
;;           elpy-rpc-python-command "python3"
;;           elpy-rpc-backend "jedi"
;;           elpy-syntax-check-command "pylint"
;;           python-check-command "pylint"
;;           elpy-company-add-completion-from-shell t)

;;     (elpy-mode 1))

;;   :init
;;   ;;  (add-hook 'python-mode-hook #'sb/elpy-setup)
;;   (advice-add 'python-mode :before 'elpy-enable)

;;   :config
;;   (add-hook 'elpy-mode-hook #'flycheck-mode)
;;   ;; ;; http://www.wilfred.me.uk/.emacs.d/init.html
;;   ;; (add-hook 'python-mode-hook
;;   ;;           (lambda ()
;;   ;;             (add-to-list 'flycheck-disabled-checkers 'python-pylint)))

;;   :bind (:map elpy-mode-map
;;               ("C-c c e" . python-nav-forward-defun)
;;               ("C-c c a" . python-nav-backward-defun)
;;               ("M-<left>" . nil)
;;               ("M-<right>" . nil)
;;               ("M-." . nil)
;;               ("C-c C-d" . nil)
;;               ("C-c C-r i" . nil)))

;; (use-package python-docstring
;;   :ensure t
;;   :diminish python-docstring-mode
;;   :hook (python-mode . python-docstring-mode))

;; (use-package pyvenv
;;   :ensure t
;;   :config (pyvenv-mode 1))

;; (use-package pyimport
;;   :ensure t)

;; (use-package py-isort
;;   :ensure t)

;; (use-package company-jedi
;;   :ensure t
;;   :ensure company
;;   :after company
;;   :config (add-to-list 'company-backends company-jedi))

;; (defun sb/company-python-backends ()
;;   "Add backends for Python completion in company mode."
;;   (make-local-variable 'company-backends)
;;   (setq company-backends
;;         '((;; Generic backends
;;            company-files
;;            company-keywords
;;            company-capf
;;            company-dabbrev
;;            company-dabbrev-code
;;            company-gtags
;;            ;; Python specific backends
;;            company-jedi
;;            elpy-company-backend))))
;; (add-hook 'python-mode-hook #'sb/company-python-backends)

;; (defhydra sb/hydra-python-indent (global-map "C-c c n")
;;   "indent"
;;   ("l" elpy-nav-indent-shift-left "left")
;;   ("r" elpy-nav-indent-shift-right "right"))

;; (add-hook 'before-save-hook
;;           (lambda ()
;;             (when (string-equal major-mode "python-mode")
;;               (py-isort-before-save)
;;               ;; (pyimport-remove-unused) ; This can be irritating if you are yet to use the imports.
;;               (elpy-yapf-fix-code))))


;; Java mode

(add-hook 'java-mode-hook
          (lambda ()
            (setq-default c-basic-offset 2
                          c-set-style "java")))

;; (use-package ant
;;   :ensure t)

;; (use-package autodisass-java-bytecode ; Can disassemble .class files from within jars as well
;;   :ensure t)

;; (use-package jdee
;;   :ensure t
;;   :config
;;   (setq jdee-server-dir dotemacs-extras-directory
;;         jdee-complete-function 'jdee-complete-minibuf)
;;   (setq jdee-global-classpath '("/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/rt.jar"
;;                                 "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/jce.jar"
;;                                 "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/jsse.jar"
;;                                 "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/charsets.jar"
;;                                 "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/resources.jar"
;;                                 "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/management.jar")))

;; (use-package eclim
;;   :ensure t
;;   :init
;;   (use-package eclimd
;;     :config (setq eclimd-autostart t
;;                   eclimd-executable "/home/swarnendu/software/eclipse/eclimd"))
;;   (setq eclim-eclipse-dirs "/home/swarnendu/software/eclipse/eclipse"
;;         eclim-executable "/home/swarnendu/software/eclipse/plugins/org.eclim_2.8.0/bin/eclim")
;;   (setq eclim-auto-save t)
;;   (add-hook 'java-mode-hook #'eclim-mode)
;;   :config
;;   (use-package company-emacs-eclim
;;     :ensure t
;;     :config (company-emacs-eclim-setup)))


;; Shell script mode

(use-package sh-script ; Shell script mode
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\bashrc\\'" . sh-mode))
  :config
  (setq sh-basic-offset 4
        sh-indent-comment t
        sh-indent-after-continuation 'always)
  (unbind-key "C-c C-d" sh-mode-map) ; Was bound to sh-cd-here

  (use-package company-shell
    :ensure t
    :after company
    :config
    (setq company-shell-delete-duplicates t)
    (add-to-list 'company-backends 'company-shell)
    (add-to-list 'company-backends 'company-fish-shell)))

(use-package fish-mode
  :ensure t
  :mode "\\.fish\\'")

(use-package shfmt
  :ensure nil
  :load-path "extras/shfmt"
  :ensure-system-package shfmt
  :custom (shfmt-arguments "-i 4 -p -ci")
  :hook (sh-mode . shfmt-enable-on-save))

(use-package flycheck-shfmt
  :ensure nil
  :after flycheck
  :load-path "extras/shfmt"
  :config (flycheck-shfmt-setup))

(defun sb/company-sh-backends ()
  "Add backends for C/C++ completion in company mode."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((;; Generic backends
           company-files
           company-keywords
           company-capf
           company-dabbrev
           company-dabbrev-code
           ;; Mode-specific
           company-shell
           company-fish-shell))))
(add-hook 'sh-mode-hook 'sb/company-sh-backends)


;; Shell mode

;; (use-package shell
;;   :disabled t
;;   :config
;;   (use-package comint
;;     :config
;;     (setq comint-scroll-to-bottom-on-input t ; Always insert at the bottom
;;           ;; No duplicates in command history
;;           comint-input-ignoredups t))

;;   (use-package shell-command
;;     :ensure t
;;     :config (shell-command-completion-mode 1))

;;   ;; Set up shell (not eshell) mode:
;;   ;; https://github.com/monsanto/readline-complete.el/blob/master/readline-complete.el
;;   ;; https://stackoverflow.com/questions/37409085/how-to-define-a-default-shell-for-emacs
;;   (setq-default explicit-shell-file-name "/bin/bash"
;;                 shell-file-name explicit-shell-file-name
;;                 explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
;;                 comint-process-echoes t)
;;   (setenv "ESHELL" shell-file-name)

;;   (use-package readline-complete
;;     :ensure t
;;     :config
;;     (push 'company-readline company-backends)
;;     (add-hook 'rlc-no-readline-hook
;;               (lambda ()
;;                 (company-mode -1))))

;;   (use-package bash-completion
;;     :ensure t
;;     :config (bash-completion-setup))

;;   ;; http://www.joshstaiger.org/archives/2005/07/fixing_garbage.html
;;   (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;;   (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on))

;; ;; Avoid Emacs querying "active processes exist; kill them and exit anyway?", since we are creating
;; ;; an inferior python process and aspell
;; (add-hook 'comint-exec-hook
;;           (lambda ()
;;             (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(use-package fish-completion
  :ensure t
  :if (when (executable-find "fish"))
  :config (global-fish-completion-mode))


;; Setup VCS

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (setq transient-levels-file (concat dotemacs-temp-directory "transient/levels.el")
        transient-values-file (concat dotemacs-temp-directory "transient/values.el")
        transient-history-file (concat dotemacs-temp-directory "transient/history.el")
        magit-save-repository-buffers t
        magit-completing-read-function 'ivy-completing-read)
  :config
  (setq magit-post-display-buffer-hook
        #'(lambda ()
            (when (derived-mode-p 'magit-status-mode)
              (delete-other-windows)))))

(use-package magit-popup
  :after magit)

(use-package git-modes
  :after magit)

(use-package git-gutter
  :ensure t
  :after magit
  :diminish
  :hook (after-init . global-git-gutter-mode))

;; (use-package magit-svn
;;   :defer t)

;; (use-package psvn
;;   :load-path "extras"
;;   :bind ("C-c d s" . svn-status)
;;   :config
;;   (setq svn-status-verbose nil
;;         svn-status-hide-unknown nil
;;         svn-status-hide-unmodified t
;;         svn-status-display-full-path t
;;         svn-status-auto-revert-buffers t))


;; LSP implementation for GNU Emacs

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((java-mode c-mode c++-mode python-mode) . lsp)
  :config
  (require 'lsp-clients)
  (setq lsp-auto-guess-root t
        lsp-enable-snippet t
        lsp-document-sync-method 'incremental
        lsp-prefer-flymake nil
        lsp-enable-completion-at-point t
        lsp-enable-xref t
        lsp-session-file (concat dotemacs-temp-directory ".lsp-session-v1")
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-pyls-configuration-sources ["pylint"]
        lsp-pyls-plugins-pydocstyle-enabled t
        lsp-pyls-plugins-pydocstyle-ignore ["D101","D103","D213"]
        lsp-pyls-plugins-pydocstyle-convention "pep257"
        lsp-pyls-plugins-pycodestyle-enabled nil
        lsp-pyls-plugins-pycodestyle-max-line-length 100
        lsp-pyls-plugins-pyflakes-enabled nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-position 'right)
  (lsp-ui-flycheck-live-reporting t)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-peek-peek-height 25))

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

(use-package company-lsp
  :ensure t
  ;; :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet t
        company-lsp-async t
        company-lsp-enable-recompletion t
        company-lsp-cache-candidates 'auto))

(use-package lsp-java
  :ensure t
  :after lsp
  :init
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-inhibit-message t))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java
  :after lsp-java)

(use-package lsp-java-treemacs
  :after treemacs)

(use-package lsp-clangd
  :ensure t
  :after lsp
  :config
  (add-hook 'c-mode-hook #'lsp-clangd-c-enable)
  (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
  (setq lsp-clangd-executable "/usr/bin/clangd"))

(use-package lsp-treemacs
  :ensure t
  :after (lsp treemacs)
  :commands lsp-treemacs-errors-list)


;; ORG mode

;; (use-package org
;;   :ensure t
;;   :disabled t
;;   :config
;;   ;; (add-hook 'org-mode-hook #'turn-on-auto-fill)
;;   (setq org-src-fontify-natively t ; Code block fontification using the major-mode of the code
;;         org-startup-indented t
;;         org-startup-truncated nil
;;         org-src-preserve-indentation t
;;         org-src-tabs-acts-natively t
;;         org-src-window-setup 'current-window
;;         org-fontify-done-headline t
;;         org-fontify-whole-heading-line t
;;         org-startup-folded 'showeverything ; options: nil
;;         org-hide-leading-stars t
;;         org-hide-leading-stars-before-indent-mode t
;;         org-support-shift-select t ; use shift-select
;;         ;; See org-speed-commands-default for a list of the keys and commands enabled at the
;;         ;; beginning of headlines. See org-babel-describe-bindings will display a list of the code
;;         ;; blocks commands and their related keys.
;;         org-use-speed-commands t
;;         org-src-strip-leading-and-trailing-blank-lines t
;;         ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
;;         org-pretty-entities t
;;         ;; Render subscripts and superscripts in org buffers
;;         org-pretty-entities-include-sub-superscripts t)

;;   ;; Allow syntax highlighting for parts of a word
;;   ;; http://stackoverflow.com/questions/1218238/how-to-make-part-of-a-word-bold-in-org-mode
;;   (setcar org-emphasis-regexp-components " \t('\"`{[:alpha:]=")
;;   (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}=\\")
;;   (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;;   ;; Does not work for me
;;   ;; disable including title if not explicitly specified, default was to use the buffer name
;;   ;; (defadvice org-export-grab-title-from-buffer (around org-export-grab-title-from-buffer-disable activate))

;;   (require 'org-inlinetask)

;;   (add-hook 'org-mode-hook #'org-toggle-blocks)
;;   (add-hook 'org-mode-hook #'which-function-mode)

;;   (use-package ox-latex
;;     :config
;;     ;; include the listings package
;;     (add-to-list 'org-latex-packages-alist '("" "listings"))
;;     ;; if you want colored source code then you need to include the color package
;;     ;; (add-to-list 'org-latex-packages-alist '("" "color"))
;;     ;; Add minted to the defaults packages to include when exporting.
;;     ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
;;     ;; tell org to use listings, options: t, 'minted
;;     (setq org-latex-listings 't
;;           org-latex-table-caption-above nil))

;;   (use-package org-indent
;;     :diminish org-indent-mode
;;     :config (org-indent-mode 1))

;;   (use-package org-bullets
;;     :ensure t
;;     :init (add-hook 'org-mode-hook #'org-bullets-mode))

;;   (use-package org-autolist
;;     :ensure t)

;;   (use-package org-footnote
;;     :config
;;     (setq org-footnote-define-inline t
;;           org-footnote-auto-label 'random)))


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

;; In a line with comments, "C-u M-;" removes the comments altogether. That means deleting the
;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
(bind-keys*
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box))

(bind-keys
 ("<f10>" . other-window) ; Switch to the other buffer
 ("<f11>" . delete-other-windows)
 ("C-x k" . kill-this-buffer)
 ("<f12>" . sb/kill-other-buffers))

(bind-keys*
 ("C-s" . save-buffer)
 ("C-S-s" . sb/save-all-buffers))
(unbind-key "C-x s") ; Bound to save-some-buffers
(bind-key "C-x s" #'sb/switch-to-scratch)

;; (bind-keys
;;  ("C-c d b" . sb/byte-compile-current-file)
;;  ("C-c d i" . sb/byte-compile-init-dir))

(bind-key "C-c d f" #'auto-fill-mode)

(use-package which-key ; Show help popups for prefix keys
  :ensure t
  ;; :if (and (not (bound-and-true-p dotemacs-use-ecb)) (and (version<= "24.4.0" emacs-version)))
  :hook (after-init . which-key-mode)
  :config (which-key-setup-side-window-right-bottom)
  :diminish which-key-mode)


;; Mark safe variables

(put 'company-clang-arguments 'safe-local-variable 'listp)
(put 'company-c-headers-path-user 'safe-local-variable 'listp)
(put 'reftex-default-bibliography 'safe-local-variable 'listp)
(put 'company-bibtex-bibliography 'safe-local-variable 'listp)
(put 'bibtex-completion-bibliography 'safe-local-variable 'listp)
(put 'flycheck-clang-include-path 'safe-local-variable 'listp)
(put 'flycheck-gcc-include-path 'safe-local-variable 'listp)

;;; init.el ends here
