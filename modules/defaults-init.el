;;; defaults-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup and tweak Emacs defaults.

;;; Code:

(defvar dotemacs-window-split)
(defvar dotemacs-temp-directory)
(defvar select-enable-clipboard)

(setq inhibit-default-init t ; Disable loading of "default.el" at startup, inhibits site default settings
      inhibit-startup-screen t ; inhibit-splash-screen is an alias
      inhibit-startup-echo-area-message t
      initial-major-mode 'text-mode ; *scratch* is in Lisp interaction mode by default, use text mode instead
      initial-scratch-message nil
      create-lockfiles nil
      message-log-max 5000
      line-number-display-limit 2000000
      ring-bell-function 'ignore ; Turn off alarms completely: https://www.emacswiki.org/emacs/AlarmBell
      x-underline-at-descent-line t ; Draw underline lower
      completion-ignore-case t ; Ignore case when completing
      gc-cons-threshold (* 10 1024 1024) ; Increase gc threshold
      use-dialog-box nil
      use-file-dialog nil
      delete-by-moving-to-trash t
      scroll-margin 0 ; Drag the point along while scrolling
      scroll-conservatively 1000 ; Never recenter the screen while scrolling
      scroll-error-top-bottom t ; Move to begin/end of buffer before signalling an error
      scroll-preserve-screen-position t
      ;; Ignore case when reading a file name completion
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      switch-to-buffer-preserve-window-point t)

(setq-default major-mode 'text-mode ; Major mode to use for files that do no specify a major mode, default value is
                                        ; fundamental-mode
              sentence-end-double-space nil
              truncate-lines nil
              truncate-partial-width-windows nil
              history-length 50
              history-delete-duplicates t
              ;; Disabling this is one way to speed up Emacs with buffers with long lines
              bidi-display-reordering nil)

(when (>= emacs-major-version 24)
  (setq inhibit-message t))

(unless (bound-and-true-p dotemacs-use-ignoramus-p)
  ;; Avoid completing temporary files - http://endlessparentheses.com/improving-emacs-file-name-completion.html
  (dolist (ext '("auto/"
                 "-autoloads.el"
                 ".aux"
                 ".bbl"
                 ".blg"
                 ".cb"
                 ".cb2"
                 ".dvi"
                 ".elc"
                 ".exe"
                 ".fls"
                 ".idx"
                 ".lof"
                 ".log"
                 ".lot"
                 ".o"
                 ".out"
                 ".pdf"
                 "-pkg.el"
                 ".pyc"
                 ".rel"
                 ".rip"
                 ".toc"))
    (add-to-list 'completion-ignored-extensions ext)))

(when (eq system-type 'windows-nt)
  (setq locale-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-input-method nil)

  (use-package mule
    :config
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)))

(use-package files
  :config
  (setq require-final-newline t ; Always end a file with a newline.
        make-backup-files nil ; Stop making backup ~ files
        backup-inhibited t ; Disable backup for a per-file basis, not to be used by major modes.
        auto-save-default t
        confirm-kill-emacs nil)
  (when (and (>= emacs-major-version 26)
             (>= emacs-minor-version 1))
    (confirm-kill-processes nil)))

;; Enable use of system clipboard across Emacs and other applications.
(if (and (>= emacs-major-version 25)
         (>= emacs-minor-version 1))
    (setq select-enable-clipboard t)
  (setq x-select-enable-clipboard t))

(fset 'yes-or-no-p 'y-or-n-p) ; Type "y"/"n" instead of "yes"/"no".
(fset 'display-startup-echo-area-message #'ignore)

(use-package mouse
  :config
  ;; Text selected with the mouse is automatically copied to the clipboard.
  (setq mouse-drag-copy-region t))

(use-package xt-mouse ; Mouse cursor in terminal mode
  :config (xterm-mouse-mode -1))

(use-package menu-bar
  :config
  (toggle-indicate-empty-lines 1)
  (next-error-follow-minor-mode 1))

(use-package simple
  :config
  (setq idle-update-delay 2
        save-interprogram-paste-before-kill t   ; We need to paste something from another program, but sometimes we do
                                        ; real paste after some kill action, that will erase the clipboard, so we need
                                        ; to save it to kill ring. Paste it using "C-y M-y".
        kill-whole-line t
        suggest-key-bindings t
        shift-select-mode t ; Use shift-select for marking
        blink-matching-paren t)
  (transient-mark-mode 1) ; Enable visual feedback on selections, default since v23
  (column-number-mode 1)
  (diminish 'auto-fill-function) ; This is not a library/file, so eval-after-load does not work
  :bind ("C-c d f" . auto-fill-mode))

(use-package autorevert ; Auto-refresh all buffers, does not work for remote files.
  :diminish auto-revert-mode
  :config
  (setq-default auto-revert-interval 10 ; Default is 5 s.
                auto-revert-verbose nil
                ;; Auto-refresh dired buffers.
                global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

(use-package delsel
  :config
  ;; Typing with the mark active will overwrite the marked region, pending-delete-mode is an alias
  (delete-selection-mode 1))

(file-name-shadow-mode 1) ; Dim the ignored part of the file name in the minibuffer

(use-package advice
  :config
  ;; Turn off warnings due to functions being redefined
  (setq ad-redefinition-action 'accept))

;; ;; Enable disabled commands
;; (put 'downcase-region  'disabled nil) ; Let downcasing work
;; (put 'upcase-region    'disabled nil) ; Let upcasing work
;; (put 'erase-buffer     'disabled nil)
;; (put 'eval-expression  'disabled nil) ; Let ESC-ESC work
;; (put 'narrow-to-page   'disabled nil) ; Let narrowing work
;; (put 'narrow-to-region 'disabled nil) ; Let narrowing work
;; (put 'set-goal-column  'disabled nil)

(when (<= emacs-major-version 24)
  (progn
    (advice-add 'capitalize-word :before #'goto-beginning-of-word)
    (advice-add 'downcase-word :before #'goto-beginning-of-word)
    (advice-add 'upcase-word :before #'goto-beginning-of-word)))

(use-package desktop
  :disabled t
  :config
  (desktop-save-mode -1)
  (setq-default desktop-restore-frames nil ; No need to restore frames
                desktop-load-locked-desktop nil))

(use-package font-core ; Turn on syntax coloring, on by default since Emacs 22
  :config (global-font-lock-mode 1))

(use-package font-lock
  :disabled t
  :config
  (setq font-lock-maximum-decoration t ; Maximum fontification possible
        ;; Jit locking is better than fast-lock and lazy-lock
        font-lock-support-mode 'jit-lock-mode))

(use-package jit-lock ; Improve scrolling speed with jit fontification
  :disabled t
  :config
  (setq jit-lock-defer-time 0.10 
        jit-lock-stealth-time 10
        jit-lock-defer-contextually t
        jit-lock-stealth-nice 0.5))

(use-package hi-lock ; Custom regexp highlights
  :disabled t
  :config
  (setq hi-lock-auto-select-face t)
  (global-hi-lock-mode 1)
  :diminish hi-lock-mode)

(use-package saveplace ; Remember cursor position in files
  :config
  ;; http://emacs.stackexchange.com/questions/12709/how-to-save-last-place-of-point-in-a-buffer
  (if (and (>= emacs-major-version 25)
           (>= emacs-minor-version 1))
      (save-place-mode 1)
    (setq-default save-place t
                  save-place-limit nil))
  (setq save-place-file (concat dotemacs-temp-directory "places")))

(use-package savehist ; Save minibuffer histories across sessions
  :config
  (setq savehist-save-minibuffer-history t
        savehist-file (concat dotemacs-temp-directory "savehist")
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300)
  (savehist-mode 1))

(setq enable-recursive-minibuffers nil)
(use-package mb-depth
  :if (bound-and-true-p enable-recursive-minibuffers)
  :config (minibuffer-depth-indicate-mode 1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-ignore-buffers-re "^\\*"
        uniquify-after-kill-buffer-p t
        uniquify-strip-common-suffix t))

(use-package hippie-exp ; Hippie expand is dabbrev expand on steroids
  :config
  ;; https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-expand-all-abbrevs
                                           try-complete-file-name
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol))
  (use-package hippie-exp-ext
    :ensure t)
  :bind ("M-/" . hippie-expand))

(use-package subword
  :diminish subword-mode
  :config (global-subword-mode 1))

;; Set Emacs split to horizontal or vertical
;; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(if (eq dotemacs-window-split 'horizontal)
    (setq split-height-threshold nil
          split-width-threshold 0)
  (setq split-height-threshold 0
        split-width-threshold nil))

;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun my-auto-save-wrapper (save-fn &rest args)
  "Hide 'Auto-saving...done' messages by calling the method SAVE-FN with non-nil ARGS."
  (apply save-fn '(t)))
(advice-add 'do-auto-save :around #'my-auto-save-wrapper)

(use-package warnings
  :config (add-to-list 'warning-suppress-types '(undo discard-info)))

(use-package doc-view
  :defer t
  :diminish doc-view-minor-mode)

(use-package image-mode
  :defer t
  :diminish image-minor-mode)

(use-package smerge-mode
  :defer t
  :diminish smerge-mode)

(provide 'defaults-init)

;;; defaults-init.el ends here
