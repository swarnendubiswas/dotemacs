;;; defaults-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup and tweak emacs defaults.

;;; Code:

;; SB: Should we use use-package for managing built-in modules?

(setq inhibit-default-init t ; Disable loading of "default.el" at startup.
      inhibit-startup-screen t
      ;; inhibit-splash-screen t ; Actually an alias of inhibit-startup-screen.
      initial-scratch-message nil
      ;; *scratch* is in Lisp interaction mode by default, use text mode instead.
      initial-major-mode 'text-mode
      inhibit-startup-echo-area-message t)

;; major mode to use for files that do no specify a major mode, default value is fundamental-mode
(setq-default major-mode 'text-mode)

(setq locale-coding-system 'utf-8)

(use-package mule
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-input-method nil)

(use-package files
  :config
  (setq require-final-newline t ; Always end a file with a newline.
        make-backup-files nil ; Stop making backup ~ files
        ;; Disable backup for a per-file basis, not to be used by major modes.
        backup-inhibited t
        auto-save-default t
        ;; 'y-or-n-p
        confirm-kill-emacs nil))

(setq sentence-end-double-space nil
      ;; Enable use of system clipboard across emacs and other applications.
      x-select-enable-clipboard t
      line-number-display-limit 2000000)
(fset 'yes-or-no-p 'y-or-n-p) ; Type "y"/"n" instead of "yes"/"no".
(fset 'display-startup-echo-area-message #'ignore)
(toggle-indicate-empty-lines 1)

(use-package simple
  :config
  ;; We need to paste something from another program, but sometimes we do real paste after some kill
  ;; action, that will erase the clipboard, so we need to save it to kill ring. Paste it using "C-y M-y".
  (setq save-interprogram-paste-before-kill t
        kill-whole-line t
        suggest-key-bindings t
        ;; use shift-select for marking
        shift-select-mode t)
  ;; Enable visual feedback on selections, default since v23
  (transient-mark-mode 1))

;; Auto-refresh all buffers, does not work for remote files.
(use-package autorevert
  :config
  (setq-default auto-revert-interval 10 ; Default is 5 s.
                auto-revert-verbose nil
                ;; Auto-refresh dired buffers.
                global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

;; typing with the mark active will overwrite the marked region, pending-delete-mode is an alias
(use-package delsel
  :config (delete-selection-mode 1))

(setq delete-by-moving-to-trash t)

;; /method:user@host#port:filename. Shortcut /ssh:: will connect to default user@host#port.
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh" ; faster than the default scp
        tramp-default-user "XXX"
        tramp-default-host "XXX"
        tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")
        ;; tramp history
        tramp-persistency-file-name (concat dotemacs-temp-directory "tramp"))
  (use-package password-cache
    :config (setq password-cache-expiry nil))
  (use-package tramp-term
    :ensure t))

;; ;; disable version control
;; (setq vc-ignore-dir-regexp
;;       (format "\\(%s\\)\\|\\(%s\\)"
;;               vc-ignore-dir-regexp
;;               tramp-file-name-regexp))

(setq completion-ignore-case t ; ignore case when completing
      read-file-name-completion-ignore-case t ; ignore case when reading a file name completion
      read-buffer-completion-ignore-case t)

;; increase gc threshold
(setq gc-cons-threshold (* 10 1024 1024))

;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;; do not use dialog boxes
(setq use-dialog-box nil
      use-file-dialog nil)

(use-package advice
  :config (setq ad-redefinition-action 'accept))

;; desktop save mode
(use-package desktop
  :disabled t
  :config
  (setq-default desktop-restore-frames nil ; no need to restore frames
                desktop-load-locked-desktop nil)
  (desktop-save-mode -1))

;; fully redraw the display before queued input events are processed
;; don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; fontification: turn on syntax coloring, on by default since Emacs 22
(use-package font-core
  :config (global-font-lock-mode 1))

(use-package font-lock
  :config
  (setq font-lock-maximum-decoration t ; maximum fontification possible
        ;; jit locking is better than fast-lock and lazy-lock
        font-lock-support-mode 'jit-lock-mode))

(use-package jit-lock
  :config
  (setq jit-lock-defer-time 0.10 ; improve scrolling speed with jit fontification
        jit-lock-stealth-time 10
        jit-lock-defer-contextually t
        jit-lock-stealth-nice 0.5))

(use-package hi-lock
  :config (global-hi-lock-mode 1)
  :diminish hi-lock-mode)

;; remember cursor position in files
(use-package saveplace
  :config
  (setq-default save-place t
                save-place-file (concat dotemacs-temp-directory "places")))

;; incremental minibuffer completion/suggestions
(use-package icomplete
  :disabled t
  :config
  (icomplete-mode 1)
  (use-package icomplete+
    :ensure t)
  (setq icomplete-prospects-height 2
        icomplete-compute-delay 0))

(use-package icicles
  :disabled t
  :config (icy-mode 1))

;; save minibuffer histories across sessions
(use-package savehist
  :config
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-save-minibuffer-history t
        savehist-file (concat dotemacs-temp-directory "savehist")
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring))
  (setq-default history-length 50)
  (savehist-mode 1))

(setq enable-recursive-minibuffers t)

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode 1))

(use-package uniquify
  :config
  ;; options: post-forward, reverse, forward
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets ; emacs 24.4 style ⁖ cat.png<dirName>
        uniquify-separator ":"
        ;;uniquify-min-dir-content 0
        uniquify-after-kill-buffer-p t))

;; hippie expand is dabbrev expand on steroids
(use-package hippie-exp
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
                                           try-complete-lisp-symbol))
  (use-package hippie-exp-ext
    :ensure t))

(use-package subword
  :diminish subword-mode
  :config (global-subword-mode 1))

;; Set Emacs split to horizontal or vertical
;; (if (eq dotemacs-window-split 'horizontal)
;;     (setq split-height-threshold 0
;;           split-width-threshold nil)
;;   (setq split-height-threshold nil
;;         split-width-threshold 0))

(provide 'defaults-init)

;;; defaults-init.el ends here
