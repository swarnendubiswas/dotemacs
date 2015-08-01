;;; defaults-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup and tweak Emacs defaults.

;;; Code:

;; SB: Should we use use-package for managing built-in modules?

(setq inhibit-default-init t ; Disable loading of "default.el" at startup.
      inhibit-startup-screen t
      ;; inhibit-splash-screen t ; Actually an alias of inhibit-startup-screen.
      initial-scratch-message nil
      ;; *scratch* is in Lisp interaction mode by default, use text mode instead.
      initial-major-mode 'text-mode
      inhibit-startup-echo-area-message t
      create-lockfiles nil)

;; major mode to use for files that do no specify a major mode, default value is fundamental-mode
(setq-default major-mode 'text-mode)

(setq locale-coding-system 'utf-8)

(use-package mule
  :if (eq system-type 'windows-nt)
  :config
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(when (eq system-type 'windows-nt)
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-input-method nil))

(use-package files
  :init
  (setq require-final-newline t ; Always end a file with a newline.
        make-backup-files nil ; Stop making backup ~ files
        ;; Disable backup for a per-file basis, not to be used by major modes.
        backup-inhibited t
        auto-save-default t
        ;; 'y-or-n-p
        confirm-kill-emacs nil)
  :bind (("C-c d r" . revert-buffer)))

(setq-default sentence-end-double-space nil)

(setq x-select-enable-clipboard t ; Enable use of system clipboard across Emacs and other applications.
      line-number-display-limit 2000000
      visible-bell nil
      ;; draw underline lower
      x-underline-at-descent-line t)
(fset 'yes-or-no-p 'y-or-n-p) ; Type "y"/"n" instead of "yes"/"no".
(fset 'display-startup-echo-area-message #'ignore)

;; Mouse cursor in terminal mode
(xterm-mouse-mode 1)

(use-package menu-bar
  :init (toggle-indicate-empty-lines 1))

(use-package simple
  :init
  ;; We need to paste something from another program, but sometimes we do real paste after some kill
  ;; action, that will erase the clipboard, so we need to save it to kill ring. Paste it using "C-y M-y".
  (setq save-interprogram-paste-before-kill t
        kill-whole-line t
        suggest-key-bindings t
        ;; use shift-select for marking
        shift-select-mode t)

  ;; Enable visual feedback on selections, default since v23
  (transient-mark-mode 1)
  (column-number-mode 1)

  (auto-fill-mode 1)
  ;; This is not a library/file, so eval-after-load does not work
  (diminish 'auto-fill-function)
  :bind ("C-c d f" . auto-fill-mode))

;; Auto-refresh all buffers, does not work for remote files.
(use-package autorevert
  :defer 2
  :init
  (global-auto-revert-mode 1)
  ;; (add-hook 'text-mode-hook
  ;;           (lambda ()
  ;;             (auto-revert-tail-mode 1)))
  :config
  (setq-default auto-revert-interval 10 ; Default is 5 s.
                auto-revert-verbose nil
                ;; Auto-refresh dired buffers.
                global-auto-revert-non-file-buffers t))

;; typing with the mark active will overwrite the marked region, pending-delete-mode is an alias
(use-package delsel
  :defer t
  :init (delete-selection-mode 1))

(setq delete-by-moving-to-trash t)

;; /method:user@host#port:filename. Shortcut /ssh:: will connect to default user@host#port.
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh" ; faster than the default scp
        tramp-default-user "biswass"
        tramp-default-host "XXX"
        tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")
        ;; tramp history
        tramp-persistency-file-name (concat dotemacs-temp-directory "tramp"))
  (use-package password-cache
    :init (setq password-cache-expiry nil))
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
  :defer 2
  :config
  ;; turn off warnings due to functions being redefined
  (setq ad-redefinition-action 'accept))

;; Enable disabled commands
(put 'downcase-region  'disabled nil)   ; Let downcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let upcasing work

(advice-add 'capitalize-word :before #'goto-beginning-of-word)
(advice-add 'downcase-word :before #'goto-beginning-of-word)
(advice-add 'upcase-word :before #'goto-beginning-of-word)

;; desktop save mode
(use-package desktop
  :disabled t
  :init (desktop-save-mode -1)
  :config
  (setq-default desktop-restore-frames nil ; no need to restore frames
                desktop-load-locked-desktop nil))

;; fully redraw the display before queued input events are processed
;; don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; fontification: turn on syntax coloring, on by default since Emacs 22
(use-package font-core
  :init (global-font-lock-mode 1))

(use-package font-lock
  :init
  (setq font-lock-maximum-decoration t ; maximum fontification possible
        ;; jit locking is better than fast-lock and lazy-lock
        font-lock-support-mode 'jit-lock-mode))

(use-package jit-lock
  :init
  (setq jit-lock-defer-time 0.10 ; improve scrolling speed with jit fontification
        jit-lock-stealth-time 10
        jit-lock-defer-contextually t
        jit-lock-stealth-nice 0.5))

(use-package hi-lock
  :init (global-hi-lock-mode 1)
  :diminish hi-lock-mode)

;; This config needs to be modified for Emacs 25+, check this link
;; http://emacs.stackexchange.com/questions/12709/how-to-save-last-place-of-point-in-a-buffer
;; remember cursor position in files
(use-package saveplace
  :defer 2
  :config
  (setq-default save-place t
                save-place-file (concat dotemacs-temp-directory "places")))

;; incremental minibuffer completion/suggestions
(use-package icomplete
  :disabled t
  :init (icomplete-mode 1)
  :config
  (use-package icomplete+
    :ensure t)
  (setq icomplete-prospects-height 2
        icomplete-compute-delay 0))

(use-package icicles
  :disabled t
  :init (icy-mode 1))

;; save minibuffer histories across sessions
(use-package savehist
  :defer 5
  :init (savehist-mode 1)
  :config
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-save-minibuffer-history t
        savehist-file (concat dotemacs-temp-directory "savehist")
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring))
  (setq-default history-length 50))

(setq enable-recursive-minibuffers t)

(use-package mb-depth
  :defer t
  :init (minibuffer-depth-indicate-mode 1))

(use-package uniquify
  :defer t
  :init
  ;; options: post-forward, reverse, forward
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets ; Emacs 24.4 style ⁖ cat.png<dirName>
        uniquify-separator ":"
        ;; uniquify-min-dir-content 0
        uniquify-after-kill-buffer-p t
        uniquify-strip-common-suffix t))

;; hippie expand is dabbrev expand on steroids
(use-package hippie-exp
  :defer t
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
    :ensure t)
  :bind* ("M-/" . hippie-expand))

(use-package subword
  :defer t
  :diminish subword-mode
  :init (global-subword-mode 1))

;; Set Emacs split to horizontal or vertical
;; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
;; Use  (setq split-width-threshold nil) for vertical split.
;; Use  (setq split-width-threshold 1) for horizontal split.

(if (eq dotemacs-window-split 'horizontal)
    (setq split-height-threshold 0
          split-width-threshold nil)
  (setq split-height-threshold nil
        split-width-threshold 0))

;; hide "Auto-saving...done" messages
;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun my-auto-save-wrapper (save-fn &rest args)
  (apply save-fn '(t)))
(advice-add 'do-auto-save :around #'my-auto-save-wrapper)

(provide 'defaults-init)

;;; defaults-init.el ends here
