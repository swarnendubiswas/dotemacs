;;; defaults-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup and tweak emacs defaults.

;;; Code:

(setq inhibit-default-init t ; Disable loading of "default.el" at startup.
      inhibit-startup-screen t
      ;; inhibit-splash-screen t ; Actually an alias of inhibit-startup-screen.
      initial-scratch-message nil
      ;; *scratch* is in Lisp interaction mode by default, use text mode instead.
      initial-major-mode 'text-mode)

(setq-default major-mode 'text-mode)

(use-package files
  :config
  (setq require-final-newline t ; Always end a file with a newline.
        make-backup-files nil ; Stop making backup ~ files
        ;; Disable backup for a per-file basis, not to be used by major modes.
        backup-inhibited t)) 

(setq sentence-end-double-space nil)
(setq x-select-enable-clipboard t) ; Enable use of system clipboard across emacs and other applications.
(fset 'yes-or-no-p 'y-or-n-p) ; Type "y"/"n" instead of "yes"/"no".
(fset 'display-startup-echo-area-message #'ignore)

(use-package simple
  :config
  ;; We need to paste something from another program, but sometimes we do real paste after some kill
  ;; action, that will erase the clipboard, so we need to save it to kill ring. Paste it using "C-y M-y".
  (setq save-interprogram-paste-before-kill t)
  ;; Enable visual feedback on selections, default since v23 
  (transient-mark-mode 1))

(use-package autorevert
  :defer 10
  :init (global-auto-revert-mode 1) ;; Auto-refresh all buffers, does not work for remote files.
  :config
  (setq-default auto-revert-interval 5 ; Default is 5 s.
                auto-revert-verbose nil
                global-auto-revert-non-file-buffers t)) ; Auto-refresh dired buffers.

(delete-selection-mode 1) ; typing with the mark active will overwrite the marked region

(setq search-highlight t ; highlight incremental search
      query-replace-highlight t ; highlight during query
      case-fold-search t) ; make search ignore case

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh" ; faster than the default scp
        tramp-default-user "XXX"
        tramp-default-host "XXX"
        tramp-auto-save-directory (locate-user-emacs-file "tramp-auto-save")))

;; disable version control
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(setq completion-ignore-case t ; ignore case when completing
      read-file-name-completion-ignore-case t ; ignore case when reading a file name completion
      read-buffer-completion-ignore-case t) 

;; dim the ignored part of the file name
(file-name-shadow-mode 1)

;; do not use dialog boxes
(setq use-dialog-box nil
      use-file-dialog nil)

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

;; fontification
(global-font-lock-mode 1) ; turn on syntax coloring, on by default since Emacs 22

(use-package font-lock
  :config
  (setq font-lock-maximum-decoration t ; maximum fontification possible
        font-lock-support-mode 'jit-lock-mode)) ; jit locking is better than fast-lock and lazy-lock       

(use-package jit-lock
  :config
  (setq jit-lock-defer-time 0.10 ; improve scrolling speed with jit fontification
        jit-lock-stealth-time 10
        jit-lock-defer-contextually t
        jit-lock-stealth-nice 0.5))

;;(highlight-changes-mode 1) ; not very useful usually

;; remember cursor position in files
(use-package saveplace
  :defer t
  :config (setq-default save-place t))

;; incremental minibuffer completion/suggestions
(use-package icomplete
  :disabled t
  :config
  (icomplete-mode 1)
  (use-package icomplete+)
  (setq icomplete-prospects-height 2
        icomplete-compute-delay 0))

(use-package icicles
  :disabled t
  :config (icy-mode 1))

;; save minibuffer histories across sessions
(use-package savehist
  :defer t
  :config
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
        savehist-save-minibuffer-history t
        savehist-file "~/.emacs.d/savehist") 
  (savehist-mode 1))

(use-package uniquify
  :defer t
  :config
  ;; options: post-forward, reverse, forward
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets ; emacs 24.4 style ⁖ cat.png<dirName>
        ;;uniquify-separator ":"
        uniquify-after-kill-buffer-p t))

(provide 'defaults-init)

;;; defaults-init.el ends here
