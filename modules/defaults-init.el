;;; defaults-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup and tweak Emacs defaults.

;;; Code:

(setq inhibit-default-init t ; Disable loading of "default.el" at startup.
      inhibit-startup-screen t
      ;; inhibit-splash-screen t ; Actually an alias of inhibit-startup-screen.
      initial-scratch-message nil
      initial-major-mode 'text-mode ; *scratch* is in Lisp interaction mode by default, use text mode instead.
      inhibit-startup-echo-area-message t
      create-lockfiles nil
      message-log-max 5000
      line-number-display-limit 2000000
      visible-bell nil
      x-underline-at-descent-line t ; Draw underline lower
      completion-ignore-case t ; Ignore case when completing
      read-file-name-completion-ignore-case t ; Ignore case when reading a file name completion
      read-buffer-completion-ignore-case t
      gc-cons-threshold (* 10 1024 1024) ; Increase gc threshold
      ;; Do not use dialog boxes
      use-dialog-box nil
      use-file-dialog nil
      enable-recursive-minibuffers t
      delete-by-moving-to-trash t
      scroll-margin 0 ; Drag the point along while scrolling
      scroll-conservatively 1000 ; Never recenter the screen while scrolling
      scroll-error-top-bottom t ; Move to begin/end of buffer before signalling an error
      scroll-preserve-screen-position t)

(setq-default major-mode 'text-mode ; Major mode to use for files that do no specify a major mode, default value is fundamental-mode
              sentence-end-double-space nil
              truncate-lines nil
              truncate-partial-width-windows nil)

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
  :init
  (setq require-final-newline t ; Always end a file with a newline.
        make-backup-files nil ; Stop making backup ~ files
        backup-inhibited t ; Disable backup for a per-file basis, not to be used by major modes.
        auto-save-default t
        ;; 'y-or-n-p
        confirm-kill-emacs nil)
  :bind (("C-c d r" . revert-buffer)))

;; Enable use of system clipboard across Emacs and other applications.
(if (and (>= emacs-major-version 25)
         (>= emacs-minor-version 1))
    (setq select-enable-clipboard t)
  (setq x-select-enable-clipboard t))

(fset 'yes-or-no-p 'y-or-n-p) ; Type "y"/"n" instead of "yes"/"no".
(fset 'display-startup-echo-area-message #'ignore)

;; (xterm-mouse-mode 1) ; Mouse cursor in terminal mode

(use-package menu-bar
  :init (toggle-indicate-empty-lines 1))

(use-package simple
  :init
  ;; We need to paste something from another program, but sometimes we do real paste after some kill
  ;; action, that will erase the clipboard, so we need to save it to kill ring. Paste it using "C-y M-y".
  (setq save-interprogram-paste-before-kill t
        kill-whole-line t
        suggest-key-bindings t
        ;; Use shift-select for marking
        shift-select-mode t
        blink-matching-paren t)
  (transient-mark-mode 1) ; Enable visual feedback on selections, default since v23
  (column-number-mode 1)
  (auto-fill-mode 1)
  (diminish 'auto-fill-function) ; This is not a library/file, so eval-after-load does not work
  :bind ("C-c d f" . auto-fill-mode))

(use-package autorevert ; Auto-refresh all buffers, does not work for remote files.
  :config
  (global-auto-revert-mode 1)
  (setq-default auto-revert-interval 10 ; Default is 5 s.
                auto-revert-verbose nil
                ;; Auto-refresh dired buffers.
                global-auto-revert-non-file-buffers t))

(use-package delsel ; typing with the mark active will overwrite the marked region, pending-delete-mode is an alias
  :config (delete-selection-mode 1))

;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
(use-package tramp ; /method:user@host#port:filename. Shortcut /ssh:: will connect to default user@host#port.
  :defer t
  :config
  (setq tramp-default-method "ssh" ; ssh is faster than the default scp
        tramp-default-user "biswass"
        tramp-default-host "stdlinux.cse.ohio-state.edu"
        tramp-auto-save-directory (concat dotemacs-temp-directory "tramp-auto-save")
        tramp-persistency-file-name (concat dotemacs-temp-directory "tramp"))
  ;; Disable backup
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  ;; Disable version control. If you access remote files which are not under version control, a lot of check operations
  ;; can be avoided by disabling VC.
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (use-package password-cache
    :init (setq password-cache-expiry nil))
  (use-package tramp-term
    :ensure t
    :disabled t))

(file-name-shadow-mode 1) ; Dim the ignored part of the file name in the minibuffer

(use-package advice
  :init
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
  :init (desktop-save-mode -1)
  :config
  (setq-default desktop-restore-frames nil ; No need to restore frames
                desktop-load-locked-desktop nil))

;; Fully redraw the display before queued input events are processed don't defer screen updates when performing
;; operations, obsolete since 24.5
(if (and (<= emacs-major-version 24)
         (<= emacs-minor-version 4))
    (setq redisplay-dont-pause t))

(use-package font-core ; Turn on syntax coloring, on by default since Emacs 22
  :init (global-font-lock-mode 1))

(use-package font-lock
  :init
  (setq font-lock-maximum-decoration t ; Maximum fontification possible
        ;; Jit locking is better than fast-lock and lazy-lock
        font-lock-support-mode 'jit-lock-mode))

(use-package jit-lock
  :init
  (setq jit-lock-defer-time 0.10 ; Improve scrolling speed with jit fontification
        jit-lock-stealth-time 10
        jit-lock-defer-contextually t
        jit-lock-stealth-nice 0.5))

(use-package hi-lock ; Custom regexp highlights
  :defer t
  :config
  (setq hi-lock-auto-select-face t)
  (global-hi-lock-mode 1)
  :diminish hi-lock-mode)

;; This config needs to be modified for Emacs 25+, check this link
;; http://emacs.stackexchange.com/questions/12709/how-to-save-last-place-of-point-in-a-buffer
(use-package saveplace ; remember cursor position in files
  :config
  (if (and (>= emacs-major-version 25)
           (>= emacs-minor-version 1))
      (save-place-mode 1)
    (setq-default save-place t))
  (setq save-place-file (concat dotemacs-temp-directory "places")))

(use-package icomplete ; Incremental minibuffer completion/suggestions
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

(use-package savehist ; Save minibuffer histories across sessions
  :config
  (savehist-mode 1)
  (setq savehist-save-minibuffer-history t
        savehist-file (concat dotemacs-temp-directory "savehist")
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300)
  (setq-default history-length 50
                history-delete-duplicates t))

(use-package bookmark
  :defer t
  :config (setq bookmark-default-file (concat dotemacs-temp-directory "bookmarks")))

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode 1))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets ; options: post-forward, reverse, forward
        uniquify-separator ":"
        ;; uniquify-min-dir-content 0
        uniquify-after-kill-buffer-p t
        uniquify-strip-common-suffix t))

(use-package hippie-exp ; Hippie expand is dabbrev expand on steroids
  :config
  ;; https://github.com/bbatsov/prelude/blob/master/core/prelude-editor.el
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

;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun my-auto-save-wrapper (save-fn &rest args)
  "Hide 'Auto-saving...done' messages."
  (apply save-fn '(t)))
(advice-add 'do-auto-save :around #'my-auto-save-wrapper)

(use-package warnings
  :config (add-to-list 'warning-suppress-types '(undo discard-info)))

(use-package abbrev
  :diminish abbrev-mode
  :config
  (setq-default abbrev-file-name (concat dotemacs-temp-directory "abbrev_defs"))
  (setq save-abbrevs 'silently) ; Do not ask to save new abbrevs when quitting
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  (add-hook 'text-mode-hook
            (lambda ()
              (abbrev-mode -1)))
  (add-hook 'prog-mode-hook
            (lambda ()
              (abbrev-mode -1))))

(provide 'defaults-init)

;;; defaults-init.el ends here
