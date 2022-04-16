;;; core-config.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(setq initial-scratch-message nil   ;; "make scratch buffer empty"
      inhibit-startup-message t)    ;; "disable splash screen"

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; widen `fill-column'
(setq-default fill-column 80)

;; no beep and visual blinking
(setq ring-bell-function 'ignore
      visible-bell nil)

(setq frame-resize-pixelwise t)

;; highlight current line
(global-hl-line-mode 1)
;; no blink
(blink-cursor-mode 0)
;; prettify symbols
(global-prettify-symbols-mode 1)

;; Single space between sentences is more widespread than double
(setq sentence-end-double-space nil)

;; smooth scrolling
(setq scroll-conservatively 101
      scroll-margin 2)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; enable flymake-mode in prog-mode
(add-hook 'prog-mode-hook #'flymake-mode)
;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
;; enable subword-mode in prog-mode
(add-hook 'prog-mode-hook #'subword-mode)

;; scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Use system trash for file deletion.
(setq delete-by-moving-to-trash t)

;; autosave each change
(setq bookmark-save-flag 1)

;; don't set a fringe mark at bookmarked lines
(setq bookmark-set-fringe-mark nil)

;; keep focus while navigating help buffers
(setq help-window-select t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
;; (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; don't load outdated compiled files.
(setq load-prefer-newer t)

;; don't save duplicates in kill-ring
(setq kill-do-not-save-duplicates t)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t))

(use-package dired
  :defer t
  :config
  (setq dired-auto-revert-buffer t
        dired-create-destination-dirs 'ask
        dired-dwim-target t
        dired-listing-switches "-aBhl --group-directories-first"
        dired-vc-rename-file t))

(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 400))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(use-package ediff
  :hook ((ediff-quit . winner-undo)
         (ediff-prepare-buffer . outline-show-all))
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally))

(use-package project
  :defer t
  :config
  (setq project-current-inhibit-prompt t
        project-vc-merge-submodules nil
        project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory")
                                  (project-vc-dir "VC-Dir"))
        project-switch-use-entire-map t)

  (defun project-vc-dir ()
    "Run VC-Dir in the current project's root."
    (interactive)
    (let ((project-root (project-root (project-current t))))
      (if (file-exists-p (expand-file-name ".git" project-root))
          (cond ((fboundp 'magit-status-internal)
                 (magit-status-internal project-root))
                ((fboundp 'magit-status)
                 (with-no-warnings (magit-status project-root)))
                (t
                 (vc-dir project-root)))
        (vc-dir project-root)))))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-max-saved-items 100
        recentf-initialize-file-name-history nil)

  (add-to-list 'recentf-exclude `(recentf-expand-file-name ,(straight--emacs-dir "straight"))))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package tramp
  :defer t
  :config
  (setq remote-file-name-inhibit-cache 60
        tramp-verbose 1
        vc-handled-backends '(SVN Git))

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package xref
  :defer t
  :config
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))

  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function       #'xref-show-definitions-completing-read))

(use-package whitespace
  :hook ((prog-mode . show-trailing-whitespace)
         (diff-mode . whitespace-mode))
  :config
  (defun show-trailing-whitespace ()
    (set-face-attribute 'trailing-whitespace nil :background
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (setq show-trailing-whitespace t)))

(use-package winner
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (setq winner-boring-buffers-regexp "\\*.*\\*"))


(setq-default custom-file (no-littering-expand-var-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


(provide 'core-config)
;;; core-config.el ends here
