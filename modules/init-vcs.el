;;; init-vcs.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(declare-function sb/smerge-hydra/body "init-keybindings")

(use-package vc-hooks
  :straight (:type built-in)
  :custom (vc-follow-symlinks t "No need to ask")
  ;; Disabling vc is said to improve performance. However, I find it useful to show branch
  ;; information on the modeline and highlight modifications in the current file.
  (vc-handled-backends '(Git)))

(use-package magit
  :bind (("C-x g" . magit-status) ("C-c M-g" . magit-file-dispatch) ("C-x M-g" . magit-dispatch))
  :custom
  ;; Open the status buffer in a full frame
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; Suppress the message "Turning on magit-auto-revert-mode" when loading Magit
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  ;; https://irreal.org/blog/?p=8877
  (magit-section-initial-visibility-alist
    '((stashes . show) (untracked . show) (unpushed . show) (unpulled . show)))
  :config
  (require 'magit-diff)
  (setq
    magit-diff-refine-hunk t
    magit-diff-highlight-trailing nil))

(use-package git-modes
  :commands (gitignore-mode gitattributes-mode gitconfig-mode)
  :mode ("dotgitconfig" . gitconfig-mode))

;; Diff-hl looks nicer than git-gutter, and is based on `vc'
(use-package diff-hl
  :if (boundp 'vc-handled-backends)
  :commands diff-hl-dired-mode-unless-remote
  :hook
  (
    (diff-hl-mode-on-hook
      .
      (lambda ()
        (unless (display-graphic-p)
          (diff-hl-margin-local-mode 1))))
    (dired-mode-hook . diff-hl-dired-mode-unless-remote) (emacs-startup-hook . global-diff-hl-mode))
  :custom
  (diff-hl-draw-borders nil "Highlight without a border looks nicer")
  (diff-hl-disable-on-remote t)
  :config (diff-hl-flydiff-mode 1)

  ;; Display margin since the fringe is unavailable in TTY
  ;; (unless (display-graphic-p)
  ;;   (diff-hl-margin-mode 1))

  (with-eval-after-load "magit"
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)))

;; Use "M-p/n" to cycle between older commit messages.
(use-package git-commit
  :commands git-commit-turn-on-flyspell
  :hook (git-commit-setup-hook . git-commit-turn-on-flyspell)
  :custom (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

;; Use the minor mode `smerge-mode' to move between conflicts and resolve them
(use-package smerge-mode
  :preface
  (defun sb/enable-smerge-maybe-with-vc ()
    "Enable `smerge-mode' automatically based on conflict markers."
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1)))))

  (defun sb/enable-smerge-maybe-without-vc ()
    "Enable `smerge-mode' automatically based on conflict markers."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  :straight (:type built-in)
  :commands
  (smerge-next
    smerge-prev
    smerge-auto-leave
    smerge-keep-base
    smerge-keep-upper
    smerge-keep-lower
    smerge-keep-all
    smerge-diff-base-lower
    smerge-diff-base-upper
    smerge-diff-upper-lower
    smerge-refine
    smerge-combine-with-next
    smerge-resolve)
  :init
  (add-hook 'find-file-hook #'sb/enable-smerge-maybe-with-vc :append)
  (add-hook 'after-revert-hook #'sb/enable-smerge-maybe-with-vc :append)
  :bind
  (:map
    smerge-mode-map
    ("M-g n" . smerge-next)
    ("M-g p" . smerge-prev)
    ("M-g c" . smerge-keep-current)
    ("M-g u" . smerge-keep-upper)
    ("M-g l" . smerge-keep-lower)
    ("M-g b" . smerge-keep-base)
    ("M-g a" . smerge-keep-all)
    ("M-g e" . smerge-ediff)
    ("M-g K" . smerge-kill-current)
    ("M-g m" . smerge-context-menu)
    ("M-g M" . smerge-popup-context-menu)))

(use-package with-editor :diminish)

(provide 'init-vcs)

;;; init-vcs.el ends here
