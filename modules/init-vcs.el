;;; init-vcs.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(declare-function sb/smerge-hydra/body "init-keybindings")

(setq vc-follow-symlinks t ; No need to ask
      ;; Disabling vc improves performance, the alternate option is '(Git) to show branch
      ;; information on the modeline.
      vc-handled-backends '(Git))

;; Remove `vc-refresh-state' if we are not using `vc', i.e., `vc-handled-backends' is nil
(if (boundp 'vc-handled-backends)
    (add-hook 'find-file-hook #'vc-refresh-state)
  (remove-hook 'find-file-hook #'vc-refresh-state))

(use-package magit
  :commands magit-display-buffer-fullframe-status-v1
  :bind
  (("C-x g"   . magit-status)
   ("C-c M-g" . magit-file-dispatch)
   ("C-x M-g" . magit-dispatch))
  :custom
  ;; Open the status buffer in a full frame
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; Suppress the message we get about "Turning on magit-auto-revert-mode" when loading Magit
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  ;; https://irreal.org/blog/?p=8877
  (magit-section-initial-visibility-alist '((stashes   . show)
                                            (untracked . show)
                                            (unpushed  . show)
                                            (unpulled  . show)))
  (magit-repository-directories '(("/home/swarnendu/books" . 0)
                                  ("/home/swarnendu/bitbucket" . 1)
                                  ("/home/swarnendu/github" . 1)
                                  ("/home/swarnendu/prospar-workspace")
                                  ("/home/swarnendu/plass-workspace")
                                  ("/home/swarnendu/iss-workspace")))
  :config
  ;; These give a performance boost to Magit
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

  (require 'magit-diff)

  (defvar magit-diff-refine-hunk)
  (defvar magit-diff-paint-whitespace)
  (defvar magit-diff-highlight-trailing)

  (setq magit-diff-refine-hunk t
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil))

(use-package git-modes
  :commands gitignore-mode gitattributes-mode gitconfig-mode)

(use-package git-gutter
  :if (unless (boundp 'vc-handled-backends))
  :disabled t
  :commands global-git-gutter-mode
  :diminish
  :bind
  (("C-x p" . git-gutter:previous-hunk)
   ("C-x n" . git-gutter:next-hunk))
  :hook (after-init-hook . global-git-gutter-mode)
  :custom
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  (git-gutter:modified-sign " ")
  (git-gutter:update-interval 1)
  :config
  ;; https://github.com/syl20bnr/spacemacs/issues/10555
  ;; https://github.com/syohex/emacs-git-gutter/issues/24
  (git-gutter:disabled-modes '(fundamental-mode org-mode image-mode doc-view-mode pdf-view-mode)))

;; Diff-hl looks nicer than git-gutter, based on `vc'
(use-package diff-hl
  :if (boundp 'vc-handled-backends)
  :commands diff-hl-dired-mode-unless-remote
  :custom
  (diff-hl-draw-borders nil "Highlight without a border looks nicer")
  :config
  (diff-hl-flydiff-mode 1)
  ;; Display margin since the fringe is unavailable in TTY
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
  :hook
  ((magit-post-refresh-hook . diff-hl-magit-post-refresh)
   (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
   (after-init-hook         . global-diff-hl-mode)))

(use-package git-commit
  :commands git-commit-turn-on-flyspell
  :hook (git-commit-setup-hook . git-commit-turn-on-flyspell)
  :custom
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

;; Use the minor mode `smerge-mode' to move between conflicts and resolve them
(use-package smerge-mode
  :straight (:type built-in)
  :after hydra
  :commands (smerge-next smerge-prev smerge-auto-leave
                         smerge-keep-base smerge-keep-upper
                         smerge-keep-lower smerge-keep-all
                         smerge-diff-base-lower
                         smerge-diff-base-upper
                         smerge-diff-upper-lower smerge-refine
                         smerge-combine-with-next smerge-resolve)
  :preface
  (defun sb/enable-smerge-maybe ()
    "Enable smerge automatically based on conflict markers."
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1)))))

  (defun sb/enable-smerge-maybe2 ()
    "Enable `smerge-mode' automatically."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  :init
  (add-hook 'find-file-hook #'sb/enable-smerge-maybe2 :append)
  (add-hook 'magit-diff-visit-file-hook (lambda nil
                                          (when smerge-mode
                                            (sb/smerge-hydra/body))))
  :bind-keymap ("C-c v" . smerge-command-prefix)
  :bind
  (:map smerge-mode-map
        ("M-g n"   . smerge-next)
        ("M-g p"   . smerge-prev)
        ("M-g k c" . smerge-keep-current)
        ("M-g k u" . smerge-keep-upper)
        ("M-g k l" . smerge-keep-lower)
        ("M-g k b" . smerge-keep-base)
        ("M-g k a" . smerge-keep-all)
        ("M-g e"   . smerge-ediff)
        ("M-g K"   . smerge-kill-current)
        ("M-g m"   . smerge-context-menu)
        ("M-g M"   . smerge-popup-context-menu)))

(use-package ediff
  :straight (:type built-in)
  :after magit
  :demand t
  :defines ediff-window-setup-function
  :commands (ediff-setup-windows-plain ediff-set-diff-options)
  :custom
  ;; Change default ediff style: do not start another frame with `ediff-setup-windows-default'
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split windows horizontally in ediff (instead of vertically)
  (ediff-split-window-function #'split-window-horizontally)
  :config
  (ediff-set-diff-options 'ediff-diff-options "-w"))

(use-package treemacs-magit
  :after (treemacs magit)
  :demand t)

(use-package consult-ls-git
  :bind
  (("C-c g f" . consult-ls-git)
   ("C-c g F" . consult-ls-git-other-window)))

(provide 'init-vcs)

;;; init-vcs.el ends here
