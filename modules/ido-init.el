;;; ido-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure ido.

;;; Code:

;; C-j to create a new buffer rather than switching to an existing buffer
;; C-x C-f C-f to kick you out of ido mode into "normal" find file mode
(use-package ido
  :ensure t
  :preface
  ;; https://www.emacswiki.org/emacs/RecentFiles#toc5
  ;; https://www.masteringemacs.org/article/find-files-faster-recent-files-package
  (defun dotemacs--ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
  :config
  (setq ido-enable-flex-matching t
        ido-enable-prefix nil
        ido-max-prospects 20
        ido-case-fold t ; Searching of buffer and file names should ignore case
        ido-use-filename-at-point 'guess ; Other options: 'ffap-guesser, 'guess
        ido-use-url-at-point nil
        ido-show-dot-for-dired nil ; Don't show current directory as the first choice
        ido-create-new-buffer 'always ; Other options: prompt, never
        ido-default-file-method 'selected-window
        ido-save-directory-list-file (concat dotemacs-temp-directory "ido.last")
        ido-enable-last-directory-history t
        ido-max-work-directory-list 50
        ido-max-work-file-list 50
        confirm-nonexistent-file-or-buffer t
        ido-use-faces nil ; Disable ido faces to see flx highlights
        ido-use-virtual-buffers 'auto
        ido-auto-merge-work-directories-length -1
        ido-ignore-buffers '("^ "
                             "*Completions*"
                             "*Shell Command Output*"
                             "*Compile-Log*"
                             "Flycheck error messages*"
                             ;; "*Messages*"
                             "\\`\\*"
                             "Async Shell Command"
                             "*Paradox Report*")
        ido-confirm-unique-completion nil
        ido-ignore-extensions t ; Make ido use completion-ignored-extensions
        ido-enable-tramp-completion t)

  (unless (bound-and-true-p dotemacs-use-ignoramus-p)
    (setq ido-ignore-files (append '("GTAGS"
                                     "GPATH"
                                     "GRTAGS"
                                     "GSYMS"
                                     "TAGS"
                                     "\\`\\.")
                                   ido-ignore-files))
    (dolist (dirs '(".svn"
                    ".git"
                    ".hg"
                    "\\`\\."))
      (add-to-list 'ido-ignore-directories dirs)))

  (ido-mode 1)
  (ido-everywhere 1)

  (use-package ido-yes-or-no ; Overkill
    :ensure t
    :disabled t
    :config (ido-yes-or-no-mode))

  (use-package ido-hacks
    :ensure t
    :functions ido-hacks-mode
    :config (ido-hacks-mode 1))

  (use-package ido-ubiquitous ; Allow ido-style completion in more places
    :ensure t
    :config (ido-ubiquitous-mode 1))

  (use-package ido-completing-read+
    :ensure t)

  (or (use-package flx-ido ; Smarter fuzzy matching for ido
        :ensure t
        :config (flx-ido-mode 1))

      (use-package ido-better-flex ; Can add more noise while matching patterns
        :ensure t
        :disabled t
        :config (ido-better-flex/enable)))

  (use-package ido-at-point
    :ensure t
    :config (ido-at-point-mode 1))

  (use-package ido-describe-bindings
    :ensure t
    :bind ([remap describe-bindings] . ido-describe-bindings))

  (use-package ido-sort-mtime
    :ensure t
    :disabled t
    :config (ido-sort-mtime-mode 1))

  (cond ((eq dotemacs-ido-view-mode 'vertical) (use-package ido-vertical-mode
                                                 :ensure t
                                                 :config
                                                 ;; Up and down keys to navigate options, left and right to move through history/directories
                                                 (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
                                                 (ido-vertical-mode 1)))

        ((eq dotemacs-ido-view-mode 'grid) (use-package ido-grid-mode
                                             :ensure t
                                             :config
                                             (setq ido-grid-mode-order nil ;; Listing order, t: left-right then top-bottom, nil: top-bottom then left-right
                                                   ido-grid-mode-min-rows 10
                                                   ido-grid-mode-max-rows 20)
                                             (ido-grid-mode 1))))

  :bind
  (("<f1>" . smex)
   ([remap find-file] . ido-find-file)
   ("<f2>" . ido-find-file)
   ([remap switch-to-buffer] . ido-switch-buffer)
   ("<f3>" . ido-switch-buffer)
   ("C-x d" . ido-dired)
   ("<f9>" . dotemacs--ido-recentf-open)))

(provide 'ido-init)

;;; ido-init.el ends here
