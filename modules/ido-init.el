;;; ido-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure ido.

;;; Code:

(defvar recentf-list)
(defvar dotemacs-temp-directory)
(defvar dotemacs-ido-view-mode)

;; C-j to create a new buffer rather than switching to an existing buffer
;; C-x C-f C-f to kick you out of ido mode into "normal" find file mode
(use-package ido
  :ensure t
  :preface
  ;; https://www.emacswiki.org/emacs/RecentFiles
  (defun dotemacs--recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " (mapcar #'abbreviate-file-name recentf-list) nil t)))
      (when file
        (find-file file))))
  :config
  (setq ido-enable-flex-matching t
        ido-enable-prefix nil
        ido-max-prospects 20
        ido-case-fold t ; Searching of buffer and file names should ignore case
        ido-use-filename-at-point 'guess
        ido-use-url-at-point nil
        ido-show-dot-for-dired nil ; Don't show current directory as the first choice
        ido-enable-dot-prefix t
        ido-create-new-buffer 'always
        ido-default-file-method 'selected-window
        ido-save-directory-list-file (concat dotemacs-temp-directory "ido.last")
        ido-enable-last-directory-history t
        ido-max-work-directory-list 50
        ido-max-work-file-list 50
        confirm-nonexistent-file-or-buffer t
        ido-use-faces nil ; Disable ido faces to see flx highlights
        ido-use-virtual-buffers 'auto
        ido-auto-merge-work-directories-length -1
        ido-confirm-unique-completion nil
        ido-ignore-extensions t ; Make ido use completion-ignored-extensions
        ido-enable-tramp-completion t
        ;; The ido-completion-help window is distracting
        ido-cannot-complete-command 'ido-next-match)

  (unless (bound-and-true-p dotemacs-use-ignoramus-p)
    (setq ido-ignore-buffers (append '("^ "
                                       "*Completions*"
                                       "*Shell Command Output*"
                                       "*Compile-Log*"
                                       "Flycheck error messages*"
                                       "\\`\\*"
                                       "Async Shell Command"
                                       "*Paradox Report*")
                                     ido-ignore-buffers))
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

  (use-package ido-hacks
    :ensure t
    :config (ido-hacks-mode 1))

  (use-package ido-ubiquitous ; Allow ido-style completion in more places
    :ensure t
    :config (ido-ubiquitous-mode 1))

  (use-package ido-completing-read+
    :ensure t)

  (use-package flx-ido ; Smarter fuzzy matching for ido
    :ensure t
    :config (flx-ido-mode 1))

  (use-package ido-at-point
    :ensure t
    :config (ido-at-point-mode 1))

  (use-package ido-describe-bindings
    :ensure t
    :bind ([remap describe-bindings] . ido-describe-bindings))

  (use-package ido-sort-mtime
    :ensure t
    :config (ido-sort-mtime-mode 1))

  (cond ((eq dotemacs-ido-view-mode 'vertical) (use-package ido-vertical-mode
                                                 :ensure t
                                                 :config
                                                 (set-face-attribute 'ido-vertical-first-match-face nil
                                                                     :background nil
                                                                     :foreground "orange")
                                                 (set-face-attribute 'ido-vertical-only-match-face nil
                                                                     :background nil
                                                                     :foreground nil)
                                                 (set-face-attribute 'ido-vertical-match-face nil
                                                                     :foreground nil)
                                                 ;; Up and down keys to navigate options, left and right to move through history/directories
                                                 (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right
                                                       ido-vertical-show-count t
                                                       ido-max-window-height 20 ; Increase the height of the minibuffer
                                                       ido-vertical-pad-list t)
                                                 (ido-vertical-mode 1)))

        ((eq dotemacs-ido-view-mode 'grid) (use-package ido-grid-mode
                                             :ensure t
                                             :config
                                             (setq ido-grid-mode-min-rows 10
                                                   ido-grid-mode-max-rows 20
                                                   ;; Listing order, t: left-right then top-bottom, nil: top-bottom then
                                                   ;; left-right
                                                   ido-grid-mode-order nil)
                                             (ido-grid-mode 1))))

  :bind
  (("<f1>" . smex)
   ([remap find-file] . ido-find-file)
   ("<f2>" . ido-find-file)
   ([remap switch-to-buffer] . ido-switch-buffer)
   ("<f3>" . ido-switch-buffer)
   ("C-x d" . ido-dired)
   ("<f9>" . dotemacs--recentf-ido-find-file)))

(provide 'ido-init)

;;; ido-init.el ends here
