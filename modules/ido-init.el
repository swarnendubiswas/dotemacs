;;; ido-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure ido.

;;; Code:

(use-package ido
  :ensure t
  :init
  (setq ido-enable-flex-matching t
        ido-enable-prefix nil
        ido-max-prospects 10
        ido-case-fold t
        ;;ido-use-filename-at-point 'guess ; other options: 'ffap-guesser
        ido-show-dot-for-dired nil ; don't show current directory as the first choice
        ido-create-new-buffer 'always ; other options: prompt, never
        ido-default-file-method 'selected-window
        ido-save-directory-list-file (concat dotemacs--temp-directory ".ido.last")
        ido-enable-last-directory-history t
        ido-max-work-directory-list 20
        ido-max-work-file-list 50
        confirm-nonexistent-file-or-buffer nil
        ido-use-faces nil ; disable ido faces to see flx highlights
        ido-use-virtual-buffers 'auto
        ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Compile-Log*" "Flycheck error messages*"
                             ;; "*Messages*"
                             "Async Shell Command" "*Paradox Report*")
        ido-enable-tramp-completion t
        ido-ignore-extensions t)
  (ido-mode 1) ; options: 'files, 'buffers
  (ido-everywhere 1)

  :config
  (setq ido-ignore-files
        (append '("GTAGS" "GPATH" "GRTAGS" "GSYMS" "TAGS")
                ido-ignore-files))
  (dolist (dirs '(".svn" ".dropbox .git"))
    (add-to-list 'ido-ignore-directories dirs))

  (use-package ido-vertical-mode
    :ensure t
    :init
    ;; up and down keys to navigate options, left and right to move through history/directories
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (ido-vertical-mode 1))

  (use-package ido-yes-or-no
    :ensure t)

  (use-package ido-hacks
    :ensure t)

  ;; allow ido-style completion in more places
  (use-package ido-ubiquitous
    :ensure t
    :init (ido-ubiquitous-mode 1))

  (use-package ido-better-flex
    :ensure t
    :init (ido-better-flex/enable))

  (use-package ido-at-point
    :ensure t
    :init (ido-at-point-mode 1))

  ;; smarter fuzzy matching for ido
  (use-package flx-ido
    :ensure t
    :init (flx-ido-mode 1))

  (use-package ido-completing-read+
    :ensure t)

  :bind
  (("<f2>" . ido-find-file)
   ("C-x C-f" . ido-find-file)
   ("C-x b" . ido-switch-buffer)
   ("C-x d" . ido-dired)))

(provide 'ido-init)

;;; ido-init.el ends here
