;;; ido-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure ido

;;; Code:

(use-package ido
  :ensure t
  :demand t
  :init
  (progn
    (ido-mode 1)
    (ido-everywhere 1))
  :config
  (setq ido-enable-flex-matching t
        ido-enable-prefix nil
        ido-max-prospects 10
        ido-case-fold t
        ido-use-filename-at-point 'guess ; other options: 'ffap-guesser
        ;;ido-show-dot-for-dired t ; don't show current directory as the first choice
        ido-create-new-buffer 'always ; other options: prompt, never
        ido-default-file-method 'selected-window
        ido-save-directory-list-file "~/.emacs.d/.ido.last"
        ido-enable-last-directory-history t
        ido-max-work-directory-list 20
        ido-max-work-file-list 50
        confirm-nonexistent-file-or-buffer nil
        ido-use-faces nil ; disable ido faces to see flx highlights
        ido-use-virtual-buffers t
        ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Compile-Log*" "Flycheck error messages*"
                             "*Messages*" "Async Shell Command")
        ido-enable-tramp-completion t))

(use-package ido-vertical-mode
  :ensure t
  :demand t
  :init (ido-vertical-mode 1)
  :config
  ;; up and down keys to navigate options, left and right to move through history/directories
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package ido-yes-or-no
  :ensure t
  :defer t)

(use-package ido-hacks
  :ensure t
  :defer t)

;; allow ido-style completion in more places
(use-package ido-ubiquitous
  :ensure t
  :init (ido-ubiquitous-mode 1))

(use-package ido-better-flex
  :ensure t
  :defer t
  :config (ido-better-flex/enable))

(use-package ido-better-flex
  :ensure t
  :defer t)

(use-package ido-at-point
  :ensure t
  :defer t
  :init (ido-at-point-mode 1))

;; smarter fuzzy matching for ido
(use-package flx-ido
  :ensure t
  :init (flx-ido-mode 1))

(provide 'ido-init)

;;; ido-init.el ends here
