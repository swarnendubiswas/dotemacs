;;; ido-init.el --- Part of emacs initialization

;;; Commentary:
;; Configure ido

;;; Code:

(use-package ido
             :ensure t
             :init
               (setq ido-everywhere t
                     ido-enable-flex-matching t
                     ido-enable-prefix nil
                     ido-max-prospects 10
                     ido-case-fold t
                     ;;ido-use-filename-at-point 'guess ; other options: 'ffap-guesser
                     ;;ido-show-dot-for-dired t ; don't show current directory as the first choice
                     ido-create-new-buffer 'always ; other options: prompt, never
                     ido-save-directory-list-file "~/.emacs.d/.ido.last"
                     ido-enable-last-directory-history t
                     ido-max-work-directory-list 20
                     ido-max-work-file-list 50
                     confirm-nonexistent-file-or-buffer nil
                     ido-use-faces nil ; disable ido faces to see flx highlights
                     ido-use-virtual-buffers t
                     ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Compile-Log*" "Flycheck error messages*"
			     "*Messages*" "Async Shell Command"))
  :config (ido-mode 1))

(use-package ido-vertical-mode
             :ensure t
  :config (ido-vertical-mode 1))

(use-package ido-yes-or-no
             :ensure t
  :defer t)

(use-package ido-hacks
             :ensure t
  :defer t)

(use-package ido-ubiquitous
             :ensure t
  :config (ido-ubiquitous-mode 1) ; allow ido-style completion in more places
  :defer t)

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
  :config (ido-at-point-mode 1))

(use-package flx-ido
             :ensure t
             :init (flx-ido-mode 1) ; smarter fuzzy matching for ido
  :defer t)

(provide 'ido-init)

;;; ido-init.el ends here
