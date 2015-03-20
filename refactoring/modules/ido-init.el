(use-package ido
             :ensure t
             :config
             (progn
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
                                          "*Messages*" "Async Shell Command")
                     )
               )
             :init (ido-mode 1)
             )

(use-package ido-vertical-mode
             :ensure t
             :init (ido-vertical-mode 1)
             )

(use-package ido-yes-or-no
             :ensure t
             )

(use-package ido-hacks
             :ensure t
             )

(use-package ido-ubiquitous
             :ensure t
             :init (ido-ubiquitous-mode 1) ; allow ido-style completion in more places
             )

(use-package ido-better-flex
             :ensure t
             :init (ido-better-flex/enable)
             )

(use-package ido-better-flex
             :ensure t
             )

(use-package ido-at-point
             :ensure t
             :init (ido-at-point-mode 1)
             )

(use-package flx-ido
             :ensure t
             :init (flx-ido-mode 1) ; smarter fuzzy matching for ido
             )

