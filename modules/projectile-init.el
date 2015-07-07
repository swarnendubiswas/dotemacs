;;; projectile-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup projectile for improved handling of projects.

;;; Code:

(use-package projectile
  :defer 2
  :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :init (projectile-global-mode 1)

  :config
  (setq projectile-enable-caching t
        projectile-cache-file (concat dotemacs--temp-directory "projectile.cache")
        projectile-completion-system 'helm
        ;; use projectile in every directory without requiring a project file
        projectile-require-project-root nil
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'projectile-dired
        projectile-mode-line '(:propertize
                               (:eval (concat " " (projectile-project-name)))
                               face font-lock-constant-face))
  (dolist (dirs '(".svn" ".dropbox"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  ;; Don't consider my home dir as a project
  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))
  (dolist (item '("GTAGS" "GRTAGS" "GPATH" "TAGS" "GSYMS"))
    (add-to-list 'projectile-globally-ignored-files item))

  (use-package helm-projectile
    :ensure t
    :defer t
    :init (helm-projectile-on)
    :config
    (setq helm-projectile-fuzzy-match t
          ;; other options: 'helm-projectile-find-file
          projectile-switch-project-action #'helm-projectile))

  ;; Group buffers by Projectile project
  (use-package ibuffer-projectile
    :ensure t
    :init
    (add-hook 'ibuffer-hook
              (lambda()
                (ibuffer-projectile-set-filter-groups)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-mode-name)
                  (ibuffer-do-sort-by-major-mode))))
    (setq ibuffer-show-empty-filter-groups nil))
  :diminish projectile-mode)

(defhydra hydra-projectile (:color blue)
  "projectile"
  ("h" helm-projectile "helm-projectile")
  ("p" helm-projectile-switch-project "switch project")
  ("f" helm-projectile-find-file-dwim "find file dwim")
  ("d" helm-projectile-find-dir "find dir")
  ("b" helm-projectile-switch-to-buffer "switch to another buffer in the project")
  ("a" helm-projectile-find-other-file "find other file")
  ("i" projectile-ibuffer "ibuffer")
  ("S" projectile-save-project-buffers "save project buffers")
  ("e" helm-projectile-recentf "recentf")
  ("r" projectile-replace "replace")
  ("K" projectile-kill-buffers "kill buffers")
  ("g" helm-projectile-grep "grep")
  ("j" ggtags-update-tags "ggtags update tags"))
(global-unset-key (kbd "C-c p"))
(bind-key "C-c p" 'hydra-projectile/body)


(provide 'projectile-init)

;;; projectile-init.el ends here
