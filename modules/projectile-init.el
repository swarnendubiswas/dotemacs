;;; projectile-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup projectile for improved handling of projects.

;;; Code:

(use-package projectile
  :ensure t
  :disabled t
  :functions (projectile-find-file projectile-switch-project)
  :config
  (projectile-global-mode 1)

  (setq projectile-enable-caching t
        projectile-cache-file (concat dotemacs-temp-directory "projectile.cache")
        projectile-verbose nil
        projectile-require-project-root nil ; use projectile in every directory without requiring a project file
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'projectile-dired
        projectile-mode-line '(:propertize
                               (:eval (concat " " (projectile-project-name)))
                               face font-lock-constant-face)
        projectile-file-exists-remote-cache-expire nil
        projectile-known-projects-file (concat dotemacs-temp-directory "projectile-bookmarks.eld"))

  (cond ((eq dotemacs-selection 'helm) (setq projectile-completion-system 'helm))
        ((eq dotemacs-selection 'ido)  (setq projectile-completion-system 'ido))
        ((eq dotemacs-selection 'ivy)  (setq projectile-completion-system 'ivy)))

  (dolist (dirs '(".svn" ".dropbox" ".git" ".hg" ".cache" "elpa"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/")) ; Don't consider my home dir as a project
  (dolist (item '("GTAGS" "GRTAGS" "GPATH" "TAGS" "GSYMS"))
    (add-to-list 'projectile-globally-ignored-files item))

  (use-package helm-projectile
    :ensure t
    :if (eq dotemacs-selection 'helm)
    :init
    (setq helm-projectile-fuzzy-match t
          projectile-switch-project-action #'helm-projectile-find-file-dwim)
    (helm-projectile-on))

  :diminish projectile-mode)

(provide 'projectile-init)

;;; projectile-init.el ends here
