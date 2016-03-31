;;; projectile-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup projectile for improved handling of projects.

;;; Code:

(use-package projectile
  :ensure t
  :functions (projectile-find-file projectile-switch-project)
  :commands (projectile-switch-project)
  :init (setq projectile-known-projects-file (concat dotemacs-temp-directory "projectile-bookmarks.eld")
              projectile-cache-file (concat dotemacs-temp-directory "projectile.cache"))
  (projectile-global-mode 1) ; Otherwise keybindings not bound explicitly with bind* will not be respected
  :config
  (setq projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil
        projectile-verbose nil
        projectile-require-project-root nil ; Use projectile in every directory without requiring a project file
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'projectile-dired
        projectile-mode-line '(:propertize
                               (:eval (concat " " (projectile-project-name)))
                               face font-lock-constant-face))

  (cond ((eq dotemacs-selection 'helm) (setq projectile-completion-system 'helm))
        ((eq dotemacs-selection 'ido)  (setq projectile-completion-system 'ido))
        ((eq dotemacs-selection 'ivy)  (setq projectile-completion-system 'ivy)))

  (dolist (dirs '(".svn" ".dropbox" ".git" ".hg" ".cache" "elpa" "auto"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/")) ; Do not consider the home dir as a project
  (dolist (item '("GTAGS" "GRTAGS" "GPATH" "TAGS" "GSYMS"))
    (add-to-list 'projectile-globally-ignored-files item))

  ;; https://github.com/bbatsov/helm-projectile
  ;; helm grep is different from projectile-grep because the helm grep is incremental
  (use-package helm-projectile
    :ensure t
    :config
    (setq helm-projectile-fuzzy-match t
          projectile-switch-project-action #'helm-projectile-find-file-dwim)
    (helm-projectile-on))

  :bind (("<f5>" . helm-projectile-switch-project)
         ("<f6>" . helm-projectile-find-file)
         ("<f7>" . helm-projectile-switch-to-buffer)
         ("<f8>" . helm-projectile-grep)
         ("C-c r" . helm-resume)
         :map helm-projectile-find-file-map
         ("<tab>" . helm-execute-persistent-action) ; Do not rebind <tab> globally
         ("C-z" . helm-select-action)
         :map helm-projectile-projects-map
         ("<tab>" . helm-execute-persistent-action) ; Do not rebind <tab> globally
         ("C-z" . helm-select-action))
  :diminish projectile-mode)

(provide 'projectile-init)

;;; projectile-init.el ends here
