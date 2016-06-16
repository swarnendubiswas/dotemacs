;;; projectile-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup projectile for improved handling of projects.

;;; Code:

(use-package projectile
  :ensure t
  :functions (projectile-find-file projectile-switch-project)
  :commands (projectile-switch-project)
  :init (setq projectile-known-projects-file (concat dotemacs-temp-directory "projectile-known-projects.eld")
              projectile-cache-file (concat dotemacs-temp-directory "projectile.cache"))
  (projectile-global-mode 1) ; Otherwise keybindings not bound explicitly with bind* will not be respected
  :config
  (setq projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil
        projectile-verbose nil
        projectile-require-project-root nil ; Use projectile in every directory without requiring a project file
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'projectile-dired)

  (cond ((eq dotemacs-selection 'helm) (setq projectile-completion-system 'helm))
        ((eq dotemacs-selection 'ido)  (setq projectile-completion-system 'ido))
        ((eq dotemacs-selection 'ivy)  (setq projectile-completion-system 'ivy)))

  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/")) ; Do not consider the home dir as a project
  (dolist (dirs '(".cache"
                  ".dropbox"
                  ".git"
                  ".hg"
                  ".svn"
                  "elpa"
                  "auto"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  (dolist (item '("GPATH"
                  "GRTAGS"
                  "GTAGS"
                  "GSYMS"
                  "TAGS"))
    (add-to-list 'projectile-globally-ignored-files item))

  (when (or (eq dotemacs-selection 'ido) (eq dotemacs-selection 'none))
    (bind-key "<f5>" #'projectile-switch-project)
    (bind-key "<f6>" #'projectile-find-file)
    (bind-key "<f7>" #'projectile-switch-to-buffer)
    ;; projectile-grep fails with fish shell
    (bind-key "<f8>" #'projectile-ag))

  :diminish projectile-mode)

(use-package helm-projectile
  :ensure t
  :disabled t
  :if (eq dotemacs-selection 'helm)
  :config
  (setq helm-projectile-fuzzy-match t
        projectile-switch-project-action #'helm-projectile-find-file-dwim)
  (helm-projectile-on)
  :bind (("<f5>" . helm-projectile-switch-project)
         ("<f6>" . helm-projectile-find-file)
         ("<f7>" . helm-projectile-switch-to-buffer)
         ;; helm grep is different from projectile-grep because the helm grep is incremental
         ("<f8>" . helm-projectile-grep)))

(use-package counsel-projectile
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :bind (("<f5>" . counsel-projectile)
         ("<f6>" . counsel-projectile-find-file)
         ("<f7>" . counsel-projectile-switch-to-buffer)
         ;; projectile-grep fails with fish shell
         ("<f8>" . projectile-ag)))

(provide 'projectile-init)

;;; projectile-init.el ends here
