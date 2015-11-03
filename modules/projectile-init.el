;;; projectile-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup projectile for improved handling of projects.

;;; Code:

(use-package projectile
  :ensure t
  ;; TODO: I read a post that says projectile-mode slows tramp. Currently I am no longer sorting buffers based on
  ;; projects, so I want to check whether tramp is helped by disabling this module.
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
  (if (bound-and-true-p dotemacs-use-helm-p)
      (setq projectile-completion-system 'helm)
    (setq projectile-completion-system 'ido))

  (dolist (dirs '(".svn" ".dropbox" ".git" ".hg" ".cache" "elpa"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/")) ; Don't consider my home dir as a project
  (dolist (item '("GTAGS" "GRTAGS" "GPATH" "TAGS" "GSYMS"))
    (add-to-list 'projectile-globally-ignored-files item))

  (use-package helm-projectile
    :ensure t
    :if (bound-and-true-p dotemacs-use-helm-p)
    :init
    (setq helm-projectile-fuzzy-match t
          projectile-switch-project-action #'helm-projectile-find-file-dwim)
    (helm-projectile-on))

  (defhydra hydra-projectile (:color teal)
    "projectile"
    ("h" helm-projectile "helm-projectile")
    ("p" helm-projectile-switch-project "switch project")
    ("f" helm-projectile-find-file-dwim "find file dwim")
    ("d" helm-projectile-find-dir "find dir")
    ("b" helm-projectile-switch-to-buffer "switch to another buffer in the project")
    ("a" helm-projectile-find-other-file "find other file")
    ("c" projectile-invalidate-cache "invalidate cache")
    ("i" projectile-ibuffer "ibuffer")
    ("S" projectile-save-project-buffers "save project buffers")
    ("l" projectile-replace "replace")
    ("r" helm-projectile-recentf "recentf")
    ("K" projectile-kill-buffers "kill buffers")
    ("g" helm-projectile-grep "grep")
    ("o" projectile-multi-occur "multi-occur"))
  ;; i want my hydra bindings over projectile default
  (bind-key* "C-c p" 'hydra-projectile/body)

  :diminish projectile-mode)

(provide 'projectile-init)

;;; projectile-init.el ends here
