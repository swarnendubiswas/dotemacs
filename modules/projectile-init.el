;;; projectile-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup projectile for improved handling of projects.

;;; Code:

(use-package projectile
  :ensure t
  :commands (projectile-find-file projectile-switch-project)
  :init
  (projectile-global-mode 1)
  (setq projectile-enable-caching t
        projectile-cache-file (concat dotemacs-temp-directory "projectile.cache")
        projectile-completion-system 'helm
        projectile-verbose nil
        projectile-require-project-root nil ; use projectile in every directory without requiring a project file
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'projectile-dired
        projectile-mode-line '(:propertize
                               (:eval (concat " " (projectile-project-name)))
                               face font-lock-constant-face))
  (dolist (dirs '(".svn" ".dropbox" ".git" ".hg" ".cache" "elpa"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/")) ; Don't consider my home dir as a project
  (dolist (item '("GTAGS" "GRTAGS" "GPATH" "TAGS" "GSYMS"))
    (add-to-list 'projectile-globally-ignored-files item))

  :config
  (use-package helm-projectile
    :ensure projectile
    :config
    (setq helm-projectile-fuzzy-match t
          projectile-switch-project-action #'helm-projectile-find-file)
    (helm-projectile-on))

  (use-package ibuffer-projectile ; group buffers by projectile project
    :ensure projectile
    :init
    (with-eval-after-load "ibuffer"
      (add-hook 'ibuffer-hook #'ibuffer-projectile-set-filter-groups)
      (setq ibuffer-show-empty-filter-groups nil)))

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
    ("g" helm-projectile-grep "grep"))
  (unbind-key "C-c p")
  (bind-key "C-c p" 'hydra-projectile/body)

  :diminish projectile-mode)

(provide 'projectile-init)

;;; projectile-init.el ends here
