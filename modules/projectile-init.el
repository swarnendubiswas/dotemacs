;;; projectile-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup projectile for improved handling of projects.

;;; Code:

(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t
        ;; use projectile in every directory without requiring a project file
        projectile-require-project-root nil
        projectile-switch-project-action 'projectile-dired)
  (add-to-list 'projectile-globally-ignored-directories ".svn")
  (dolist (item '("GTAGS" "GRTAGS" "GPATH" "TAGS" "GSYMS"))
    (add-to-list 'projectile-globally-ignored-files item))
  (projectile-global-mode 1)
  (use-package helm-projectile
    :ensure t
    :config
    (setq helm-projectile-fuzzy-match t
          projectile-completion-system 'helm
          ;; other options: 'helm-projectile-find-file
          projectile-switch-project-action 'helm-projectile)
    (helm-projectile-on))
  :diminish projectile-mode
  :bind ("C-c p h" . helm-projectile))

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
