;;; projectile-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup projectile for improved handling of projects.

;;; Code:

(defvar dotemacs-temp-directory)
(defvar dotemacs-selection)

(use-package projectile
  :ensure t
  :init
  (setq projectile-known-projects-file (concat dotemacs-temp-directory "projectile-known-projects.eld")
        projectile-cache-file (concat dotemacs-temp-directory "projectile.cache"))
  (projectile-mode 1) ; Otherwise keybindings not bound explicitly with bind* will not be respected
  :config
  (setq projectile-enable-caching t
        projectile-file-exists-remote-cache-expire nil
        projectile-verbose nil
        projectile-require-project-root t ; Use projectile only in desired directories, too much noise otherwise
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action 'projectile-find-file ; Use projectile-dired to view in dired
        projectile-mode-line nil)

  (cond ((eq dotemacs-selection 'ido)  (setq projectile-completion-system 'ido))
        ((eq dotemacs-selection 'ivy)  (setq projectile-completion-system 'ivy)))

  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/")) ; Do not consider the home dir as a project
  (dolist (dirs '(".cache"
                  ".dropbox"
                  ".git"
                  ".hg"
                  ".svn"
                  ".nx"
                  "elpa"
                  "auto"
                  "__pycache__"))
    (add-to-list 'projectile-globally-ignored-directories dirs))
  (dolist (item '("GPATH"
                  "GRTAGS"
                  "GTAGS"
                  "GSYMS"
                  "TAGS"))
    (add-to-list 'projectile-globally-ignored-files item))
  (dolist (list '("\\.out$"
                  "\\.pdf$"
                  "\\.pyc$"
                  "\\.rel$"
                  "\\.rip$"
                  "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes list))

  ;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  (when (or (eq dotemacs-selection 'ido) (eq dotemacs-selection 'none))
    (bind-key "<f5>" #'projectile-switch-project)
    (bind-key "<f6>" #'projectile-find-file)
    (bind-key "<f7>" #'projectile-switch-to-buffer)
    (bind-key "<f8>" #'projectile-ag)))

(use-package counsel-projectile
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :after (counsel projectile)
  :config (counsel-projectile-mode)
  :bind (("<f5>" . counsel-projectile-switch-project)
         ("<f6>" . counsel-projectile-find-file)
         ("<f7>" . counsel-projectile-switch-to-buffer)
         ("<f8>" . counsel-projectile-rg)))

(use-package projectile-ripgrep
  :ensure t)

(provide 'projectile-init)

;;; projectile-init.el ends here
