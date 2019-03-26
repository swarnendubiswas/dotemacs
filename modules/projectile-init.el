;;; projectile-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup projectile for improved handling of projects.

;;; Code:

(defvar dotemacs-temp-directory)

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
        ;; projectile-mode-line nil
        projectile-completion-system 'ivy
        ;; The contents of .projectile are ignored when using the alien project indexing method
        projectile-indexing-method 'hybrid
        projectile-enable-idle-timer t ; Runs "regenerate ctags" by default
        projectile-idle-timer-seconds 120
        projectile-project-search-path '("/home/swarnendu/github/"
                                         "/home/swarnendu/bitbucket/"
                                         "/home/swarnendu/plass-workspace"
                                         "/home/swarnendu/iss-workspace"
                                         "/home/swarnendu/iitk-workspace"))

  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/")) ; Do not consider the home dir as a project

  (dolist (dirs '(".cache"
                  ".dropbox"
                  ".git"
                  ".hg"
                  ".svn"
                  ".nx"
                  "elpa"
                  "auto"
                  "__pycache__"
                  ".vscode"))
    (add-to-list 'projectile-globally-ignored-directories dirs))

  (dolist (item '("GPATH"
                  "GRTAGS"
                  "GTAGS"
                  "GSYMS"
                  "TAGS"
                  ".tags"
                  "__init__.py"))
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

  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (unless (bound-and-true-p treemacs-mode)
                (treemacs)
                (other-window 1))))

  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (bind-keys ("<f5>" . projectile-switch-project)
             ("<f6>" . projectile-find-file)
             ("<f7>" . projectile-ripgrep)))

(use-package counsel-projectile
  :ensure t
  :disabled t
  :after (counsel projectile)
  :hook (after-init . counsel-projectile-mode)
  :config
  ;; Sort projects from newest to oldest
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-projectile-switch-project . file-newer-than-file-p))
  ;; :bind (("<f5>" . counsel-projectile-switch-project)
  ;;        ("<f6>" . counsel-projectile)
  ;;        ("<f7>" . counsel-projectile-rg))

  (bind-keys ("<f5>" . counsel-projectile-switch-project)))

(provide 'projectile-init)

;;; projectile-init.el ends here
