;;; init-project.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/user-home-directory)
(defvar sb/minibuffer-completion)

(use-package ffap ; Find FILENAME, guessing a default from text around point
  :commands ffap)

;; TODO: Try https://github.com/redguardtoo/find-file-in-project

;; Projectile is unable to remember remote projects which is also not supported by the
;; current version of `project'. So then why not rely on `project'?

(use-package project
  :commands (project-switch-project project-current
                                    project-find-file project-execute-extended-command
                                    project-known-project-roots
                                    project-remove-known-project
                                    project-remember-project
                                    project-kill-buffers
                                    project-switch-to-buffer
                                    project-search
                                    project-compile)
  :bind
  (("<f5>" . project-switch-project)
   ("<f6>" . project-find-file)
   :map project-prefix-map
   ("f" . project-find-file)
   ("F" . project-or-external-find-file)
   ("b" . project-switch-to-buffer)
   ("d" . project-dired)
   ("v" . project-vc-dir)
   ("c" . project-compile)
   ("k" . project-kill-buffers)
   ("p" . project-switch-project)
   ("g" . project-find-regexp)
   ("r" . project-query-replace-regexp)))

(use-package consult-project-extra
  :if (eq sb/minibuffer-completion 'vertico)
  :after (consult project))

(use-package projectile
  :disabled t
  :commands (projectile-project-p projectile-project-name
                                  projectile-expand-root
                                  projectile-project-root
                                  projectile-mode
                                  projectile-compile
                                  projectile-compile-project)
  :preface
  (defun sb/close-treemacs-with-projectile (orig-fun &rest args)
    (let ((res (apply orig-fun args)))
      (treemacs)
      res))
  :config
  (setq projectile-enable-caching nil ; Caching will not watch for file system changes
        projectile-file-exists-remote-cache-expire nil
        projectile-mode-line-prefix "" ; Save modeline space
        ;; Use only in desired directories, too much noise otherwise
        projectile-require-project-root t
        ;; The contents of ".projectile" are ignored when using the `alien' project indexing method
        projectile-indexing-method 'alien
        ;; No sorting should be faster, note that files are not sorted if
        ;; `projectile-indexing-method' is set to `alien'.
        projectile-sort-order 'recently-active
        projectile-verbose nil)

  ;; https://github.com/MatthewZMD/.emacs.d
  (when (and (symbol-value 'sb/IS-WINDOWS)
             (executable-find "tr"))
    (setq projectile-indexing-method 'alien))

  ;; Disable computing the project type that is shown on the modeline
  (defun projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      ;; (format " [%s: %s]"
      ;;         projectile-mode-line-prefix
      ;;         (or project-name "-"))
      (format " [%s]" (or project-name "-"))))

  (setq projectile-project-root-files '("build.gradle"
                                        "setup.py"
                                        "requirements.txt"
                                        "package.json"
                                        "composer.json"
                                        "CMakeLists.txt"
                                        "Makefile"
                                        "WORKSPACE"
                                        "meson.build"
                                        "SConstruct"
                                        "configure.ac"
                                        "configure.in"))

  ;; Set search path for finding projects when `projectile-mode' is enabled, however auto-search for
  ;; projects is disabled for faster startup.
  (setq projectile-auto-discover nil
        projectile-project-search-path
        (list
         (concat `,(getenv "HOME") "/bitbucket")
         (expand-file-name "github"            sb/user-home-directory)
         (expand-file-name "iitk-workspace"    sb/user-home-directory)
         (expand-file-name "iitkgp-workspace"  sb/user-home-directory)
         (expand-file-name "iss-workspace"     sb/user-home-directory)
         (expand-file-name "plass-workspace"   sb/user-home-directory)
         (expand-file-name "prospar-workspace" sb/user-home-directory)
         ))

  (dolist (prjs (list
                 (expand-file-name sb/user-home-directory) ; Do not consider $HOME as a project
                 "~/" ; Do not consider $HOME as a project
                 (expand-file-name "/tmp")
                 ))
    (add-to-list 'projectile-ignored-projects prjs))

  ;; Filtering works with `alien' indexing
  (dolist (dirs
           '(".cache" ".clangd" ".dropbox" ".git" ".hg" ".metadata" ".nx" ".recommenders" ".svn"
             ".vscode" "__pycache__" "auto" "elpa" "node_modules"))
    (add-to-list 'projectile-globally-ignored-directories dirs))

  (dolist (items
           '("GPATH" "GRTAGS" "GTAGS" "GSYMS" "TAGS" "tags" ".tags" "__init__.py"))
    (add-to-list 'projectile-globally-ignored-files items))

  (dolist (exts
           '(".a" ".aux" ".bak" ".blg" ".class" ".deb" ".doc" ".docx" "egg-info" ".elc" ".o" ".odt"
             ".ppt" ".pptx" ".pt" ".pyc" ".rel" ".rip" ".rpm" ".so" "swp" ".xls" ".xlsx" "~$"
             ))
    (add-to-list 'projectile-globally-ignored-file-suffixes exts))

  (projectile-mode 1)

  ;; https://github.com/Alexander-Miller/treemacs/issues/660
  ;; TODO: These do not achieve what I want.

  ;; (add-hook 'projectile-after-switch-project-hook
  ;;           (lambda ()
  ;;             (treemacs-add-and-display-current-project)
  ;;             (treemacs-display-current-project-exclusively)
  ;;             (other-window 1)))

  ;; (add-hook 'projectile-after-switch-project-hook
  ;;           (lambda ()
  ;;             (treemacs)
  ;;             (treemacs-display-current-project-exclusively)
  ;;             (other-window 1)))

  ;; (advice-add 'projectile-kill-buffers :around #'sb/close-treemacs-with-projectile)

  ;; Set in case `counsel-projectile' is disabled. For `vertico', we use `consult-projectile'.
  (when (eq sb/minibuffer-completion 'ivy)
    (bind-key "<f5>" #'projectile-switch-project)
    (bind-key "<f6>" #'projectile-find-file))

  :bind-keymap ("C-c p" . projectile-command-map)
  ;; :init (run-with-idle-timer 2 nil #'projectile-mode)
  ;; We can open a project file without enabling projectile via bind-keys
  :hook (after-init-hook . projectile-mode)
  :bind
  (:map projectile-command-map
        ("A"    . projectile-add-known-project)))

;; `counsel-projectile' has poor performance with large projects, unlike `projectile'. Furthermore,
;; I am unsure how does this package advances `projectile' in terms of usability.
(use-package counsel-projectile
  :disabled t
  :if (eq sb/minibuffer-completion 'ivy)
  :defines counsel-projectile-default-file
  :commands (counsel-projectile-switch-project-by-name counsel-projectile-mode)
  :preface
  (defun sb/counsel-projectile-switch-project-magit (project)
    "Open Magit for the PROJECT."
    (let ((projectile-switch-project-action 'magit-status))
      (counsel-projectile-switch-project-by-name project)))

  ;; Set a default landing file: https://github.com/ericdanan/counsel-projectile/issues/172
  (defun sb/counsel-projectile-open-default-file ()
    "Open the current project's default file.
        This file is specified in `counsel-projectile-default-file'."
    (interactive)
    (let ((file counsel-projectile-default-file))
      (if (and file
               (setq file (projectile-expand-root file))
               (file-exists-p file))
          (find-file file)
        (message "File %s doesn't exist." file))))

  ;; Set `counsel-projectile-switch-project-action' to the following action
  (defun sb/counsel-projectile-switch-project-action-default-file (project)
    "Open PROJECT's default file.
        This file is specified in `counsel-projectile-default-file'."
    (let ((projectile-switch-project-action #'sb/counsel-projectile-open-default-file))
      (counsel-projectile-switch-project-by-name project)))
  :config
  ;; Setting these to `t' can be slow for large projects
  (setq counsel-projectile-remove-current-buffer t
        counsel-projectile-sort-directories nil
        counsel-projectile-find-file-more-chars 0
        counsel-projectile-sort-buffers nil
        counsel-projectile-sort-projects nil
        counsel-projectile-sort-files nil)

  (counsel-projectile-mode 1)

  ;; (counsel-projectile-modify-action
  ;;  'counsel-projectile-switch-project-action
  ;;  '((default sb/counsel-projectile-switch-project-action-default-file)))
  :bind
  ;; The `counsel' actions seem to be slower than base `projectile'
  (("<f5>" . counsel-projectile-switch-project)
   ("<f6>" . counsel-projectile-find-file)
   ;; ("<f7>" . counsel-projectile-rg)
   ;; ([remap projectile-switch-project]   . counsel-projectile-switch-project)
   ;; ([remap projectile-find-file]        . counsel-projectile-find-file)
   ;; ([remap projectile-find-dir]         . counsel-projectile-find-dir)
   ;; ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
   ))

(use-package consult-projectile
  :if (eq sb/minibuffer-completion 'vertico)
  :after projectile
  :commands consult-projectile-recentf
  :bind
  (("<f5>" . consult-projectile-switch-project)
   ("<f6>" . consult-projectile)))

;; Allows to quickly add projectile projects to the treemacs workspace
(use-package treemacs-projectile
  :after (treemacs projectile)
  :commands treemacs-projectile
  :demand t)

(provide 'init-project)

;;; init-project.el ends here
