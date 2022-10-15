;;; init-project.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/user-home-directory)
(defvar sb/minibuffer-completion)
(defvar sb/project-handler)

(declare-function consult--customize-put "consult")

;; Projectile is unable to remember remote projects which is also not supported by the current
;; version of `project'.

(use-package project
  :if (eq sb/project-handler 'project)
  :commands
  (project-switch-project project-current
                          project-find-file project-execute-extended-command
                          project-known-project-roots
                          project-remove-known-project
                          project-remember-project
                          project-kill-buffers
                          project-switch-to-buffer
                          project-search
                          project-compile)
  :bind-keymap
  ("C-x p" . project-prefix-map)
  :bind
  (("<f5>" . project-switch-project)
   ("<f6>" . project-find-file)
   :map project-prefix-map
   ("f"    . project-find-file)
   ("F"    . project-or-external-find-file)
   ("b"    . project-switch-to-buffer)
   ("d"    . project-dired)
   ("v"    . project-vc-dir)
   ("c"    . project-compile)
   ("k"    . project-kill-buffers)
   ("p"    . project-switch-project)
   ("g"    . project-find-regexp)
   ("r"    . project-query-replace-regexp)
   ("m"    . magit-project-status)
   ("C"    . recompile))
  :custom
  (project-switch-commands 'project-find-file)
  ;; :config
  ;; (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  )

(use-package consult-project-extra
  :if (and (eq sb/project-handler 'project)
           (eq sb/minibuffer-completion 'vertico))
  :after (consult project)
  :commands consult-project-extra-find-other-window
  :bind
  (:map project-prefix-map
        ("z" . consult-project-extra-find)))

;; Emacs 29 will possibly have "project.el" built-in. We cannot ignore `projectile' now because of
;; the dependencies by `ibuffer-projectile' and `centaur-tabs'.

(use-package projectile
  :preface
  (defun sb/projectile-dont-visit-tags-table ()
    "Do not visit the tags table."
    nil)
  :if (eq sb/project-handler 'projectile)
  :commands
  (projectile-project-p projectile-project-name
                        projectile-expand-root
                        projectile-project-root
                        projectile-mode
                        projectile-compile
                        projectile-compile-project)
  ;; We can open a project file without enabling projectile via bind-keys
  :hook (after-init-hook . projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (:map projectile-command-map
        ("A" . projectile-add-known-project))
  :custom
  (projectile-enable-caching nil "Caching will not watch for file system changes")
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-mode-line-prefix "" "Save modeline space")
  (projectile-require-project-root t "Use only in desired directories, too much noise otherwise")
  ;; The contents of ".projectile" are ignored when using the `alien' project indexing method
  (projectile-indexing-method 'alien)
  ;; No sorting should be faster, note that files are not sorted if `projectile-indexing-method' is
  ;; set to `alien'.
  (projectile-sort-order 'recently-active)
  (projectile-verbose nil)
  (projectile-project-root-files '("build.gradle"
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
  :config
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

  (advice-add 'projectile-visit-project-tags-table :override
              #'sb/projectile-dont-visit-tags-table)

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
             ".ppt" ".pptx" ".pt" ".pyc" ".rel" ".rip" ".rpm" ".so" "swp" ".xls" ".xlsx" "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes exts))

  ;; Set in case `counsel-projectile' is disabled. For `vertico', we use `consult-projectile'.
  (when (eq sb/minibuffer-completion 'ivy)
    (bind-key "<f5>" #'projectile-switch-project)
    (bind-key "<f6>" #'projectile-find-file)))

(use-package consult-projectile
  :if (and (eq sb/minibuffer-completion 'vertico)
           (eq sb/project-handler 'projectile))
  :commands consult-projectile-recentf
  :bind
  (("<f5>" . consult-projectile-switch-project)
   ("<f6>" . consult-projectile))
  :config
  (consult-customize consult-projectile :preview-key nil))

(provide 'init-project)

;;; init-project.el ends here
