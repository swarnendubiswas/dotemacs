;;; init-project.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
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
  (project-switch-project
    project-current
    project-find-file
    project-execute-extended-command
    project-known-project-roots
    project-remove-known-project
    project-forget-project
    project-remember-project
    project-kill-buffers
    project-switch-to-buffer
    project-search
    project-compile)
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind
  (("<f5>" . project-switch-project)
    ("<f6>" . project-find-file)
    :map
    project-prefix-map
    ("f" . project-find-file)
    ("F" . project-or-external-find-file)
    ("b" . project-switch-to-buffer)
    ("d" . project-dired)
    ("v" . project-vc-dir)
    ("c" . project-compile)
    ("k" . project-kill-buffers)
    ("p" . project-switch-project)
    ("g" . project-find-regexp)
    ("r" . project-query-replace-regexp)
    ("m" . magit-project-status)
    ("C" . recompile))
  ;; :config
  ;; (setq project-switch-commands (delete '(project-find-file "Find file") project-switch-commands))
  ;; (setq project-switch-commands (delete '(project-eshell "Eshell") project-switch-commands))
  ;; (setq project-switch-commands
  ;;   (delete '(project-find-regexp "Find regexp") project-switch-commands))
  ;; (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)
  ;; (add-to-list 'project-switch-commands '(project-compile "Compile") t)
  ;; (add-to-list 'project-switch-commands '(project-dired "Project Root") t)
  )

;; (use-package projectile
;;   :preface
;;   (defun sb/projectile-do-not-visit-tags-table ()
;;     "Do not visit the tags table automatically even if it is present."
;;     nil)
;;   :if (eq sb/project-handler 'projectile)
;;   :commands (projectile-mode projectile-compile-project)
;;   :hook
;;   ;; We can open a project file without enabling projectile via `bind-keys'
;;   (emacs-startup-hook . projectile-mode)
;;   :bind-keymap ("C-c p" . projectile-command-map)
;;   :bind
;;   (([project-switch-to-buffer] . projectile-switch-to-buffer)
;;     ([project-compile] . projectile-compile-project)
;;     ([project-find-dir] . projectile-find-dir)
;;     ([project-dired] . projectile-dired)
;;     ([project-find-file] . projectile-find-file)
;;     ([project-or-external-find-file] . projectile-find-other-file)
;;     ([project-kill-buffers] . projectile-kill-buffers)
;;     ([project-switch-project] . projectile-switch-project)
;;     ([project-vc-dir] . projectile-vc)
;;     ([project-forget-project] . projectile-remove-known-project)
;;     :map
;;     projectile-command-map
;;     ("A" . projectile-add-known-project))
;;   :custom
;;   (projectile-file-exists-remote-cache-expire nil)
;;   (projectile-mode-line-prefix "" "Save modeline space")
;;   (projectile-require-project-root t "Use only in desired directories, too much noise otherwise")
;;   ;; No sorting should be faster, note that files are not sorted if `projectile-indexing-method' is
;;   ;; set to `alien'.
;;   (projectile-sort-order 'recently-active)
;;   (projectile-verbose nil)
;;   (projectile-project-root-files
;;     '
;;     ("setup.py"
;;       "requirements.txt"
;;       "package.json"
;;       "CMakeLists.txt"
;;       "Makefile"
;;       "meson.build"
;;       "SConstruct"
;;       "configure.ac"
;;       "configure.in"))
;;   (projectile-auto-discover nil "Disable auto-search for projects for faster startup")
;;   :config
;;   ;; Set the indexing method after checks on Windows platform since otherwise the following error
;;   ;; shows up: "'tr' is not recognized as an internal or external command, operable program or batch
;;   ;; file."

;;   ;; The contents of ".projectile" are ignored when using the `alien' project indexing.
;;   (when (symbol-value 'sb/IS-LINUX)
;;     (setq projectile-indexing-method 'alien))

;;   ;; https://github.com/MatthewZMD/.emacs.d
;;   (when (and (symbol-value 'sb/IS-WINDOWS) (executable-find "tr"))
;;     (setq projectile-indexing-method 'alien))

;;   ;; Caching will not watch for file system changes
;;   (setq projectile-enable-caching (symbol-value 'sb/IS-WINDOWS))

;;   ;; Disable computing the project type that is shown on the modeline
;;   (defun projectile-default-mode-line ()
;;     "Report project name and type in the modeline."
;;     (let ((project-name (projectile-project-name)))
;;       (format " [%s]" (or project-name "-"))))

;;   (advice-add
;;     'projectile-visit-project-tags-table
;;     :override #'sb/projectile-do-not-visit-tags-table)

;;   (dolist
;;     (prjs
;;       (list
;;         (expand-file-name sb/user-home-directory) ; Do not consider $HOME as a project
;;         "~/" ; Do not consider $HOME as a project
;;         (expand-file-name "/tmp")))
;;     (add-to-list 'projectile-ignored-projects prjs))

;;   ;; Filtering works with `alien' indexing
;;   (dolist
;;     (dirs
;;       '
;;       (".cache"
;;         ".clangd"
;;         ".dropbox"
;;         ".git"
;;         ".hg"
;;         ".metadata"
;;         ".nx"
;;         ".recommenders"
;;         ".svn"
;;         ".vscode"
;;         "__pycache__"
;;         "auto"
;;         "elpa"
;;         "node_modules"))
;;     (add-to-list 'projectile-globally-ignored-directories dirs))

;;   (dolist (items '("GPATH" "GRTAGS" "GTAGS" "GSYMS" "TAGS" "tags" ".tags" "__init__.py"))
;;     (add-to-list 'projectile-globally-ignored-files items))

;;   (dolist
;;     (exts
;;       '
;;       (".a"
;;         ".aux"
;;         ".bak"
;;         ".blg"
;;         ".class"
;;         ".deb"
;;         ".doc"
;;         ".docx"
;;         "egg-info"
;;         ".elc"
;;         ".o"
;;         ".odt"
;;         ".ppt"
;;         ".pptx"
;;         ".pt"
;;         ".pyc"
;;         ".rel"
;;         ".rip"
;;         ".rpm"
;;         ".so"
;;         "swp"
;;         ".xls"
;;         ".xlsx"
;;         "~$"))
;;     (add-to-list 'projectile-globally-ignored-file-suffixes exts))

;;   (when (eq sb/minibuffer-completion 'ivy)
;;     (bind-key "<f5>" #'projectile-switch-project)
;;     (bind-key "<f6>" #'projectile-find-file)))

(provide 'init-project)

;;; init-project.el ends here
