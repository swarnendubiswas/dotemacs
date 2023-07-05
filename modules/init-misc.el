;;; init-misc.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar which-key-use-C-h-commands)
(defvar sb/user-home-directory)
(defvar sb/minibuffer-completion)
(defvar sb/project-handler)

(declare-function sb/smerge-hydra/body "init-keybindings")
(declare-function consult--customize-put "consult")

(use-package transient
  :commands transient-bind-q-to-quit
  :custom (transient-semantic-coloring t)
  :config
  ;; Allow using `q' to quit out of popups, in addition to `C-g'
  (transient-bind-q-to-quit))

;; The built-in `describe-function' includes both functions and macros. `helpful-function' is
;; functions only, so we use `helpful-callable' as a drop-in replacement.
(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable) ; "C-h f"
    ([remap describe-variable] . helpful-variable) ; "C-h v"
    ([remap describe-symbol] . helpful-symbol) ; "C-h o"
    ([remap describe-key] . helpful-key) ; "C-h k"
    ("C-h c" . helpful-command) ("C-h p" . helpful-at-point)
    :map helpful-mode-map ("q" . helpful-kill-buffers)))

;; Erase all consecutive white space characters in a given direction
(use-package hungry-delete
  :hook
  ((minibuffer-setup-hook . (lambda () (hungry-delete-mode -1)))
    (emacs-startup-hook . global-hungry-delete-mode))
  :diminish)

(use-package move-text ; Move lines with "M-<up>" and "M-<down>"
  :bind (("M-<down>" . move-text-down) ("M-<up>" . move-text-up)))

(use-package expand-region ; Expand region by semantic units
  :bind (("C-=" . er/expand-region) ("C-M-=" . er/contract-region)))

(use-package expand-line
  :bind ("M-i" . turn-on-expand-line-mode)
  :diminish)

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  :hook (emacs-startup-hook . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :commands (whole-line-or-region-local-mode)
  :hook (emacs-startup-hook . whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode)

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

;; The real beginning and end of buffers (i.e., `point-min' and `point-max') are accessible by
;; pressing the keys "M-<" and "M->" keys again.
(use-package beginend
  :hook (emacs-startup-hook . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))

(use-package vundo
  :straight (:host github :repo "casouri/vundo")
  :bind
  (([remap undo] . vundo)
    ("C-z" . vundo)
    :map vundo-mode-map ("C-a" . vundo-stem-root) ("C-e" . vundo-stem-end)
    ;; These are for horizontal movements.
    ("C-f" . vundo-forward) ("C-b" . vundo-backward)
    ;; These are for vertical movements.
    ("C-n" . vundo-next) ("C-p" . vundo-previous)))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :bind* ("C-." . iedit-mode))

(use-package hl-todo
  :hook (emacs-startup-hook . global-hl-todo-mode)
  :config
  (setq
    hl-todo-highlight-punctuation ":"
    hl-todo-keyword-faces
    (append
      '
      (("LATER" . "#d0bf8f")
        ("ISSUE" . "#ff8c00")
        ("DEBUG" . "#ff8c00")
        ("TEST" . "tomato")
        ("WARNING" . "#cc0000")
        ("BEWARE" . "#aa0000")
        ("REFACTOR" . "#cc9393"))
      hl-todo-keyword-faces)))

(use-package highlight-numbers
  :hook
  ((prog-mode-hook yaml-mode-hook conf-mode-hook css-mode-hook html-mode-hook)
    .
    highlight-numbers-mode))

(use-package page-break-lines ; Display ugly "^L" page breaks as tidy horizontal lines
  :commands page-break-lines-mode
  :hook (emacs-startup-hook . global-page-break-lines-mode)
  :diminish)

;; https://emacs.stackexchange.com/questions/19686/how-to-use-pdf-tools-pdf-view-mode-in-emacs
;; Use `isearch', `swiper' will not work

;; (use-package pdf-tools
;;   :if (display-graphic-p)
;;   :defines pdf-annot-activate-created-annotations
;;   :commands
;;   (pdf-tools-install
;;     pdf-loader-install
;;     pdf-view-mode
;;     pdf-annot-delete
;;     pdf-annot-add-highlight-markup-annotation
;;     pdf-annot-add-text-annotation)
;;   :mode ("\\.pdf\\'" . pdf-view-mode)
;;   ;; Register an autoloaded command for `pdf-view-mode', defer loading of `pdf-tools', and run
;;   ;; `pdf-view-mode' if the beginning of a buffer matches the string "%PDF".
;;   :magic ("%PDF" . pdf-view-mode)
;;   :bind
;;   (:map
;;     pdf-view-mode-map
;;     ("j" . pdf-view-next-line-or-next-page)
;;     ("k" . pdf-view-previous-line-or-previous-page)
;;     ("n" . pdf-view-next-page-command)
;;     ("p" . pdf-view-previous-page-command)
;;     ("a" . pdf-view-first-page)
;;     ("e" . pdf-view-last-page)
;;     ("l" . pdf-view-goto-page)
;;     ("P" . pdf-view-fit-page-to-window)
;;     ("W" . pdf-view-fit-width-to-window)
;;     ("H" . pdf-view-fit-height-to-window)
;;     ("+" . pdf-view-enlarge)
;;     ("-" . pdf-view-shrink)
;;     ("r" . pdf-view-revert-buffer)
;;     ("d" . pdf-annot-delete)
;;     ("h" . pdf-annot-add-highlight-markup-annotation)
;;     ("t" . pdf-annot-add-text-annotation)
;;     ("M" . pdf-view-midnight-minor-mode))
;;   :custom
;;   (pdf-annot-activate-created-annotations t "Automatically annotate highlights")
;;   (pdf-view-resize-factor 1.1 "Fine-grained zoom factor of 10%")
;;   :config
;;   (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install :no-query)'

;;   (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable

;;   ;; We do not enable `pdf-view-themed-minor-mode' since it can change plot colors
;;   (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes))

;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.

;; (use-package saveplace-pdf-view
;;   :after (pdf-tools saveplace)
;;   :demand t)

(use-package wc-mode
  :commands wc-mode)

;; Gets the definition of word or phrase at point from https://wordnik.com/
(use-package define-word
  :commands (define-word define-word-at-point))

;; (use-package esup
;;   :if (bound-and-true-p sb/debug-init-file)
;;   :commands (esup))

;; (use-package bug-hunter
;;   :commands (bug-hunter-init-file bug-hunter-file))

;; Save a bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark, use `bookmark-jump'
;; ("C-x r b") or `bookmark-bmenu-list' ("C-x r l"). Rename the bookmarked location in
;; `bookmark-bmenu-mode' with `R'.

(use-package bm
  :preface
  (defun sb/bm-setup ()
    "Wrapper function to help call with a timer."
    ;; `kill-buffer-hook' is not called when Emacs is killed
    (add-hook
      'kill-emacs-hook
      (lambda ()
        (bm-buffer-save-all)
        (bm-repository-save)))
    (add-hook 'after-save-hook #'bm-buffer-save)
    (add-hook 'kill-buffer-hook #'bm-buffer-save)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
    (add-hook 'after-revert-hook #'bm-buffer-restore)
    (add-hook 'find-file-hook #'bm-buffer-restore)
    (add-hook 'emacs-startup-hook #'bm-repository-load))
  :commands (bm-buffer-save-all bm-repository-save)
  :init
  ;; Must be set before `bm' is loaded
  (setq
    bm-restore-repository-on-load t
    bm-verbosity-level 1
    bm-modeline-display-total t)
  :hook
  (
    (kill-emacs-hook
      .
      (lambda ()
        (bm-buffer-save-all)
        (bm-repository-save)))
    (after-save-hook . bm-buffer-save)
    (kill-buffer-hook . bm-buffer-save)
    (vc-before-checkin-hook . bm-buffer-save)
    (after-revert-hook . bm-buffer-restore)
    (find-file-hook . bm-buffer-restore)
    (emacs-startup-hook . bm-repository-load))
  :bind (("C-<f1>" . bm-toggle) ("C-<f3>" . bm-next) ("C-<f2>" . bm-previous))
  :custom (bm-buffer-persistence t "Save bookmarks"))

(use-package crux
  :bind
  (("C-c d i" . crux-ispell-word-then-abbrev)
    ("<f12>" . crux-kill-other-buffers)
    ("C-c d s" . crux-sudo-edit))
  :bind* ("C-c C-d" . crux-duplicate-current-line-or-region))

(use-package rainbow-mode
  :hook ((css-mode-hook html-mode-hook web-mode-hook help-mode-hook) . rainbow-mode)
  :diminish)

(use-package volatile-highlights
  :hook (emacs-startup-hook . volatile-highlights-mode)
  :diminish volatile-highlights-mode)

(use-package unfill
  :commands (unfill-region unfill-paragraph unfill-toggle))

(use-package xclip
  :if (or (executable-find "xclip") (executable-find "xsel"))
  :hook (emacs-startup-hook . xclip-mode))

(use-package fix-word
  :bind (("M-u" . fix-word-upcase) ("M-l" . fix-word-downcase) ("M-c" . fix-word-capitalize)))

(use-package string-inflection
  :bind (:map prog-mode-map ("C-c C-u" . string-inflection-all-cycle)))

(use-package gcmh ; Allow GC to happen after a period of idle time
  :hook (emacs-startup-hook . gcmh-mode)
  :diminish)

(use-package kill-file-path
  :straight (:host github :repo "chyla/kill-file-path")
  :commands
  (kill-file-path-basename
    kill-file-path-basename-without-extension
    kill-file-path-dirname
    kill-file-path))

(use-package change-inner
  :commands (change-inner change-outer yank-inner yank-outer))

(use-package link-hint
  :bind ("C-c C-l" . link-hint-open-link))

;; This is independent of LSP support and is more flexible. On the other hand, `which-func-mode'
;; consumes less vertical space.

;; (use-package breadcrumb
;;   :straight (:host github :repo "joaotavora/breadcrumb")
;;   :hook (emacs-startup-hook . breadcrumb-mode))

;; Call `whitespace-cleanup' only if the initial buffer was clean. This mode works on the entire
;; file unlike `ws-butler'. To enable the mode for an entire project, set `whitespace-cleanup-mode'
;; to `t' in the `.dir-locals.el' file.
(use-package whitespace-cleanup-mode
  :defines whitespace-cleanup-mode-ignore-modes
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  :custom
  (whitespace-cleanup-mode-preserve-point t)
  (whitespace-cleanup-mode-only-if-initially-clean t)
  :config (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)
  :diminish)

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :hook (prog-mode-hook . ws-butler-mode)
  :diminish)

;; Both project.el and projectile are unable to remember remote projects.

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

;; The contents of ".projectile" are ignored when using the `alien' project indexing.
(use-package projectile
  :preface
  (defun sb/projectile-do-not-visit-tags-table ()
    "Do not visit the tags table automatically even if it is present."
    nil)
  :if (eq sb/project-handler 'projectile)
  ;; We can open a project file without enabling projectile via `bind-keys'
  :hook (emacs-startup-hook . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind
  (([remap project-switch-to-buffer] . projectile-switch-to-buffer)
    ([remap project-compile] . projectile-compile-project)
    ([remap project-find-dir] . projectile-find-dir)
    ([remap project-dired] . projectile-dired)
    ([remap project-find-file] . projectile-find-file)
    ([remap project-or-external-find-file] . projectile-find-other-file)
    ([remap project-kill-buffers] . projectile-kill-buffers)
    ([remap project-switch-project] . projectile-switch-project)
    ([remap project-vc-dir] . projectile-vc)
    ([remap project-forget-project] . projectile-remove-known-project)
    :map
    projectile-command-map
    ("A" . projectile-add-known-project))
  :custom
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-mode-line-prefix "" "Save modeline space")
  (projectile-require-project-root t "Use only in desired directories, too much noise otherwise")
  ;; No sorting should be faster. Files are not sorted if `projectile-indexing-method' is set to
  ;; `alien'.
  (projectile-sort-order 'recently-active)
  (projectile-verbose nil)
  (projectile-project-root-files
    '
    ("setup.py"
      "requirements.txt"
      "package.json"
      "CMakeLists.txt"
      "Makefile"
      "meson.build"
      "SConstruct"
      "configure.ac"
      "configure.in"))
  (projectile-auto-discover nil "Disable auto-search for projects for faster startup")
  ;; Caching will not watch for file system changes
  (projectile-enable-caching (symbol-value 'sb/IS-WINDOWS))
  :config
  ;; Set the indexing method after checks on Windows platform since otherwise the following error
  ;; shows up: "'tr' is not recognized as an internal or external command, operable program or batch
  ;; file."
  (when (and (symbol-value 'sb/IS-WINDOWS) (executable-find "tr"))
    (setq projectile-indexing-method 'alien))

  ;; Disable computing the project type that is shown on the modeline
  (defun projectile-default-mode-line ()
    "Report only the project name in the modeline."
    (let ((project-name (projectile-project-name)))
      (format " [%s]" (or project-name "-"))))

  (advice-add
    'projectile-visit-project-tags-table
    :override #'sb/projectile-do-not-visit-tags-table)

  (dolist
    (prjs
      (list
        (expand-file-name sb/user-home-directory) ; Do not consider $HOME as a project
        "~/" ; Do not consider $HOME as a project
        (expand-file-name "/tmp")))
    (add-to-list 'projectile-ignored-projects prjs))

  ;; Filtering works with `alien' indexing
  (dolist
    (dirs
      '
      (".dropbox"
        ".git"
        ".hg"
        ".metadata"
        ".nx"
        ".recommenders"
        ".svn"
        ".vscode"
        "__pycache__"
        "auto"
        "elpa"
        "node_modules"))
    (add-to-list 'projectile-globally-ignored-directories dirs))

  (dolist (items '("GPATH" "GRTAGS" "GTAGS" "GSYMS" "TAGS" "tags" ".tags" "__init__.py"))
    (add-to-list 'projectile-globally-ignored-files items))

  (dolist
    (exts
      '
      (".a"
        ".aux"
        ".bak"
        ".blg"
        ".class"
        ".deb"
        ".doc"
        ".docx"
        "egg-info"
        ".elc"
        ".o"
        ".odt"
        ".ppt"
        ".pptx"
        ".pt"
        ".pyc"
        ".rel"
        ".rip"
        ".rpm"
        ".so"
        "swp"
        ".xls"
        ".xlsx"
        "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes exts))

  (when (eq sb/minibuffer-completion 'ivy)
    (bind-key "<f5>" #'projectile-switch-project)
    (bind-key "<f6>" #'projectile-find-file)))

(use-package isearch
  :straight (:type built-in)
  :bind
  ;; Change the bindings for `isearch-forward-regexp' and `isearch-repeat-forward'
  (("C-s")
    ("C-M-f") ; Was bound to `isearch-forward-regexp', but we use it for `sp-forward-sexp'
    ("C-f" . isearch-forward-regexp) ("C-r" . isearch-backward-regexp)
    :map isearch-mode-map ("C-s") ("C-f" . isearch-repeat-forward) ("C-c C-o" . isearch-occur))
  :custom
  (search-highlight t "Highlight incremental search")
  (isearch-lazy-highlight t)
  (isearch-lazy-count t))

(use-package isearch-symbol-at-point ; Auto populate `isearch' with the symbol at point
  :after isearch
  :commands (isearch-forward-symbol-at-point isearch-backward-symbol-at-point)
  :bind (("M-s ." . isearch-symbol-at-point) ("M-s _" . isearch-forward-symbol)))

;; (use-package anzu
;;   :init
;;   (setq
;;     anzu-search-threshold 10000
;;     anzu-minimum-input-length 2)
;;   (global-anzu-mode 1)
;;   :bind ([remap query-replace-regexp] . anzu-query-replace-regexp)
;;   :diminish anzu-mode)

(with-eval-after-load "grep"
  (defvar grep-highlight-matches)
  (defvar grep-scroll-output)
  (defvar grep-find-ignored-directories)

  (setq
    grep-command "grep --color -irHn "
    grep-highlight-matches t
    grep-scroll-output t)

  (when (executable-find "rg")
    (setq grep-program "rg")
    (grep-apply-setting 'grep-find-command '("rg -n -H --no-heading -e" . 27)))

  (dolist (dirs '(".cache" "node_modules" "vendor" ".clangd"))
    (add-to-list 'grep-find-ignored-directories dirs)))

(when (executable-find "fd")
  (setq find-program "fd"))

;; When the "*grep*" buffer is huge, `wgrep-change-to-wgrep-mode' might freeze Emacs for several
;; minutes.
(use-package wgrep ; Writable grep
  ;; Allows you to edit a deadgrep buffer and apply those changes to the file buffer.
  :hook (deadgrep-finished-hook . wgrep-deadgrep-setup)
  :bind
  (:map
    grep-mode-map ; These keybindings are also defined in `wgrep-mode-map'
    ("C-x C-p" . wgrep-change-to-wgrep-mode)
    ("C-x C-s" . wgrep-finish-edit)
    ("C-x C-k" . wgrep-abort-changes)
    ("C-x C-q" . wgrep-exit))
  :custom (wgrep-auto-save-buffer t))

(use-package deadgrep
  :bind ("C-c s d" . deadgrep)
  :custom (deadgrep-max-buffers 1))

;; `avy-setup-default' will bind `avy-isearch' to "C-'" in `isearch-mode-map', so that you can
;; select one of the currently visible `isearch' candidates using `avy'.
(use-package avy
  :commands avy-setup-default
  :bind
  (("C-\\" . avy-goto-word-1)
    ("C-'" . avy-goto-char-timer) ("C-/" . avy-goto-line)
    :map isearch-mode-map
    ;; Use "C-'" in `isearch-mode-map' to use `avy-isearch' to select one of the currently visible
    ;; `isearch' candidates.
    ("C-'" . avy-isearch))
  :custom (avy-background t "Provides better contrast"))

(use-package re-builder
  :custom (reb-re-syntax 'string))

;; Package `visual-regexp' provides an alternate version of `query-replace' which highlights matches
;; and replacements as you type.
(use-package visual-regexp
  :bind
  ([remap query-replace] . vr/query-replace)
  ([remap replace-regex] . vr/replace))

(use-package ripgrep
  :commands (ripgrep-regexp projectile-ripgrep))

(use-package rg
  :commands
  (rg-menu
    rg-isearch-menu
    rg-project
    rg
    rg-literal
    rg-dwim
    rg-dwim-current-dir
    rg-dwim-project-dir))

(use-package vc-hooks
  :straight (:type built-in)
  :custom (vc-follow-symlinks t "No need to ask")
  ;; Disabling vc is said to improve performance. However, I find it useful to show branch
  ;; information on the modeline and highlight modifications in the current file.
  (vc-handled-backends '(Git)))

(use-package magit
  :bind (("C-x g" . magit-status) ("C-c M-g" . magit-file-dispatch) ("C-x M-g" . magit-dispatch))
  :custom
  ;; Open the status buffer in a full frame
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; Suppress the message "Turning on magit-auto-revert-mode" when loading Magit
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  ;; https://irreal.org/blog/?p=8877
  (magit-section-initial-visibility-alist
    '((stashes . show) (untracked . show) (unpushed . show) (unpulled . show)))
  :config
  (require 'magit-diff)
  (setq
    magit-diff-refine-hunk t ; Show fine differences for the current diff hunk only
    magit-diff-highlight-trailing nil))

(use-package git-modes
  :commands (gitignore-mode gitattributes-mode gitconfig-mode)
  :mode ("dotgitconfig" . gitconfig-mode))

;; Diff-hl looks nicer than git-gutter, and is based on `vc'
(use-package diff-hl
  :if (boundp 'vc-handled-backends)
  :hook
  (
    (diff-hl-mode-on-hook
      .
      (lambda ()
        (unless (display-graphic-p)
          (diff-hl-margin-local-mode 1))))
    (dired-mode-hook . diff-hl-dired-mode-unless-remote) (emacs-startup-hook . global-diff-hl-mode))
  :custom
  (diff-hl-draw-borders nil "Highlight without a border looks nicer")
  (diff-hl-disable-on-remote t)
  :config (diff-hl-flydiff-mode 1)

  ;; Display margin since the fringe is unavailable in TTY
  ;; (unless (display-graphic-p)
  ;;   (diff-hl-margin-mode 1))

  (with-eval-after-load "magit"
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)))

;; Use "M-p/n" to cycle between older commit messages.
(use-package git-commit
  :hook (git-commit-setup-hook . git-commit-turn-on-flyspell)
  :custom (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

;; Use the minor mode `smerge-mode' to move between conflicts and resolve them
(use-package smerge-mode
  :preface
  (defun sb/enable-smerge-maybe-with-vc ()
    "Enable `smerge-mode' automatically based on conflict markers."
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1)))))

  (defun sb/enable-smerge-maybe-without-vc ()
    "Enable `smerge-mode' automatically based on conflict markers."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  :straight (:type built-in)
  :commands
  (smerge-next
    smerge-prev
    smerge-auto-leave
    smerge-keep-base
    smerge-keep-upper
    smerge-keep-lower
    smerge-keep-all
    smerge-diff-base-lower
    smerge-diff-base-upper
    smerge-diff-upper-lower
    smerge-refine
    smerge-combine-with-next
    smerge-resolve)
  :init
  (add-hook 'find-file-hook #'sb/enable-smerge-maybe-with-vc :append)
  (add-hook 'after-revert-hook #'sb/enable-smerge-maybe-with-vc :append)
  :bind
  (:map
    smerge-mode-map
    ("M-g n" . smerge-next)
    ("M-g p" . smerge-prev)
    ("M-g c" . smerge-keep-current)
    ("M-g u" . smerge-keep-upper)
    ("M-g l" . smerge-keep-lower)
    ("M-g b" . smerge-keep-base)
    ("M-g a" . smerge-keep-all)
    ("M-g e" . smerge-ediff)
    ("M-g K" . smerge-kill-current)
    ("M-g m" . smerge-context-menu)
    ("M-g M" . smerge-popup-context-menu)))

(use-package with-editor :diminish)

(use-package paren
  :straight (:type built-in)
  :hook (emacs-startup-hook . show-paren-mode)
  :custom
  (show-paren-style 'parenthesis) ; `mixed' may lead to performance problems
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Enable autopairing

;; (use-package elec-pair
;;   :straight (:type built-in)
;;   :disabled t
;;   :hook (emacs-startup-hook . electric-pair-mode)
;;   :config
;;   ;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
;;   (defvar sb/markdown-pairs '((?` . ?`)) "Electric pairs for `markdown-mode'.")
;;   (defvar electric-pair-pairs)
;;   (defvar electric-pair-text-pairs)
;;   (defvar electric-pair-preserve-balance)

;;   (declare-function sb/add-markdown-pairs "init-parens")

;;   (defun sb/add-markdown-pairs ()
;;     "Add custom pairs to `markdown-mode'."
;;     (setq-local electric-pair-pairs (append electric-pair-pairs sb/markdown-pairs))
;;     (setq-local electric-pair-text-pairs electric-pair-pairs))

;;   (add-hook 'markdown-mode-hook #'sb/add-markdown-pairs)

;;   ;; Avoid balancing parentheses since they can be both irritating and slow
;;   (setq electric-pair-preserve-balance nil)

;;   ;; Disable pairs when entering minibuffer
;;   (add-hook 'minibuffer-setup-hook (lambda ()
;;                                      (electric-pair-mode -1)))
;;   ;; Re-enable pairs when existing minibuffer
;;   (add-hook 'minibuffer-exit-hook (lambda ()
;;                                     (electric-pair-mode 1))))

;; `sp-cheat-sheet' will show you all the commands available, with examples.
(use-package smartparens
  :commands
  (sp-local-pairs
    sp-raise-sexp
    sp-join-sexp
    sp-absorb-sexp
    sp-transpose-sexp
    sp-copy-sexp
    sp-backward-kill-sexp
    sp-kill-sexp
    sp-change-inner
    sp-change-enclosing
    sp-convolute-sexp
    sp-emit-sexp
    sp-backward-down-sexp
    sp-backward-up-sexp
    sp-backward-slurp-sexp
    sp-backward-barf-sexp
    sp-forward-barf-sexp
    sp-forward-slurp-sexp
    sp-rewrap-sexp
    sp-unwrap-sexp
    sp-backward-unwrap-sexp
    sp-wrap-round
    sp-wrap-curly
    sp-wrap-square
    sp-split-sexp)
  :hook
  (
    (emacs-startup-hook
      .
      (lambda ()
        (smartparens-global-mode 1)
        (show-smartparens-global-mode 1))))
  :bind
  (("C-M-a" . sp-beginning-of-sexp) ; "foo ba_r" -> "_foo bar"
    ("C-M-e" . sp-end-of-sexp) ; "f_oo bar" -> "foo bar_"
    ("C-M-u" . sp-up-sexp) ; "f_oo bar" -> "foo bar"_
    ("C-M-w" . sp-down-sexp) ; "foo ba_r" -> "_foo bar"
    ("C-M-f" . sp-forward-sexp) ; "foo ba_r" -> "foo bar"_
    ("C-M-b" . sp-backward-sexp) ; "foo ba_r" -> "_foo bar"
    ("C-M-n" . sp-next-sexp) ; ))" -> ((foo) (bar))"
    ("C-M-p" . sp-previous-sexp) ; "(foo (b|ar baz))" -> "(foo| (bar baz))"
    ("C-S-b" . sp-backward-symbol) ; "foo bar| baz" -> "foo |bar baz"
    ("C-S-f" . sp-forward-symbol) ; "|foo bar baz" -> "foo| bar baz"
    ("C-M-k" . sp-splice-sexp) ; "(foo bar)" -> "foo bar"
    ;; "foo(2,3)" -> "foo[2,3]"
    ("C-M-r" . sp-rewrap-sexp))
  :custom
  (sp-show-pair-from-inside nil "show-parens is faster")
  (sp-highlight-pair-overlay nil "show-parens is faster")
  (sp-highlight-wrap-overlay nil "show-parens is faster")
  (sp-highlight-wrap-tag-overlay nil)
  :config (require 'smartparens)
  ;; Introduces overhead to track parentheses pairs
  (smartparens-strict-mode -1)

  ;; Do not insert a parenthesis pair when the point is at the beginning of a word
  (sp-pair "(" nil :unless '(sp-point-before-word-p))
  (sp-pair "[" nil :unless '(sp-point-before-word-p))
  (sp-pair "{" nil :unless '(sp-point-before-word-p))
  ;; Do not pair quotes unless they are free
  (sp-pair "\"" nil :unless '(sp-point-before-word-p sp-point-after-word-p))

  (with-eval-after-load "cc-mode"
    (require 'smartparens-c)
    (sp-with-modes '(c-mode c++-mode) (sp-local-pair "/\*\*" "\*\*/")))

  (dolist (majmode '("html-mode" "nxml-mode" "web-mode"))
    (with-eval-after-load majmode
      (require 'smartparens-html)))

  (with-eval-after-load "latex-mode"
    (require 'smartparens-latex)
    ;; Do not insert a "$" pair when the point is at the beginning or the end of a word
    (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p sp-point-after-word-p)))

  (with-eval-after-load "markdown-mode"
    (require 'smartparens-markdown))

  (with-eval-after-load "python-mode"
    (require 'smartparens-python))

  (with-eval-after-load "text-mode"
    (require 'smartparens-text))

  (sp-with-modes 'java-mode (sp-local-pair "/\*\*" "\*\*/") (sp-local-pair "/\*" "\*/"))

  (with-eval-after-load "org"
    (require 'smartparens-org)
    (sp-with-modes
      'org-mode
      (sp-local-pair
        "*"
        "*"
        :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-at-bol-p)
        :skip-match 'sp--org-skip-asterisk)
      (sp-local-pair "_" "_" :unless '(sp-point-before-word-p sp-point-after-word-p))
      (sp-local-pair
        "/"
        "/"
        :unless '(sp-point-before-word-p sp-point-after-word-p sp-org-point-after-left-square-bracket-p)
        :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair
        "~"
        "~"
        :unless '(sp-point-after-word-p sp-point-before-word-p)
        :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair
        "="
        "="
        :unless '(sp-point-after-word-p sp-point-before-word-p)
        :post-handlers '(("[d1]" "SPC")))
      (sp-local-pair "«" "»")))
  :diminish)

(provide 'init-misc)

;;; init-misc.el ends here
