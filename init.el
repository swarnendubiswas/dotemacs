;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding: utf-8; no-byte-compile: t; fill-column: 80 -*-

;; Swarnendu Biswas

;;; Commentary: My configuration is mostly targeted toward GNU Linux.

;;; Code:

(defgroup sb/emacs nil
  "Personal configuration for GNU Emacs."
  :group 'local)

(defcustom sb/extras-directory (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type 'string
  :group 'sb/emacs)

(defcustom sb/debug-init-perf t
  "Enable features to debug errors and performance bottlenecks."
  :type 'boolean
  :group 'sb/emacs)

(defcustom sb/theme 'modus-vivendi
  "Specify which Emacs theme to use."
  :type
  '(radio
    (const :tag "doom-nord" doom-nord)
    (const :tag "modus-vivendi" modus-vivendi)
    (const :tag "catppuccin" catppuccin)
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme 'powerline
  "Specify the mode-line theme to use."
  :type
  '(radio
    (const :tag "powerline" powerline)
    (const :tag "doom-modeline" doom-modeline)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Corfu integrates nicely with `orderless' and provides better completion for
;; Elisp symbols with `cape-symbol'. But `corfu-terminal-mode' has a rendering
;; problem for completion popups appearing near the right edges with terminal
;; Emacs. The completion entries wrap around sometimes, and messes up the
;; completion. Company works better with both Windows and TUI Emacs, and has
;; more extensive LaTeX support than Corfu. We can set up separate completion
;; files with `company-ispell' and `company-dict'. `company-anywhere' allows
;; completion from inside a word/symbol. However, `company-ispell' does not keep
;; prefix case when used as a grouped backend.
(defcustom sb/in-buffer-completion
  (if (display-graphic-p)
      'corfu
    'company)
  "Choose the framework to use for completion at point."
  :type
  '(radio
    (const :tag "corfu" corfu)
    (const :tag "company" company)
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/enable-icons t
  "Should icons be enabled?
The provider is nerd-icons."
  :type 'boolean
  :group 'sb/emacs)

(defconst sb/user-home-directory (getenv "HOME")
  "User HOME directory.")

;; "straight.el" makes it easy to install packages from arbitrary sources like
;; GitHub.
(setq
 straight-build-dir
 (format "build/%d%s%d"
         emacs-major-version
         version-separator
         emacs-minor-version)
 ;; Do not check packages on startup to reduce load time
 straight-check-for-modifications '(check-on-save find-when-checking)
 straight-use-package-by-default t
 ;; There is no need to download the whole Git history, and a single branch
 ;; often suffices.
 straight-vc-git-default-clone-depth '(1 single-branch))

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir) user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; These variables need to be set before loading `use-package'.
(setq use-package-enable-imenu-support t)
(straight-use-package '(use-package))

(if (bound-and-true-p sb/debug-init-perf)
    (setq
     ;; Use "M-x use-package-report" to see results
     use-package-compute-statistics t
     use-package-verbose t
     use-package-minimum-reported-time 0 ; Show everything
     use-package-always-demand t)
  (setq
   use-package-always-defer t
   use-package-expand-minimally t
   use-package-minimum-reported-time 0 ; Show everything
   use-package-compute-statistics nil))

;; Check "use-package-keywords.org" for a suggested order of `use-package'
;; keywords.

(use-package bind-key
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish
  :demand t)

(use-package no-littering
  :demand t
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (custom-file (no-littering-expand-var-file-name "custom.el")))

(use-package exec-path-from-shell
  :when (eq system-type 'gnu/linux)
  :init
  (setq
   exec-path-from-shell-check-startup-files nil
   exec-path-from-shell-variables
   '("PATH"
     "COLORTERM"
     "VISUAL"
     "EDITOR"
     "ALTERNATE_EDITOR"
     "JAVA_HOME"
     "TERM"
     "PYTHONPATH"
     "LANG"
     "LC_ALL"
     "LSP_USE_PLISTS")
   exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package emacs
  :hook ((emacs-startup . garbage-collect) (emacs-startup . save-place-mode))
  :custom
  (ad-redefinition-action 'accept "Turn off warnings due to redefinitions")
  (auto-save-no-message t "Do not print frequent autosave messages")
  ;; Disable autosaving based on number of characters typed
  (auto-save-interval 0)
  ;; Save bookmark after every bookmark edit and also when Emacs is killed
  (bookmark-save-flag 1)
  ;; Autofill comments in modes that define them
  (comment-auto-fill-only-comments t)
  (create-lockfiles nil)
  (custom-safe-themes t)
  (delete-by-moving-to-trash t)
  (help-window-select t "Makes it easy to close the window")
  (history-delete-duplicates t)
  (read-process-output-max (* 4 1024 1024))
  (remote-file-name-inhibit-locks t)
  (ring-bell-function 'ignore "Disable beeping sound")
  (save-interprogram-paste-before-kill t)
  (select-enable-clipboard t)
  (sentence-end-double-space nil)
  (shift-select-mode nil)
  (sort-fold-case nil "Do not ignore case when sorting")
  (standard-indent 2)
  (switch-to-buffer-preserve-window-point t)
  (use-dialog-box nil "Do not use dialog boxes with mouse commands")
  (use-file-dialog nil)
  (view-read-only t "View mode for read-only buffers")
  (visible-bell nil)
  (warning-minimum-level :error)
  (window-combination-resize t "Resize windows proportionally")
  (x-gtk-use-system-tooltips nil "Do not use system tooltips")
  ;; Always trigger an immediate resize of the child frame
  (x-gtk-resize-child-frames 'resize-mode)
  (x-underline-at-descent-line t)
  ;; Ignore case when reading a buffer name
  (read-buffer-completion-ignore-case t)
  (kill-do-not-save-duplicates t "Do not save duplicates to kill ring")
  (tags-add-tables nil)
  (tags-case-fold-search nil "case-sensitive")
  (tags-revert-without-query t)
  ;; Disable the warning "X and Y are the same file" in case of symlinks
  (find-file-suppress-same-file-warnings t)
  (auto-mode-case-fold nil "Avoid a second pass through `auto-mode-alist'")
  (backup-inhibited t "Disable backup for a per-file basis")
  (confirm-nonexistent-file-or-buffer t)
  (confirm-kill-emacs nil)
  ;; Prevent 'Active processes exist' when you quit Emacs
  (confirm-kill-processes nil)
  (make-backup-files nil "Stop making backup `~' files")
  (require-final-newline t "Always end a file with a newline")
  ;; Unlike `auto-save-mode', `auto-save-visited-mode' saves the buffer contents
  ;; to the visiting file and runs all save-related hooks. We disable
  ;; `auto-save-mode' and prefer `auto-save-visited-mode' instead.
  (auto-save-default nil)
  ;; Save buffer to file after idling for some time, the default of 5s may be
  ;; too frequent since it runs all the save-related hooks.
  (auto-save-visited-interval 30)
  (revert-without-query '("\\.*") "Revert all files without asking")
  (max-mini-window-height 0.4)
  (bidi-inhibit-bpa nil) ; Disabling BPA makes redisplay faster
  ;; *scratch* is in `lisp-interaction-mode' by default.
  (initial-major-mode 'text-mode)
  :config
  (dolist (exts
           '(".directory"
             ".dll"
             ".exe"
             ".fdb_latexmk"
             ".fls"
             ".lof"
             ".nav"
             ".pyc"
             ".rel"
             ".rip"
             ".snm"
             ".synctex.gz"
             ".toc"
             ".vrb"
             "TAGS"
             "indent.log"))
    (add-to-list 'completion-ignored-extensions exts))

  (when (boundp next-error-message-highlight)
    (setq next-error-message-highlight t))
  (when (boundp read-minibuffer-restore-windows)
    (setq read-minibuffer-restore-windows t))
  (when (boundp use-short-answers)
    (setq use-short-answers t))
  ;; Hide commands in "M-x" which do not work in the current mode.
  (when (boundp read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))
  (when (boundp help-window-keep-selected)
    (setq help-window-keep-selected t))
  (when (boundp find-sibling-rules)
    (setq find-sibling-rules
          '(("\\([^/]+\\)\\.c\\'" "\\1.h")
            ("\\([^/]+\\)\\.cpp\\'" "\\1.h")
            ("\\([^/]+\\)\\.h\\'" "\\1.c")
            ("\\([^/]+\\)\\.hpp\\'" "\\1.cpp"))))
  (when (eq system-type 'windows-nt)
    (setq w32-get-true-file-attributes nil))

  (tooltip-mode -1)
  (auto-encryption-mode -1)

  (dolist
      (mode
       '(
         ;; Auto-save file-visiting buffers at idle time intervals
         auto-save-visited-mode
         column-number-mode size-indication-mode
         ;; Typing with the mark active will overwrite the marked region
         delete-selection-mode
         ;; Use soft wraps, wrap lines without the ugly continuation marks
         global-visual-line-mode
         ;; When you call `find-file', you do not need to clear the existing
         ;; file path before adding the new one. Just start typing the whole
         ;; path and Emacs will "shadow" the current one. For example, you are
         ;; at "~/Documents/notes/file.txt" and you want to go to
         ;; "~/.emacs.d/init.el", type the latter directly and Emacs will take
         ;; you there.
         file-name-shadow-mode))
    (when (fboundp mode)
      (funcall mode 1)))

  ;; Changing buffer-local variables will only affect a single buffer.
  ;; `setq-default' changes the buffer-local variable's default value.
  (setq-default
   fill-column 80
   cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
   indent-tabs-mode nil ; Spaces instead of tabs
   tab-width 4
   ;; TAB first tries to indent the current line, and if the line was already
   ;; indented, then try to complete the thing at point.
   tab-always-indent 'complete
   bidi-display-reordering 'left-to-right
   bidi-paragraph-direction 'left-to-right)

  ;; Not a library/file, so `eval-after-load' does not work
  (diminish 'auto-fill-function)

  (advice-add 'risky-local-variable-p :override #'ignore)

  ;; Hide "When done with a buffer, type C-x 5" message
  (when (bound-and-true-p server-client-instructions)
    (setq server-client-instructions nil))

  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))

  ;; Mark safe variables
  (put 'compilation-read-command 'safe-local-variable #'stringp)
  (put 'reftex-default-bibliography 'safe-local-variable #'stringp)
  :diminish visual-line-mode)

;; `pixel-scroll-mode' uses line-by-line scrolling.
;; (use-package pixel-scroll
;;   :custom
;;   (pixel-scroll-precision-use-momentum nil)
;;   (pixel-scroll-precision-interpolate-page t)
;;   (scroll-preserve-screen-position t)
;;   (scroll-margin 3)
;;   (scroll-step 1)
;;   (scroll-conservatively 10000)
;;   (scroll-error-top-bottom t)
;;   (auto-window-vscroll nil)
;;   (fast-but-imprecise-scrolling t)
;;   (hscroll-margin 2)
;;   (hscroll-step 1)
;;   (mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
;;   (mouse-wheel-scroll-amount-horizontal 1)
;;   :config
;;   (when (fboundp 'pixel-scroll-mode)
;;     (pixel-scroll-mode 1))
;;   (when (fboundp 'pixel-scroll-precision-mode)
;;     (pixel-scroll-precision-mode 1)))

(use-package autorevert
  :straight (:type built-in)
  :hook (emacs-startup . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  ;; Revert `dired' buffers if the directory contents change
  (global-auto-revert-non-file-buffers t)
  :diminish auto-revert-mode)

(use-package savehist
  :straight (:type built-in)
  :hook (emacs-startup . savehist-mode)
  :custom
  (savehist-additional-variables
   '(savehist-minibuffer-history-variables
     bookmark-history
     command-history
     compile-command
     compile-history
     extended-command-history
     file-name-history
     kill-ring
     mark-ring
     minibuffer-history
     search-ring
     regexp-search-ring)))

(use-package abbrev
  :straight (:type built-in)
  :hook (emacs-startup . abbrev-mode)
  :custom
  (abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory))
  (save-abbrevs 'silently)
  :diminish)

;; This puts the buffer in read-only mode and disables font locking, revert with
;; "C-c C-c".
(use-package so-long
  :straight (:type built-in)
  :when (featurep 'so-long)
  :hook (emacs-startup . global-so-long-mode))

(use-package imenu
  :straight (:type built-in)
  :after
  (:any
   makefile-mode
   markdown-mode
   org-mode
   yaml-mode
   yaml-ts-mode
   prog-mode
   latex-mode
   LaTeX-mode)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items 1000)
  (imenu-use-popup-menu nil))

(use-package recentf
  :straight (:type built-in)
  :hook (emacs-startup . recentf-mode)
  :bind ("<f9>" . recentf-open-files)
  :custom
  (recentf-auto-cleanup 30 "Cleanup after idling for 30s")
  (recentf-exclude
   '("[/\\]elpa/"
     "[/\\]\\.git/"
     ".*\\.gz\\'"
     ".*\\.xz\\'"
     ".*\\.zip\\'"
     ".*-autoloads.el\\'"
     "[/\\]archive-contents\\'"
     "[/\\]\\.loaddefs\\.el\\'"
     "[/\\]tmp/.*"
     ".*/recentf\\'"
     ".*/recentf-save.el\\'"
     "~$"
     ".*/TAGS\\'"
     "*.cache"
     "*[/\\]straight/repos/"))
  ;; Keep remote file without testing if they still exist
  (recentf-keep '(file-remote-p file-readable-p))
  ;; Larger values help in lookup but takes more time to check if the files
  ;; exist
  (recentf-max-saved-items 250)
  :config
  ;; Abbreviate the home directory to make it easy to read the actual file name.
  (unless (> emacs-major-version 27)
    (setq recentf-filename-handlers '(abbreviate-file-name)))

  (dolist (exclude
           `(,(recentf-expand-file-name no-littering-etc-directory)
             ,(recentf-expand-file-name no-littering-var-directory)
             ,(recentf-expand-file-name (straight--emacs-dir "straight"))))
    (add-to-list 'recentf-exclude exclude))

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent
  ;; list periodically after idling for a few seconds.
  (run-with-idle-timer 30 t #'recentf-save-list))

(progn
  (defun sb/inhibit-message-call-orig-fun (orig-fun &rest args)
    "Hide messages appearing in ORIG-FUN, forward ARGS."
    (let ((inhibit-message t))
      (apply orig-fun args)))

  ;; Hide the "Wrote to recentf" message
  (advice-add 'recentf-save-list :around #'sb/inhibit-message-call-orig-fun)
  ;; Hide the "Cleaning up the recentf list...done" message
  (advice-add 'recentf-cleanup :around #'sb/inhibit-message-call-orig-fun)
  ;; Hide the "Wrote ..." message
  (advice-add 'write-region :around #'sb/inhibit-message-call-orig-fun))

(progn
  (defun sb/auto-save-wrapper (save-fn &rest args)
    "Hide 'Auto-saving...done' messages by calling the method.
  SAVE-FN with non-nil ARGS."
    (ignore args)
    (apply save-fn '(t)))

  (advice-add 'do-auto-save :around #'sb/auto-save-wrapper))

;; Use "Shift + direction" arrows for moving around windows.
(use-package windmove
  :straight (:type built-in)
  :when (display-graphic-p)
  :init (windmove-default-keybindings))

(use-package doc-view
  :straight (:type built-in)
  :hook
  (doc-view-mode
   .
   (lambda ()
     (when (and buffer-file-name (string-suffix-p ".pdf" buffer-file-name))
       (auto-revert-mode 1))))
  :bind
  (:map
   doc-view-mode-map
   ("=" . doc-view-enlarge)
   ("-" . doc-view-shrink)
   ("n" . doc-view-next-page)
   ("p" . doc-view-previous-page)
   ("0" . doc-view-scale-reset)
   ("M-<" . doc-view-first-page)
   ("M->" . doc-view-last-page)
   ("C-l" . doc-view-goto-page))
  :custom
  (doc-view-continuous t)
  (doc-view-resolution 120))

(use-package ffap
  :straight (:type built-in)
  :bind ("<f2>" . ffap)
  :custom
  ;; Do not ping things that look like domain names
  (ffap-machine-p-known 'reject))

;; Highlight and open http links in strings and comments in buffers.
(use-package goto-addr
  :straight (:type built-in)
  :hook ((prog-mode . goto-address-prog-mode) (text-mode . goto-address-mode))
  :bind
  (("C-c C-o" . goto-address-at-point)
   :map
   goto-address-highlight-keymap
   ("M-RET" . goto-address-at-point)))

(use-package winner
  :hook (emacs-startup . winner-mode)
  :bind (("C-c <left>" . winner-undo) ("C-c <right>" . winner-redo)))

;; Use `ediff-regions-wordwise' for small regions and `ediff-regions-linewise'
;; for larger regions.
(use-package ediff
  :straight (:type built-in)
  :commands (ediff-buffers ediff-regions-linewise ediff-regions-wordwise)
  :hook (ediff-cleanup . (lambda () (ediff-janitor nil nil)))
  :custom
  ;; Put the control panel in the same frame as the diff windows
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split diffs side by side
  (ediff-split-window-function #'split-window-horizontally)
  ;; Prompt and kill file variants on quitting an Ediff session
  (ediff-keep-variants nil)
  :config
  (ediff-set-diff-options 'ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

;; To edit remote files, use "/method:user@host#port:filename".
;; The shortcut "/ssh::" will connect to default "user@host#port".
;; To edit a local file with sudo, use "C-x C-f /sudo::/etc/hosts".
;; To open a remote file with ssh + sudo, use "C-x C-f /ssh:host|sudo:root:/etc/passwd".
;; Multihop syntax: "C-x C-f /ssh:bird@bastion|ssh:you@remotehost:/path"
;; Multihop with sudo: "C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file"
;; Multihop with sudo with custom user: "C-x C-f /ssh:you@remotehost|sudo:them@remotehost:/path/to/file"
;; Sudo over ssh: "emacs -nw /ssh:user@172.16.42.1\|sudo:172.16.42.1:/etc/hosts"
;; Connect as non-root user and then use sudo: "C-x C-f /ssh:192.168.249.10|su::/some/file"
(use-package tramp
  :preface
  (defun sb/cleanup-tramp ()
    (interactive)
    (progn
      (tramp-cleanup-all-buffers)
      (tramp-cleanup-all-connections)))
  :straight (:type built-in)
  :bind ("C-M-g" . sb/cleanup-tramp)
  :custom
  ;; Remote files are not updated outside of Tramp
  (remote-file-name-inhibit-cache nil)
  (tramp-verbose 1 "Only errors and warnings")
  :config
  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  ;; Include this directory in $PATH on remote
  (add-to-list
   'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setenv "SHELL" shell-file-name) ; Recommended to connect with Bash
  (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))

(use-package whitespace
  :straight (:type built-in)
  :custom (whitespace-line-column fill-column)
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode))

(use-package ibuffer
  :straight (:type built-in)
  :hook (ibuffer . ibuffer-auto-mode)
  :bind
  (("C-x C-b" . ibuffer-jump)
   :map
   ibuffer-mode-map
   ("`" . ibuffer-switch-format))
  :custom
  (ibuffer-display-summary nil)
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-formats
   '((mark modified read-only " " (name 24 24 :left :elide) " " filename)))
  (ibuffer-never-show-predicates
   '("*Help\\*"
     "*Quick Help\\*"
     "*Warnings\\*"
     "*Calc Trail\\*"
     "*Compile-Log\\*"
     "*Async-native-compile-log\\*"
     "*Native-compile-log\\*"
     "*Calculator\\*"
     "*Calendar\\*"
     "*Warning\\*"
     "magit:.*"
     "*Org Help\\*"
     "*lsp-log*"
     "*bash-ls.*"
     "*marksman.*"
     "*yaml-ls.*"
     "*clangd.*"
     "*texlab2.*"))
  :config
  (require 'ibuf-ext)
  (defalias 'list-buffers 'ibuffer))

;; By default buffers are grouped by `project-current' or by
;; `default-directory'.
(use-package ibuffer-project
  :hook
  (ibuffer
   .
   (lambda ()
     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative))))
  :custom (ibuffer-project-use-cache t)
  :config
  ;; Remote buffers will be grouped by protocol and host
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote")))

(use-package immortal-scratch
  :hook (emacs-startup . immortal-scratch-mode))

(use-package persistent-scratch
  :hook
  (emacs-startup
   .
   (lambda ()
     (ignore-errors
       (persistent-scratch-setup-default))))
  :config
  (advice-add
   'persistent-scratch-setup-default
   :around #'sb/inhibit-message-call-orig-fun))

(use-package window
  :straight (:type built-in)
  :custom
  (display-buffer-alist
   '(
     ;; Allow *Help* buffers to use the full frame
     ("*Help*" (display-buffer-same-window))
     ("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
      (display-buffer-no-window)
      (allow-no-window . t)))))

;; Show temporary buffers as a popup window, and close them with "C-g"
(use-package popwin
  :hook (emacs-startup . popwin-mode)
  :config
  (push '(helpful-mode :noselect t :position bottom :height 0.5)
        popwin:special-display-config))

;; Jump to visible text using a char-based decision tree
(use-package avy
  :bind
  (("C-\\" . avy-goto-word-1)
   ("C-'" . avy-goto-char-timer)
   ("C-/" . avy-goto-line)
   ("C-M-c" . avy-copy-line)
   ("C-M-m" . avy-move-line)
   :map isearch-mode-map
   ;; Use "C-'" in `isearch-mode-map' to use `avy-isearch' to select one of the
   ;; many currently visible `isearch' candidates.
   ("C-'" . avy-isearch)))

;; Quickly select a window to jump to
(use-package ace-window
  :bind (([remap other-window] . ace-window) ("M-o" . ace-window))
  :custom (aw-minibuffer-flag t)
  :config (ace-window-display-mode 1))

;; Jump around buffers in few keystrokes
(use-package frog-jump-buffer
  :bind ("C-b" . frog-jump-buffer)
  :config
  (dolist (regexp
           '("TAGS"
             "^\\*Compile-log"
             "^\\:"
             "errors\\*$"
             "^\\*Backtrace"
             "-ls\\*$"
             "stderr\\*$"
             "^\\*Flymake"
             "^\\*vc"
             "^\\*Warnings"
             "^\\*eldoc"))
    (push regexp frog-jump-buffer-ignore-buffers)))

(use-package dired
  :preface
  (defun sb/dired-go-home ()
    (interactive)
    (dired sb/user-home-directory))

  (defun sb/dired-jump-to-top ()
    (interactive)
    (goto-char (point-min)) ; Faster than `(beginning-of-buffer)'
    (dired-next-line 1))

  (defun sb/dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max)) ; Faster than `(end-of-buffer)'
    (dired-next-line -1))
  :straight (:type built-in)
  :commands dired
  :hook
  ((dired-mode . auto-revert-mode) ; Auto refresh `dired' when files change
   (dired-mode . dired-hide-details-mode))
  :bind
  (:map
   dired-mode-map
   ("M-<home>" . sb/dired-go-home)
   ("M-<up>" . sb/dired-jump-to-top)
   ("M-<down>" . sb/dired-jump-to-bottom)
   ("i" . find-file)
   ("_" . dired-create-empty-file))
  :custom
  ;; When there are two `dired' buffer windows in the same frame, Emacs will
  ;; select the other buffer as the target directory (e.g., for copying or
  ;; renaming files).
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  ;; "A" is to avoid listing "." and "..", "B" is to avoid listing backup
  ;; entries ending with "~", "F" appends indicator to entries, "g" omits the
  ;; owner, "h" is to print human-readable sizes, "N" prints entry names without
  ;; quoting, "si" is to use powers of 1000 not 1024, "o" does not print group
  ;; information, "p" is to append "/" indicator to directories, "v" uses
  ;; natural sort of (version) numbers within text. Check "ls" for additional
  ;; options.
  (dired-listing-switches
   "-aBFghlNopv --group-directories-first --time-style=locale")
  (dired-ls-F-marks-symlinks t "-F marks links with @")
  (dired-recursive-copies 'always "Single prompt for all n directories")
  (dired-recursive-deletes 'always "Single prompt for all n directories")
  ;; Do not ask whether to kill buffers visiting deleted files
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-hide-details-hide-symlink-targets nil)
  :config
  (when (boundp 'dired-kill-when-opening-new-dired-buffer)
    (setq dired-kill-when-opening-new-dired-buffer t)))

(use-package dired-x
  :straight (:type built-in)
  :hook
  (dired-mode
   .
   (lambda ()
     (require 'dired-x)
     (dired-omit-mode)))
  :bind ("C-x C-j" . dired-jump)
  :custom (dired-omit-verbose nil "Do not show messages when omitting files")
  ;; Do not ask whether to kill buffers visiting deleted files
  (dired-clean-confirm-killing-deleted-buffers nil)
  :config
  ;; Obsolete from Emacs 28+
  (unless (> emacs-major-version 27)
    (setq dired-bind-jump t))

  (setq dired-omit-files
        (concat
         dired-omit-files
         "\\|^\\..*$" ; Hide all dotfiles
         "\\|^.DS_Store\\'"
         "\\|^.project\\(?:ile\\)?\\'"
         "\\|^.\\(svn\\|git\\)\\'"
         "\\|^.cache\\'"
         "\\|^.ccls-cache\\'"
         "\\|^__pycache__\\'"
         "\\|^eln-cache\\'"
         "\\|\\(?:\\.js\\)?\\.meta\\'"
         "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))

  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Remove 'Omit' from the modeline."
    (diminish 'dired-omit-mode)
    dired-mode-map))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(use-package dired-hist
  :straight (:host github :repo "karthink/dired-hist")
  :hook (dired-mode . dired-hist-mode)
  :bind
  (:map
   dired-mode-map ("l" . dired-hist-go-back) ("r" . dired-hist-go-forward)))

;; In Emacs Lisp mode, `xref-find-definitions' will by default find only
;; functions and variables from Lisp packages which are loaded into the current
;; Emacs session or are auto-loaded.
(use-package xref
  :bind
  (("M-." . xref-find-definitions)
   ("M-?" . xref-find-references)
   ;; Find all identifiers whose name matches pattern
   ("C-M-." . xref-find-apropos) ("M-," . xref-go-back))
  :custom (xref-search-program 'ripgrep))

(use-package project
  :bind
  (("<f5>" . project-switch-project)
   ("<f6>" . project-find-file)
   :map
   project-prefix-map
   ("f" . project-or-external-find-file)
   ("b" . project-switch-to-buffer)
   ("d" . project-dired)
   ("v" . project-vc-dir)
   ("c" . project-compile)
   ("k" . project-kill-buffers)
   ("g" . project-find-regexp)
   ("r" . project-query-replace-regexp))
  :custom
  ;; Start `project-find-file' by default
  (project-switch-commands 'project-find-file))

;; Allows identifying custom projects with a ".project" file
(use-package project-x
  :straight (:host github :repo "karthink/project-x")
  :after project
  :demand t
  :config (add-hook 'project-find-functions 'project-x-try-local 90))

(use-package vertico
  :straight
  (vertico
   :files (:defaults "extensions/*")
   :includes (vertico-directory vertico-repeat vertico-quick))
  :hook
  ((emacs-startup . vertico-mode)
   ;; Tidy shadowed file names. That is, when using a command for selecting a
   ;; file in the minibuffer, the following fixes the path so the selected path
   ;; does not have prepended junk left behind. This works with
   ;; `file-name-shadow-mode' enabled. When you are in a sub-directory and use,
   ;; say, `find-file' to go to your home '~/' or root '/' directory, Vertico
   ;; will clear the old path to keep only your current input.
   (rfn-eshadow-update-overlay . vertico-directory-tidy)
   (minibuffer-setup . vertico-repeat-save))
  :bind
  (("C-c r" . vertico-repeat)
   ("M-r" . vertico-repeat-select)
   :map
   vertico-map
   ("M-<" . vertico-first)
   ("M->" . vertico-last)
   ("C-M-j" . vertico-exit-input)
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)
   ("C-c q" . vertico-quick-insert)
   ("C-'" . vertico-quick-jump))
  :custom (vertico-cycle t)
  :config
  (when (eq sb/theme 'catppuccin)
    (with-eval-after-load 'vertico
      (set-face-attribute 'vertico-current nil
                          :background "#676767"
                          :foreground "#FFFFFF")))
  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history)))

(use-package consult
  :after vertico
  :commands consult-fd
  :bind
  ( ;; Press "SPC" to show ephemeral buffers, "b SPC" to filter by buffers, "f
   ;; SPC" to filter by files, "p SPC" to filter by projects. If you press "DEL"
   ;; afterwards, the full candidate list will be shown again.
   ([remap switch-to-buffer] . consult-buffer)
   ("<f3>" . consult-buffer)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-from-kill-ring)
   ([remap goto-line] . consult-goto-line)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap bookmark-set] . consult-bookmark)
   ([remap list-bookmarks] . consult-bookmark)
   ([remap bookmark-bmenu-list] . consult-bookmark)
   ("M-g o" . consult-outline)
   ("C-c C-m" . consult-mark)
   ([remap imenu] . consult-imenu) ; "M-g i"
   ("C-c C-j" . consult-imenu)
   ([remap customize] . consult-customize)
   ([remap load-theme] . consult-theme)
   ("C-c s f" . consult-find)
   ([remap locate] . consult-locate)
   ("C-c s l" . consult-locate)
   ;; Prefix argument "C-u" allows to specify the directory. You can pass
   ;; additional grep flags to `consult-grep' with the "--" separator. E.g.:
   ;; "foo bar -- -A3" to get matches with 3 lines of 'after' context.
   ([remap rgrep] . consult-grep)
   ("C-c s g" . consult-grep)
   ([remap vc-git-grep] . consult-git-grep)
   ("C-c s G" . consult-git-grep)
   ;; Filter by file extension with `consult-ripgrep' "... -- -g *.jsx"
   ("C-c s r" . consult-ripgrep)
   ("C-c s h" . consult-isearch-history)
   ("<f4>" . consult-line)
   ([remap multi-occur] . consult-multi-occur)
   ("M-s m" . consult-multi-occur)
   ([remap recentf-open-files] . consult-recent-file)
   ("M-g r" . consult-register)
   :map
   isearch-mode-map
   ("M-s e" . consult-isearch-history))
  :custom
  (consult-line-start-from-top t "Start search from the beginning")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  ;; Disable preview by default, enable for selected commands
  (consult-preview-key nil)
  (completion-in-region-function #'consult-completion-in-region "Complete M-:")
  ;; Having multiple other sources like `recentf' makes it difficult to identify
  ;; and switch quickly between only buffers, especially while wrapping around.
  ;; (consult-buffer-sources '(consult--source-buffer))
  (consult-narrow-key "<")
  (consult-widen-key ">")
  (consult-buffer-filter
   '("^ "
     "\\` "
     "^:"
     "\\*Echo Area"
     "\\*Minibuf"
     "\\*Backtrace"
     "\\*Warning"
     "Flymake log"
     "\\*Flycheck"
     "Shell command output"
     "direnv"
     "\\*magit-"
     "magit-process"
     ".+-shell*"
     "\\*straight-"
     "\\*Compile-Log"
     "\\*Native-*"
     "\\*Async-"
     "\\*format-all-error"
     "COMMIT_EDITMSG"
     "TAGS"
     "\\*lsp-*"
     "\\*EGLOT"
     "\\*pylsp"
     "\\*vc"
     "\\*citre-ctags*"
     "\\*ltex-ls"
     "\\*texlab"
     "\\*tramp"
     "\\*bash-ls*"
     "\\*json-ls*"))
  :config
  (consult-customize
   consult-line
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-recent-file
   consult-bookmark
   consult-xref
   consult-yank-from-kill-ring
   :preview-key
   '(:debounce 1.5 any)
   consult-theme
   consult-buffer
   :preview-key
   "M-."
   consult-find
   :sort
   t
   consult-line
   consult-ripgrep
   consult-grep
   ;; Initialize search string with the highlighted region
   :initial
   (when (use-region-p)
     (buffer-substring-no-properties (region-beginning) (region-end))))

  (with-eval-after-load "tex-mode"
    (bind-key "C-c C-j" #'consult-outline tex-mode-map)))

;; Easily add file and directory paths into the minibuffer.
(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map
   vertico-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file))
  :config (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

(use-package embark
  :bind
  ( ;; "C-h b" lists all the bindings available in a buffer
   ([remap describe-bindings] . embark-bindings)
   ("C-`" . embark-act)
   ("C-;" . embark-dwim)
   :map
   minibuffer-local-map
   ("C-`" . embark-act)
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export)
   :map
   minibuffer-local-completion-map
   ("C-`" . embark-act))
  :custom
  ;; Replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command))

;; Supports exporting search results to a `grep-mode' buffer, on which you can
;; use `wgrep'.
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Rich annotations in the minibuffer, e.g., documentation strings or file
;; information.
(use-package marginalia
  :after vertico
  :init (marginalia-mode 1)
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :config
  (setq marginalia-annotator-registry
        (assq-delete-all 'file marginalia-annotator-registry))
  (add-to-list
   'marginalia-annotator-registry '(symbol-help marginalia-annotate-variable))
  (add-to-list
   'marginalia-annotator-registry
   '(project-buffer marginalia-annotate-project-buffer)))

;; Use `consult' to select Tramp targets. Supported completion sources are ssh
;; config, known hosts, and docker containers.
(use-package consult-tramp
  :straight (:host github :repo "Ladicle/consult-tramp")
  :bind ("C-c d t" . consult-tramp))

(use-package consult-flycheck
  :after flycheck
  :demand t
  :bind (:map flycheck-command-map ("!" . consult-flycheck)))

(use-package ispell
  :straight (:type built-in)
  :bind ("M-$" . ispell-word)
  :custom
  (ispell-dictionary "en_US")
  (ispell-local-dictionary "en_US")
  (ispell-personal-dictionary (expand-file-name "spell" sb/extras-directory))
  (ispell-alternate-dictionary
   (expand-file-name "wordlist.5" sb/extras-directory))
  ;; Save a new word to personal dictionary without asking
  (ispell-silently-savep t)
  :config
  (cond
   ((executable-find "hunspell")
    (progn
      (setenv "LANG" "en_US")
      (setenv "DICTIONARY" "en_US")
      (setenv "DICPATH" `,(concat user-emacs-directory "hunspell"))
      (setq
       ispell-program-name "hunspell"
       ispell-local-dictionary-alist
       '(("en_US"
          "[[:alpha:]]"
          "[^[:alpha:]]"
          "[']"
          nil
          ("-d" "en_US")
          nil
          utf-8))
       ispell-hunspell-dictionary-alist ispell-local-dictionary-alist
       ispell-hunspell-dict-paths-alist `(("en_US" ,(concat user-emacs-directory "hunspell/en_US.aff"))))))
   ((executable-find "aspell")
    (progn
      (setq
       ispell-program-name "aspell"
       ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90")))))

  ;; Skip regions in `org-mode'
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list
   'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE" . "^#\\+END_EXAMPLE"))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("\:PROPERTIES\:$" . "\:END\:$"))
  ;; Footnotes in org that have http links that are line breaked should not be
  ;; ispelled
  (add-to-list 'ispell-skip-region-alist '("^http" . "\\]"))
  (add-to-list 'ispell-skip-region-alist '("`" "`"))
  (add-to-list 'ispell-skip-region-alist '("cite:" . "[[:space:]]"))
  (add-to-list 'ispell-skip-region-alist '("label:" . "[[:space:]]"))
  (add-to-list 'ispell-skip-region-alist '("ref:" . "[[:space:]]"))
  (add-to-list
   'ispell-skip-region-alist '("\\\\begin{multline}" . "\\\\end{multline}"))
  (add-to-list
   'ispell-skip-region-alist '("\\\\begin{equation}" . "\\\\end{equation}"))
  (add-to-list
   'ispell-skip-region-alist '("\\\\begin{align}" . "\\\\end{align}"))

  ;; Hide the "Starting new Ispell process" message
  (advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)
  (advice-add 'ispell-lookup-words :around #'sb/inhibit-message-call-orig-fun))

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; "M-$" triggers correction for the misspelled word before point, "C-u M-$"
;; triggers correction for the entire buffer, "C-u C-u M-$" forces correction of
;; the word at point, even if it is not misspelled.
(use-package jinx
  :when (and (eq system-type 'gnu/linux) (executable-find "enchant-2"))
  :hook ((text-mode conf-mode prog-mode) . jinx-mode)
  :bind (([remap ispell-word] . jinx-correct) ("C-M-$" . jinx-languages))
  :custom (jinx-languages "en_US")
  :diminish)

(use-package helpful
  :bind
  (([remap describe-variable] . helpful-variable) ; "C-h v"
   ;; The built-in `describe-function' includes both functions and macros.
   ;; `helpful-function' is only for functions, so we use `helpful-callable' as
   ;; a replacement.
   ([remap describe-function] . helpful-callable) ; "C-h f"
   ([remap describe-symbol] . helpful-symbol) ; "C-h o"
   ([remap describe-key] . helpful-key) ; "C-h k"
   ("C-h c" . helpful-command) ("C-h p" . helpful-at-point)
   :map helpful-mode-map ("q" . helpful-kill-buffers)))

(use-package hungry-delete
  :hook
  ((minibuffer-setup . (lambda () (hungry-delete-mode -1)))
   (emacs-startup . global-hungry-delete-mode))
  :diminish)

(use-package move-text
  :bind (("M-<down>" . move-text-down) ("M-<up>" . move-text-up)))

(use-package expand-region
  :bind (("C-=" . er/expand-region) ("C-M-=" . er/contract-region)))

(use-package change-inner
  :commands (change-inner change-outer))

(use-package expand-line
  :bind ("M-i" . turn-on-expand-line-mode)
  :diminish)

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  :hook (emacs-startup . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :hook (emacs-startup . whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode)

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

(use-package vundo
  :bind
  (([remap undo] . vundo)
   ("C-z" . vundo)
   :map vundo-mode-map ("C-a" . vundo-stem-root) ("C-e" . vundo-stem-end)
   ;; These are for horizontal movements.
   ("C-f" . vundo-forward) ("C-b" . vundo-backward)
   ;; These are for vertical movements.
   ("C-n" . vundo-next) ("C-p" . vundo-previous))
  :custom
  ;; Use pretty Unicode glyphs to draw the tree
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package iedit
  :bind* ("C-." . iedit-mode))

(use-package hl-todo
  :hook (emacs-startup . global-hl-todo-mode)
  :bind (("C-c p" . hl-todo-previous) ("C-c n" . hl-todo-next))
  :config
  (setq
   hl-todo-highlight-punctuation ":"
   hl-todo-keyword-faces
   (append
    '(("LATER" . "#d0bf8f")
      ("IMP" . "#7cb8bb")
      ("ISSUE" . "#ff8c00")
      ("DEBUG" . "#ff8c00")
      ("TEST" . "tomato")
      ("WARNING" . "#cc0000")
      ("REFACTOR" . "#cc9393"))
    hl-todo-keyword-faces)))

;; Save a bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark,
;; use `bookmark-jump' ("C-x r b") or `bookmark-bmenu-list' ("C-x r l"). Rename
;; the bookmarked location in `bookmark-bmenu-mode' with `R'.
(use-package bm
  :init
  (setq
   bm-restore-repository-on-load t
   bm-verbosity-level 0)
  :hook
  ((kill-emacs
    .
    (lambda ()
      (bm-buffer-save-all)
      (bm-repository-save)))
   (after-save . bm-buffer-save)
   (kill-buffer . bm-buffer-save)
   (vc-before-checkin . bm-buffer-save)
   (after-revert . bm-buffer-restore)
   (find-file . bm-buffer-restore)
   (emacs-startup . bm-repository-load))
  :bind (("C-<f1>" . bm-toggle) ("C-<f3>" . bm-next) ("C-<f2>" . bm-previous))
  :custom (bm-buffer-persistence t "Save bookmarks"))

(use-package crux
  :commands crux-kill-other-buffers
  :bind (("C-c d s" . crux-sudo-edit) ("C-<f9>" . crux-recentf-find-directory))
  :bind* ("C-c C-d" . crux-duplicate-current-line-or-region))

(use-package rainbow-mode
  :hook
  ((LaTeX-mode
    latex-mode
    css-mode
    css-ts-mode
    html-mode
    html-ts-mode
    web-mode
    help-mode
    helpful-mode)
   . rainbow-mode)
  :diminish)

(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :diminish)

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :diminish)

(use-package isearch
  :straight (:type built-in)
  :bind
  (("C-s")
   ("C-M-f")
   ("C-f" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regexp)
   :map
   isearch-mode-map
   ("C-s")
   ("C-f" . isearch-repeat-forward)
   ("C-c C-o" . isearch-occur))
  :custom (isearch-lazy-count t "Show match count"))

;; Auto populate `isearch' with the symbol at point
(use-package isearch-symbol-at-point
  :commands (isearch-forward-symbol-at-point isearch-backward-symbol-at-point)
  :bind (("M-s ." . isearch-symbol-at-point) ("M-s _" . isearch-forward-symbol)))

(with-eval-after-load "grep"
  (setq
   grep-command "grep --color -irHn "
   grep-highlight-matches t
   grep-scroll-output t)

  (when (executable-find "rg")
    (setq grep-program "rg")
    (grep-apply-setting 'grep-find-command '("rg -n -H --no-heading -e" . 27)))

  (dolist (dirs '(".cache" "node_modules" "vendor" ".clangd"))
    (add-to-list 'grep-find-ignored-directories dirs)))

;; `consult-rg' provides live search, while `deadgrep' provides a resulting
;; search buffer.
(use-package deadgrep
  :bind ("C-c s d" . deadgrep)
  :custom (deadgrep-max-buffers 1))

(use-package wgrep
  :bind
  (:map
   grep-mode-map ; These keybindings are also defined in `wgrep-mode-map'
   ("C-x C-p" . wgrep-change-to-wgrep-mode)
   ("C-x C-s" . wgrep-finish-edit)
   ("C-x C-k" . wgrep-abort-changes)
   ("C-x C-q" . wgrep-exit))
  :custom (wgrep-auto-save-buffer t)
  :config
  (with-eval-after-load "deadgrep"
    (bind-key "e" #'wgrep-change-to-wgrep-mode deadgrep-mode-map)))

(use-package wgrep-deadgrep
  :hook (deadgrep-finished . wgrep-deadgrep-setup))

(use-package re-builder
  :commands re-builder
  :custom (reb-re-syntax 'string))

;; Package `visual-regexp' provides an alternate version of `query-replace'
;; which highlights matches and replacements as you type.

;; (use-package visual-regexp
;;   :bind
;;   (([remap query-replace] . vr/query-replace)
;;    ([remap replace-regex] . vr/replace)))

(use-package visual-replace
  :bind
  (([remap query-replace] . visual-replace)
   ([remap replace-string] . visual-replace)
   ([remap isearch-query-replace] . visual-replace-from-isearch)
   ([remap isearch-query-replace-regexp] . visual-replace-from-isearch)))

(use-package vc-hooks
  :straight (:type built-in)
  :custom (vc-handled-backends '(Git))
  ;; Disable version control for remote files to improve performance
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))

(use-package transient
  :custom (transient-semantic-coloring t)
  :config (transient-bind-q-to-quit))

(use-package with-editor :diminish)

(use-package magit
  :hook
  ;; Use "M-p/n" to cycle between older commit messages.
  (git-commit-setup
   .
   (lambda ()
     (git-commit-save-message)
     (git-commit-turn-on-auto-fill)))
  :bind
  (("C-x g" . magit-status)
   ("C-c M-g" . magit-file-dispatch)
   ("C-x M-g" . magit-dispatch))
  :custom
  ;; Open the status buffer in a full frame
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-section-initial-visibility-alist
   '((stashes . show) (untracked . show) (unpushed . show) (unpulled . show)))
  (magit-save-repository-buffers 'dontask)
  ;; Show fine differences for the current diff hunk only
  (magit-diff-refine-hunk t)
  (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package git-modes
  :mode ("dotgitconfig" . gitconfig-mode)
  :mode ("/\\.gitignore\\'" . gitignore-mode)
  :mode ("/\\.gitattributes\\'" . gitattributes-mode))

;; Fringe is unavailable in TTY
(use-package diff-hl
  :hook
  ((diff-hl-mode-on . diff-hl-margin-local-mode)
   (dired-mode . diff-hl-dired-mode-unless-remote)
   (find-file . global-diff-hl-mode))
  :bind (("C-x v [" . diff-hl-previous-hunk) ("C-x v ]" . diff-hl-next-hunk))
  :custom (diff-hl-draw-borders nil "Highlight without a border looks nicer")
  :config
  (diff-hl-flydiff-mode 1) ; For unsaved buffers

  (with-eval-after-load "magit"
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)))

(use-package smerge-mode
  :straight (:type built-in)
  :bind
  (:map
   smerge-mode-map
   ("M-g n" . smerge-next)
   ("M-g p" . smerge-prev)
   ("M-g u" . smerge-keep-upper)
   ("M-g l" . smerge-keep-lower)
   ("M-g a" . smerge-keep-all)))

(use-package elec-pair
  :straight (:type built-in)
  :hook (emacs-startup . electric-pair-mode)
  :custom
  ;; Avoid balancing parentheses since they can be both irritating and slow
  (electric-pair-preserve-balance nil)
  (electric-pair-skip-self nil)
  :config
  (setq electric-pair-inhibit-predicate
        (lambda (c)
          (if (char-equal c ?\")
              t
            (electric-pair-default-inhibit c))))

  (defvar sb/markdown-pairs '((?` . ?`)))

  (defun sb/add-markdown-pairs ()
    (setq-local electric-pair-pairs
                (append electric-pair-pairs sb/markdown-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (add-hook 'markdown-mode-hook #'sb/add-markdown-pairs)

  ;; (defvar sb/latex-pairs '((?\{ . ?\}) (?\[ . ?\]) (?\( . ?\))))

  ;; (defun sb/add-latex-pairs ()
  ;;   (setq-local electric-pair-pairs (append electric-pair-pairs sb/latex-pairs))
  ;;   (setq-local electric-pair-text-pairs electric-pair-pairs))

  ;; (dolist (mode '(latex-mode-hook LaTeX-mode-hook))
  ;;   (add-hook mode #'sb/add-latex-pairs))
  )

;; Discover key bindings for the current Emacs major mode
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package mode-minder
  :straight (:host github :repo "jdtsmith/mode-minder")
  :commands mode-minder)

(use-package flycheck
  :hook (emacs-startup . global-flycheck-mode)
  :custom
  ;; Remove newline checks, since they would trigger an immediate check when we
  ;; want the `flycheck-idle-change-delay' to be in effect while editing.
  (flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change))
  (flycheck-checker-error-threshold nil)
  ;; Increase the time (s) to allow for quick transitions
  (flycheck-idle-buffer-switch-delay 2)
  ;; Increase the time (s) to allow for transient edits
  (flycheck-idle-change-delay 2)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-global-modes '(not csv-mode conf-mode))
  :config
  (dolist (checkers '(proselint textlint tex-chktex emacs-lisp-checkdoc))
    (delq checkers flycheck-checkers))

  ;; These themes have their own styles for displaying flycheck info.
  (when (eq sb/modeline-theme 'doom-modeline)
    (setq flycheck-mode-line nil))

  (setq-default
   flycheck-markdown-markdownlint-cli-config
   (expand-file-name ".markdownlint.json" sb/user-home-directory)
   flycheck-pylintrc '("setup.cfg" "pylintrc")
   flycheck-python-pylint-executable "python3"
   flycheck-shellcheck-follow-sources nil)

  ;; https://github.com/flycheck/flycheck/issues/1833
  (add-to-list 'flycheck-hooks-alist '(after-revert-hook . flycheck-buffer))

  ;; Chain flycheck checkers with lsp, using per-project directory local
  ;; variables. https://github.com/flycheck/flycheck/issues/1762
  (defvar-local sb/flycheck-local-checkers nil)
  (defun sb/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker sb/flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get))

(use-package sideline
  :init (setq sideline-backends-left nil)
  :hook ((flycheck-mode lsp-mode) . sideline-mode)
  :diminish)

(use-package sideline-flycheck
  :after sideline
  :hook (flycheck-mode . sideline-flycheck-setup))

(use-package sideline-lsp
  :after sideline
  :config
  (setq sideline-backends-right
        '((sideline-lsp . up) (sideline-flycheck . down))))

(use-package format-all
  :hook
  ((format-all-mode . format-all-ensure-formatter)
   ((markdown-mode markdown-ts-mode) . format-all-mode))
  :config
  (setq-default format-all-formatters
                '(("Assembly" asmfmt)
                  ("Awk" gawk)
                  ("BibTeX" latexindent)
                  ("C" clang-format)
                  ("C++" clang-format)
                  ("CMake" cmake-format)
                  ("CSS" prettier)
                  ("Cuda" clang-format)
                  ("Dockerfile" dockfmt)
                  ("Emacs Lisp" emacs-lisp)
                  ("Fish" fish-indent)
                  ("HTML" (prettier "--print-width" "80"))
                  ("LaTeX" latexindent)
                  ("Markdown" (prettier "--print-width" "80"))
                  ("Perl" (perltidy
                    "--quiet"
                    "--standard-error-output"
                    "--perl-best-practices"
                    "-l=80"))
                  ("Python" (yapf "--style" "file") isort)
                  ("Shell" (shfmt "-i" "4" "-ci"))
                  ("XML" tidy)
                  ("YAML" prettier "--print-width" "80")))
  (with-eval-after-load "markdown-mode"
    (bind-key "C-x f" #'format-all-buffer markdown-mode-map))
  (with-eval-after-load "tex-mode"
    (bind-key "C-x f" #'format-all-buffer tex-mode-map))
  :diminish)

;; Provides indentation guide bars with tree-sitter support
(use-package indent-bars
  :hook
  ((python-mode python-ts-mode yaml-mode yaml-ts-mode tex-mode LaTeX-mode)
   .
   indent-bars-mode)
  :config
  (when (and (executable-find "tree-sitter")
             (fboundp 'treesit-available-p)
             (treesit-available-p))
    (setq
     indent-bars-treesit-support t
     indent-bars-treesit-ignore-blank-lines-types '("module")
     indent-bars-treesit-scope
     '((python
        function_definition
        class_definition
        for_statement
        if_statement
        with_statement
        while_statement)))))

(use-package elisp-autofmt
  :hook ((emacs-lisp-mode lisp-data-mode) . elisp-autofmt-mode)
  :custom
  (elisp-autofmt-python-bin "python3")
  (elisp-autofmt-on-save-p 'always))

(use-package shfmt
  :hook ((sh-mode bash-ts-mode) . shfmt-on-save-mode)
  :custom
  ;; p: Posix, ci: indent case labels, i: indent with spaces
  (shfmt-arguments '("-i" "4" "-ln" "bash" "-ci")))

(use-package flycheck-hl-todo
  :after flycheck
  :init (flycheck-hl-todo-setup))

(use-package consult-todo
  :after (consult hl-todo)
  :commands consult-todo)

;; "basic" matches only the prefix, "substring" matches the whole string.
;; "initials" matches acronyms and initialisms, e.g., can complete "M-x lch" to
;; "list-command-history". "partial-completion" style allows to use wildcards
;; for file completion and partial paths, e.g., "/u/s/l" for "/usr/share/local".
;; While "partial-completion" matches search terms must match in order,
;; "orderless" can match search terms in any order.
(use-package minibuffer
  :straight (:type built-in)
  :bind
  (("M-p" . minibuffer-previous-completion)
   ("M-n" . minibuffer-next-completion)
   ("M-RET" . minibuffer-choose-completion))
  :custom
  ;; Ignore case when reading a file name
  (read-file-name-completion-ignore-case t)
  (completion-cycle-threshold 3 "TAB cycle if there are only few candidates")
  :config
  ;; Show docstring description for completion candidates in commands like
  ;; `describe-function'.
  (when (boundp completions-detailed)
    (setq completions-detailed t))

  (when (fboundp 'dabbrev-capf)
    (add-to-list 'completion-at-point-functions 'dabbrev-capf t))

  (with-eval-after-load "orderless"
    ;; substring is needed to complete common prefix, orderless does not
    (setq completion-styles '(orderless substring partial-completion basic)))

  ;; The "basic" completion style needs to be tried first for TRAMP hostname
  ;; completion to work. I also want substring matching for file names.
  (setq completion-category-overrides
        '((file (styles basic partial-completion)))))

;; Use "C-M-;" for `dabbrev-completion' which finds all expansions in the
;; current buffer and presents suggestions for completion.
(use-package dabbrev
  :straight (:type built-in)
  :bind ("C-M-;" . dabbrev-completion)
  :custom
  (dabbrev-ignored-buffer-regexps
   '("^ "
     "\\.\\(?:jpe?g\\|png\\|pdf\\)\\'"
     "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (dabbrev-upcase-means-case-search t)
  :config
  (dolist (exclude '(doc-view-mode pdf-view-mode tags-table-mode))
    (add-to-list 'dabbrev-ignored-buffer-modes exclude)))

(use-package hippie-exp
  :straight (:type built-in)
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))
  (hippie-expand-verbose nil)
  :bind (("M-/" . hippie-expand) ([remap dabbrev-expand] . hippie-expand)))

;; Use "M-SPC" for space-separated completion lookups.
(use-package orderless
  :demand t
  :config
  (with-eval-after-load "company"
    (defun sb/just-one-face (fn &rest args)
      (let ((orderless-match-faces [completions-common-part]))
        (apply fn args)))
    (advice-add 'company-capf--candidates :around #'sb/just-one-face))

  (with-eval-after-load "lsp-mode"
    (defun sb/lsp-mode-setup-orderless ()
      (setf (alist-get
             'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
    (add-hook 'lsp-completion-mode-hook #'sb/lsp-mode-setup-orderless)))

(use-package yasnippet
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook ((prog-mode LaTeX-mode latex-mode) . yas-global-mode)
  :custom
  (yas-verbosity 0)
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (with-eval-after-load "hippie-expand"
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  (unbind-key "<tab>" yas-minor-mode-map)
  :diminish yas-minor-mode)

(use-package yasnippet-snippets
  :after yasnippet
  :init (yasnippet-snippets-initialize))

(use-package consult-yasnippet
  :bind ("C-M-y" . consult-yasnippet))

;; Use "M-x company-diag" or the modeline status (without diminish) to see the
;; backend used for the last completion. Try "M-x company-complete-common" when
;; there are no completions. Use "C-M-i" for `complete-symbol' with regex
;; search.
(use-package company
  :when (eq sb/in-buffer-completion 'company)
  :hook (emacs-startup . global-company-mode)
  :bind
  (
   ;; Invoke the next backend in `company-backends'
   ("C-M-/" . company-other-backend)
   :map
   company-active-map
   ("C-M-/" . company-other-backend)
   ("C-s" . company-search-candidates)
   ("C-M-s" . company-filter-candidates)
   ;; When using graphical Emacs, you need to bind both (kbd "<tab>") and (kbd
   ;; "TAB"). First TAB keypress will complete the common part of all
   ;; completions, and the next will switch to the next completion in a cyclic
   ;; fashion, meaning that if you reach the end you continue from the top.
   ;; S-TAB will go in reverse direction.
   ("<tab>" . company-complete-common-or-cycle)
   ("TAB" . company-complete-common-or-cycle)
   ("<backtab>" .
    (lambda ()
      (interactive)
      (company-complete-common-or-cycle -1)))
   ("S-TAB" .
    (lambda ()
      (interactive)
      (company-complete-common-or-cycle -1)))
   ;; ([escape]
   ;;  .
   ;;  (lambda ()
   ;;    (interactive)
   ;;    (company-abort)))
   ;; ("ESCAPE" .
   ;;  (lambda ()
   ;;    (interactive)
   ;;    (company-abort)))
   ("M-." . company-show-location)
   ("C-h" . company-show-doc-buffer)
   :map
   company-search-map
   ("C-s" . company-search-repeat-forward)
   ("C-r" . company-search-repeat-backward)
   ("C-g" . company-search-abort)
   ("DEL" . company-search-delete-char))
  :custom
  (company-dabbrev-downcase nil "Do not downcase returned candidates")
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-code-completion-styles '(basic flex))
  (company-ispell-dictionary
   (expand-file-name "wordlist.5" sb/extras-directory))
  (company-show-quick-access t "Speed up selecting a completion")
  (company-tooltip-align-annotations t)
  (company-global-modes
   '(not dired-mode
         magit-status-mode
         help-mode
         helpful-mode
         csv-mode
         minibuffer-inactive-mode))
  ;; Convenient to wrap around completion items at boundaries
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 3)
  (company-frontends
   '(
     ;; Always show candidates in overlay tooltip
     company-pseudo-tooltip-frontend
     ;; Show selected candidate docs in echo area
     company-echo-metadata-frontend))
  (company-format-margin-function (bound-and-true-p sb/enable-icons)))

(use-package kind-icon
  :when (bound-and-true-p sb/enable-icons)
  :after company
  :demand t
  :config
  (require 'svg-lib)
  (add-to-list
   'svg-lib-icon-collections
   '("nerd-fonts-codicons"
     .
     "https://github.com/microsoft/vscode-codicons/raw/HEAD/src/icons/%s.svg"))

  (setq kind-icon-mapping
        '((array
           "a"
           :icon "symbol-array"
           :face font-lock-type-face
           :collection "nerd-fonts-codicons")
          (boolean
           "b"
           :icon "symbol-boolean"
           :face font-lock-builtin-face
           :collection "nerd-fonts-codicons")
          (color
           "#"
           :icon "symbol-color"
           :face success
           :collection "nerd-fonts-codicons")
          (command
           "cm"
           :icon "chevron-right"
           :face default
           :collection "nerd-fonts-codicons")
          (constant
           "co"
           :icon "symbol-constant"
           :face font-lock-constant-face
           :collection "nerd-fonts-codicons")
          (class
           "c"
           :icon "symbol-class"
           :face font-lock-type-face
           :collection "nerd-fonts-codicons")
          (constructor
           "cn"
           :icon "symbol-method"
           :face font-lock-function-name-face
           :collection "nerd-fonts-codicons")
          (enum
           "e"
           :icon "symbol-enum"
           :face font-lock-builtin-face
           :collection "nerd-fonts-codicons")
          (enummember
           "em"
           :icon "symbol-enum-member"
           :face font-lock-builtin-face
           :collection "nerd-fonts-codicons")
          (enum-member
           "em"
           :icon "symbol-enum-member"
           :face font-lock-builtin-face
           :collection "nerd-fonts-codicons")
          (event
           "ev"
           :icon "symbol-event"
           :face font-lock-warning-face
           :collection "nerd-fonts-codicons")
          (field
           "fd"
           :icon "symbol-field"
           :face font-lock-variable-name-face
           :collection "nerd-fonts-codicons")
          (file
           "f"
           :icon "symbol-file"
           :face font-lock-string-face
           :collection "nerd-fonts-codicons")
          (folder
           "d"
           :icon "folder"
           :face font-lock-doc-face
           :collection "nerd-fonts-codicons")
          (function "f"
                    :icon "symbol-method"
                    :face font-lock-function-name-face
                    :collection "nerd-fonts-codicons")
          (interface
           "if"
           :icon "symbol-interface"
           :face font-lock-type-face
           :collection "nerd-fonts-codicons")
          (keyword
           "kw"
           :icon "symbol-keyword"
           :face font-lock-keyword-face
           :collection "nerd-fonts-codicons")
          (macro "mc" :icon "lambda" :face font-lock-keyword-face)
          (magic
           "ma"
           :icon "lightbulb-autofix"
           :face font-lock-builtin-face
           :collection "nerd-fonts-codicons")
          (method
           "m"
           :icon "symbol-method"
           :face font-lock-function-name-face
           :collection "nerd-fonts-codicons")
          (module
           "{"
           :icon "file-code-outline"
           :face font-lock-preprocessor-face)
          (numeric
           "nu"
           :icon "symbol-numeric"
           :face font-lock-builtin-face
           :collection "nerd-fonts-codicons")
          (operator
           "op"
           :icon "symbol-operator"
           :face font-lock-comment-delimiter-face
           :collection "nerd-fonts-codicons")
          (param
           "pa"
           :icon "gear"
           :face default
           :collection "nerd-fonts-codicons")
          (property
           "pr"
           :icon "symbol-property"
           :face font-lock-variable-name-face
           :collection "nerd-fonts-codicons")
          (reference
           "rf"
           :icon "library"
           :face font-lock-variable-name-face
           :collection "nerd-fonts-codicons")
          (snippet
           "S"
           :icon "symbol-snippet"
           :face font-lock-string-face
           :collection "nerd-fonts-codicons")
          (string
           "s"
           :icon "symbol-string"
           :face font-lock-string-face
           :collection "nerd-fonts-codicons")
          (struct
           "%"
           :icon "symbol-structure"
           :face font-lock-variable-name-face
           :collection "nerd-fonts-codicons")
          (text
           "tx"
           :icon "symbol-key"
           :face font-lock-doc-face
           :collection "nerd-fonts-codicons")
          (typeparameter
           "tp"
           :icon "symbol-parameter"
           :face font-lock-type-face
           :collection "nerd-fonts-codicons")
          (type-parameter
           "tp"
           :icon "symbol-parameter"
           :face font-lock-type-face
           :collection "nerd-fonts-codicons")
          (unit
           "u"
           :icon "symbol-ruler"
           :face font-lock-constant-face
           :collection "nerd-fonts-codicons")
          (value
           "v"
           :icon "symbol-enum"
           :face font-lock-builtin-face
           :collection "nerd-fonts-codicons")
          (variable
           "va"
           :icon "symbol-variable"
           :face font-lock-variable-name-face
           :collection "nerd-fonts-codicons")
          (t
           "."
           :icon "question"
           :face font-lock-warning-face
           :collection "nerd-fonts-codicons")))

  (let* ((kind-func (lambda (cand) (company-call-backend 'kind cand)))
         (formatter
          (kind-icon-margin-formatter `((company-kind . ,kind-func)))))
    (defun my-company-kind-icon-margin (cand _selected)
      (funcall formatter cand))
    (setq company-format-margin-function #'my-company-kind-icon-margin)))

;; Show documentation popups
(use-package company-quickhelp
  :after company
  :when (display-graphic-p)
  :hook (prog-mode . company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :after company-quickhelp
  :unless (display-graphic-p)
  :hook (prog-mode . company-quickhelp-terminal-mode))

(use-package company-statistics
  :after company
  :init (company-statistics-mode 1))

;; By default, Unicode symbols backend (`company-math-symbols-unicode') is not
;; active in latex math environments and latex math symbols
;; (`company-math-symbols-latex') is not available outside of math latex
;; environments
(use-package company-math
  :after (:all tex-mode company)
  :demand t)

(use-package company-anywhere
  :straight (:host github :repo "zk-phi/company-anywhere")
  :after company
  :demand t)

(use-package company-dict
  :after company
  :demand t
  :custom
  (company-dict-dir (expand-file-name "company-dict" user-emacs-directory))
  (company-dict-enable-yasnippet nil))

;; Use "<" to trigger company completion of org blocks.
(use-package company-org-block
  :after (company org)
  :demand t)

;; Enables completion of C/C++ header file names
(use-package company-c-headers
  :after (company cc-mode)
  :demand t
  :custom
  (company-c-headers-path-system
   '("/usr/include/c++/11" "/usr/include" "/usr/local/include")))

(use-package company-ctags
  :after (company prog-mode)
  :demand t)

;; Try completion backends in order until there is a non-empty completion list:
;; (setq company-backends '(company-xxx company-yyy company-zzz))

;; Merge completions of all the backends:
;; (setq company-backends '((company-xxx company-yyy company-zzz)))

;; Merge completions of all the backends but keep the candidates organized in
;; accordance with the grouped backends order.
;; (setq company-backends '((company-xxx company-yyy company-zzz :separate)))

;; A few backends are applicable to all modes: `company-yasnippet',
;; `company-ispell', `company-dabbrev-code', and `company-dabbrev'.
;; `company-yasnippet' is blocking. `company-dabbrev' returns a non-nil prefix
;; in almost any context (major mode, inside strings or comments). That is why
;; it is better to put `company-dabbrev' at the end. The ‘prefix’ bool command
;; always returns non-nil for following backends even when their ‘candidates’
;; list command is empty: `company-abbrev', `company-dabbrev',
;; `company-dabbrev-code'.

;; The keyword :with helps to make sure the results from major/minor mode
;; agnostic backends (such as company-yasnippet, company-dabbrev-code) are
;; returned without preventing results from context-aware backends (such as
;; company-capf or company-clang). For this feature to work, put backends
;; dependent on a mode at the beginning of the grouped backends list, then put a
;; keyword :with, and then put context agnostic backend(s).
;; (setq company-backends '((company-capf :with company-yasnippet)))

;; Most backends (e.g., `company-yasnippet') will not pass control to subsequent
;; backends . Only a few backends are specialized on certain major modes or
;; certain contexts (e.g. outside of strings and comments), and pass on control
;; to later backends when outside of that major mode or context.

;; Company does not support grouping of entirely arbitrary backends, they need
;; to be compatible in what `prefix' returns. If the group contains keyword
;; `:with', the backends listed after this keyword are ignored for the purpose
;; of the `prefix' command. If the group contains keyword `:separate', the
;; candidates that come from different backends are sorted separately in the
;; combined list. That is, with `:separate', the multi-backend-adapter will stop
;; sorting and keep the order of completions just like the backends returned
;; them.

(with-eval-after-load "company"
  ;; Override `company-backends' for unhandled major modes.
  (setq
   company-backends
   '(company-files
     (company-capf :with company-dabbrev-code company-ctags company-yasnippet)
     ;; If we have `company-dabbrev' first, then other matches from
     ;; `company-ispell' will be ignored.
     company-ispell company-dict company-dabbrev)
   company-transformers
   '( ;company-sort-by-occurrence
     delete-dups
     company-sort-by-statistics
     ;company-sort-prefer-same-case-prefix
     ))

  ;; Ignore matches from `company-dabbrev' that consist solely of numbers
  ;; https://github.com/company-mode/company-mode/issues/358

  ;; (push (apply-partially #'cl-remove-if
  ;;                        (lambda (c) (string-match-p "\\`[0-9]+\\'" c)))
  ;;       company-transformers)

  (progn
    (defun sb/company-latex-mode ()
      (make-local-variable 'company-backends)

      ;; `company-capf' with Texlab does not pass to later backends, so it makes
      ;; it difficult to complete non-LaTeX commands (e.g. words) which is the
      ;; majority.
      (setq company-backends
            '((company-files
               company-capf
               ;; Math latex tags
               company-math-symbols-latex
               ;; Math Unicode symbols and sub(super)scripts
               ;; company-math-symbols-unicode
               company-yasnippet
               company-ctags
               company-dict
               company-dabbrev
               company-ispell
               :separate))))

    (add-hook 'latex-mode-hook (lambda () (sb/company-latex-mode))))

  (progn
    (defun sb/company-org-mode ()
      (set
       (make-local-variable 'company-backends)
       '(company-files
         company-org-block company-ispell company-dict company-dabbrev)))

    (add-hook 'org-mode-hook (lambda () (sb/company-org-mode))))

  (progn
    (defun sb/company-text-mode ()
      "Add backends for `text-mode' completion in company mode."
      (set
       (make-local-variable 'company-backends)
       '(company-files (company-ispell company-dict company-dabbrev))))

    ;; Extends to derived modes like `markdown-mode' and `org-mode'
    (add-hook
     'text-mode-hook
     (lambda ()
       (unless (or (derived-mode-p 'LaTeX-mode) (derived-mode-p 'latex-mode))
         (sb/company-text-mode)))))

  (progn
    (defun sb/company-yaml-mode ()
      (make-local-variable 'company-backends)
      (setq company-backends
            '(company-files
              (company-capf
               :with
               company-dabbrev-code ; Useful for variable names
               company-yasnippet
               :separate)
              company-dict company-ispell company-dabbrev)))

    (dolist (mode '(yaml-mode-hook yaml-ts-mode-hook))
      (add-hook mode (lambda () (sb/company-yaml-mode)))))

  (progn
    (defun sb/company-html-mode ()
      (set
       (make-local-variable 'company-backends)
       '(company-files
         company-capf company-dict company-ispell company-dabbrev)))

    (dolist (hook '(html-mode-hook html-ts-mode-hook))
      (add-hook hook (lambda () (sb/company-html-mode)))))

  (progn
    (defun sb/company-prog-mode ()
      (setq-local company-backends
                  '(company-files
                    (company-capf
                     ;; company-citre-tags
                     company-c-headers
                     :with company-keywords
                     company-dabbrev-code ; Useful for variable names
                     company-ctags company-yasnippet
                     :separate)
                    company-dict company-ispell company-dabbrev)))

    (add-hook
     'prog-mode-hook
     (lambda ()
       (unless (or (derived-mode-p 'emacs-lisp-mode)
                   (derived-mode-p 'flex-mode)
                   (derived-mode-p 'bison-mode))
         (sb/company-prog-mode)))))

  (progn
    (defun sb/company-elisp-mode ()
      (setq-local company-backends
                  '(company-files
                    (company-capf
                     :with company-keywords
                     company-dabbrev-code ; Useful for variable names
                     company-yasnippet
                     :separate)
                    company-dict company-ispell company-dabbrev)))

    (dolist (hook '(emacs-lisp-mode-hook lisp-data-mode-hook))
      (add-hook hook (lambda () (sb/company-elisp-mode))))))

;; Corfu is not a completion framework, it is a front-end for
;; `completion-at-point'.
(use-package corfu
  :straight
  (corfu
   :files (:defaults "extensions/*")
   :includes
   (corfu-info
    corfu-history corfu-echo corfu-popupinfo corfu-indexed corfu-quick))
  :when (eq sb/in-buffer-completion 'corfu)
  :hook
  ((emacs-startup . global-corfu-mode)
   (corfu-mode
    .
    (lambda ()
      (corfu-history-mode)
      (corfu-echo-mode)
      (corfu-popupinfo-mode)
      (corfu-indexed-mode))))
  :bind
  (:map
   corfu-map
   ("ESCAPE" . corfu-quit)
   ([escape] . corfu-quit)
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous)
   ("M-c" . corfu-quick-insert)
   ("M-q" . corfu-quick-complete)
   ("M-d" . corfu-info-documentation)
   ("M-l" . corfu-info-location)
   ("M-n" . corfu-popupinfo-scroll-up)
   ("M-p" . corfu-popupinfo-scroll-down)
   ([remap corfu-show-documentation] . corfu-popupinfo-toggle))
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-prefix 3)
  (global-corfu-modes
   '((not dired-mode
          inferior-python-mode
          magit-status-mode
          help-mode
          csv-mode
          minibuffer-inactive-mode)
     t))
  (corfu-on-exact-match nil) ; Do not auto expand snippets
  :config
  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'corfu-history))

  (with-eval-after-load "lsp-mode"
    (defun sb/lsp-mode-setup-corfu ()
      (setf (alist-get
             'styles (alist-get 'lsp-capf completion-category-defaults))
            '(substring)))
    (add-hook 'lsp-completion-mode-hook #'sb/lsp-mode-setup-corfu)))

;; Emacs 31+ has in-built support for child frames in the terminal
(use-package corfu-terminal
  :straight (:host codeberg :repo "akib/emacs-corfu-terminal")
  :when
  (and (eq sb/in-buffer-completion 'corfu)
       (not (display-graphic-p))
       (< emacs-major-version 31))
  :hook (corfu-mode . corfu-terminal-mode)
  :custom
  ;; Prevent wraparound at the right edge
  (corfu-terminal-position-right-margin 2))

(use-package yasnippet-capf
  :straight (:host github :repo "elken/yasnippet-capf")
  :after (yasnippet corfu)
  :demand t)

(use-package cape
  :after corfu
  :demand t
  :custom
  (cape-dabbrev-min-length 3)
  (cape-dabbrev-check-other-buffers 'cape--buffers-major-mode)
  (cape-dict-file
   `(,(expand-file-name "wordlist.5" sb/extras-directory)
     ,(expand-file-name "company-dict/text-mode" user-emacs-directory)))
  :config (add-to-list 'completion-category-overrides '((cape-dict (styles (basic)))))

  ;; Make the capf composable
  (with-eval-after-load "lsp-mode"
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))

  ;; Initialize for all generic languages that are not specifically handled. The
  ;; order of the functions matters, unless they are merged, the first function
  ;; returning a result wins. Note that the list of buffer-local completion
  ;; functions takes precedence over the global list.

  (setq-local completion-at-point-functions
              (list
               #'cape-keyword
               #'cape-file
               #'cape-dict
               #'cape-dabbrev
               #'yasnippet-capf))

  ;; Override CAPFS for specific major modes
  (dolist (mode '(emacs-lisp-mode-hook lisp-data-mode-hook))
    (add-hook
     mode
     (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    (cape-capf-super
                     #'elisp-completion-at-point #'cape-elisp-symbol)
                    #'cape-file #'cape-dabbrev #'cape-dict #'yasnippet-capf)))))

  ;; https://github.com/minad/cape/discussions/130
  ;; There is no mechanism to force deduplication if candidates from cape-dict
  ;; and cape-dabbrev are not exactly equal (equal string and equal text
  ;; properties).

  (add-hook
   'text-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  #'cape-file #'cape-dict #'cape-dabbrev #'yasnippet-capf))))

  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  #'cape-elisp-block
                  #'cape-file
                  #'cape-dict
                  #'cape-dabbrev
                  #'yasnippet-capf))))

  (dolist (mode '(latex-mode-hook LaTeX-mode-hook bibtex-mode-hook))
    (add-hook
     mode
     (lambda ()
       (setq-local
        completion-at-point-functions
        (list
         #'lsp-completion-at-point
         ;; Math latex tags
         ;; (cape-company-to-capf #'company-math-symbols-latex)
         ;; Math Unicode symbols and sub(super)scripts
         ;; (cape-company-to-capf #'company-math-symbols-unicode)
         ;; (cape-company-to-capf #'company-latex-commands)
         ;; Used for Unicode symbols and not for the corresponding LaTeX names.
         #'cape-tex
         #'citar-capf
         #'bibtex-capf
         #'cape-file
         #'cape-dict
         #'cape-dabbrev
         #'yasnippet-capf)))))

  (dolist (mode
           '(c-mode-hook
             c-ts-mode-hook
             c++-mode-hook
             c++-ts-mode-hook
             cmake-mode-hook
             cmake-ts-mode-hook
             css-mode-hook
             css-ts-mode-hook
             fish-mode-hook
             java-mode-hook
             java-ts-mode-hook
             makefile-mode
             python-mode-hook
             python-ts-mode-hook
             sh-mode-hook
             bash-ts-mode-hook
             json-mode-hook
             json-ts-mode-hook
             jsonc-mode-hook
             yaml-mode-hook
             yaml-ts-mode-hook))
    (add-hook
     mode
     (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    #'lsp-completion-at-point
                    #'cape-keyword
                    #'cape-file
                    #'cape-dabbrev
                    #'cape-dict
                    #'yasnippet-capf))))))

(use-package lsp-mode
  :bind
  (:map
   lsp-command-map
   ("l" . lsp)
   ("q" . lsp-disconnect)
   ("Q" . lsp-workspace-shutdown)
   ("R" . lsp-workspace-restart)
   ("d" . lsp-find-declaration)
   ("e" . lsp-find-definition)
   ("r" . lsp-find-references)
   ("i" . lsp-find-implementation)
   ("I" . lsp-goto-implementation)
   ("t" . lsp-goto-type-definition)
   ("r" . lsp-rename)
   ("f" . lsp-format-buffer)
   ("x" . lsp-execute-code-action)
   ("c" . lsp-imenu-create-categorised-index) ; sorts the items by kind.
   ("u" . lsp-imenu-create-uncategorised-index) ; sorts the items by position
   ("a" . lsp-workspace-folders-add)
   ("v" . lsp-workspace-folders-remove)
   ("b" . lsp-workspace-blacklist-remove))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-clangd-version "19.1.2")
  (lsp-clients-clangd-args
   '("-j=4"
     "--all-scopes-completion"
     "--background-index"
     "--clang-tidy"
     "--completion-style=detailed"
     "--fallback-style=LLVM"
     ;; Do not automatically insert #include statements when editing code
     "--header-insertion=never"
     "--header-insertion-decorators=0"
     "--log=error"
     ;; Unsupported options with Clangd 10: malloc-trim and enable-config
     "--malloc-trim" ; Release memory periodically
     "--enable-config"
     "--pch-storage=memory" ; Increases memory usage but can improve performance
     "--pretty"))
  ;; Enable integration of custom backends other than `capf'
  (lsp-completion-provider :none)
  ;; Show/hide completion metadata, e.g., "java.util.ArrayList"
  (lsp-completion-show-detail t)
  ;; Show/hide completion kind, e.g., interface/class
  (lsp-completion-show-kind t)
  ;; Show/hide description of completion candidates
  (lsp-completion-show-label-description t)
  (lsp-eldoc-enable-hover nil "Do not show noisy hover info with mouse")
  (lsp-enable-dap-auto-configure nil "I do not use dap-mode")
  (lsp-enable-on-type-formatting nil "Reduce unexpected modifications to code")
  (lsp-enable-folding nil "I do not find the feature useful")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-html-format-wrap-line-length fill-column)
  (lsp-html-format-end-with-newline t)
  (lsp-html-format-indent-inner-html t)
  (lsp-imenu-sort-methods '(position) "More natural way of listing symbols")
  (lsp-lens-enable nil "Lenses are intrusive")
  (lsp-modeline-diagnostics-enable nil)
  ;; Simpler to focus on the errors at hand
  (lsp-modeline-diagnostics-scope :file)
  ;; Sudden changes in the height of the echo area causes the cursor to lose
  ;; position, manually request via `lsp-signature-activate'.
  (lsp-signature-auto-activate nil)
  ;; Avoid annoying questions, we expect a server restart to succeed
  (lsp-restart 'auto-restart)
  (lsp-xml-logs-client nil)
  ;; Avoid warning messages for unsupported modes like `csv-mode'
  (lsp-warn-no-matched-clients nil)
  (lsp-enable-file-watchers nil "Avoid watcher warnings")
  ;; Use `symbol-overlay' to include languages that do not have a language
  ;; server
  (lsp-enable-symbol-highlighting nil)
  (lsp-pylsp-configuration-sources ["setup.cfg"])
  (lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-pylsp-plugins-preload-modules
   ["numpy" "csv" "pandas" "statistics" "json"])
  (lsp-pylsp-plugins-pydocstyle-convention "pep257")
  (lsp-pylsp-plugins-pylint-enabled t)
  (lsp-pylsp-plugins-yapf-enabled t)
  (lsp-pylsp-plugins-flake8-enabled nil)
  (lsp-pylsp-plugins-isort-enabled t)
  (lsp-pylsp-plugins-mypy-enabled t)
  (lsp-use-plists t)
  (lsp-auto-register-remote-clients nil)
  (lsp-enable-snippet nil)
  (lsp-keep-workspace-alive nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-enable-suggest-server-download nil)
  :config
  (when (or (display-graphic-p) (daemonp))
    (setq lsp-modeline-code-actions-segments '(count icon name)))

  ;; (dolist (ignore-dirs
  ;;          '("/build\\'"
  ;;            "/\\.metadata\\'"
  ;;            "/\\.recommenders\\'"
  ;;            "/\\.clangd\\'"
  ;;            "/\\.cache\\'"
  ;;            "/__pycache__\\'"))
  ;;   (add-to-list 'lsp-file-watch-ignored-directories ignore-dirs))

  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or (when (equal (following-char) ?#)
          (let ((bytecode (read (current-buffer))))
            (when (byte-code-function-p bytecode)
              (funcall bytecode))))
        (apply old-fn args)))
  (advice-add
   (if (progn
         (require 'json)
         (fboundp 'json-parse-buffer))
       'json-parse-buffer
     'json-read)
   :around #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and
           (not test?) ;; for check lsp-server-present?
           ;; see lsp-resolve-final-command, it would add extra shell wrapper
           (not (file-remote-p default-directory)) lsp-use-plists
           (not (functionp 'json-rpc-connection)) ;; native json-rpc
           (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add
   'lsp-resolve-final-command
   :around #'lsp-booster--advice-final-command)

  :diminish)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; Disable intrusive on-hover dialogs, invoke with `lsp-ui-doc-show'
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-sideline-enable nil)
  ;; Enables understanding when to invoke code actions
  (lsp-ui-sideline-show-code-actions t)
  ;; Hide diagnostics when typing because they can be intrusive,
  ;; Flycheck/flymake already highlights errors
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-max-width 72 "150 (default) is too wide")
  (lsp-ui-doc-delay 0.75 "0.2 (default) is too naggy")
  (lsp-ui-peek-enable nil))

(use-package consult-lsp
  :after (consult lsp)
  :demand t
  :commands consult-lsp-diagnostics
  :bind
  (:map
   lsp-command-map
   ("g" . consult-lsp-symbols)
   ("h" . consult-lsp-file-symbols)))

(use-package lsp-java
  :hook
  ((java-mode java-ts-mode)
   .
   (lambda ()
     (setq-local
      c-basic-offset 4
      c-set-style "java")
     (lsp-deferred)))
  :custom
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-format-settings-profile "Swarnendu")
  (lsp-java-format-settings-url
   (expand-file-name "github/dotfiles/java/eclipse-format-swarnendu.xml"
                     sb/user-home-directory)))

(use-package lsp-ltex-plus
  :straight (:host github :repo "emacs-languagetool/lsp-ltex-plus")
  :init (setq lsp-ltex-plus-version "18.4.0")
  :hook
  ((text-mode markdown-mode org-mode LaTeX-mode latex-mode)
   .
   (lambda ()
     (require 'lsp-ltex-plus)
     (lsp-deferred)))
  :custom
  ;; Recommended to set a generic language to disable spell check
  (lsp-ltex-plus-plus-language "en")
  (lsp-ltex-plus-check-frequency "save")
  ;; (lsp-ltex-plus-dictionary
  ;;  '((expand-file-name "company-dict/text-mode" user-emacs-directory)))
  (lsp-ltex-plus-log-level "warning")
  (lsp-ltex-plus-disabled-rules
   '(:en
     ["EN_QUOTES"
      "OXFORD_SPELLING_Z_NOT_S"
      "MORFOLOGIK_RULE_EN_US"
      "WANT"
      "EN_DIACRITICS_REPLACE"]))
  ;; :config
  ;; ;; Disable spell checking since we cannot get `lsp-ltex' to work with custom
  ;; ;; dict words.
  ;; (setq lsp-ltex-plus-disabled-rules
  ;;       #s(hash-table
  ;;          size 30 data
  ;;          ("en-US"
  ;;           ["MORFOLOGIK_RULE_EN_US,WANT,EN_QUOTES,EN_DIACRITICS_REPLACE"])))
  )

(use-package lsp-latex
  :hook
  ((latex-mode LaTeX-mode bibtex-mode)
   .
   (lambda ()
     (require 'lsp-latex)
     (lsp-deferred)))
  :custom
  (lsp-latex-bibtex-formatter "latexindent")
  (lsp-latex-latex-formatter "latexindent")
  (lsp-latex-bibtex-formatter-line-length fill-column)
  (lsp-latex-diagnostics-delay 2000)

  ;; Support forward search with Okular. Perform inverse search with Shift+Click
  ;; in the PDF.
  (lsp-latex-forward-search-executable "okular")
  (lsp-latex-forward-search-args '("--noraise --unique" "file:%p#src:%l%f"))
  :config
  (with-eval-after-load "tex-mode"
    (bind-key "C-c C-c" #'lsp-latex-build tex-mode-map)))

(use-package subword
  :straight (:type built-in)
  :hook ((LaTeX-mode latex-mode prog-mode) . subword-mode)
  :diminish)

(use-package symbol-overlay
  :hook ((prog-mode conf-mode) . symbol-overlay-mode)
  :bind
  (:map
   symbol-overlay-map
   ("M-p" . symbol-overlay-jump-prev)
   ("M-n" . symbol-overlay-jump-next)
   ("<" . symbol-overlay-jump-first)
   (">" . symbol-overlay-jump-last)
   ("d" . symbol-overlay-jump-to-definition)
   ("r" . symbol-overlay-rename))
  :custom
  ;; Delay highlighting to allow for transient cursor placements
  (symbol-overlay-idle-time 2)
  :diminish)

(use-package compile
  :straight (:type built-in)
  :bind (:map prog-mode-map ("<f10>" . compile) ("<f11>" . recompile))
  :custom
  (compile-command (format "make -k -j%s " (num-processors)))
  (compilation-always-kill t)
  (compilation-ask-about-save nil "Save all modified buffers without asking")
  (compilation-scroll-output t)
  (compilation-auto-jump-to-first-error t)
  (compilation-max-output-line-length nil)
  :config
  (with-eval-after-load "tex-mode"
    (bind-key "<f10>" #'compile tex-mode-map)
    (bind-key "<f11>" #'recompile tex-mode-map)))

(use-package fancy-compilation
  :after compile
  :init (fancy-compilation-mode 1)
  :custom (fancy-compilation-scroll-output 'first-error))

(use-package eldoc
  :straight (:type built-in)
  :hook (find-file . global-eldoc-mode)
  :custom
  (eldoc-area-prefer-doc-buffer t "Disable popups")
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :config
  ;; Allow eldoc to trigger after completions
  (with-eval-after-load "company"
    (eldoc-add-command
     'company-complete-selection
     'company-complete-common
     'company-capf
     'company-abort))
  :diminish)

(use-package eldoc-box
  :when (display-graphic-p)
  :commands eldoc-box-help-at-point
  :hook (prog-mode . eldoc-box-hover-mode)
  :custom (eldoc-box-clear-with-C-g t)
  :diminish)

(use-package treesit
  :straight (:type built-in)
  :when
  (and (executable-find "tree-sitter")
       (fboundp 'treesit-available-p)
       (treesit-available-p))
  :demand t
  :bind (("C-M-a" . treesit-beginning-of-defun) ("C-M-e" . treesit-end-of-defun))
  :custom
  ;; Increased default font locking may hurt performance
  (treesit-font-lock-level 4)
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (cuda "https://github.com/tree-sitter-grammars/tree-sitter-cuda")
     (docker "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (js "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (kdl "https://github.com/tree-sitter-grammars/tree-sitter-kdl")
     (latex "https://github.com/latex-lsp/tree-sitter-latex")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (markdown-inline
      "https://github.com/tree-sitter-grammars/tree-sitter-markdown")
     (org "https://github.com/milisims/tree-sitter-org")
     (perl "https://github.com/tree-sitter-perl/tree-sitter-perl")
     (php "https://github.com/tree-sitter/tree-sitter-php")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (when (unless (and (treesit-language-available-p 'bash)
                     (treesit-language-available-p 'bibtex)
                     (treesit-language-available-p 'c)
                     (treesit-language-available-p 'cpp)
                     (treesit-language-available-p 'cmake)
                     (treesit-language-available-p 'css)
                     (treesit-language-available-p 'cuda)
                     (treesit-language-available-p 'docker)
                     (treesit-language-available-p 'elisp)
                     (treesit-language-available-p 'html)
                     (treesit-language-available-p 'java)
                     (treesit-language-available-p 'json)
                     (treesit-language-available-p 'kdl)
                     (treesit-language-available-p 'latex)
                     (treesit-language-available-p 'make)
                     (treesit-language-available-p 'markdown)
                     (treesit-language-available-p 'org)
                     (treesit-language-available-p 'perl)
                     (treesit-language-available-p 'python)
                     (treesit-language-available-p 'toml)
                     (treesit-language-available-p 'yaml)))
    (mapc
     #'treesit-install-language-grammar
     (mapcar #'car treesit-language-source-alist))))

(use-package treesit-auto
  :after treesit
  :config
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all))

(with-eval-after-load "c++-ts-mode"
  (bind-key "C-M-a" #'treesit-beginning-of-defun c++-ts-mode-map)
  (bind-key "C-M-e" #'treesit-end-of-defun c++-ts-mode-map))

;; (with-eval-after-load "treesit"
;;   ;; Improves performance with large files without significantly diminishing
;;   ;; highlight quality
;;   (setq font-lock-maximum-decoration '((c-mode . 2) (c++-mode . 2) (t . t))))

(use-package c-ts-mode
  :straight (:type built-in)
  :when
  (and (executable-find "treesitter")
       (fboundp 'treesit-available-p)
       (treesit-available-p))
  :mode (("\\.h\\'" . c++-ts-mode) ("\\.c\\'" . c++-ts-mode))
  :hook
  ((c-ts-mode c++-ts-mode)
   .
   (lambda ()
     (setq-local
      c-ts-mode-indent-style 'linux
      c-ts-mode-indent-offset 2
      c-ts-mode-toggle-comment-style -1
      c-auto-newline nil ; Disable electric indentation and on-type formatting
      c-electric-flag nil
      c-enable-auto-newline nil
      c-syntactic-indentation nil)
     (lsp-deferred)))
  :bind
  (:map
   c-ts-mode-map
   ("C-M-a" . treesit-beginning-of-defun)
   ("C-M-e" . treesit-end-of-defun)
   :map
   c++-ts-mode-map
   ("C-M-a" . treesit-beginning-of-defun)
   ("C-M-e" . treesit-end-of-defun)))

(use-package cuda-mode
  :mode ("\\.cu\\'" . c++-ts-mode)
  :mode ("\\.cuh\\'" . c++-ts-mode))

(use-package opencl-c-mode
  :mode "\\.cl\\'")

(use-package cmake-mode
  :when (executable-find "cmake")
  :mode ("\(CMakeLists\.txt|\.cmake\)$" . cmake-ts-mode)
  :hook
  ((cmake-mode cmake-ts-mode)
   .
   (lambda ()
     ;; `cmake-mode' is derived from `text-mode', so disable grammar and spell
     ;; checking.
     (jinx-mode -1)
     (setq-local lsp-disabled-clients '(ltex-ls-plus))
     (lsp-deferred))))

(use-package python
  :straight (:type built-in)
  :mode
  (("SCon\(struct\|script\)$" . python-ts-mode)
   ("[./]flake8\\'" . conf-mode)
   ("/Pipfile\\'" . conf-mode))
  :hook ((python-mode python-ts-mode) . lsp-deferred)
  :bind
  (:map
   python-mode-map
   ("C-c C-d")
   ("M-a" . python-nav-backward-block)
   ("M-e" . python-nav-forward-block)
   ("C-c <" . python-indent-shift-left)
   ("C-c >" . python-indent-shift-right))
  :custom
  ;; Disable readline based native completion
  (python-shell-completion-native-enable nil)
  ;; Remove guess indent python message
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4)
  (python-shell-exec-path "python3")
  (python-shell-interpreter "python3"))

(use-package python-docstring
  :hook (python-mode . python-docstring-mode)
  :diminish)

(use-package pyvenv
  :hook ((python-mode python-ts-mode) . pyvenv-mode)
  :custom
  (pyvenv-mode-line-indicator
   '(pyvenv-virtual-env-name (" [venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-post-activate-hooks
   (list
    (lambda ()
      (setq python-shell-interpreter
            (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks
   (list (lambda () (setq python-shell-interpreter "python3")))))

(use-package cperl-mode
  :straight (:type built-in)
  :mode "latexmkrc\\'"
  :config (fset 'perl-mode 'cperl-mode))

(use-package sh-script
  :straight (:type built-in)
  :mode ("\\bashrc\\'" . bash-ts-mode)
  :hook ((sh-mode bash-ts-mode) . lsp-deferred)
  :bind (:map sh-mode-map ("C-c C-d"))
  :custom
  (sh-basic-offset 2)
  (sh-indent-after-continuation 'always)
  (sh-indent-comment t "Indent comments as a regular line"))

(use-package fish-mode
  :mode "\\.fish\\'"
  :interpreter "fish"
  :hook
  (fish-mode
   . (lambda () (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package highlight-doxygen
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode cuda-mode) . highlight-doxygen-mode))

(use-package lisp-mode
  :straight (:type built-in)
  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . lisp-data-mode)
  :hook
  (lisp-data-mode
   .
   (lambda ()
     (when buffer-file-name
       (add-hook 'after-save-hook #'check-parens nil t)))))

(use-package elisp-mode
  :straight (:type built-in)
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook
  (emacs-lisp-mode
   .
   (lambda ()
     (when buffer-file-name
       (add-hook 'after-save-hook #'check-parens nil t)))))

(use-package paren-face
  :straight (:host github :repo "tarsius/paren-face")
  :hook (emacs-startup . global-paren-face-mode))

(use-package ini-mode
  :commands (ini-mode))

(use-package conf-mode
  :straight (:type built-in)
  :mode
  "\\.cfg\\'"
  "\\.conf\\'")

(use-package yaml-mode
  :mode
  (("\\.ya?ml\\'" . yaml-ts-mode)
   (".clang-format" . yaml-ts-mode)
   (".clang-tidy" . yaml-ts-mode)
   (".clangd" . yaml-ts-mode))
  :hook
  ((yaml-mode yaml-ts-mode)
   .
   (lambda ()
     ;; `yaml-mode' is derived from `text-mode', so disable grammar and spell
     ;; checking.
     (jinx-mode -1)
     (setq-local lsp-disabled-clients '(ltex-ls-plus))
     (lsp-deferred))))

(use-package yaml-imenu
  :hook ((yaml-mode yaml-ts-mode) . yaml-imenu-enable))

(use-package css-mode
  :straight (:type built-in)
  :hook ((css-mode css-ts-mode) . lsp-deferred)
  :custom (css-indent-offset 2))

(use-package make-mode
  :straight (:type built-in)
  :mode
  (("\\Makefile\\'" . makefile-mode)
   ("\\Makefile.common\\'" . makefile-mode)
   ("makefile\\.rules\\'" . makefile-mode))
  :hook
  (makefile-mode
   .
   (lambda ()
     (setq-local indent-tabs-mode t)
     (lsp-deferred))))

(use-package bison-mode
  :mode ("\\.flex\\'" . flex-mode)
  :mode ("\\.bison\\'" . bison-mode)
  :hook
  (flex-mode
   .
   (lambda ()
     ;; Disable electric indentation and on-type formatting
     (setq-local
      c-auto-newline nil
      ;; c-electric-brace nil
      c-electric-flag nil
      ;; c-electric-indent nil
      c-enable-auto-newline nil
      c-syntactic-indentation nil))))

;; More shortcuts: https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :hook
  (markdown-mode
   .
   (lambda ()
     (require 'lsp-marksman)
     (lsp-deferred)))
  :bind
  (:map
   markdown-mode-map
   ("C-c C-d")
   ("C-c C-j")
   ("C-c C-c l" . markdown-live-preview-mode))
  :custom
  (markdown-command
   "pandoc -f markdown -s --mathjax --standalone --quiet --highlight-style=pygments")
  (markdown-enable-math t "Syntax highlight for LaTeX fragments")
  (markdown-enable-wiki-links t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-list-indent-width 2)
  (markdown-split-window-direction 'horizontal)
  (markdown-hide-urls t))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf. Convert
;; `markdown' to `org': "pandoc -f markdown -t org -o output-file.org
;; input-file.md"
(use-package pandoc-mode
  :hook (markdown-mode . pandoc-mode)
  :config (pandoc-load-default-settings)
  :diminish)

(use-package nxml-mode
  :straight (:type built-in)
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$" "\\.drawio$")
  :hook
  (nxml-mode
   .
   (lambda ()
     ;; `xml-mode' is derived from `text-mode', so disable grammar and spell
     ;; checking.
     (jinx-mode -1)
     (setq-local lsp-disabled-clients '(ltex-ls-plus))
     (lsp-deferred)))
  :custom
  (nxml-auto-insert-xml-declaration-flag t)
  (nxml-slash-auto-complete-flag t)
  (nxml-sexp-element-flag t)
  :config (fset 'xml-mode 'nxml-mode))

(use-package json-mode
  :mode
  (("\\.json\\'" . json-ts-mode)
   ("pyrightconfig.json" . jsonc-mode)
   (".*/vscode/settings.json$" . jsonc-mode)
   (".*/\\.vscode/settings.json$" . jsonc-mode)
   ("User/settings.json$" . jsonc-mode))
  :hook
  ((json-mode json-ts-mode jsonc-mode)
   .
   (lambda ()
     (setq-local js-indent-level 2)
     (lsp-deferred))))

(use-package org
  :defer 2
  :custom
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t "Hide *, ~, and / in Org text unless you edit")
  (org-hide-leading-stars-before-indent-mode nil)
  (org-src-preserve-indentation t)
  (org-src-tabs-acts-natively t "TAB behavior depends on the major mode")
  (org-src-window-setup 'current-window)
  (org-startup-truncated nil)
  (org-startup-folded 'showeverything)
  (org-startup-with-inline-images t)
  ;; See `org-speed-commands-default' for a list of the keys and commands
  ;; enabled at the beginning of headlines. `org-babel-describe-bindings' will
  ;; display a list of the code blocks commands and their related keys.
  (org-use-speed-commands t)
  (org-src-strip-leading-and-trailing-blank-lines t)
  ;; Display entities like `\tilde' and `\alpha' in UTF-8 characters
  (org-pretty-entities t)
  ;; Render subscripts and superscripts in org buffers
  (org-pretty-entities-include-sub-superscripts t)
  ;; Automatically sorted and renumbered whenever I insert a new one
  (org-footnote-auto-adjust t)
  (org-return-follows-link t)
  (org-export-with-smart-quotes t "#+OPTIONS ':t")
  (org-export-with-section-numbers nil "#+OPTIONS num:nil")
  ;; #+OPTIONS toc:nil, use "#+TOC: headlines 2" or similar if you need a
  ;; headline
  (org-export-with-toc nil)
  (org-export-with-sub-superscripts nil "#+OPTIONS ^:{}")
  ;; This exports broken links as [BROKEN LINK %s], so we can actually find
  ;; them. The default value nil just aborts the export process with an error
  ;; message "Unable to resolve link: nil". This doesn't give any hint on which
  ;; line the broken link actually is.
  (org-export-with-broken-links 'mark)
  (org-latex-listings 'minted "Syntax coloring is more extensive than listings")
  (org-highlight-latex-and-related '(native))
  (org-imenu-depth 4)
  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq
   org-latex-pdf-process
   '("latexmk -pdflatex='-shell-escape -interaction nonstopmode -output-directory %o' -pdf -bibtex -f %f"))

  :bind-keymap ("C-c o" . org-mode-map)
  :bind
  (:map
   org-mode-map
   ("M-<left>")
   ("M-<right>")
   ("M-<up>")
   ("M-<down>")
   ("C-'")
   ("C-c C-d")
   ("C-c C-j")
   ("M-e")
   ("<tab>" . org-indent-item)
   ("<backtab>" . org-outdent-item)
   ("M-{" . org-backward-element)
   ("M-}" . org-forward-element)
   ("C-c C-," . org-insert-structure-template)
   ("C-c C-j" . consult-outline)))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autolinks t)
  (org-appear-autoemphasis t)
  (org-appear-autokeywords t))

(use-package ox-gfm
  :after org
  :commands (org-gfm-export-as-markdown org-gfm-export-to-markdown))

(use-package ox-pandoc
  :after org
  :commands
  (org-pandoc-export-to-markdown
   org-pandoc-export-as-markdown org-pandoc-export-to-markdown-and-open))

(use-package org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :hook (org-mode . org-modern-indent-mode))

;; Use "<" to trigger org block completion at point.
(use-package org-block-capf
  :straight (:host github :repo "xenodium/org-block-capf")
  :after corfu
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions)
  :custom (org-block-capf-edit-style 'inline))

(with-eval-after-load "tex-mode"
  (setq tex-command "pdflatex"))

(use-package bibtex
  :straight (:type built-in)
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-maintain-sorted-entries t)
  (bibtex-comma-after-last-field nil))

(use-package consult-reftex
  :straight (:host github :repo "karthink/consult-reftex")
  :after (consult tex-mode)
  :bind
  (("C-c [" . consult-reftex-insert-reference)
   ("C-c )" . consult-reftex-goto-label)))

;; Set `bibtex-capf-bibliography' in `.dir-locals.el'.
(use-package bibtex-capf
  :straight (:host github :repo "mclear-tools/bibtex-capf")
  :when (eq sb/in-buffer-completion 'corfu)
  :after (:any LaTeX-mode latex-mode)
  :demand t)

(use-package math-delimiters
  :straight (:host github :repo "oantolin/math-delimiters")
  :after tex-mode
  :bind (:map tex-mode-map ("$" . math-delimiters-insert)))

(use-package citar
  :after (:any latex-mode LaTeX-mode)
  :demand t)

(use-package citar-embark
  :after (citar embark)
  :config (citar-embark-mode)
  :diminish)

(use-package citre
  :hook (prog-mode . citre-mode)
  :bind
  (("C-x c j" . citre-jump)
   ("C-x c b" . citre-jump-back)
   ("C-x c p" . citre-peek)
   ("C-x c a" . citre-ace-peek)
   ("C-x c c" . citre-create-tags-file)
   ("C-x c u" . citre-update-tags-file)
   ("C-x c e" . citre-edit-tags-file-recipe))
  :custom
  (citre-default-create-tags-file-location 'in-dir)
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-enable-imenu-integration nil)
  (citre-ctags-default-options
   "-o
%TAGSFILE%
-L
%LISTFILE%
--languages=BibTeX,C,C++,CUDA,CMake,EmacsLisp,Java,Make,Python,Sh,TeX
--kinds-all=*
--fields=*
--extras=*
--recurse
;; add exclude by: --exclude=target or by --exclude=@./.ctagsignore
;; add dirs/files to scan here, one line per dir/file
")
  :config
  ;; Try LSP first, and then use Citre if the enabled xref backends cannot find
  ;; a definition
  (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
    (let ((fetcher (apply -fn -args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (apply -fn -args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))

  ;; Use `citre' with Emacs Lisp
  ;; https://github.com/universal-ctags/citre/blob/master/docs/user-manual/adapt-an-existing-xref-backend.md
  (citre-register-backend
   'elisp
   (citre-xref-backend-to-citre-backend
    'elisp (lambda () (derived-mode-p 'emacs-lisp-mode))))

  ;; Add Elisp to the backend lists.
  (add-to-list 'citre-find-definition-backends 'elisp)
  (add-to-list 'citre-find-reference-backends 'elisp)

  ;; ;; This is a completion-at-point-function that tries lsp first, and if no
  ;; ;; completions are given, try Citre.
  ;; (defun lsp-citre-capf-function ()
  ;;   "A capf backend that tries lsp first, then Citre."
  ;;   (let ((lsp-result (lsp-completion-at-point)))
  ;;     (if (and lsp-result
  ;;              (try-completion
  ;;               (buffer-substring
  ;;                (nth 0 lsp-result) (nth 1 lsp-result))
  ;;               (nth 2 lsp-result)))
  ;;         lsp-result
  ;;       (citre-completion-at-point))))

  ;; (defun enable-lsp-citre-capf-backend ()
  ;;   "Enable the lsp + Citre capf backend in current buffer."
  ;;   (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

  ;; (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend)

  (defmacro citre-backend-to-company-backend (backend)
    "Create a company backend from Citre completion backend BACKEND.
The result is a company backend called
`company-citre-<backend>' (like `company-citre-tags') and can be
used in `company-backends'."
    (let ((backend-name
           (intern (concat "company-citre-" (symbol-name backend))))
          (docstring
           (concat
            "`company-mode' backend from the `"
            (symbol-name backend)
            "' Citre backend.\n"
            "`citre-mode' needs to be enabled to use this.")))
      `(defun ,backend-name (command &optional arg &rest ignored)
         ,docstring
         (pcase command
           ('interactive (company-begin-backend ',backend-name))
           ('prefix
            (and (bound-and-true-p citre-mode)
                 (citre-backend-usable-p ',backend)
                 ;; We shouldn't use this as it's defined for getting
                 ;; definitions/references.  But the Citre completion
                 ;; backend design is not fully compliant with company's
                 ;; design so there's no simple "right" solution, and this
                 ;; works for tags/global backends.
                 (or (citre-get-symbol-at-point-for-backend ',backend) 'stop)))
           ('meta (citre-get-property 'signature arg))
           ('annotation (citre-get-property 'annotation arg))
           ('candidates
            (let ((citre-completion-backends '(,backend)))
              (all-completions arg (nth 2 (citre-completion-at-point)))))))))

  (citre-backend-to-company-backend tags)
  :diminish)

(progn
  (defun sb/decrease-minibuffer-font ()
    "Decrease minibuffer font size."
    (set (make-local-variable 'face-remapping-alist) '((default :height 0.95))))

  (add-hook 'minibuffer-setup-hook #'sb/decrease-minibuffer-font))

(use-package doom-themes
  :when (eq sb/theme 'doom-nord)
  :init (load-theme 'doom-nord t)
  :config
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-themes
  :when (eq sb/theme 'modus-vivendi)
  :init (load-theme 'modus-vivendi t))

(use-package catppuccin-theme
  :when (eq sb/theme 'catppuccin)
  :init (load-theme 'catppuccin t)
  :custom (catppuccin-flavor 'mocha)
  :config
  (custom-set-faces
   `(diff-hl-change
     ((t (:background unspecified :foreground ,(catppuccin-get-color 'blue))))))
  (custom-set-faces
   `(diff-hl-delete
     ((t (:background unspecified :foreground ,(catppuccin-get-color 'red))))))
  (custom-set-faces
   `(diff-hl-insert
     ((t
       (:background unspecified :foreground ,(catppuccin-get-color 'green)))))))

(use-package nerd-icons
  :when (bound-and-true-p sb/enable-icons)
  :custom (nerd-icons-scale-factor 0.8))

(use-package nerd-icons-corfu
  :straight (:host github :repo "LuigiPiucco/nerd-icons-corfu")
  :when (and (bound-and-true-p sb/enable-icons) (eq sb/in-buffer-completion 'corfu))
  :after corfu
  :demand t
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Icons in the minibuffer
(use-package nerd-icons-completion
  :straight (:host github :repo "rainstormstudio/nerd-icons-completion")
  :when (bound-and-true-p sb/enable-icons)
  :init (nerd-icons-completion-mode 1)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :straight (:host github :repo "rainstormstudio/nerd-icons-dired")
  :when (bound-and-true-p sb/enable-icons)
  :hook (dired-mode . nerd-icons-dired-mode)
  :diminish)

(use-package nerd-icons-ibuffer
  :when (bound-and-true-p sb/enable-icons)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom (nerd-icons-ibuffer-icon-size 1.0))

;; Powerline theme for Nano looks great, and takes less space on the modeline.
;; It does not show lsp status, flycheck information, and Python virtualenv
;; information on the modeline. The package is not being actively maintained.
;; Inspired by
;; https://github.com/dgellow/config/blob/master/emacs.d/modules/01-style.el
(use-package powerline
  :preface
  (defun sb/powerline-raw (str &optional face pad)
    "Render STR as mode-line data using FACE and optionally PAD import.
PAD can be left (`l') or right (`r')."
    (when str
      (let* ((rendered-str (format-mode-line str))
             (padded-str
              (concat
               (when (and (> (length rendered-str) 0) (eq pad 'l))
                 "")
               (if (listp str)
                   rendered-str
                 str)
               (when (and (> (length rendered-str) 0) (eq pad 'r))
                 ""))))
        (if face
            (pl/add-text-property padded-str 'face face)
          padded-str))))

  (defun sb/powerline-nano-theme ()
    "Setup a Nano-like modeline"
    (interactive)
    (setq-default mode-line-format
                  '("%e" (:eval
                     (let* ((active (powerline-selected-window-active))
                            (face0
                             (if active
                                 'powerline-active0
                               'powerline-inactive0))
                            (lhs
                             (list
                              (powerline-raw
                               (concat
                                "GNU Emacs "
                                (number-to-string emacs-major-version)
                                "."
                                (number-to-string emacs-minor-version))
                               nil 'l)))
                            (rhs
                             (list
                              (when which-function-mode
                                (sb/powerline-raw which-func-format nil 'l))
                              (powerline-vc nil 'l)
                              (powerline-raw "")
                              (powerline-raw "%4l" nil 'l)
                              (powerline-raw ",")
                              (powerline-raw "%3c" nil 'r)
                              (if (buffer-modified-p)
                                  (powerline-raw " ⠾" nil 'r)
                                (powerline-raw "  " nil 'r))))
                            (center (list (powerline-raw "%b" nil 'r))))
                       (concat
                        (powerline-render lhs)
                        (powerline-fill-center
                         nil (/ (powerline-width center) 2.0))
                        (powerline-render center)
                        (powerline-fill nil (powerline-width rhs))
                        (powerline-render rhs)))))))
  :when (eq sb/modeline-theme 'powerline)
  :hook (emacs-startup . sb/powerline-nano-theme)
  :custom
  ;; Visualization of the buffer position is not useful
  (powerline-display-hud nil)
  (powerline-display-buffer-size nil)
  (powerline-display-mule-info nil "File encoding information is not useful")
  (powerline-gui-use-vcs-glyph t)
  (powerline-height 20))

(use-package doom-modeline
  :when (eq sb/modeline-theme 'doom-modeline)
  :hook (emacs-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 30 "Respected only in GUI")
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-enable-word-count t))

;; (use-package centaur-tabs
;;   :disabled
;;   :hook ((emacs-startup . centaur-tabs-mode) (dired-mode . centaur-tabs-local-mode))
;;   :bind*
;;   (("M-<right>" . centaur-tabs-forward-tab)
;;    ("C-<tab>" . centaur-tabs-forward-tab)
;;    ("M-<left>" . centaur-tabs-backward-tab)
;;    ("C-S-<iso-lefttab>" . centaur-tabs-backward-tab)
;;    ("M-'" . centaur-tabs-ace-jump))
;;   :custom
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker "•") ; Unicode Bullet (0x2022)
;;   (centaur-tabs-set-close-button nil "I do not use the mouse")
;;   (centaur-tabs-show-new-tab-button nil "I do not use the mouse")
;;   (centaur-tabs-set-bar 'under)
;;   :config
;;   (when (display-graphic-p)
;;     (setq
;;      centaur-tabs-set-icons t
;;      centaur-tabs-icon-type 'nerd-icons))
;;   ;; Make the headline face match `centaur-tabs-default' face
;;   (centaur-tabs-headline-match))

;; Center the text environment
(use-package olivetti
  :hook ((text-mode prog-mode conf-mode) . olivetti-mode)
  :bind (:map olivetti-mode-map ("C-c {") ("C-c }") ("C-c \\"))
  :diminish)

(use-package kdl-ts-mode
  :straight (:host github :repo "dataphract/kdl-ts-mode")
  :mode ("\\.kdl\\'" . kdl-ts-mode)
  :hook (kdl-ts-mode . kdl-format-on-save-mode)
  :config
  (use-package reformatter
    :after kdl-ts-mode
    :demand t
    :config
    (reformatter-define
     kdl-format
     :program kdlfmt
     :args '("format")
     :lighter " KDLFMT"
     :group 'reformatter)))

;; Fontify ssh files
(use-package ssh-config-mode
  :mode ("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys\\'" . ssh-authorized-keys-mode))

(use-package asm-mode
  :straight (:type built-in)
  :hook (asm-mode . lsp-deferred))

;; Guess the indentation offset originally used in foreign source code files and
;; transparently adjust the corresponding settings in Emacs making it more
;; convenient to edit the foreign files.
(use-package dtrt-indent
  :straight (:host github :repo "jscheid/dtrt-indent")
  :hook (find-file . dtrt-indent-mode)
  :diminish)

;; Navigate the xref stack with consult
(use-package consult-xref-stack
  :straight (:host github :repo "brett-lempereur/consult-xref-stack")
  :bind ("C-," . consult-xref-stack-backward))

;; Kill Emacs buffers automatically after a timeout
(use-package buffer-terminator
  :straight (:host github :repo "jamescherti/buffer-terminator.el")
  :hook (find-file . buffer-terminator-mode)
  :custom (buffer-terminator-verbose nil)
  :diminish)

(use-package hl-line
  :hook (dired-mode . hl-line-mode))

(use-package xclip
  :when (or (executable-find "xclip") (executable-find "xsel"))
  :hook (emacs-startup . xclip-mode))

;; Send every kill from a TTY frame to the system clipboard
(use-package clipetty
  :hook (emacs-startup . global-clipetty-mode)
  :diminish)

;; Provides pixel-precise smooth scrolling which can keep up with the very high
;; event rates of modern trackpads and high-precision wheel mice.
(use-package ultra-scroll
  :straight (:host github :repo "jdtsmith/ultra-scroll")
  :custom
  (scroll-conservatively 101)
  (scroll-margin 0)
  :hook (find-file . ultra-scroll-mode))

;; Fold text using indentation levels
(use-package outline-indent
  :hook
  ((python-mode python-ts-mode yaml-mode yaml-ts-mode)
   .
   outline-indent-minor-mode)
  :custom
  (outline-indent-ellipsis " ▼ ")
  (outline-blank-line t))

;; Allows to easily identify the file path in a project
(use-package project-headerline
  :straight (:host github :repo "gavv/project-headerline")
  :hook (emacs-startup . global-project-headerline-mode))

;; Hide a block with "C-c @ C-d", hide all folds with "C-c @ C-t", show a block
;; with "C-c @ C-s", show all folds with "C-c @ C-a", and toggle hiding of a
;; block with "C-c @ C-c".
(use-package hideshow
  :hook
  ((c-mode-common
    c-ts-mode
    c++-mode
    c++-ts-mode
    cmake-mode
    cmake-ts-mode
    css-mode
    css-ts-mode
    emacs-lisp-mode
    fish-mode
    html-mode
    java-mode
    java-ts-mode
    makefile-mode
    perl-mode
    python-mode
    python-ts-mode
    sh-mode
    bash-ts-mode
    json-mode
    json-ts-mode
    jsonc-mode
    yaml-mode
    yaml-ts-mode)
   . hs-minor-mode)
  :diminish hs-minor-mode)

(use-package dogears
  :straight (:host github :repo "alphapapa/dogears.el")
  :hook (find-file . dogears-mode)
  :bind
  (("M-g d" . dogears-go)
   ("M-g M-b" . dogears-back)
   ("M-g M-f" . dogears-forward)
   ("M-g M-d" . dogears-list)
   ("M-g M-D" . dogears-sidebar))
  :config (add-to-list 'dogears-hooks 'xref-after-jump-hook))

(defun sb/save-all-buffers ()
  "Save all modified buffers without prompting."
  (interactive)
  (save-some-buffers t))

(defun sb/comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position) (goto-char (line-end-position n)))))
      (comment-or-uncomment-region (apply #'min range) (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(defcustom sb/skippable-buffers
  '("TAGS"
    "*Messages*"
    "*Backtrace*"
    "*scratch*"
    "*company-documentation*"
    "*Help*"
    "*Packages*"
    "*prettier (local)*"
    "*emacs*"
    "*Warnings*"
    "*Compile-Log* *lsp-log*"
    "*pyright*"
    "*texlab::stderr*"
    "*texlab*"
    "*Paradox Report*"
    "*perl-language-server*"
    "*perl-language-server::stderr*"
    "*json-ls*"
    "*json-ls::stderr*"
    "*xmlls*"
    "*xmlls::stderr*"
    "*pyright::stderr*"
    "*yamlls*"
    "*yamlls::stderr*"
    "*jdtls*"
    "*jdtls::stderr*"
    "*clangd::stderr*"
    "*shfmt errors*")
  "Buffer names (not regexps) ignored by `sb/next-buffer' and `sb/previous-buffer'."
  :type '(repeat string)
  :group 'sb/emacs)

(defun sb/get-buffer-major-mode (buffer-or-string)
  "Return the major mode associated with BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
    major-mode))

(defcustom sb/skippable-modes
  '(dired-mode
    fundamental-mode
    helpful-mode
    special-mode
    paradox-menu-mode
    lsp-log-io-mode
    help-mode
    magit-status-mode
    magit-process-mode
    magit-diff-mode
    tags-table-mode
    compilation-mode
    emacs-lisp-compilation-mode
    flycheck-verify-mode
    ibuffer-mode
    bs-mode
    ediff-meta-mode)
  "List of major modes to skip over when calling `change-buffer'."
  :type '(repeat string)
  :group 'sb/emacs)

(defun sb/change-buffer (change-buffer)
  "Call CHANGE-BUFFER.
Keep trying until current buffer is not in `sb/skippable-buffers'
or the major mode is not in `sb/skippable-modes'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (or (member (buffer-name) sb/skippable-buffers)
                   (member
                    (sb/get-buffer-major-mode (buffer-name))
                    sb/skippable-modes))
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun sb/next-buffer ()
  "Variant of `next-buffer' that skips `sb/skippable-buffers'."
  (interactive)
  (sb/change-buffer 'next-buffer))

(defun sb/previous-buffer ()
  "Variant of `previous-buffer' that skips `sb/skippable-buffers'."
  (interactive)
  (sb/change-buffer 'previous-buffer))

;; Inside strings, special keys like tab or F1-Fn have to be written inside
;; angle brackets, e.g., "C-<up>". Standalone special keys (and some
;; combinations) can be written in square brackets, e.g. [tab] instead of
;; "<tab>".

;; ESC serves as a substitute for META, but there is no need to hold down ESC -
;; instead "M-something" keybindings can be triggered by pressing ESC and the
;; other key sequentially.

(bind-keys
 ("C-l" . goto-line)
 ("C-c z" . repeat)
 ("C-z" . undo)

 ("<f1>" . execute-extended-command)
 ("<f7>" . previous-error) ; "M-g p" is the default keybinding
 ("<f8>" . next-error) ; "M-g n" is the default keybinding

 ("C-x x a" . find-alternate-file)
 ("C-x x g" . revert-buffer-quick)
 ("C-x x r" . rename-file)

 ;; In a line with comments, "C-u M-;" removes the comments altogether. That
 ;; means deleting the comment, NOT UNCOMMENTING but removing all commented text
 ;; and the comment marker itself.
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box)

 ("C-s" . save-buffer)
 ("C-S-s" . sb/save-all-buffers)
 ("C-x k" . kill-current-buffer)

 ("C-<left>" . backward-word)
 ("C-<right>" . forward-word)

 ("M-\\" . delete-horizontal-space)
 ("M-#" . cycle-spacing)

 ("C-M-b" . backward-sexp)
 ("C-M-f" . forward-sexp)
 ("C-M-k" . kill-sexp))

;; Bound to `abort-recursive-edit', I use it as the prefix key for Zellij
(unbind-key "C-]")
(unbind-key "C-j") ; Bound to `electric-newline-and-maybe-indent'
(unbind-key "C-x f") ; Bound to `set-fill-column'
(unbind-key "M-'") ; Bound to `abbrev-prefix-mark'

(bind-key* "C-x s" #'scratch-buffer) ; Bound to `save-some-buffers'

(global-set-key [remap next-buffer] #'sb/next-buffer)
(global-set-key [remap previous-buffer] #'sb/previous-buffer)

(bind-keys
 ("M-<left>" . sb/previous-buffer)
 ("C-S-<iso-lefttab>" . sb/previous-buffer)
 ("M-<right>" . sb/next-buffer)
 ("C-<tab>" . sb/next-buffer))

(use-package default-text-scale
  :when (display-graphic-p)
  :bind
  (("C-M-+" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

;; Show free bindings in current buffer
(use-package free-keys
  :commands free-keys)

;; Displays available keybindings following the currently entered incomplete
;; command/prefix in a popup
(use-package which-key
  :hook (emacs-startup . which-key-mode)
  :diminish)

;; Support the Kitty Keyboard protocol in Emacs
(use-package kkp
  :hook (emacs-startup . global-kkp-mode)
  ;; :bind ("M-<backspace>" . backward-kill-word) ; should be remapped to "M-DEL"
  :config (define-key key-translation-map (kbd "M-S-4") (kbd "M-$")))

;;; init.el ends here

;; Local variables:
;; elisp-autofmt-load-packages-local: ("use-package")
;; end:
