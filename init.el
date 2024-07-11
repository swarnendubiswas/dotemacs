;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

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

(defcustom sb/op-mode 'standalone
  "Specify the way you expect Emacs to be used."
  :type '(radio (const :tag "daemon" daemon) (const :tag "standalone" standalone))
  :group 'sb/emacs)

(defcustom sb/debug-init-file nil
  "Enable features to debug errors and performance bottlenecks."
  :type 'boolean
  :group 'sb/emacs)

(defcustom sb/theme 'doom-nord
  "Specify which Emacs theme to use."
  :type
  '(radio
    (const :tag "doom-nord" doom-nord)
    (const :tag "modus-vivendi" modus-vivendi)
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme 'doom-modeline
  "Specify the mode-line theme to use."
  :type
  '(radio
    (const :tag "powerline" powerline)
    (const :tag "doom-modeline" doom-modeline)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Large values make reading difficult when the window is split side-by-side, 100 is also a stretch
;; for smaller screens.
(defcustom sb/fill-column 100
  "Column beyond which lines should not extend."
  :type 'number
  :group 'sb/emacs)

;; Helper const variables

(defconst sb/user-home-directory (getenv "HOME")
  "User HOME directory.")

(defconst sb/EMACS28+ (> emacs-major-version 27)
  "Non-nil if Emacs version is 28 and above.")

(defconst sb/EMACS29+ (> emacs-major-version 28)
  "Non-nil if Emacs version is 29 and above.")

(defconst sb/IS-LINUX (eq system-type 'gnu/linux)
  "Non-nil if the OS is GNU/Linux.")

(defconst sb/IS-WINDOWS (eq system-type 'windows-nt)
  "Non-nil if the OS is Windows.")

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order
  '(elpaca
    :repo "https://github.com/progfolio/elpaca.git"
    :ref nil
    :depth 1
    :files (:defaults "elpaca-test.el" (:exclude "extensions"))
    :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list
   'load-path
   (if (file-exists-p build)
       build
     repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28)
      (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop
                   (apply #'call-process
                          `("git" nil ,buffer t "clone" ,@
                            (when-let ((depth (plist-get order :depth)))
                              (list (format "--depth=%d" depth) "--no-single-branch"))
                            ,(plist-get order :repo) ,repo))))
                 ((zerop
                   (call-process "git" nil buffer t "checkout" (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop
                   (call-process emacs
                                 nil
                                 buffer
                                 nil
                                 "-Q"
                                 "-L"
                                 "."
                                 "--batch"
                                 "--eval"
                                 "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
          (progn
            (message "%s" (buffer-string))
            (kill-buffer buffer))
          (error
           "%s"
           (with-current-buffer buffer
             (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; These variables need to be set before loading `use-package'.
(setopt use-package-enable-imenu-support t)

(elpaca elpaca-use-package (elpaca-use-package-mode) (setopt elpaca-use-package-by-default t))

(cond
 ((eq sb/op-mode 'daemon)
  (setopt
   use-package-always-demand t
   use-package-minimum-reported-time 0 ; Show everything
   use-package-verbose t))
 ((eq sb/op-mode 'standalone)
  (if (bound-and-true-p sb/debug-init-file)
      (setopt
       debug-on-error nil debug-on-event 'sigusr2
       use-package-compute-statistics t ; Use "M-x use-package-report" to see results
       use-package-verbose t
       use-package-minimum-reported-time 0 ; Show everything
       use-package-always-demand t)
    (setopt use-package-always-defer t use-package-expand-minimally t))))

(elpaca-wait)

;; Check "use-package-keywords.org" for a suggested order of `use-package' keywords.

(use-package diminish
  :demand t)

(use-package no-littering
  :demand t
  :custom (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(progn
  (unload-feature 'eldoc t) ;; Unload built-in eldoc
  (setq custom-delayed-init-variables '())
  (defvar global-eldoc-mode nil)
  (elpaca
   eldoc (require 'eldoc) (global-eldoc-mode 1)
   (setopt eldoc-area-prefer-doc-buffer t) ; Disable popups
   ;; The variable-height minibuffer and extra eldoc buffers are distracting. We can limit ElDoc
   ;; messages to one line which prevents the echo area from resizing itself unexpectedly when point
   ;; is on a variable with a multiline docstring, but then it cuts of useful information.
   ;; (setopt eldoc-echo-area-use-multiline-p nil)
   ;; Allow eldoc to trigger after completions
   (with-eval-after-load "company"
     (eldoc-add-command
      'company-complete-selection 'company-complete-common 'company-capf 'company-abort))
   (diminish 'eldoc-mode)))

(elpaca-wait)

;; NOTE: Make a symlink to "private.el" in "$HOME/.emacs.d/etc".
(defcustom sb/private-file (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type 'string
  :group 'sb/emacs)

(use-package exec-path-from-shell
  :when (symbol-value 'sb/IS-LINUX)
  :init
  (setopt
   exec-path-from-shell-check-startup-files
   nil
   exec-path-from-shell-variables
   '("PATH" "JAVA_HOME" "TERM" "PYTHONPATH" "LANG" "LC_CTYPE" "XAUTHORITY" "LSP_USE_PLISTS")
   exec-path-from-shell-arguments
   nil)
  (exec-path-from-shell-initialize))

(use-package emacs
  :ensure nil
  :hook
  ((elpaca-after-init . garbage-collect)
   (prog-mode . auto-fill-mode)
   (elpaca-after-init . save-place-mode))
  :custom
  (ad-redefinition-action 'accept "Turn off warnings due to redefinitions")
  (apropos-do-all t "Make `apropos' search more extensively")
  (auto-save-no-message t "Do not print frequent autosave messages")
  (auto-save-interval 0 "Disable autosaving based on number of characters typed")
  (bookmark-save-flag 1 "Save bookmark after every bookmark edit and also when Emacs is killed")
  (comment-auto-fill-only-comments t "Autofill comments in modes that define them")
  (create-lockfiles nil)
  (custom-safe-themes t)
  (delete-by-moving-to-trash t "Use system trash to deal with mistakes while deleting")
  ;; Accelerate scrolling operations by fontifying only those portions of the buffer which are
  ;; actually going to be displayed.
  (fast-but-imprecise-scrolling t)
  (help-window-select t "Makes it easy to close the window")
  (history-delete-duplicates t)
  (read-process-output-max (* 5 1024 1024) "`lsp-mode' suggests increasing the value")
  (remote-file-name-inhibit-locks t)
  (ring-bell-function 'ignore "Disable beeping sound")
  ;; If you have something on the system clipboard, and then kill something in Emacs, then by
  ;; default whatever you had on the system clipboard is gone and there is no way to get it back.
  ;; Setting the following option makes it so that when you kill something in Emacs, whatever was
  ;; previously on the system clipboard is pushed into the kill ring. This way, you can paste it
  ;; with `yank-pop'.
  (save-interprogram-paste-before-kill t)
  ;; Enable use of system clipboard across Emacs and other applications, does not work on the TUI
  (select-enable-clipboard t)
  (sentence-end-double-space nil)
  (shift-select-mode nil "Do not use `shift-select' for marking, possibly use it for `windmove'")
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
  (x-gtk-resize-child-frames 'resize-mode "Always trigger an immediate resize of the child frame")
  (x-underline-at-descent-line t "Underline looks a bit better when drawn lower")
  (read-buffer-completion-ignore-case t "Ignore case when reading a buffer name")
  (kill-do-not-save-duplicates t "Do not save duplicates to kill ring")
  (tags-add-tables nil)
  (tags-case-fold-search nil "case-sensitive")
  (tags-revert-without-query
   t "Do not ask before rereading the \"TAGS\" files after it has changed")
  (max-mini-window-height 0.35)
  ;; Disable the warning "X and Y are the same file" in case of symlinks
  (find-file-suppress-same-file-warnings t)
  ;; ISSUE: There is a known bug with Emacs upstream.
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=52292
  (find-file-visit-truename nil "Show true name, useful in case of symlinks")
  (auto-mode-case-fold nil "Avoid a second pass through `auto-mode-alist'")
  (backup-inhibited t "Disable backup for a per-file basis")
  (confirm-nonexistent-file-or-buffer t)
  (confirm-kill-emacs nil)
  (confirm-kill-processes nil "Prevent 'Active processes exist' when you quit Emacs")
  (make-backup-files nil "Stop making backup `~' files")
  (require-final-newline t "Always end a file with a newline")
  ;; Unlike `auto-save-mode', `auto-save-visited-mode' saves the buffer contents to the visiting
  ;; file and runs all save-related hooks. We disable `auto-save-mode' and prefer
  ;; `auto-save-visited-mode' instead.
  (auto-save-default nil)
  ;; Save buffer to file after idling for some time, the default of 5s may be too frequent since
  ;; it runs all the save-related hooks.
  (auto-save-visited-interval 30)
  (revert-without-query '("\\.*") "Revert all files without asking")
  (custom-file (no-littering-expand-var-file-name "custom.el"))
  :config
  (dolist (exts
           '(".directory"
             ".dll"
             ".exe"
             ".fdb_latexmk"
             ".fls"
             ".lof"
             ".pyc"
             ".rel"
             ".rip"
             ".synctex.gz"
             "TAGS"))
    (add-to-list 'completion-ignored-extensions exts))

  (when sb/EMACS28+
    (setopt
     next-error-message-highlight t read-minibuffer-restore-windows t
     ;; Hide commands in "M-x" in Emacs 28 which do not work in the current mode.
     read-extended-command-predicate #'command-completion-default-include-p use-short-answers t))

  (when sb/EMACS29+
    (setopt
     help-window-keep-selected t find-sibling-rules
     '(("\\([^/]+\\)\\.c\\'" "\\1.h")
       ("\\([^/]+\\)\\.cpp\\'" "\\1.h")
       ("\\([^/]+\\)\\.h\\'" "\\1.c")
       ("\\([^/]+\\)\\.hpp\\'" "\\1.cpp"))))

  (when sb/IS-WINDOWS
    (setopt w32-get-true-file-attributes nil))

  ;; Disable unhelpful modes, ignore disabling for modes I am not bothered with
  (tooltip-mode -1)

  ;; Enable the following modes
  (dolist
      (mode
       '(auto-save-visited-mode ; Auto-save file-visiting buffers at idle time intervals
         column-number-mode
         delete-selection-mode ; Typing with the mark active will overwrite the marked region
         global-visual-line-mode ; Use soft wraps, wrap lines without the ugly continuation marks
         size-indication-mode
         ;; When you call `find-file', you do not need to clear the existing file path before adding
         ;; the new one. Just start typing the whole path and Emacs will "shadow" the current one. For
         ;; example, you are at "~/Documents/notes/file.txt" and you want to go to
         ;; "~/.emacs.d/init.el", type the latter directly and Emacs will take you there.
         file-name-shadow-mode))
    (when (fboundp mode)
      (funcall mode 1)))

  (setopt
   ;; Scroll settings from Doom Emacs
   scroll-preserve-screen-position t
   scroll-margin 5 ; Add margin lines when scrolling vertically to have a sense of continuity
   scroll-conservatively 101
   ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines
   auto-window-vscroll nil)

  ;; Changing buffer-local variables will only affect a single buffer. `setq-default' changes the
  ;; buffer-local variable's default value.
  (setq-default
   cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
   fill-column sb/fill-column
   indent-tabs-mode nil ; Spaces instead of tabs
   tab-width 4
   ;; TAB first tries to indent the current line, and if the line was already indented,
   ;; then try to complete the thing at point.
   tab-always-indent 'complete
   bidi-inhibit-bpa nil ; Disabling BPA makes redisplay faster
   bidi-paragraph-direction 'left-to-right)

  (diminish 'auto-fill-function) ; Not a library/file, so `eval-after-load' does not work

  (advice-add 'risky-local-variable-p :override #'ignore)

  ;; Hide "When done with a buffer, type C-x 5" message
  (when (bound-and-true-p server-client-instructions)
    (setopt server-client-instructions nil))

  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))
  (when (file-exists-p sb/private-file)
    (load sb/private-file 'noerror 'nomessage))

  ;; Mark safe variables
  (put 'compilation-read-command 'safe-local-variable #'stringp)
  (put 'reftex-default-bibliography 'safe-local-variable #'stringp)
  :diminish visual-line-mode)

(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  ;; Revert dired buffers if the contents of the directory changes
  (global-auto-revert-non-file-buffers t)
  :diminish auto-revert-mode)

(use-package savehist
  :ensure nil
  :hook (elpaca-after-init . savehist-mode)
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
  :ensure nil
  :hook (elpaca-after-init . abbrev-mode)
  :custom
  (abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory))
  (save-abbrevs 'silently)
  :diminish)

;; This puts the buffer in read-only mode and disables font locking, revert with "C-c C-c".
(use-package so-long
  :ensure nil
  :when sb/EMACS28+
  :hook (elpaca-after-init . global-so-long-mode))

(use-package imenu
  :ensure nil
  :after (:any makefile-mode markdown-mode org-mode yaml-mode yaml-ts-mode prog-mode)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items 1000)
  (imenu-use-popup-menu nil))

(use-package recentf
  :ensure nil
  :hook (elpaca-after-init . recentf-mode)
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
     "*[/\\]straight/repos/*"))
  ;; Keep remote file without testing if they still exist
  (recentf-keep '(file-remote-p file-readable-p))
  ;; Larger values help in lookup but takes more time to check if the files exist
  (recentf-max-saved-items 250)
  :config
  ;; Abbreviate the home directory to "~/" to make it easy to read the actual file name.
  (unless sb/EMACS28+
    (setopt recentf-filename-handlers '(abbreviate-file-name)))

  ;; Use the true file name and not the symlink name
  (dolist (exclude
           `(,(recentf-expand-file-name no-littering-etc-directory)
             ,(recentf-expand-file-name no-littering-var-directory)
             ;; ,(recentf-expand-file-name (straight--emacs-dir "straight"))
             ))
    (add-to-list 'recentf-exclude exclude))

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
  ;; after idling for a few seconds.
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

;; Use "Shift + direction" arrows for moving around windows. I also use the "Shift + direction"
;; keybindings for moving around windows in Tmux which is okay because I do not split Emacs frames
;; often.
(use-package windmove
  :ensure nil
  :when (display-graphic-p)
  :init (windmove-default-keybindings))

(use-package doc-view
  :ensure nil
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

;; Highlight and allow to open http links in strings and comments in buffers.
(use-package goto-addr
  :ensure nil
  :hook ((prog-mode . goto-address-prog-mode) (text-mode . goto-address-mode))
  :bind ("C-c RET" . goto-address-at-point))

(use-package ediff
  :ensure nil
  :hook (ediff-cleanup . (lambda () (ediff-janitor nil nil)))
  :custom
  ;; Put the control panel in the same frame as the diff windows
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally "Split diffs side by side")
  (ediff-keep-variants nil "Prompt and kill file variants on quitting an Ediff session"))

;; To edit remote files, use "/method:user@host#port:filename". The shortcut "/ssh::" will connect
;; to default "user@host#port". To edit a local file with sudo, use "C-x C-f /sudo::/etc/hosts". To
;; open a remote file with ssh + sudo, use "C-x C-f /ssh:host|sudo:root:/etc/passwd".
;; Multihop syntax: "C-x C-f /ssh:bird@bastion|ssh:you@remotehost:/path"
;; Multihop with sudo: "C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file"
;; Multihop with sudo with custom user: "C-x C-f
;; /ssh:you@remotehost|sudo:them@remotehost:/path/to/file"
;; Sudo over ssh: "emacs -nw /ssh:user@172.16.42.1\|sudo:172.16.42.1:/etc/hosts"
(use-package tramp
  :ensure nil
  :bind ("C-S-q" . tramp-cleanup-all-buffers)
  :custom
  (remote-file-name-inhibit-cache nil "Remote files are not updated outside of Tramp")
  (tramp-verbose 1 "Only errors and warnings")
  :config
  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Recommended to connect with Bash
  (setenv "SHELL" shell-file-name)
  (setopt debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))

(use-package whitespace
  :ensure nil
  :custom (whitespace-line-column sb/fill-column)
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode))

(use-package ibuffer
  :ensure nil
  :hook (ibuffer . ibuffer-auto-mode)
  :bind (("C-x C-b" . ibuffer-jump) :map ibuffer-mode-map ("`" . ibuffer-switch-format))
  :custom
  (ibuffer-display-summary nil)
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-show-empty-filter-groups nil "Do not show empty groups if there are no buffers")
  (ibuffer-formats '((mark modified read-only " " (name 24 24 :left :elide) " " filename)))
  :config
  (require 'ibuf-ext)
  (add-to-list 'ibuffer-never-show-predicates "^\\*")
  (defalias 'list-buffers 'ibuffer))

;; Provides ibuffer filtering and sorting functions to group buffers by function or regexp applied
;; to `default-directory'. By default buffers are grouped by `project-current' or by
;; `default-directory'.
(use-package ibuffer-project
  :hook
  (ibuffer
   .
   (lambda ()
     (setopt ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative))))
  :custom (ibuffer-project-use-cache t "Avoid calculating project root, use cache")
  :config
  ;; Remote buffers will be grouped by protocol and host
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote")))

(use-package immortal-scratch
  :hook (elpaca-after-init . immortal-scratch-mode))

(use-package persistent-scratch
  :hook
  (elpaca-after-init
   .
   (lambda ()
     (ignore-errors
       (persistent-scratch-setup-default))))
  :config (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

(use-package popwin
  :hook (elpaca-after-init . popwin-mode)
  :config (push '(helpful-mode :noselect t :position bottom :height 20) popwin:special-display-config))

(use-package ace-window
  :bind (([remap other-window] . ace-window) ("M-o" . ace-window))
  :custom (aw-minibuffer-flag t)
  :config
  (add-to-list 'aw-ignored-buffers "*toc*")
  (ace-window-display-mode 1))

(use-package ace-jump-buffer
  :bind ("C-b" . ace-jump-buffer)
  :custom (ajb-bs-configuration "files-and-scratch"))

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
  :ensure nil
  :hook
  ((dired-mode . auto-revert-mode) ; Auto refresh dired when files change
   (dired-mode . dired-hide-details-mode))
  :bind
  (:map
   dired-mode-map
   ("M-<home>" . sb/dired-go-home)
   ("M-<up>" . sb/dired-jump-to-top)
   ("M-<down>" . sb/dired-jump-to-bottom)
   ("i" . find-file))
  :custom
  ;; When there are two dired buffer windows in the same frame, Emacs will select the other buffer
  ;; as the target directory (e.g., for copying or renaming files).
  (dired-dwim-target t)
  (dired-auto-revert-buffer t "Revert each dired buffer automatically when you revisit it")
  ;; "A" is to avoid listing "." and "..", "B" is to avoid listing backup entries ending with "~",
  ;; "F" appends indicator to entries, "g" omits the owner, "h" is to print human-readable sizes,
  ;; "N" prints entry names without quoting, "si" is to use powers of 1000 not 1024, "o" does not
  ;; print group information, "p" is to append "/" indicator to directories, "v" uses natural sort
  ;; of (version) numbers within text. Check "ls" for additional options.
  (dired-listing-switches "-ABFghlNopv --group-directories-first --time-style=locale")
  (dired-ls-F-marks-symlinks t "-F marks links with @")
  (dired-recursive-copies 'always "Single prompt for all n directories")
  (dired-recursive-deletes 'always "Single prompt for all n directories")
  ;; Do not ask whether to kill buffers visiting deleted files
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-hide-details-hide-symlink-targets nil)
  :config
  (when (boundp 'dired-kill-when-opening-new-dired-buffer)
    (setopt dired-kill-when-opening-new-dired-buffer t)))

(use-package dired-x
  :ensure nil
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
  (unless sb/EMACS28+
    (setopt dired-bind-jump t))

  (setopt
   dired-omit-files
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
  :ensure (:host github :repo "karthink/dired-hist")
  :hook (dired-mode . dired-hist-mode)
  :bind (:map dired-mode-map ("l" . dired-hist-go-back) ("r" . dired-hist-go-forward)))

(use-package vertico
  :ensure
  (vertico
   :files (:defaults "extensions/*")
   :includes (vertico-directory vertico-repeat vertico-quick))
  :hook
  ((elpaca-after-init . vertico-mode)
   ;; Tidy shadowed file names. That is, when using a command for selecting a file in the minibuffer,
   ;; the following fixes the path so the selected path does not have prepended junk left behind.
   ;; This works with `file-name-shadow-mode' enabled. When you are in a sub-directory and use, say,
   ;; `find-file' to go to your home '~/' or root '/' directory, Vertico will clear the old path to
   ;; keep only your current input.
   (rfn-eshadow-update-overlay . vertico-directory-tidy) (minibuffer-setup . vertico-repeat-save))
  :bind
  (("C-c r" . vertico-repeat-last)
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
  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history)))

;; Useful search and navigation commands
(use-package consult
  :after vertico
  :commands consult-fd
  :bind
  ( ;; Press "SPC" to show ephemeral buffers, "b SPC" to filter by buffers, "f SPC" to filter by
   ;; files, "p SPC" to filter by projects. If you press "DEL" afterwards, the full candidate list
   ;; will be shown again.
   ([remap switch-to-buffer] . consult-buffer)
   ("<f3>" . consult-buffer)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ([remap apropos] . consult-apropos)
   ([remap goto-line] . consult-goto-line)
   ([remap bookmark-jump] . consult-bookmark)
   ("M-g o" . consult-outline)
   ("C-c C-m" . consult-mark)
   ([remap imenu] . consult-imenu) ; "M-g i"
   ("C-c C-j" . consult-imenu)
   ([remap customize] . consult-customize)
   ([remap load-theme] . consult-theme)
   ("C-c s f" . consult-find)
   ([remap locate] . consult-locate)
   ("C-c s l" . consult-locate)
   ;; Prefix argument "C-u" allows to specify the directory. You can pass additional grep flags to
   ;; `consult-grep' with the "--" separator. E.g.: "foo bar -- -A3" to get matches with 3 lines of
   ;; 'after' context.
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
  (consult-preview-key nil "Disable preview by default, enable for selected commands")
  (completion-in-region-function #'consult-completion-in-region "Complete M-:")
  ;; Having multiple other sources like recentf makes it difficult to identify and switch quickly
  ;; between only buffers, especially while wrapping around.
  (consult-buffer-sources '(consult--source-buffer))
  (consult-narrow-key "<")
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
     (buffer-substring-no-properties (region-beginning) (region-end)))))

(use-package embark
  :bind
  (([remap describe-bindings] . embark-bindings) ; "C-h b"
   ("C-`" . embark-act)
   ;; ("C-`" . embark-dwim)
   :map
   minibuffer-local-map
   ("C-`" . embark-act)
   ("C-c C-c" . embark-collect)
   ("C-c C-e" . embark-export)
   :map
   minibuffer-local-completion-map
   ("C-`" . embark-act)
   :map embark-file-map
   ;; ("s" . sudo-edit)
   ("l" . vlf))
  :custom
  ;; Replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :config
  (with-eval-after-load "vertico"
    (bind-keys :map vertico-map ("C-`" . embark-act) ("C-c C-e" . embark-export)))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list
   'display-buffer-alist
   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
     nil
     (window-parameters (mode-line-format . none)))))

;; Supports exporting search results to a `grep-mode' buffer, on which you can use `wgrep'.
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Rich annotations in the minibuffer, e.g., documentation strings or file information.
(use-package marginalia
  :after vertico
  :init (marginalia-mode 1)
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :config
  (setopt marginalia-annotator-registry (assq-delete-all 'file marginalia-annotator-registry))
  (add-to-list 'marginalia-annotator-registry '(symbol-help marginalia-annotate-variable))
  (add-to-list 'marginalia-annotator-registry '(project-buffer marginalia-annotate-project-buffer)))

;; ":after consult" prevents `consult-tramp' keybinding from being registered
(use-package consult-tramp
  :ensure (:host github :repo "Ladicle/consult-tramp")
  :bind ("C-c d t" . consult-tramp))

(use-package consult-flyspell
  :after (consult flyspell)
  :bind ("C-c f l" . consult-flyspell)
  :config
  (setopt
   consult-flyspell-select-function
   (lambda ()
     (flyspell-correct-at-point)
     (consult-flyspell))))

(use-package consult-flycheck
  :after (consult flycheck)
  :bind (:map flycheck-command-map ("!" . consult-flycheck)))

(use-package consult-yasnippet
  :bind ("C-M-y" . consult-yasnippet))

(use-package ispell
  :ensure nil
  :bind ("M-$" . ispell-word)
  :custom
  (ispell-dictionary "en_US")
  (ispell-local-dictionary "en_US")
  (ispell-personal-dictionary (expand-file-name "spell" sb/extras-directory))
  (ispell-alternate-dictionary (expand-file-name "wordlist.5" sb/extras-directory))
  (ispell-silently-savep t "Save a new word to personal dictionary without asking")
  :config
  (cond
   ((and (symbol-value 'sb/IS-LINUX) (executable-find "hunspell"))
    (progn
      (setenv "LANG" "en_US")
      (setenv "DICTIONARY" "en_US")
      (setenv "DICPATH" `,(concat user-emacs-directory "hunspell"))
      (setopt
       ispell-program-name
       "hunspell"
       ispell-local-dictionary-alist
       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
       ispell-hunspell-dictionary-alist
       ispell-local-dictionary-alist
       ispell-hunspell-dict-paths-alist
       `(("en_US" ,(concat user-emacs-directory "hunspell/en_US.aff"))))))
   ((and (symbol-value 'sb/IS-LINUX) (executable-find "aspell"))
    (progn
      (setopt
       ispell-program-name
       "aspell"
       ispell-extra-args
       '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90"))))
   ((and (symbol-value 'sb/IS-WINDOWS) (executable-find "hunspell"))
    (progn
      (setenv "LANG" "en_US")
      (setenv "DICTIONARY" "en_US")
      (setenv "DICPATH" `,(concat user-emacs-directory "hunspell"))
      (setopt
       ispell-program-name
       "hunspell"
       ispell-local-dictionary-alist
       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
       ispell-hunspell-dictionary-alist
       ispell-local-dictionary-alist
       ispell-hunspell-dict-paths-alist
       `(("en_US" ,(concat user-emacs-directory "hunspell/en_US.aff")))))))

  ;; Skip regions in `org-mode'
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXAMPLE" . "^#\\+END_EXAMPLE"))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("\:PROPERTIES\:$" . "\:END\:$"))
  ;; Footnotes in org that have http links that are line breaked should not be ispelled
  (add-to-list 'ispell-skip-region-alist '("^http" . "\\]"))
  (add-to-list 'ispell-skip-region-alist '("`" "`"))
  (add-to-list 'ispell-skip-region-alist '("cite:" . "[[:space:]]"))
  (add-to-list 'ispell-skip-region-alist '("label:" . "[[:space:]]"))
  (add-to-list 'ispell-skip-region-alist '("ref:" . "[[:space:]]"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{multline}" . "\\\\end{multline}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{equation}" . "\\\\end{equation}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{align}" . "\\\\end{align}"))

  ;; Hide the "Starting new Ispell process" message
  (advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)
  (advice-add 'ispell-lookup-words :around #'sb/inhibit-message-call-orig-fun))

;; As of Emacs 29, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.
(use-package flyspell
  :ensure nil
  :hook ((prog-mode . flyspell-prog-mode) (text-mode . flyspell-mode))
  :bind ("C-c f b" . flyspell-buffer)
  :custom
  (flyspell-abbrev-p t "Add corrections to abbreviation table")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :diminish)

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-at-point) ("C-," . flyspell-correct-previous)))

;; "M-$" triggers correction for the misspelled word before point, "C-u M-$" triggers correction for
;; the entire buffer.
(use-package jinx
  :when (symbol-value 'sb/IS-LINUX)
  :hook (text-mode . jinx-mode)
  :bind (([remap ispell-word] . jinx-correct) ("C-M-$" . jinx-languages))
  :custom (jinx-languages "en_US")
  :diminish)

(use-package helpful
  :bind
  (([remap describe-variable] . helpful-variable) ; "C-h v"
   ;; The built-in `describe-function' includes both functions and macros. `helpful-function' is only
   ;; for functions, so we use `helpful-callable' as a replacement.
   ([remap describe-function] . helpful-callable) ; "C-h f"
   ([remap describe-symbol] . helpful-symbol) ; "C-h o"
   ([remap describe-key] . helpful-key) ; "C-h k"
   ("C-h c" . helpful-command) ("C-h p" . helpful-at-point)
   :map helpful-mode-map ("q" . helpful-kill-buffers)))

;; Erase all consecutive white space characters in a given direction
(use-package hungry-delete
  :hook
  ((minibuffer-setup . (lambda () (hungry-delete-mode -1)))
   (elpaca-after-init . global-hungry-delete-mode))
  :diminish)

;; Move lines with "M-<up>" and "M-<down>"
(use-package move-text
  :bind (("M-<down>" . move-text-down) ("M-<up>" . move-text-up)))

;; Expand region by semantic units
(use-package expand-region
  :bind (("C-=" . er/expand-region) ("C-M-=" . er/contract-region)))

(use-package change-inner
  :commands (change-inner change-outer))

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  :hook (elpaca-after-init . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :hook (elpaca-after-init . whole-line-or-region-global-mode)
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
   ("C-n" . vundo-next) ("C-p" . vundo-previous)))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :bind* ("C-." . iedit-mode))

(use-package hl-todo
  :ensure (:depth nil)
  :hook (elpaca-after-init . global-hl-todo-mode)
  :bind (("C-c p" . hl-todo-previous) ("C-c n" . hl-todo-next))
  :config
  (setopt
   hl-todo-highlight-punctuation ":" hl-todo-keyword-faces
   (append
    '(("LATER" . "#d0bf8f")
      ("IMP" . "#7cb8bb")
      ("ISSUE" . "#ff8c00")
      ("DEBUG" . "#ff8c00")
      ("TEST" . "tomato")
      ("WARNING" . "#cc0000")
      ("BEWARE" . "#aa0000")
      ("REFACTOR" . "#cc9393"))
    hl-todo-keyword-faces)))

;; Save a bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark, use `bookmark-jump'
;; ("C-x r b") or `bookmark-bmenu-list' ("C-x r l"). Rename the bookmarked location in
;; `bookmark-bmenu-mode' with `R'.
(use-package bm
  :init (setopt bm-verbosity-level 1 bm-modeline-display-total t)
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
   (elpaca-after-init . bm-repository-load))
  :bind (("C-<f1>" . bm-toggle) ("C-<f3>" . bm-next) ("C-<f2>" . bm-previous))
  :custom (bm-buffer-persistence t "Save bookmarks"))

(use-package crux
  :bind (("<f12>" . crux-kill-other-buffers) ("C-c d s" . crux-sudo-edit))
  :bind* ("C-c C-d" . crux-duplicate-current-line-or-region))

;; (use-package rainbow-mode
;;   :hook ((LaTeX-mode css-mode css-ts-mode html-mode html-ts-mode web-mode help-mode) . rainbow-mode)
;;   :diminish)

(use-package colorful-mode
  :hook
  ((LaTeX-mode css-mode css-ts-mode html-mode html-ts-mode web-mode help-mode helpful-mode)
   .
   colorful-mode)
  :diminish)

;; Temporarily highlight the region involved in certain operations like `kill-line' and `yank'.
(use-package volatile-highlights
  :hook (elpaca-after-init . volatile-highlights-mode)
  :diminish volatile-highlights-mode)

(use-package xclip
  :when (or (executable-find "xclip") (executable-find "xsel"))
  :hook (elpaca-after-init . xclip-mode))

;; Allow GC to happen after a period of idle time
(use-package gcmh
  :hook (elpaca-after-init . gcmh-mode)
  :diminish)

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :diminish)

;; Both project.el and projectile are unable to remember remote projects.
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
  :custom (project-switch-commands 'project-find-file "Start `project-find-file' by default"))

(use-package project-x
  :ensure (:host github :repo "karthink/project-x")
  :after project
  :demand t
  :config (project-x-mode 1))

(use-package isearch
  :ensure nil
  :bind
  (("C-s")
   ("C-M-f") ; Was bound to `isearch-forward-regexp', but we use it for `forward-sexp'
   ("C-f" . isearch-forward-regexp) ("C-r" . isearch-backward-regexp)
   :map isearch-mode-map ("C-s") ("C-f" . isearch-repeat-forward) ("C-c C-o" . isearch-occur))
  :custom (isearch-lazy-count t "Show match count"))

;; Auto populate `isearch' with the symbol at point
(use-package isearch-symbol-at-point
  :commands (isearch-forward-symbol-at-point isearch-backward-symbol-at-point)
  :bind (("M-s ." . isearch-symbol-at-point) ("M-s _" . isearch-forward-symbol)))

(with-eval-after-load "grep"
  (setopt grep-command "grep --color -irHn " grep-highlight-matches t grep-scroll-output t)

  (when (executable-find "rg")
    (setopt grep-program "rg")
    (grep-apply-setting 'grep-find-command '("rg -n -H --no-heading -e" . 27)))

  (dolist (dirs '(".cache" "node_modules" "vendor" ".clangd"))
    (add-to-list 'grep-find-ignored-directories dirs)))

;; Writable grep. When the "*grep*" buffer is huge, `wgrep-change-to-wgrep-mode' might freeze
;; Emacs for several minutes.
(use-package wgrep
  ;; Allows you to edit a deadgrep buffer and apply those changes to the file buffer.
  :hook (deadgrep-finished . wgrep-deadgrep-setup)
  :bind
  (:map
   grep-mode-map ; These keybindings are also defined in `wgrep-mode-map'
   ("C-x C-p" . wgrep-change-to-wgrep-mode)
   ("C-x C-s" . wgrep-finish-edit)
   ("C-x C-k" . wgrep-abort-changes)
   ("C-x C-q" . wgrep-exit))
  :custom (wgrep-auto-save-buffer t))

;; `consult-rg' provides live search, while `deadgrep' provides a resulting search buffer.
(use-package deadgrep
  :bind ("C-c s d" . deadgrep)
  :custom (deadgrep-max-buffers 1))

(use-package avy
  :bind
  (("C-\\" . avy-goto-word-1)
   ("C-'" . avy-goto-char-timer)
   ("C-/" . avy-goto-line)
   ("C-M-c" . avy-copy-line)
   ("C-M-m" . avy-move-line)
   :map isearch-mode-map
   ;; Use "C-'" in `isearch-mode-map' to use `avy-isearch' to select one of the many currently
   ;; visible `isearch' candidates.
   ("C-'" . avy-isearch)))

;; Package `visual-regexp' provides an alternate version of `query-replace' which highlights matches
;; and replacements as you type.
(use-package visual-regexp
  :bind
  ([remap query-replace] . vr/query-replace)
  ([remap replace-regex] . vr/replace))

(use-package vc-hooks
  :ensure nil
  :custom (vc-handled-backends '(Git))
  ;; (vc-follow-symlinks t "No need to ask")
  ;; Disable version control for remote files to improve performance
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))

(use-package transient
  :custom (transient-semantic-coloring t)
  :config
  ;; Allow using `q' to quit out of popups in addition to `C-g'
  (transient-bind-q-to-quit))

(use-package magit
  :bind (("C-x g" . magit-status) ("C-c M-g" . magit-file-dispatch) ("C-x M-g" . magit-dispatch))
  :custom
  ;; Open the status buffer in a full frame
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  ;; Suppress the message "Turning on magit-auto-revert-mode" when loading Magit
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  ;; https://irreal.org/blog/?p=8877
  (magit-section-initial-visibility-alist
   '((stashes . show) (untracked . show) (unpushed . show) (unpulled . show)))
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk t "Show fine differences for the current diff hunk only"))

(use-package difftastic
  :commands (difftastic-files difftastic-dired-diff difftastic-magit-diff)
  :bind
  (:map
   difftastic-mode-map
   ("e" . difftastic-leave)
   ("g" . difftastic-rerun)
   ("p" . difftastic-previous-chunk)
   ("n" . difftastic-next-chunk)
   ("q" . difftastic-quit)))

(use-package git-modes
  :mode ("dotgitconfig" . gitconfig-mode)
  :mode ("/\\.gitignore\\'" . gitignore-mode)
  :mode ("/\\.gitattributes\\'" . gitattributes-mode))

(use-package diff-hl
  :when (boundp 'vc-handled-backends)
  :hook
  (
   ;; Display in the margin since the fringe is unavailable in TTY
   (diff-hl-mode-on
    .
    (lambda ()
      (unless (display-graphic-p)
        (diff-hl-margin-local-mode 1))))
   (dired-mode . diff-hl-dired-mode-unless-remote) (elpaca-after-init . global-diff-hl-mode))
  :bind (("C-x v [" . diff-hl-previous-hunk) ("C-x v ]" . diff-hl-next-hunk))
  :custom (diff-hl-draw-borders nil "Highlight without a border looks nicer")
  :config
  (diff-hl-flydiff-mode 1) ; For unsaved buffers

  (with-eval-after-load "magit"
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)))

;; Use "M-p/n" to cycle between older commit messages.
(use-package git-commit
  :hook
  (git-commit-setup
   .
   (lambda ()
     (git-commit-save-message)
     (git-commit-turn-on-auto-fill)
     (git-commit-turn-on-flyspell))))

;; Use the minor mode `smerge-mode' to move between conflicts and resolve them.
(use-package smerge-mode
  :ensure nil
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

(use-package paren
  :ensure nil
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package elec-pair
  :ensure nil
  :hook
  ((elpaca-after-init . electric-pair-mode)
   ;; Disable pairs when entering minibuffer
   (minibuffer-setup . (lambda () (electric-pair-local-mode -1))))
  :custom
  ;; Avoid balancing parentheses since they can be both irritating and slow
  (electric-pair-preserve-balance nil)
  :config
  (setopt
   electric-pair-inhibit-predicate
   (lambda (c)
     (if (char-equal c ?\")
         t
       (electric-pair-default-inhibit c))))

  (defvar sb/markdown-pairs '((?` . ?`))
    "Electric pairs for `markdown-mode'.")

  (defun sb/add-markdown-pairs ()
    "Add custom pairs to `markdown-mode'."
    (setq-local electric-pair-pairs (append electric-pair-pairs sb/markdown-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (add-hook 'markdown-mode-hook #'sb/add-markdown-pairs)

  (defvar sb/latex-pairs '((?\{ . ?\}) (?\[ . ?\]) (?\( . ?\)))
    "Electric pairs for `LaTeX-mode'.")

  (defun sb/add-latex-pairs ()
    "Add custom pairs to `LaTeX-mode'."
    (setq-local electric-pair-pairs (append electric-pair-pairs sb/latex-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (add-hook 'LaTeX-mode-hook #'sb/add-latex-pairs))

;; Discover key bindings for the current Emacs major mode
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package mode-minder
  :ensure (:host github :repo "jdtsmith/mode-minder")
  :commands mode-minder)

(use-package flycheck
  :hook (elpaca-after-init . global-flycheck-mode)
  :custom
  ;; Remove newline checks, since they would trigger an immediate check when we want the
  ;; `flycheck-idle-change-delay' to be in effect while editing.
  (flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change))
  (flycheck-checker-error-threshold nil)
  (flycheck-idle-buffer-switch-delay 2 "Increase the time (s) to allow for quick transitions")
  (flycheck-idle-change-delay 2 "Increase the time (s) to allow for transient edits")
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-global-modes '(not csv-mode conf-mode))
  :config
  (dolist (checkers '(proselint textlint tex-chktex emacs-lisp-checkdoc))
    (delq checkers flycheck-checkers))

  ;; These themes have their own styles for displaying flycheck info.
  (when (eq sb/modeline-theme 'doom-modeline)
    (setopt flycheck-mode-line nil))

  (setq-default
   flycheck-markdown-markdownlint-cli-config
   (expand-file-name ".markdownlint.json" sb/user-home-directory)
   flycheck-pylintrc '("setup.cfg" "pylintrc")
   flycheck-python-pylint-executable "python3"
   flycheck-shellcheck-follow-sources nil)

  ;; https://github.com/flycheck/flycheck/issues/1833
  (add-to-list 'flycheck-hooks-alist '(after-revert-hook . flycheck-buffer))

  ;; Chain flycheck checkers with lsp, using per-project directory local variables.
  ;; https://github.com/flycheck/flycheck/issues/1762
  (defvar-local sb/flycheck-local-checkers nil)
  (defun sb/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker sb/flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get))

(use-package format-all
  :hook
  ((format-all-mode . format-all-ensure-formatter)
   ((markdown-mode markdown-ts-mode) . format-all-mode))
  :bind (:map markdown-mode-map ("C-x f" . format-all-buffer))
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
                  ("HTML" tidy)
                  ("LaTeX" latexindent)
                  ("Markdown" (prettier "--print-width" "100"))
                  ("Perl"
                   (perltidy "--quiet" "--standard-error-output" "--perl-best-practices" "-l=100"))
                  ("Python" (yapf "--style" "file") isort)
                  ("Shell" (shfmt "-i" "4" "-ci"))
                  ("XML" tidy)
                  ("YAML" prettier "--print-width" "100")))
  :diminish)

(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
  :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode) . indent-bars-mode)
  :config
  (when (executable-find "tree-sitter")
    (setopt
     indent-bars-treesit-support
     t
     indent-bars-treesit-ignore-blank-lines-types
     '("module")
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
  :commands consult-todo)

;; "basic" matches only the prefix, "substring" matches the whole string. "initials" matches
;; acronyms and initialisms, e.g., can complete "M-x lch" to "list-command-history".
;; "partial-completion" style allows to use wildcards for file completion and partial paths, e.g.,
;; "/u/s/l" for "/usr/share/local". While "partial-completion" matches search terms must match in
;; order, "orderless" can match search terms in any order.
(use-package minibuffer
  :ensure nil
  :bind
  (("M-p" . minibuffer-previous-completion)
   ("M-n" . minibuffer-next-completion)
   ("M-RET" . minibuffer-choose-completion))
  :custom
  (read-file-name-completion-ignore-case t "Ignore case when reading a file name")
  (completion-cycle-threshold 3 "TAB cycle if there are only few candidates")
  :config
  ;; Show docstring description for completion candidates in commands like `describe-function'.
  (when sb/EMACS28+
    (setopt completions-detailed t))

  (when (fboundp 'dabbrev-capf)
    (add-to-list 'completion-at-point-functions 'dabbrev-capf t))

  (with-eval-after-load "orderless"
    ;; substring is needed to complete common prefix, orderless does not
    (setopt completion-styles '(orderless substring basic)))

  ;; The "basic" completion style needs to be tried first for TRAMP hostname completion to
  ;; work. I also want substring matching for file names.
  (setopt completion-category-overrides '((file (styles basic substring partial-completion)))))

;; Use "C-M-;" for `dabbrev-completion' which finds all expansions in the current buffer and
;; presents suggestions for completion.
(use-package dabbrev
  :ensure nil
  :bind ("C-M-;" . dabbrev-completion)
  :custom
  (dabbrev-ignored-buffer-regexps
   '("^ "
     "\\.\\(?:jpe?g\\|png\\)\\'"
     "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (dabbrev-upcase-means-case-search t)
  :config
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package hippie-exp
  :ensure nil
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
    (advice-add 'company-capf--candidates :around #'sb/just-one-face)))

(use-package yasnippet
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (elpaca-after-init . yas-global-mode)
  :custom
  (yas-verbosity 0)
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (with-eval-after-load "hippie-expand"
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  (unbind-key "<tab>" yas-minor-mode-map)
  :diminish yas-minor-mode)

;; YASnippet no longer bundles snippets directly
(use-package yasnippet-snippets
  :after yasnippet
  :init (yasnippet-snippets-initialize))

;; Use "M-x company-diag" or the modeline status (without diminish) to see the backend used for the
;; last completion. Try "M-x company-complete-common" when there are no completions. Use "C-M-i" for
;; `complete-symbol' with regex search.
(use-package company
  :hook (elpaca-after-init . global-company-mode)
  :bind
  (("C-M-/" . company-other-backend) ; Invoke the next backend in `company-backends'
   :map
   company-active-map
   ("C-M-/" . company-other-backend)
   ("C-s" . company-search-candidates)
   ("C-M-s" . company-filter-candidates)
   ("<tab>" . company-complete-common-or-cycle)
   ("TAB" . company-complete-common-or-cycle)
   ("<backtab>" .
    (lambda ()
      (interactive)
      (company-complete-common-or-cycle -1)))
   ;; ([escape] . company-abort)
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
  (company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory))
  (company-show-quick-access t "Speed up selecting a completion")
  (company-tooltip-align-annotations t)
  (company-global-modes
   '(not dired-mode magit-status-mode help-mode csv-mode minibuffer-inactive-mode))
  (company-format-margin-function nil "Disable icons")
  (company-selection-wrap-around t "Convenient to wrap around completion items at boundaries"))

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

(use-package company-auctex
  :after (:all tex-mode company)
  :demand t)

(use-package company-math
  :after (:all tex-mode company)
  :demand t)

(use-package company-reftex
  :after (:all tex-mode company)
  :demand t
  :custom
  ;; https://github.com/TheBB/company-reftex/pull/13
  (company-reftex-labels-parse-all nil))

(use-package company-anywhere
  :ensure (:host github :repo "zk-phi/company-anywhere")
  :after company
  :demand t)

(use-package company-dict
  :after company
  :demand t
  :custom
  (company-dict-dir (expand-file-name "company-dict" user-emacs-directory))
  (company-dict-enable-yasnippet nil))

(use-package company-org-block
  :after (company org)
  :demand t)

(use-package company-c-headers
  :after (company cc-mode)
  :demand t
  :custom (company-c-headers-path-system '("/usr/include/c++/11" "/usr/include" "/usr/local/include")))

(use-package company-web
  :after company
  :demand t
  :config (require 'company-web-html))

(use-package company-bibtex
  :ensure (:host github :repo "gbgar/company-bibtex")
  :after (:all tex-mode company)
  :demand t)

;; Try completion backends in order untill there is a non-empty completion list:
;; (setq company-backends '(company-xxx company-yyy company-zzz))

;; Merge completions of all the backends:
;; (setq company-backends '((company-xxx company-yyy company-zzz)))

;; Merge completions of all the backends but keep the candidates organized in accordance with the
;; grouped backends order.
;; (setq company-backends '((company-xxx company-yyy company-zzz :separate)))

;; The keyword :with helps to make sure the results from major/minor mode agnostic backends (such as
;; company-yasnippet, company-dabbrev-code) are returned without preventing results from
;; context-aware backends (such as company-capf or company-clang). For this feature to work, put
;; backends dependent on a mode at the beginning of the grouped backends list, then put a keyword
;; :with, and only then put context agnostic backend(s).
;; (setq company-backends '((company-capf :with company-yasnippet)))

;; Most backends (e.g., `company-yasnippet') will not pass control to subsequent backends . Only a
;; few backends are specialized on certain major modes or certain contexts (e.g. outside of strings
;; and comments), and pass on control to later backends when outside of that major mode or context.

;; A few backends are applicable to all modes: `company-yasnippet', `company-ispell',
;; `company-dabbrev-code', and `company-dabbrev'. `company-yasnippet' is blocking. `company-dabbrev'
;; returns a non-nil prefix in almost any context (major mode, inside strings or comments). That is
;; why it is better to put `company-dabbrev' at the end. The ‘prefix’ bool command always returns
;; non-nil for following backends even when their ‘candidates’ list command is empty:
;; `company-abbrev', `company-dabbrev', `company-dabbrev-code'.

;; Company does not support grouping of entirely arbitrary backends, they need to be compatible in
;; what `prefix' returns. If the group contains keyword `:with', the backends listed after this
;; keyword are ignored for the purpose of the `prefix' command. If the group contains keyword
;; `:separate', the candidates that come from different backends are sorted separately in the
;; combined list. That is, with `:separate', the multi-backend-adapter will stop sorting and keep
;; the order of completions just like the backends returned them.

(with-eval-after-load "company"
  ;; Override `company-backends' for unhandled major modes.
  (setq
   company-backends
   '(company-files
     (company-capf :with company-dabbrev-code company-yasnippet)
     ;; If we have `company-dabbrev' first, then other matches from `company-ispell' will be
     ;; ignored.
     company-ispell company-dict company-dabbrev)
   ;; Options: `company-sort-prefer-same-case-prefix', `company-sort-by-occurrence',
   ;; `company-sort-by-statistics', `company-sort-by-length', `company-sort-by-backend-importance',
   ;; `delete-dups'.
   company-transformers '(delete-dups company-sort-by-statistics company-sort-prefer-same-case-prefix))

  ;; Ignore matches from `company-dabbrev' that consist solely of numbers
  ;; https://github.com/company-mode/company-mode/issues/358
  (push (apply-partially #'cl-remove-if (lambda (c) (string-match-p "\\`[0-9]+\\'" c)))
        company-transformers)

  (progn
    (defun sb/company-latex-mode ()
      (make-local-variable 'company-backends)

      ;; `company-capf' does not pass to later backends with Texlab, so it makes it difficult to
      ;; complete non-LaTeX commands (e.g. words) which is the majority. We therefore exclude it
      ;; from `company-backends' and invoke it on demand.
      (setq company-backends
            '(company-files
              company-bibtex
              company-reftex-citations
              company-math-symbols-latex ; Math latex tags
              company-latex-commands
              company-reftex-labels
              company-auctex-environments
              company-auctex-macros
              company-auctex-labels
              company-math-symbols-unicode ; Math unicode symbols and sub(super)scripts
              company-auctex-symbols
              company-auctex-bibs
              company-ispell
              company-dict
              company-dabbrev)))

    (add-hook 'LaTeX-mode-hook (lambda () (sb/company-latex-mode))))

  (progn
    (defun sb/company-org-mode ()
      (set
       (make-local-variable 'company-backends)
       '(company-files company-org-block company-ispell company-dict company-dabbrev)))

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
       (unless (derived-mode-p 'LaTeX-mode)
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
      (setq-local company-minimum-prefix-length 2)

      (set
       (make-local-variable 'company-backends)
       '(company-files
         (company-capf company-web-html) company-ispell company-dict company-dabbrev)))

    (dolist (hook '(html-mode-hook html-ts-mode-hook))
      (add-hook hook (lambda () (sb/company-html-mode)))))

  (progn
    (defun sb/company-prog-mode ()
      ;; Typing short prefixes help with faster completion and a more responsive UI
      (setq-local company-minimum-prefix-length 2)

      (make-local-variable 'company-backends)

      (setq company-backends
            '(company-files
              (company-capf
               company-c-headers
               :with company-keywords
               company-dabbrev-code ; Useful for variable names
               company-yasnippet
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
      ;; Typing short prefixes help with faster completion and a more responsive UI
      (setq-local company-minimum-prefix-length 2)

      (make-local-variable 'company-backends)

      (setq company-backends
            '(company-files
              (company-capf
               :with company-keywords
               company-dabbrev-code ; Useful for variable names
               company-yasnippet
               :separate)
              company-dict company-ispell company-dabbrev)))

    (dolist (hook '(emacs-lisp-mode-hook lisp-data-mode-hook))
      (add-hook hook (lambda () (sb/company-elisp-mode))))))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
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
   ("h" . lsp-symbol-highlight)
   ("f" . lsp-format-buffer)
   ("x" . lsp-execute-code-action)
   ("c" . lsp-imenu-create-categorised-index) ; sorts the items by kind.
   ("u" . lsp-imenu-create-uncategorised-index) ; sorts the items by position
   ("a" . lsp-workspace-folders-add)
   ("v" . lsp-workspace-folders-remove)
   ("b" . lsp-workspace-blacklist-remove))
  :custom
  ;; We can add "--compile-commands-dir=<build-dir>" option to indicate the directory where
  ;; "compile_commands.json" reside. If path is invalid, clangd will look in the current directory
  ;; and parent paths of each source file. We can also use the environment variable CLANGD_FLAGS as
  ;; "export CLANGD_FLAGS="--header-insertion=never"".
  (lsp-clients-clangd-args
   '("-j=4"
     "--all-scopes-completion"
     "--background-index"
     "--clang-tidy"
     "--completion-style=detailed"
     "--fallback-style=LLVM"
     ;; Do not automatically insert #include statements when editing C/C++ code
     "--header-insertion=never"
     "--header-insertion-decorators=0"
     "--log=error"
     ;; Unsupported option with Clangd 10: malloc-trim and enable-config
     ;; "--malloc-trim" ; Release memory periodically
     ;; "--enable-config"
     "--pch-storage=memory" ; Increases memory usage but can improve performance
     "--pretty"))
  (lsp-completion-provider :none "Enable integration of custom backends other than `capf'")
  (lsp-completion-show-detail t "Disable completion metadata, e.g., java.util.ArrayList")
  (lsp-completion-show-kind t "Show completion kind, e.g., interface/class")
  (lsp-completion-show-label-description t "Show description of completion candidates")
  (lsp-eldoc-enable-hover nil "Do not show noisy hover info with mouse")
  (lsp-enable-dap-auto-configure nil "I do not use dap-mode")
  (lsp-enable-on-type-formatting nil "Reduce unexpected modifications to code")
  (lsp-enable-folding nil "I do not find the feature useful")
  (lsp-headerline-breadcrumb-enable nil "Breadcrumb is not useful for all modes")
  (lsp-html-format-wrap-line-length sb/fill-column)
  (lsp-html-format-end-with-newline t)
  (lsp-html-format-indent-inner-html t)
  (lsp-imenu-sort-methods '(position) "More natural way of listing symbols")
  (lsp-lens-enable nil "Lenses are intrusive")
  (lsp-modeline-diagnostics-enable nil "We have Flycheck, and the modeline gets congested")
  (lsp-modeline-diagnostics-scope :file "Simpler to focus on the errors at hand")
  ;; Sudden changes in the height of the echo area causes the cursor to lose position, manually
  ;; request via `lsp-signature-activate'.
  (lsp-signature-auto-activate nil)
  (lsp-restart 'auto-restart "Avoid annoying questions, we expect a server restart to succeed")
  (lsp-xml-logs-client nil)
  (lsp-warn-no-matched-clients nil "Avoid warning messages for unsupported modes like csv-mode")
  (lsp-enable-file-watchers nil "Avoid watcher warnings")
  ;; I am using symbol-overlay for languages that do not have a server
  ;; (lsp-enable-symbol-highlighting nil)
  (lsp-pylsp-configuration-sources ["setup.cfg"])
  (lsp-pylsp-plugins-mccabe-enabled nil)
  ;; We can also set this per-project
  (lsp-pylsp-plugins-preload-modules ["numpy" "csv" "pandas" "statistics" "json"])
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pycodestyle-max-line-length sb/fill-column)
  (lsp-pylsp-plugins-pydocstyle-convention "pep257")
  (lsp-pylsp-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213")))
  (lsp-pylsp-plugins-pylint-enabled t)
  (lsp-pylsp-plugins-yapf-enabled t)
  (lsp-pylsp-plugins-flake8-enabled nil)
  (lsp-pylsp-plugins-pylint-args
   (vconcat
    (list
     "-j 2" (concat "--rcfile=" (expand-file-name ".config/pylintrc" sb/user-home-directory)))))
  (lsp-pylsp-plugins-isort-enabled t)
  (lsp-pylsp-plugins-mypy-enabled t)
  (lsp-use-plists t)
  :config
  (when (or (display-graphic-p) (daemonp))
    (setopt lsp-modeline-code-actions-segments '(count icon name)))

  (dolist (ignore-dirs
           '("/build\\'"
             "/\\.metadata\\'"
             "/\\.recommenders\\'"
             "/\\.clangd\\'"
             "/\\.cache\\'"
             "/__pycache__\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories ignore-dirs))

  (with-eval-after-load "lsp-lens"
    (diminish 'lsp-lens-mode))

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
           (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
           lsp-use-plists
           (not (functionp 'json-rpc-connection)) ;; native json-rpc
           (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

  :diminish)

(use-package consult-lsp
  :after (consult lsp)
  :commands (consult-lsp-diagnostics consult-lsp-file-symbols)
  :bind (:map lsp-mode-map ([remap xref-find-apropos] . consult-lsp-symbols)))

;; Try to delete `lsp-java-workspace-dir' if the JDTLS fails
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
   (expand-file-name "github/dotfiles/java/eclipse-format-swarnendu.xml" sb/user-home-directory)))

(use-package lsp-grammarly
  :after lsp-mode
  :hook ((text-mode markdown-mode org-mode LaTeX-mode) . lsp-deferred)
  :custom
  (lsp-grammarly-suggestions-oxford-comma t)
  (lsp-grammarly-suggestions-passive-voice t)
  (lsp-grammarly-suggestions-informal-pronouns-academic t)
  (lsp-grammarly-suggestions-preposition-at-the-end-of-sentence t)
  (lsp-grammarly-suggestions-conjunction-at-start-of-sentence t))

(use-package lsp-ltex
  :hook ((text-mode markdown-mode org-mode LaTeX-mode) . lsp-deferred)
  :custom
  (lsp-ltex-language "en" "Recommended to set a generic language to disable spell check")
  (lsp-ltex-check-frequency "save")
  (lsp-ltex-dictionary (expand-file-name "company-dict/text-mode" user-emacs-directory))
  :config
  ;; Disable spell checking since we cannot get `lsp-ltex' to work with custom dict words.
  (setq lsp-ltex-disabled-rules
        #s(hash-table
           size 30 data ("en-US" ["MORFOLOGIK_RULE_EN_US,WANT,EN_QUOTES,EN_DIACRITICS_REPLACE"]))))

;; `lsp-tex' provides minimal settings for Texlab, `lsp-latex' supports full features of Texlab.
(use-package lsp-latex
  :hook
  (LaTeX-mode
   .
   (lambda ()
     (require 'lsp-latex)
     (lsp-deferred)))
  :custom
  (lsp-latex-bibtex-formatter "latexindent")
  (lsp-latex-latex-formatter "latexindent")
  (lsp-latex-bibtex-formatter-line-length sb/fill-column)
  ;; Support forward search with Okular. Perform inverse search with Shift+Click in the PDF.
  (lsp-latex-forward-search-executable "okular")
  ;; “%f” is replaced with "The path of the current TeX file", "%p" with "The path of the current
  ;; PDF file", "%l" with "The current line number" by texlab
  (lsp-latex-forward-search-args '("--unique" "file:%p#src:%l%f")))

(use-package subword
  :ensure nil
  :hook ((LaTeX-mode prog-mode) . subword-mode)
  :diminish)

;; Highlight symbol under point for non-LSP major modes, use LSP for others
(use-package symbol-overlay
  :hook ((elisp-mode lisp-data-mode conf-mode) . symbol-overlay-mode)
  :bind (("M-p" . symbol-overlay-jump-prev) ("M-n" . symbol-overlay-jump-next))
  :custom (symbol-overlay-idle-time 2 "Delay highlighting to allow for transient cursor placements")
  :diminish)

(use-package compile
  :ensure nil
  :bind (("<f10>" . compile) ("<f11>" . recompile))
  :custom
  (compile-command (format "make -k -j%s " (num-processors)))
  (compilation-always-kill t "Kill a compilation process before starting a new one")
  (compilation-ask-about-save nil "Save all modified buffers without asking")
  ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first error.
  (compilation-scroll-output 'first-error))

(use-package fancy-compilation
  :after compile
  :init (fancy-compilation-mode 1))

;; (use-package rainbow-delimiters
;;   :hook ((prog-mode LaTeX-mode org-src-mode) . rainbow-delimiters-mode))

(use-package treesit-auto
  :when (executable-find "tree-sitter")
  :demand t
  :bind (("C-M-a" . treesit-beginning-of-defun) ("C-M-e" . treesit-end-of-defun))
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 3 "Increased default font locking may hurt performance")
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (docker "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (js "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (latex "https://github.com/latex-lsp/tree-sitter-latex")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (org "https://github.com/milisims/tree-sitter-org")
     (perl "https://github.com/tree-sitter-perl/tree-sitter-perl")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :config
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; Install grammars
  (when (unless (and (treesit-language-available-p 'bash)
                     (treesit-language-available-p 'bibtex)
                     (treesit-language-available-p 'c)
                     (treesit-language-available-p 'cpp)
                     (treesit-language-available-p 'cmake)
                     (treesit-language-available-p 'css)
                     ;; (treesit-language-available-p 'docker)
                     (treesit-language-available-p 'elisp)
                     (treesit-language-available-p 'html)
                     (treesit-language-available-p 'java)
                     ;; (treesit-language-available-p 'js)
                     (treesit-language-available-p 'json)
                     (treesit-language-available-p 'latex)
                     (treesit-language-available-p 'make)
                     (treesit-language-available-p 'markdown)
                     (treesit-language-available-p 'org)
                     (treesit-language-available-p 'perl)
                     (treesit-language-available-p 'python)
                     (treesit-language-available-p 'toml)
                     (treesit-language-available-p 'yaml)))
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

  (with-eval-after-load "c++-ts-mode"
    (bind-key "C-M-a" #'treesit-beginning-of-defun c++-ts-mode-map)
    (bind-key "C-M-e" #'treesit-end-of-defun c++-ts-mode-map)))

;; (with-eval-after-load "treesit"
;;   ;; Improves performance with large files without significantly diminishing highlight quality
;;   (setq font-lock-maximum-decoration '((c-mode . 2) (c++-mode . 2) (t . t))))

(use-package cc-mode
  :ensure nil
  :mode (("\\.h\\'" . c++-mode) ("\\.c\\'" . c++-mode))
  :hook
  ((c-mode c-ts-mode c++-mode c++-ts-mode)
   .
   (lambda ()
     (setq-local
      c-set-style "cc-mode"
      c-basic-offset 2
      c-auto-newline nil ; Disable electric indentation and on-type formatting
      c-electric-flag nil
      c-enable-auto-newline nil
      c-syntactic-indentation nil)
     (lsp-deferred)))
  :bind (:map c-mode-base-map ("C-c C-d")))

(use-package c-ts-mode
  :ensure nil
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
   c-ts-mode-map ("C-M-a" . treesit-beginning-of-defun) ("C-M-e" . treesit-end-of-defun)
   :map c++-ts-mode-map ("C-M-a" . treesit-beginning-of-defun) ("C-M-e" . treesit-end-of-defun)))

(use-package cuda-mode
  :mode ("\\.cu\\'" . cuda-mode)
  :mode ("\\.cuh\\'" . cuda-mode))

(use-package opencl-c-mode
  :ensure (:host github :repo "salmanebah/opencl-mode")
  :mode "\\.cl\\'")

(use-package cmake-mode
  :when (executable-find "cmake")
  :mode ("\(CMakeLists\.txt|\.cmake\)$" . cmake-ts-mode)
  :hook
  ((cmake-mode cmake-ts-mode)
   .
   (lambda ()
     (when (fboundp 'flyspell-mode)
       (flyspell-mode -1))
     (when (fboundp 'jinx-mode)
       (jinx-mode -1))
     (lsp-deferred))))

(use-package python
  :ensure nil
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
  (python-shell-completion-native-enable nil "Disable readline based native completion")
  (python-indent-guess-indent-offset-verbose nil "Remove guess indent python message")
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
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-post-activate-hooks
   (list (lambda () (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks (list (lambda () (setq python-shell-interpreter "python3")))))

(use-package cperl-mode
  :ensure nil
  :mode "latexmkrc\\'"
  :config (fset 'perl-mode 'cperl-mode))

(use-package sh-script
  :ensure nil
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
  :hook (fish-mode . (lambda () (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package highlight-doxygen
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode) . highlight-doxygen-mode))

(use-package lisp-mode
  :ensure nil
  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . lisp-data-mode)
  :hook
  (lisp-data-mode
   .
   (lambda ()
     (when buffer-file-name
       (add-hook 'after-save-hook #'check-parens nil t)))))

(use-package elisp-mode
  :ensure nil
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook
  (emacs-lisp-mode
   .
   (lambda ()
     (when buffer-file-name
       (add-hook 'after-save-hook #'check-parens nil t)))))

(use-package ini-mode
  :commands (ini-mode))

(use-package conf-mode
  :ensure nil
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
     ;; `yaml-mode' is derived from `text-mode', so disable grammar and spell checking.
     (when (fboundp 'flyspell-mode)
       (flyspell-mode -1))
     (when (fboundp 'jinx-mode)
       (jinx-mode -1))
     (make-local-variable 'lsp-disabled-clients)
     (setopt lsp-disabled-clients '(ltex-ls grammarly-ls))
     (lsp-deferred))))

(use-package yaml-imenu
  :hook ((yaml-mode yaml-ts-mode) . yaml-imenu-enable))

(use-package css-mode
  :ensure nil
  :hook ((css-mode css-ts-mode) . lsp-deferred)
  :custom (css-indent-offset 2))

(use-package emmet-mode
  :hook ((web-mode css-mode css-ts-mode html-mode html-ts-mode) . emmet-mode)
  :custom (emmet-move-cursor-between-quote t)
  :config (emmet-preview-mode 1))

(use-package make-mode
  :ensure nil
  :mode
  (("\\Makefile\\'" . makefile-mode)
   ("\\Makefile.common\\'" . makefile-mode)
   ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
   ("makefile\\.rules\\'" . makefile-mode))
  :hook ((makefile-mode make-ts-mode) . (lambda () (setq-local indent-tabs-mode t))))

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

;; The following page lists more shortcuts: https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode) ("\\.markdown\\'" . markdown-mode) ("README\\.md\\'" . gfm-mode))
  :bind (:map markdown-mode-map ("C-c C-d") ("C-c C-j") ("C-c C-c l" . markdown-live-preview-mode))
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

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf. Convert `markdown' to `org': "pandoc
;; -f markdown -t org -o output-file.org input-file.md"
(use-package pandoc-mode
  :hook (markdown-mode . pandoc-mode)
  :config (pandoc-load-default-settings)
  :diminish)

(use-package nxml-mode
  :ensure nil
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$" "\\.drawio$")
  :hook
  (nxml-mode
   .
   (lambda ()
     ;; `xml-mode' is derived from `text-mode', so disable grammar and spell checking.
     (when (fboundp 'flyspell-mode)
       (flyspell-mode -1))
     (when (fboundp 'jinx-mode)
       (jinx-mode -1))
     (make-local-variable 'lsp-disabled-clients)
     (setopt lsp-disabled-clients '(ltex-ls grammarly-ls))
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
     (make-local-variable 'js-indent-level)
     (setopt js-indent-level 2)
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
  ;; See `org-speed-commands-default' for a list of the keys and commands enabled at the
  ;; beginning of headlines. `org-babel-describe-bindings' will display a list of the code
  ;; blocks commands and their related keys.
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
  ;; #+OPTIONS toc:nil, use "#+TOC: headlines 2" or similar if you need a headline
  (org-export-with-toc nil)
  (org-export-with-sub-superscripts nil "#+OPTIONS ^:{}")
  ;; This exports broken links as [BROKEN LINK %s], so we can actually find them. The default value
  ;; nil just aborts the export process with an error message "Unable to resolve link: nil". This
  ;; doesn't give any hint on which line the broken link actually is.
  (org-export-with-broken-links 'mark)
  (org-latex-listings 'minted "Syntax coloring is more extensive than listings")
  (org-imenu-depth 4)
  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setopt
   org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  ;; There is a lot of visible distortion with `org-indent-mode' enabled. Emacs performance feels
  ;; better with the mode disabled.
  (with-eval-after-load "org-indent"
    (diminish 'org-indent-mode))
  :bind-keymap ("C-c o" . org-mode-map)
  :bind
  (:map
   org-mode-map
   ("M-<left>")
   ("M-<right>")
   ("M-<up>")
   ("M-<down>")
   ("C-'")
   ("C-c C-d") ; Was bound to `org-deadline', I prefer to use it for `duplicate-thing'
   ("C-c C-j") ; Was bound to `org-goto', I prefer to use it for `imenu' and its variants
   ("M-e") ; Was bound to `org-forward-paragraph', I prefer to use it for `forward-sentence'
   ("<tab>" . org-indent-item)
   ("<backtab>" . org-outdent-item)
   ("M-{" . org-backward-element)
   ("M-}" . org-forward-element)
   ("C-c C-," . org-insert-structure-template)))

(use-package org-appear
  :ensure (:host github :repo "awth13/org-appear")
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
  :ensure (:host github :repo "jdtsmith/org-modern-indent")
  :hook (org-mode . org-modern-indent-mode))

;; Auctex provides enhanced versions of `tex-mode' and `latex-mode', which automatically replace the
;; vanilla ones. Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides
;; the tex package.
(use-package tex
  :ensure
  (auctex
   :repo "https://git.savannah.gnu.org/git/auctex.git"
   :branch "main"
   :pre-build (("make" "elpa"))
   :build (:not elpaca--compile-info) ;; Make will take care of this step
   :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
   :version
   (lambda (_)
     (require 'tex-site)
     AUCTeX-version))
  :hook
  ((LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . TeX-PDF-mode) ; Use `pdflatex'
   ;; Revert PDF buffer after TeX compilation has finished
   (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
   ;; Enable rainbow mode after applying styles to the buffer
   ;; (TeX-update-style . rainbow-delimiters-mode)
   (LaTeX-mode . lsp-deferred))
  :bind (:map TeX-mode-map ("C-c ;") ("C-c C-d") ("C-c C-c" . TeX-command-master))
  :custom
  (TeX-auto-save t "Enable parse on save, stores parsed information in an `auto' directory")
  (TeX-auto-untabify t "Remove all tabs before saving")
  (TeX-clean-confirm nil)
  ;; Automatically insert braces after typing ^ and _ in math mode
  (TeX-electric-sub-and-superscript t)
  (TeX-electric-math t "Inserting $ completes the math mode and positions the cursor")
  (TeX-parse-self t "Parse documents")
  (TeX-save-query nil "Save buffers automatically when compiling")
  (LaTeX-item-indent 0 "Indent lists by two spaces")
  (LaTeX-fill-break-at-separators nil "Do not insert line-break at inline math")
  (tex-fontify-script nil "Avoid raising of superscripts and lowering of subscripts")
  ;; Avoid superscripts and subscripts from being displayed in a different font size
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 1.0 "Avoid emphasizing section headers")
  :config
  (setq-default TeX-master nil) ; Always query for the master file
  (with-eval-after-load "auctex"
    (bind-key "C-c C-e" LaTeX-environment LaTeX-mode-map)
    (bind-key "C-c C-s" LaTeX-section LaTeX-mode-map)
    (bind-key "C-c C-m" TeX-insert-macro LaTeX-mode-map))

  (with-eval-after-load "latex"
    (unbind-key "C-j" LaTeX-mode-map)
    ;; Disable `LaTeX-insert-item' in favor of `imenu'
    (unbind-key "C-c C-j" LaTeX-mode-map)
    (bind-key "C-x f" #'format-all-buffer LaTeX-mode-map)))

(use-package bibtex
  :ensure nil
  :hook (bibtex-mode . lsp-deferred)
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-maintain-sorted-entries t)
  (bibtex-comma-after-last-field nil))

(use-package consult-reftex
  :ensure (:host github :repo "karthink/consult-reftex")
  :after (consult tex-mode)
  :bind (("C-c [" . consult-reftex-insert-reference) ("C-c )" . consult-reftex-goto-label)))

(use-package auctex-latexmk
  :after tex-mode
  :when (executable-find "latexmk")
  :demand t
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t "Pass the '-pdf' flag when `TeX-PDF-mode' is active")
  :config
  (auctex-latexmk-setup)
  (add-hook 'TeX-mode-hook (lambda () (setq-default TeX-command-default "LatexMk"))))

(use-package math-delimiters
  :ensure (:host github :repo "oantolin/math-delimiters")
  :after tex
  :bind (:map TeX-mode-map ("$" . math-delimiters-insert)))

;; In Emacs Lisp mode, `xref-find-definitions' will by default find only functions and variables
;; from Lisp packages which are loaded into the current Emacs session or are auto-loaded.
(use-package xref
  :bind
  (("M-." . xref-find-definitions)
   ("M-?" . xref-find-references)
   ("C-M-." . xref-find-apropos) ; Find all identifiers whose name matches pattern
   ("M-," . xref-go-back))
  :custom (xref-search-program 'ripgrep))

(use-package citre
  :hook (prog-mode . citre-mode)
  :bind
  (("C-x c j" . citre-jump)
   ("C-x c b" . citre-jump-back)
   ("C-x c p" . citre-peek)
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
  ;; Try LSP first, and then use Citre if the enabled xref backends cannot find a definition
  (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
    (let ((fetcher (apply -fn -args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (apply -fn -args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))

  :diminish)

(use-package breadcrumb
  :hook
  ((c-mode
    c-ts-mode
    c++-mode
    c++-ts-mode
    python-mode
    python-ts-mode
    sh-mode
    bash-ts-mode
    java-mode
    java-ts-mode
    org-mode
    markdown-mode
    markdown-ts-mode)
   . breadcrumb-mode))

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
    (let ((range (list (line-beginning-position) (goto-char (line-end-position n)))))
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
    lsp-log-io-mode
    help-mode
    magit-status-mode
    magit-process-mode
    magit-diff-mode
    tags-table-mode
    compilation-mode
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
                   (member (sb/get-buffer-major-mode (buffer-name)) sb/skippable-modes))
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

(defun sb/get-derived-modes (mode)
  "Return a list of the ancestor modes that MODE is derived from."
  (interactive)
  (let ((modes ())
        (parent nil))
    (while (setq parent (get mode 'derived-mode-parent))
      (push parent modes)
      (setq mode parent))
    (setq modes (nreverse modes))))

(progn
  (defun sb/decrease-minibuffer-font ()
    "Decrease minibuffer font size."
    (set (make-local-variable 'face-remapping-alist) '((default :height 0.90))))

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

(use-package nerd-icons
  :ensure (:host github :repo "rainstormstudio/nerd-icons.el")
  :when (display-graphic-p)
  :custom
  (nerd-icons-color-icons nil)
  (nerd-icons-scale-factor 0.9))

;; Powerline theme for Nano looks great, and takes less space on the modeline. However, it does not
;; show lsp status, flycheck information, and Python virtualenv information on the modeline. The
;; package is not being actively maintained. Inspired by
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
    "Setup a nano-like modeline"
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
                        (powerline-fill-center nil (/ (powerline-width center) 2.0))
                        (powerline-render center)
                        (powerline-fill nil (powerline-width rhs))
                        (powerline-render rhs)))))))
  :when (eq sb/modeline-theme 'powerline)
  :hook (elpaca-after-init . sb/powerline-nano-theme)
  :custom
  (powerline-display-hud nil "Visualization of the buffer position is not useful")
  (powerline-display-buffer-size nil)
  (powerline-display-mule-info nil "File encoding information is not useful")
  (powerline-gui-use-vcs-glyph t)
  (powerline-height 20))

(use-package doom-modeline
  :when (eq sb/modeline-theme 'doom-modeline)
  :hook (elpaca-after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28 "Respected only in GUI")
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-checker-simple-format nil)
  (doom-modeline-buffer-file-name-style 'file-name "Reduce space on the modeline")
  (doom-modeline-unicode-fallback t "Use Unicode instead of ASCII when not using icons"))

(use-package centaur-tabs
  :hook (elpaca-after-init . centaur-tabs-mode)
  :bind*
  (("M-<right>" . centaur-tabs-forward-tab)
   ("C-<tab>" . centaur-tabs-forward-tab)
   ("M-<left>" . centaur-tabs-backward-tab)
   ("C-S-<iso-lefttab>" . centaur-tabs-backward-tab)
   ("M-\"" . centaur-tabs-ace-jump))
  :custom
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "•") ; Unicode Bullet (0x2022)
  (centaur-tabs-set-close-button nil "I do not use the mouse")
  (centaur-tabs-show-new-tab-button nil "I do not use the mouse")
  (centaur-tabs-set-bar 'under)
  :config
  (when (display-graphic-p)
    (setopt centaur-tab-style "wave" centaur-tabs-set-icons t centaur-tabs-icon-type 'nerd-icons))
  ;; Make the headline face match `centaur-tabs-default' face for an uniform face
  (centaur-tabs-headline-match))

(use-package olivetti
  :hook ((text-mode prog-mode) . olivetti-mode)
  :diminish)

;; Inside strings, special keys like tab or F1-Fn have to be written inside angle brackets, e.g.
;; "C-<up>". Standalone special keys (and some combinations) can be written in square brackets, e.g.
;; [tab] instead of "<tab>".

(bind-keys
 ("RET" . newline-and-indent)
 ("C-l" . goto-line)

 ("C-c z" . repeat)
 ("C-z" . undo)

 ("<f1>" . execute-extended-command)
 ("<f2>" . find-file)

 ("<f7>" . previous-error) ; "M-g p" is the default keybinding
 ("<f8>" . next-error) ; "M-g n" is the default keybinding

 ("C-x x a" . find-alternate-file)
 ("C-x x g" . revert-buffer-quick)
 ("C-x x r" . rename-file)

 ;; In a line with comments, "C-u M-;" removes the comments altogether. That means deleting the
 ;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box)

 ("C-s" . save-buffer)
 ("C-S-s" . sb/save-all-buffers)
 ("C-x k" . kill-this-buffer)

 ("C-<left>" . backward-word)
 ("C-<right>" . forward-word)

 ("M-\\" . delete-horizontal-space)
 ("M-#" . cycle-spacing)

 ("C-M-b" . backward-sexp)
 ("C-M-f" . forward-sexp)
 ("C-M-k" . kill-sexp)
 ("C-M-@" . mark-sexp)

 ;; "C-h b" lists all the bindings available in a buffer, "C-h m" shows the keybindings for the
 ;; major and the minor modes.
 ("C-c d k" . describe-personal-keybindings))

(unbind-key "C-]") ; Bound to `abort-recursive-edit'
(unbind-key "C-j") ; Bound to `electric-newline-and-maybe-indent'
(unbind-key "C-x f") ; Bound to `set-fill-column'

(bind-key* "C-x s" #'scratch-buffer) ; Bound to `save-some-buffers'

(use-package default-text-scale
  :when (display-graphic-p)
  :bind (("C-M-+" . default-text-scale-increase) ("C-M--" . default-text-scale-decrease)))

(use-package free-keys
  :commands free-keys)

(use-package which-key
  :hook (elpaca-after-init . which-key-mode)
  :diminish)

(use-package kkp
  :hook (elpaca-after-init . global-kkp-mode))

(use-package kdl-mode
  :ensure (:host github :repo "bobuk/kdl-mode")
  :mode ("\\.kdl\\'" . kdl-mode))

(use-package pdf-tools
  :when (display-graphic-p)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind
  (:map
   pdf-view-mode-map
   ("j" . pdf-view-next-line-or-next-page)
   ("k" . pdf-view-previous-line-or-previous-page)
   ("n" . pdf-view-next-page-command)
   ("p" . pdf-view-previous-page-command)
   ("a" . pdf-view-first-page)
   ("e" . pdf-view-last-page)
   ("l" . pdf-view-goto-page)
   ("P" . pdf-view-fit-page-to-window)
   ("W" . pdf-view-fit-width-to-window)
   ("H" . pdf-view-fit-height-to-window)
   ("+" . pdf-view-enlarge)
   ("-" . pdf-view-shrink)
   ("r" . pdf-view-revert-buffer)
   ("d" . pdf-annot-delete)
   ("h" . pdf-annot-add-highlight-markup-annotation)
   ("t" . pdf-annot-add-text-annotation)
   ("M" . pdf-view-midnight-minor-mode))
  :custom
  (pdf-annot-activate-created-annotations t "Automatically annotate highlights")
  (pdf-view-resize-factor 1.1 "Fine-grained zoom factor of 10%")
  :config
  (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install :no-query)'

  (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable

  ;; We do not enable `pdf-view-themed-minor-mode' since it can change plot colors
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes))

;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.
(use-package saveplace-pdf-view
  :after (pdf-tools saveplace)
  :demand t)

(use-package ssh-config-mode
  :mode ("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
  :mode ("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys\\'" . ssh-authorized-keys-mode)
  :hook (ssh-config-mode . turn-on-font-lock))

(add-to-list 'display-buffer-alist '("\\*Warnings*" (display-buffer-at-bottom) (window-height . 8)))
(add-to-list 'display-buffer-alist '("\\*Help" (display-buffer-same-window)))
(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
(add-to-list
 'display-buffer-alist
 '("\\*Compilation\\*"
   display-buffer-in-side-window
   (side . bottom)
   (slot . 0)
   (window-height . fit-window-to-buffer)
   (preserve-size . (nil . t))
   (no-other-window . t)
   (no-delete-other-windows . t)))

(add-hook
 'elpaca-after-init-hook
 (lambda ()
   (let ((gc-time (float-time gc-elapsed)))
     (message "Emacs ready (init time = %s, gc time = %.2fs, gc count = %d)."
              (emacs-init-time)
              gc-time
              gcs-done))))

;;; init.el ends here
