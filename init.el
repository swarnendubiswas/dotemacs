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

(defcustom sb/debug-init-perf nil
  "Enable features to debug errors and performance bottlenecks."
  :type 'boolean
  :group 'sb/emacs)

;; Modus-vivendi is the most complete, while Catppuccin is more colorful.
(defcustom sb/theme 'modus-vivendi
  "Specify which Emacs theme to use."
  :type
  '(radio
    (const :tag "doom-nord" doom-nord)
    (const :tag "modus-vivendi" modus-vivendi)
    (const :tag "catppuccin" catppuccin)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Powerline looks clean, but doom-modeline is more informative.
(defcustom sb/modeline-theme 'doom-modeline
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
;; files with `company-ispell' and `company-dict'. However, `company-ispell'
;; does not keep prefix case when used as a grouped backend.
(defcustom sb/in-buffer-completion 'company
  "Choose the framework to use for completion at point."
  :type
  '(radio
    (const :tag "corfu" corfu)
    (const :tag "company" company)
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/enable-icons t
  "Should icons be enabled?
The provider is `nerd-icons'."
  :type 'boolean
  :group 'sb/emacs)

;; Eglot does not allow multiple servers to connect to a major mode, does not
;; support semantic tokens, but is possibly more lightweight.
(defcustom sb/lsp-provider 'none
  "Choose between Lsp-mode and Eglot."
  :type
  '(radio
    (const :tag "lsp-mode" lsp-mode)
    (const :tag "eglot" eglot)
    (const :tag "none" none))
  :group 'sb/emacs)

(defconst sb/user-home-directory (getenv "HOME")
  "User HOME directory.")

;; "straight.el" makes it easy to install packages from arbitrary sources like
;; GitHub.
(setopt
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
 straight-vc-git-default-clone-depth 1)

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
(setopt
 use-package-enable-imenu-support t
 use-package-expand-minimally t
 use-package-always-defer t)
(straight-use-package '(use-package))

(when (bound-and-true-p sb/debug-init-perf)
  ;; Use "M-x use-package-report" to see results
  (setopt
   use-package-compute-statistics t
   ;; Show everything
   use-package-minimum-reported-time 0
   use-package-verbose t))

;; Check "use-package-keywords.org" for a suggested order of `use-package'
;; keywords.

;; Package `bind-key' provides macros `bind-key', `bind-key*', and `unbind-key'
;; which provides a much prettier API for manipulating keymaps than `define-key'
;; and `global-set-key'. "C-h b" lists all the bindings available in a buffer,
;; "C-h m" shows the keybindings for the major and the minor modes.
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
  (setopt exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package emacs
  :hook
  (emacs-startup
   .
   (lambda ()
     (save-place-mode 1)
     (column-number-mode 1)
     (size-indication-mode 1)
     ;; Autosave file-visiting buffers at idle time intervals
     (auto-save-visited-mode 1)
     ;; Typing with the mark active will overwrite the marked region
     (delete-selection-mode 1)
     ;; Use soft wraps, wrap lines without the ugly continuation marks
     ;; LATER: Is this why `avy' breaks?
     ;; (global-visual-line-mode 1)
     ;; When you call `find-file', you do not need to clear the existing
     ;; file path before adding the new one. Just start typing the whole
     ;; path and Emacs will "shadow" the current one. For example, you are
     ;; at "~/Documents/notes/file.txt" and you want to go to
     ;; "~/.emacs.d/init.el", type the latter directly and Emacs will take
     ;; you there.
     (file-name-shadow-mode 1)
     (auto-encryption-mode -1)))
  :custom
  (ad-redefinition-action 'accept "Turn off warnings due to redefinitions")
  (auto-save-no-message t "Do not print frequent autosave messages")
  ;; Disable autosaving based on number of characters typed
  (auto-save-interval 0)
  ;; Unlike `auto-save-mode', `auto-save-visited-mode' saves the buffer contents
  ;; to the visiting file and runs all save-related hooks. We disable
  ;; `auto-save-mode' and prefer `auto-save-visited-mode' instead.
  (auto-save-default nil)
  ;; Save buffer to file after idling for some time, the default of 5s may be
  ;; too frequent since it runs all the save-related hooks.
  (auto-save-visited-interval 30)
  (apropos-do-all t "Make `apropos' search more extensively")
  ;; Save bookmark after every bookmark edit and also when Emacs is killed
  (bookmark-save-flag 1)
  ;; Autofill comments in modes that define them
  (comment-auto-fill-only-comments t)
  (comment-empty-lines t)
  (comment-multi-lines t)
  (create-lockfiles nil)
  (backup-inhibited t "Disable backup for a per-file basis")
  (make-backup-files nil "Stop making backup `~' files")
  (custom-safe-themes t)
  (delete-by-moving-to-trash t)
  (help-window-select t "Makes it easy to close the window")
  (help-enable-autoload nil)
  (help-enable-completion-autoload nil)
  (help-enable-symbol-autoload nil)
  (history-delete-duplicates t)
  (read-process-output-max (* 4 1024 1024))
  (remote-file-name-inhibit-locks t)
  (ring-bell-function 'ignore "Disable beeping sound")
  (visible-bell nil)
  (save-interprogram-paste-before-kill t)
  (select-enable-clipboard t)
  (kill-do-not-save-duplicates t "Do not save duplicates to kill ring")
  (sentence-end-double-space nil)
  (shift-select-mode nil)
  (sort-fold-case nil "Do not ignore case when sorting")
  (standard-indent 2)
  (switch-to-buffer-preserve-window-point t)
  (view-read-only t "View mode for read-only buffers")
  (window-combination-resize t "Resize windows proportionally")
  (max-mini-window-height 0.3)
  (x-gtk-use-system-tooltips nil "Do not use system tooltips")
  ;; Always trigger an immediate resize of the child frame
  (x-gtk-resize-child-frames 'resize-mode)
  (x-underline-at-descent-line t)
  (tags-add-tables nil)
  (tags-case-fold-search nil "case-sensitive")
  (tags-revert-without-query t)
  ;; Disable the warning "X and Y are the same file" in case of symlinks
  (find-file-suppress-same-file-warnings t)
  (auto-mode-case-fold nil "Avoid a second pass through `auto-mode-alist'")
  (confirm-nonexistent-file-or-buffer t)
  (confirm-kill-emacs nil)
  ;; Prevent 'Active processes exist' when you quit Emacs
  (confirm-kill-processes nil)
  (require-final-newline t "Always end a file with a newline")
  (revert-without-query '("\\.*") "Revert all files without asking")
  (bidi-inhibit-bpa nil) ; Disabling BPA makes redisplay faster
  (vc-handled-backends '(Git))
  ;; Disable version control for remote files to improve performance
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
  (display-buffer-alist
   '(
     ;; Allow *Help* buffers to use the full frame
     ("*Help*" (display-buffer-same-window))
     ("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
      (display-buffer-no-window)
      (allow-no-window . t))))
  (scroll-preserve-screen-position t)
  (scroll-margin 3)
  (scroll-step 1)
  (scroll-conservatively 10)
  (scroll-error-top-bottom t)
  ;; Accelerate scrolling operations when non-nil. Only those portions of the
  ;; buffer which are actually going to be displayed get fontified.
  (fast-but-imprecise-scrolling t)
  (auto-window-vscroll nil)
  (hscroll-margin 2)
  (hscroll-step 1)
  (fringes-outside-margins t)
  ;; Improve Emacs' responsiveness by delaying syntax highlighting during input
  (redisplay-skip-fontification-on-input t)
  :config
  (dolist (exts
           '(".aux"
             ".blg"
             ".directory"
             ".dll"
             ".exe"
             ".fdb_latexmk"
             ".fls"
             ".lof"
             ".nav"
             ".rel"
             ".rip"
             ".snm"
             ".synctex.gz"
             ".toc"
             ".vrb"
             "TAGS"
             "indent.log"))
    (add-to-list 'completion-ignored-extensions exts))

  (when (boundp 'next-error-message-highlight)
    (setopt next-error-message-highlight t))
  (when (boundp 'read-minibuffer-restore-windows)
    (setopt read-minibuffer-restore-windows t))
  (when (boundp 'use-short-answers)
    (setopt use-short-answers t))
  ;; Hide commands in "M-x" which do not work in the current mode.
  (when (boundp 'read-extended-command-predicate)
    (setopt read-extended-command-predicate
            #'command-completion-default-include-p))
  (when (boundp 'help-window-keep-selected)
    (setopt help-window-keep-selected t))
  (when (boundp 'find-sibling-rules)
    (setopt find-sibling-rules
            '(("\\([^/]+\\)\\.c\\'" "\\1.h")
              ("\\([^/]+\\)\\.cpp\\'" "\\1.h")
              ("\\([^/]+\\)\\.h\\'" "\\1.c")
              ("\\([^/]+\\)\\.hpp\\'" "\\1.cpp"))))
  (when (eq system-type 'windows-nt)
    (setopt w32-get-true-file-attributes nil))
  ;; In Emacs 30 and newer, disable Ispell completion to avoid annotation errors
  ;; when no `ispell' dictionary is set.
  (when (boundp 'text-mode-ispell-word-completion)
    (setopt text-mode-ispell-word-completion nil))
  ;; Hide "When done with a buffer, type C-x 5" message
  (when (bound-and-true-p server-client-instructions)
    (setopt server-client-instructions nil))

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

  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))

  ;; Mark safe variables
  (put 'compilation-read-command 'safe-local-variable #'stringp)
  (put 'reftex-default-bibliography 'safe-local-variable #'stringp)

  (put 'overwrite-mode 'disabled t)

  ;; Keep the cursor out of the read-only portions of the.minibuffer
  (setopt minibuffer-prompt-properties
          '(read-only
            t intangible t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  :diminish visual-line-mode)

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
  :unless (version<= emacs-version "27")
  :hook (emacs-startup . global-so-long-mode))

(use-package imenu
  :straight (:type built-in)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items 1000)
  (imenu-use-popup-menu nil))

(use-package recentf
  :straight (:type built-in)
  :hook
  (emacs-startup
   .
   (lambda ()
     (let ((inhibit-message t))
       (recentf-mode 1))))
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
     ".*/recentf.*"
     "~$"
     ".*/TAGS\\'"
     ".*/\\.cache"
     "*[/\\]straight/repos/"))
  ;; Keep remote file without testing if they still exist
  (recentf-keep '(file-remote-p file-readable-p))
  ;; Larger values help in lookup but takes more time to check if the files
  ;; exist
  (recentf-max-saved-items 250)
  :config
  ;; Abbreviate the home directory to make it easy to read the actual file name.
  (unless (> emacs-major-version 27)
    (setopt recentf-filename-handlers '(abbreviate-file-name)))

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

;; (use-package doc-view
;;   :straight (:type built-in)
;;   :hook
;;   (doc-view-mode
;;    .
;;    (lambda ()
;;      (when (and buffer-file-name (string-suffix-p ".pdf" buffer-file-name))
;;        (auto-revert-mode 1))))
;;   :bind
;;   (:map
;;    doc-view-mode-map
;;    ("=" . doc-view-enlarge)
;;    ("-" . doc-view-shrink)
;;    ("n" . doc-view-next-page)
;;    ("p" . doc-view-previous-page)
;;    ("0" . doc-view-scale-reset)
;;    ("M-<" . doc-view-first-page)
;;    ("M->" . doc-view-last-page)
;;    ("C-l" . doc-view-goto-page))
;;   :custom
;;   (doc-view-continuous t)
;;   (doc-view-resolution 120))

;; Binds "C-x C-f" to `find-file-at-point' which will continue to work like
;; `find-file' unless a prefix argument is given. Then it will find file at
;; point.
(use-package ffap
  :straight (:type built-in)
  :bind (("<f2>" . ffap) ("C-x p o" . ff-find-other-file))
  :custom
  ;; Do not ping things that look like domain names
  (ffap-machine-p-known 'reject))

;; Highlight and open http links in strings and comments in buffers.
(use-package goto-addr
  :straight (:type built-in)
  :hook ((prog-mode . goto-address-prog-mode) (text-mode . goto-address-mode))
  :bind ("C-c C-o" . goto-address-at-point))

(use-package subword
  :straight (:type built-in)
  :hook ((LaTeX-mode prog-mode) . subword-mode)
  :diminish)

;; Use "Shift + direction" arrows for moving around windows.
;; (use-package windmove
;;   :straight (:type built-in)
;;   :when (display-graphic-p)
;;   :init (windmove-default-keybindings))

;; (use-package winner
;;   :hook (emacs-startup . winner-mode)
;;   :bind (("C-c <left>" . winner-undo) ("C-c <right>" . winner-redo)))

;; Use `ediff-regions-wordwise' for small regions and `ediff-regions-linewise'
;; for larger regions.
(use-package ediff
  :straight (:type built-in)
  :commands (ediff-buffers ediff-regions-linewise ediff-regions-wordwise)
  :bind (("C-c d e" . ediff) ("C-c d b" . ediff-buffers))
  :custom
  ;; Put the control panel in the same frame as the diff windows
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split diffs side by side
  (ediff-split-window-function #'split-window-horizontally)
  ;; Prompt and kill file variants on quitting an Ediff session
  (ediff-keep-variants nil)
  :config
  (ediff-set-diff-options 'ediff-diff-options "-w")
  (with-eval-after-load "winner"
    (add-hook 'ediff-cleanup-hook #'winner-undo)))

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
  (tramp-default-method "ssh")
  :config
  (when (boundp 'tramp-use-connection-share)
    (setopt tramp-use-connection-share nil))
  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  ;; Include "$HOME/.local/bin" directory in $PATH on remote
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; (setenv "SHELL" shell-file-name) ; Recommended to connect with Bash
  (setopt debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))

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
   '((mark
      modified read-only locked " " (name 24 24 :left :elide) " " filename)))
  (ibuffer-never-show-predicates
   '("*Help\\*"
     "*Quick Help\\*"
     "*Calc Trail\\*"
     "*Compile-Log\\*"
     "*Async-native-compile-log\\*"
     "*Native-compile-log\\*"
     "*Calculator\\*"
     "*Calendar\\*"
     "*Org Help\\*"
     "magit.*"
     "*lsp-log*"
     "*ltex-ls*"
     "*bash-ls.*"
     "*marksman.*"
     "*yaml-ls.*"
     "*clangd.*"
     "*texlab.*"))
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

;; `pixel-scroll-mode' uses line-by-line scrolling.
(use-package pixel-scroll
  :straight (:type built-in)
  :custom
  (pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-interpolate-page t)
  :config
  (cond
   ((fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
   ((fboundp 'pixel-scroll-mode)
    (pixel-scroll-mode 1))))

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
   ;; Use "C-'" to select one of the many currently visible `isearch'
   ;; candidates.
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
             "^\\*eldoc"
             "^\\*Async-native-compile-log\\*"))
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
  ;; (dired-listing-switches
  ;;  "-aBFghlNopv --group-directories-first --time-style=locale")
  (dired-listing-switches "--group-directories-first")
  (dired-ls-F-marks-symlinks t "-F marks links with @")
  (dired-recursive-copies 'always "Single prompt for all n directories")
  (dired-recursive-deletes 'always "Single prompt for all n directories")
  ;; Do not ask whether to kill buffers visiting deleted files
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-free-space nil)
  :config
  (when (boundp 'dired-kill-when-opening-new-dired-buffer)
    (setopt dired-kill-when-opening-new-dired-buffer t)))

(use-package dired-x
  :straight (:type built-in)
  :hook
  (dired-mode
   .
   (lambda ()
     (require 'dired-x)
     (dired-omit-mode -1)))
  :bind ("C-x C-j" . dired-jump)
  :custom (dired-omit-verbose nil "Do not show messages when omitting files")
  :config
  ;; Obsolete from Emacs 28+
  (unless (> emacs-major-version 27)
    (setopt dired-bind-jump t))

  (setopt dired-omit-files (concat "\\`[.]\\'"))

  (setopt dired-omit-files
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
           "\\|^.ctags.d\\'"
           "\\|^.git\\'"
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

;; (use-package dired-hist
;;   :straight (:host github :repo "karthink/dired-hist")
;;   :hook (dired-mode . dired-hist-mode)
;;   :bind
;;   (:map
;;    dired-mode-map ("l" . dired-hist-go-back) ("r" . dired-hist-go-forward)))

;; In Emacs Lisp mode, `xref-find-definitions' will by default find only
;; functions and variables from Lisp packages which are loaded into the current
;; Emacs session or are auto-loaded.
(use-package xref
  :bind
  (("M-." . xref-find-definitions)
   ("M-," . xref-go-back) ("M-?" . xref-find-references)
   ;; Find all identifiers whose name matches pattern
   ("C-M-." . xref-find-apropos))
  :custom (xref-search-program 'ripgrep))

(use-package project
  :bind
  (("<f5>" . project-switch-project)
   ("<f6>" . project-find-file)
   :map
   project-prefix-map
   ("f" . project-or-external-find-file)
   ("b" . project-switch-to-buffer)
   ("c" . project-compile)
   ("k" . project-kill-buffers)
   ("r" . project-query-replace-regexp))
  :custom
  ;; Start `project-find-file' by default
  (project-switch-commands 'project-find-file))

;; Allows identifying custom projects with a ".project" file (e.g., ~/Dropbox).
(use-package project-x
  :straight (:host github :repo "karthink/project-x")
  :after project
  :demand t
  :config (add-hook 'project-find-functions #'project-x-try-local 90))

(use-package vertico
  :straight
  (vertico
   :files (:defaults "extensions/*")
   :includes (vertico-directory vertico-repeat vertico-quick vertico-indexed))
  :hook
  ((emacs-startup . vertico-mode)
   ;; Tidy or auto-hide shadowed file names. When you are in a sub-directory and
   ;; use, say, `find-file' to go to your home '~/' or root '/' directory,
   ;; Vertico will clear the old path to keep only your current input.
   (rfn-eshadow-update-overlay . vertico-directory-tidy)
   (minibuffer-setup . vertico-repeat-save))
  :bind
  (("C-c r" . vertico-repeat)
   ("M-r" . vertico-repeat-select)
   :map vertico-map
   ;; `vertico-exit' (RET) exits with the currently selected candidate, while
   ;; `vertico-exit-input' (M-RET) exits with the minibuffer input instead.
   ;; ("C-M-j" . vertico-exit-input)
   ("M-<" . vertico-first)
   ("M->" . vertico-last)
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)
   ("C-c q" . vertico-quick-insert)
   ("C-'" . vertico-quick-jump))
  :custom (vertico-cycle t)
  :config
  (vertico-indexed-mode 1)
  (when (eq sb/theme 'catppuccin)
    (with-eval-after-load 'vertico
      (set-face-attribute 'vertico-current nil
                          :background "#676767"
                          :foreground "#FFFFFF")))

  ;; Customize the display of the current candidate in the completion list. This
  ;; will prefix the current candidate with "» " to make it stand out.
  ;; Reference:
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add
   #'vertico--format-candidate
   :around
   (lambda (orig cand prefix suffix index _start)
     (setq cand (funcall orig cand prefix suffix index _start))
     (concat
      (if (= vertico--index index)
          (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
        "  ")
      cand))))

(use-package consult
  :preface
  (defun sb/consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
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
   ("M-g l" . sb/consult-line-symbol-at-point)
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
     "\\*Help*"
     "\\*Disabled Command\\*"
     "Flymake log"
     "\\*Flycheck"
     "Shell command output"
     "direnv"
     "\\*magit-"
     "magit-.*"
     ".+-shell*"
     "\\*straight-"
     "\\*Compile-Log"
     "\\*Native-*"
     "\\*Async-"
     "\\*format-all-error"
     "\\*Ediff Registry\\*"
     "COMMIT_EDITMSG"
     "TAGS"
     "\\*vc"
     "\\*tramp"
     "\\*citre*"
     "\\*EGLOT"
     "\\*pylsp"
     "\\*ltex-ls"
     "\\*texlab"
     "\\*bash-ls.*"
     "\\*json-ls.*"
     "\\*yaml-ls.*"
     "\\*shfmt.*"
     "\\*clangd.*"
     "\\*semgrep.*"
     "\\*autotools.*"))
  :config
  (consult-customize
   consult-line
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-xref
   consult-yank-from-kill-ring
   :preview-key
   '(:debounce 1.5 any)
   consult-recent-file
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

  ;; ;; Use thing at point with `consult-line'
  ;; (consult-customize
  ;;  consult-line
  ;;  :add-history (seq-some #'thing-at-point '(region symbol)))
  ;; (defalias 'consult-line-thing-at-point 'consult-line)
  ;; (consult-customize
  ;;  consult-line-thing-at-point
  ;;  :initial (thing-at-point 'symbol))

  (with-eval-after-load "latex"
    (bind-key "C-c C-j" #'consult-outline LaTeX-mode-map)))

;; Easily add file and directory paths into the minibuffer.
(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map
   vertico-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file))
  :config (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

;; Provide context-dependent actions similar to a content menu.
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
  (setopt marginalia-annotator-registry
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
  ;; Prefer hunspell over aspell on Linux platforms
  (cond
   ((executable-find "hunspell")
    (progn
      (setenv "LANG" "en_US")
      (setenv "DICTIONARY" "en_US")
      (setenv "DICPATH" `,(concat user-emacs-directory "hunspell"))
      (setopt
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
      (setopt
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
  ;; LATER: Presence of the "enchant-2" executable does not imply that the
  ;; header files are present for compiling jinx
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
  ((emacs-startup . global-hungry-delete-mode)
   (minibuffer-setup . (lambda () (hungry-delete-mode -1))))
  :diminish)

(use-package move-text
  :bind (("M-<down>" . move-text-down) ("M-<up>" . move-text-up)))

;; Expand region increases the selected region by semantic units
(use-package expand-region
  :bind (("C-=" . er/expand-region) ("C-M-=" . er/contract-region)))

;; `change-inner "' allows to kill the string contents, `change-outer "' will
;; kill the entire string.
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

;; (use-package goto-last-change
;;   :bind ("C-x C-\\" . goto-last-change))

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

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :bind* ("C-." . iedit-mode))

;; Save a bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark,
;; use `bookmark-jump' ("C-x r b") or `bookmark-bmenu-list' ("C-x r l"). Rename
;; the bookmarked location in `bookmark-bmenu-mode' with `R'.
(use-package bm
  :init
  (setopt
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
  :config (setq-default bm-buffer-persistence t))

(use-package crux
  :bind
  (("C-c d s" . crux-sudo-edit)
   ("C-<f9>" . crux-recentf-find-directory)
   ("C-<f11>" . crux-kill-other-buffers))
  :bind* ("C-c C-d" . crux-duplicate-current-line-or-region))

;; (use-package rainbow-mode
;;   :hook
;;   ((LaTeX-mode
;;     css-mode
;;     css-ts-mode
;;     html-mode
;;     html-ts-mode
;;     web-mode
;;     help-mode
;;     helpful-mode)
;;    . rainbow-mode)
;;   :diminish)

(use-package colorful-mode
  :hook
  ((LaTeX-mode
    css-mode css-ts-mode html-mode html-ts-mode web-mode help-mode helpful-mode)
   . colorful-mode)
  :diminish)

;; LATER: The parenthesis faces are wrongly highlighted
;; (use-package rainbow-delimiters
;;   :hook ((prog-mode LaTeX-mode org-src-mode) . rainbow-delimiters-mode))

(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :diminish)

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
;; (use-package ws-butler
;;   :hook (prog-mode . ws-butler-mode)
;;   :diminish)

;; While searching, you can jump straight into `occur' with "M-s o". `isearch'
;; saves mark where the search started, so you can jump back to that point later
;; with "C-u C-SPC". Use "M-s M-<" to go to the first match and "M-s M->" to go
;; to the last match.
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
  :custom
  (isearch-lazy-count t "Show match count")
  (isearch-allow-scroll t "Scrolling should not cancel search")
  ;; Enable "M-<", "M->", "C-v" and "M-v" to jump among matches
  (isearch-allow-motion t)
  (isearch-motion-changes-direction t)
  ;; Remove delay before `isearch' highlights matches
  (lazy-highlight-initial-delay 0))

;; Auto populate `isearch' with the symbol at point
(use-package isearch-symbol-at-point
  :commands isearch-backward-symbol-at-point
  :bind
  (("M-s ." . isearch-symbol-at-point)
   ("M-s _" . isearch-forward-symbol)
   :map
   isearch-mode-map
   ("C-d" . isearch-forward-symbol-at-point)))

(with-eval-after-load "grep"
  (setopt
   grep-command "grep --color -irHn "
   grep-highlight-matches t
   grep-scroll-output t)

  (when (executable-find "rg")
    (setopt grep-program "rg")
    (grep-apply-setting 'grep-find-command '("rg -n -H --no-heading -e" . 27)))

  (dolist (dirs '(".cache" "node_modules" "vendor" ".clangd"))
    (add-to-list 'grep-find-ignored-directories dirs)))

;; `consult-rg' provides live search, while `deadgrep' provides a resulting
;; search buffer. Visit the result in another buffer with "o", move between
;; search hits with "n" and "p", and move between files with "M-n" and "M-p".
;; Change the search term with "S" and enable incremental search with "I".
(use-package deadgrep
  :commands deadgrep-edit-mode
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

;; Allows you to edit a deadgrep buffer and apply those changes to the file
;; buffer.
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
   ([remap isearch-query-replace-regexp] . visual-replace-from-isearch))
  :custom (visual-replace-display-total t))

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
   ;; ("C-x M-g" . magit-dispatch)
   ("C-c M-g" . magit-file-dispatch))
  :custom
  ;; Open the status buffer in a full frame
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-section-initial-visibility-alist
   '((stashes . show) (untracked . show) (unpushed . show) (unpulled . show)))
  (magit-save-repository-buffers 'dontask)
  (with-eval-after-load "magit-diff"
    ;; Show fine differences for the current diff hunk only
    (magit-diff-refine-hunk t)
    (magit-format-file-function #'magit-format-file-nerd-icons)))

(use-package git-modes
  :mode ("dotgitconfig" . gitconfig-mode)
  :mode ("/\\.gitignore\\'" . gitignore-mode)
  :mode ("/\\.gitattributes\\'" . gitattributes-mode))

;; Diff-hl looks nicer than git-gutter, and is based on `vc'. Fringe is
;; unavailable in TTY.
(use-package diff-hl
  :hook
  ((find-file . global-diff-hl-mode)
   (dired-mode . diff-hl-dired-mode-unless-remote)
   (diff-hl-mode-on . diff-hl-margin-local-mode))
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
  (setopt electric-pair-inhibit-predicate
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

  ;;   (add-hook 'LaTeX-mode-hook #'sb/add-latex-pairs)
  )

;; "C-h m" or `describe-mode' shows all the active minor modes (and major mode)
;; and a brief description of each.

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
  ;; Shellcheck is invoked by bash lsp
  (dolist (checkers
           '(proselint textlint tex-chktex emacs-lisp-checkdoc sh-shellcheck))
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

  ;; Chain checkers with lsp, using per-project directory local variables.
  ;; https://github.com/flycheck/flycheck/issues/1762
  (defvar-local sb/flycheck-local-checkers nil)
  (defun sb/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker sb/flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get))

(use-package consult-flycheck
  :after flycheck
  :bind (:map flycheck-command-map ("!" . consult-flycheck)))

;; Include `hl-todo' keywords in Flycheck messages.
(use-package flycheck-hl-todo
  :after flycheck
  :init (flycheck-hl-todo-setup))

;; Jump to `hl-todo' keywords in current buffer.
(use-package consult-todo
  :after (consult hl-todo)
  :commands (consult-todo consult-todo-all))

;; (use-package sideline
;;   :init (setq sideline-backends-left nil)
;;   :hook ((flycheck-mode lsp-mode eglot-managed-mode) . sideline-mode)
;;   :custom
;;   (sideline-display-backend-name t)
;;   (sideline-display-backend-type 'inner)
;;   :diminish)

;; (use-package sideline-flycheck
;;   :after sideline
;;   :demand t
;;   :init (setq sideline-flycheck-display-mode 'line)
;;   :hook (flycheck-mode . sideline-flycheck-setup))

;; (use-package sideline-lsp
;;   :when (eq sb/lsp-provider 'lsp-mode)
;;   :after sideline
;;   :demand t
;;   :config
;;   (setq sideline-backends-right
;;         '((sideline-lsp . up) (sideline-flycheck . down))))

;; (use-package sideline-eglot
;;   :when (eq sb/lsp-provider 'eglot)
;;   :after sideline
;;   :demand t
;;   :config
;;   (setq sideline-backends-right
;;         `(((when (featurep 'eglot)
;;              'sideline-eglot)
;;            . up)
;;           ((when (featurep 'flycheck)
;;              'sideline-flycheck)
;;            . down))))

;; (use-package format-all
;;   :hook
;;   ((format-all-mode . format-all-ensure-formatter)
;;    ((markdown-mode markdown-ts-mode) . format-all-mode))
;;   :config
;;   (setq-default format-all-formatters
;;                 '(("Assembly" asmfmt)
;;                   ("Awk" gawk)
;;                   ("BibTeX" latexindent)
;;                   ("C" clang-format)
;;                   ("C++" clang-format)
;;                   ("CMake" cmake-format)
;;                   ("CSS" prettier)
;;                   ("Cuda" clang-format)
;;                   ("Emacs Lisp" emacs-lisp)
;;                   ("Fish" fish-indent)
;;                   ("HTML" (prettier "--print-width" "80"))
;;                   ("LaTeX" latexindent)
;;                   ("Markdown" (prettier "--print-width" "80"))
;;                   ("Perl" (perltidy
;;                     "--quiet"
;;                     "--standard-error-output"
;;                     "--perl-best-practices"
;;                     "-l=80"))
;;                   ("Python" (yapf "--style" "file") isort)
;;                   ("Shell" (shfmt "-i" "4" "-ci"))
;;                   ("XML" tidy)
;;                   ("YAML" prettier "--print-width" "80")))
;;   (with-eval-after-load "markdown-mode"
;;     (bind-key "C-x f" #'format-all-buffer markdown-mode-map))
;;   (with-eval-after-load "tex"
;;     (bind-key "C-x f" #'format-all-buffer LaTeX-mode-map))
;;   :diminish)

(use-package apheleia
  :hook ((markdown-mode markdown-ts-mode) . apheleia-mode)
  :custom (apheleia-formatters-respect-fill-column t)
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--print-width" "80"))
  (setf (alist-get 'shfmt apheleia-formatters) '("shfmt" "-i" 4 "-ci")))

;; Auto-format Elisp code
(use-package elisp-autofmt
  :hook ((emacs-lisp-mode lisp-data-mode) . elisp-autofmt-mode)
  :custom
  (elisp-autofmt-python-bin "python3")
  (elisp-autofmt-on-save-p 'always))

(use-package shfmt
  :hook ((sh-mode bash-ts-mode) . shfmt-on-save-mode)
  :custom
  ;; p: Posix, ci: indent case labels, i: indent with spaces
  (shfmt-arguments '("-i" "4" "-ln" "bash" "-ci"))
  :diminish shfmt-on-save-mode)

;; Provides indentation guide bars with tree-sitter support
(use-package indent-bars
  :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode) . indent-bars-mode)
  :config
  (when (and (executable-find "tree-sitter")
             (fboundp 'treesit-available-p)
             (treesit-available-p))
    (setopt
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
  (dolist (exclude
           '(doc-view-mode
             pdf-view-mode tags-table-mode image-mode archive-mode))
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
    (advice-add 'company-capf--candidates :around #'sb/just-one-face)))

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
   ("M-n" . minibuffer-next-completion))
  :custom (completion-ignore-case t)
  ;; Ignore case when reading a file name
  (read-file-name-completion-ignore-case t)
  ;; Ignore case when reading a buffer name
  (read-buffer-completion-ignore-case t)
  (completion-cycle-threshold 3 "TAB cycle if there are only few candidates")
  :config
  ;; Show docstring description for completion candidates in commands like
  ;; `describe-function'.
  (when (boundp 'completions-detailed)
    (setopt completions-detailed t))
  (when (fboundp 'dabbrev-capf)
    (add-to-list 'completion-at-point-functions 'dabbrev-capf t))

  (with-eval-after-load "orderless"
    ;; substring is needed to complete common prefix, orderless does not
    (setopt completion-styles '(orderless substring partial-completion basic)))

  ;; The "basic" completion style needs to be tried first for TRAMP hostname
  ;; completion to work. I also want substring matching for file names.
  (setopt completion-category-overrides
          '((file (styles basic partial-completion)))))

;; It is recommended to load `yasnippet' before `eglot'
(use-package yasnippet
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook ((prog-mode LaTeX-mode) . yas-minor-mode)
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
  (("C-M-/" . company-other-backend) ; Invoke the next backend
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
  ;; Choices are: `company-pseudo-tooltip-unless-just-one-frontend' shows popup
  ;; unless there is only one candidate, `company-preview-frontend' shows the
  ;; preview in-place which is too intrusive,
  ;; `company-preview-if-just-one-frontend' shows in-place preview if there is
  ;; only choice, `company-echo-metadata-frontend' shows selected candidate docs
  ;; in echo area, and `company-pseudo-tooltip-frontend' which always shows the
  ;; candidates in an overlay.
  (company-frontends
   '(
     ;; Always show candidates in overlay tooltip
     company-pseudo-tooltip-frontend
     ;; Show selected candidate docs in echo area
     company-echo-metadata-frontend))
  :config
  (unless (bound-and-true-p sb/enable-icons)
    (setopt company-format-margin-function nil)))

(use-package nerd-icons
  :when (bound-and-true-p sb/enable-icons)
  :demand t
  :custom (nerd-icons-scale-factor 0.8))

;; `kind-icon' can be used for both Corfu and Company. I set up nerd icons for
;; Corfu with `nerd-icons-corfu'. I use `kind-icon' to provide nerd icons for
;; Company.
(use-package kind-icon
  :when (bound-and-true-p sb/enable-icons)
  :demand t
  :custom (kind-icon-blend-background nil)
  ;; Prefer smaller icons and a more compact popup
  (kind-icon-default-style
   '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.6))
  :config
  ;; Prefer nerd-icons for Corfu with `nerd-icons-corfu'
  ;; (when (eq sb/in-buffer-completion 'corfu)
  ;;   (with-eval-after-load "corfu"
  ;;     ;; Compute blended backgrounds correctly
  ;;     (setopt kind-icon-default-face 'corfu-default)
  ;;     (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

  ;; Replace default company icons with nerd-icons
  (when (eq sb/in-buffer-completion 'company)
    (with-eval-after-load "company"
      (require 'svg-lib)
      (add-to-list
       'svg-lib-icon-collections
       '("nerd-fonts-codicons"
         .
         "https://github.com/microsoft/vscode-codicons/raw/HEAD/src/icons/%s.svg"))

      ;; (setopt kind-icon-mapping
      ;;         '((array
      ;;            "a"
      ;;            :icon "symbol-array"
      ;;            :face font-lock-type-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (boolean
      ;;            "b"
      ;;            :icon "symbol-boolean"
      ;;            :face font-lock-builtin-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (class
      ;;            "c"
      ;;            :icon "symbol-class"
      ;;            :face font-lock-type-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (color
      ;;            "#"
      ;;            :icon "symbol-color"
      ;;            :face success
      ;;            :collection "nerd-fonts-codicons")
      ;;           (command
      ;;            "cm"
      ;;            :icon "chevron-right"
      ;;            :face default
      ;;            :collection "nerd-fonts-codicons")
      ;;           (constant
      ;;            "co"
      ;;            :icon "symbol-constant"
      ;;            :face font-lock-constant-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (constructor
      ;;            "cn"
      ;;            :icon "symbol-method"
      ;;            :face font-lock-function-name-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (enum
      ;;            "e"
      ;;            :icon "symbol-enum"
      ;;            :face font-lock-builtin-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (enummember
      ;;            "em"
      ;;            :icon "symbol-enum-member"
      ;;            :face font-lock-builtin-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (enum-member
      ;;            "em"
      ;;            :icon "symbol-enum-member"
      ;;            :face font-lock-builtin-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (event
      ;;            "ev"
      ;;            :icon "symbol-event"
      ;;            :face font-lock-warning-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (field
      ;;            "fd"
      ;;            :icon "symbol-field"
      ;;            :face font-lock-variable-name-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (file
      ;;            "f"
      ;;            :icon "symbol-file"
      ;;            :face font-lock-string-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (folder
      ;;            "d"
      ;;            :icon "folder"
      ;;            :face font-lock-doc-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (function "f"
      ;;                     :icon "symbol-method"
      ;;                     :face font-lock-function-name-face
      ;;                     :collection "nerd-fonts-codicons")
      ;;           ;; For Python
      ;;           (instance
      ;;            "in"
      ;;            :icon "symbol-variable"
      ;;            :face font-lock-variable-name-face
      ;;            :collection "vscode")
      ;;           (interface
      ;;            "if"
      ;;            :icon "symbol-interface"
      ;;            :face font-lock-type-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (keyword
      ;;            "kw"
      ;;            :icon "symbol-keyword"
      ;;            :face font-lock-keyword-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (macro "mc" :icon "lambda" :face font-lock-keyword-face)
      ;;           (magic
      ;;            "ma"
      ;;            :icon "lightbulb-autofix"
      ;;            :face font-lock-builtin-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (method
      ;;            "m"
      ;;            :icon "symbol-method"
      ;;            :face font-lock-function-name-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (module
      ;;            "{"
      ;;            :icon "file-code-outline"
      ;;            :face font-lock-preprocessor-face)
      ;;           (namespace
      ;;            "ns"
      ;;            :icon "file-code-outline"
      ;;            :face font-lock-preprocessor-face)
      ;;           (numeric
      ;;            "nu"
      ;;            :icon "symbol-numeric"
      ;;            :face font-lock-builtin-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (operator
      ;;            "op"
      ;;            :icon "symbol-operator"
      ;;            :face font-lock-comment-delimiter-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (param
      ;;            "pa"
      ;;            :icon "gear"
      ;;            :face default
      ;;            :collection "nerd-fonts-codicons")
      ;;           (property
      ;;            "pr"
      ;;            :icon "symbol-property"
      ;;            :face font-lock-variable-name-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (reference
      ;;            "rf"
      ;;            :icon "library"
      ;;            :face font-lock-variable-name-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (snippet
      ;;            "S"
      ;;            :icon "symbol-snippet"
      ;;            :face font-lock-string-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (statement
      ;;            "st"
      ;;            :icon "symbol-field"
      ;;            :face font-lock-variable-name-face
      ;;            :collection "vscode")
      ;;           (string
      ;;            "s"
      ;;            :icon "symbol-string"
      ;;            :face font-lock-string-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (struct
      ;;            "%"
      ;;            :icon "symbol-structure"
      ;;            :face font-lock-variable-name-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (text
      ;;            "tx"
      ;;            :icon "symbol-key"
      ;;            :face font-lock-doc-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (typeparameter
      ;;            "tp"
      ;;            :icon "symbol-parameter"
      ;;            :face font-lock-type-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (type-parameter
      ;;            "tp"
      ;;            :icon "symbol-parameter"
      ;;            :face font-lock-type-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (unit
      ;;            "u"
      ;;            :icon "symbol-ruler"
      ;;            :face font-lock-constant-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (value
      ;;            "v"
      ;;            :icon "symbol-enum"
      ;;            :face font-lock-builtin-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (variable
      ;;            "va"
      ;;            :icon "symbol-variable"
      ;;            :face font-lock-variable-name-face
      ;;            :collection "nerd-fonts-codicons")
      ;;           (t
      ;;            "."
      ;;            :icon "question"
      ;;            :face font-lock-warning-face
      ;;            :collection "nerd-fonts-codicons")))

      (setopt kind-icon-mapping
              `((array
                 ,(nerd-icons-codicon "nf-cod-symbol_array")
                 :face font-lock-type-face)
                (boolean
                 ,(nerd-icons-codicon "nf-cod-symbol_boolean")
                 :face font-lock-builtin-face)
                (class
                 ,(nerd-icons-codicon "nf-cod-symbol_class")
                 :face font-lock-type-face)
                (color
                 ,(nerd-icons-codicon "nf-cod-symbol_color")
                 :face success)
                (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
                (constant
                 ,(nerd-icons-codicon "nf-cod-symbol_constant")
                 :face font-lock-constant-face)
                (constructor
                 ,(nerd-icons-codicon "nf-cod-triangle_right")
                 :face font-lock-function-name-face)
                (enummember
                 ,(nerd-icons-codicon "nf-cod-symbol_enum_member")
                 :face font-lock-builtin-face)
                (enum-member
                 ,(nerd-icons-codicon "nf-cod-symbol_enum_member")
                 :face font-lock-builtin-face)
                (enum
                 ,(nerd-icons-codicon "nf-cod-symbol_enum")
                 :face font-lock-builtin-face)
                (event
                 ,(nerd-icons-codicon "nf-cod-symbol_event")
                 :face font-lock-warning-face)
                (field
                 ,(nerd-icons-codicon "nf-cod-symbol_field")
                 :face font-lock-variable-name-face)
                (file
                 ,(nerd-icons-codicon "nf-cod-symbol_file")
                 :face font-lock-string-face)
                (folder
                 ,(nerd-icons-codicon "nf-cod-folder")
                 :face font-lock-doc-face)
                (interface
                 ,(nerd-icons-codicon "nf-cod-symbol_interface")
                 :face font-lock-type-face)
                (keyword
                 ,(nerd-icons-codicon "nf-cod-symbol_keyword")
                 :face font-lock-keyword-face)
                (macro
                 ,(nerd-icons-codicon "nf-cod-symbol_misc")
                 :face font-lock-keyword-face)
                (magic
                 ,(nerd-icons-codicon "nf-cod-wand")
                 :face font-lock-builtin-face)
                (method
                 ,(nerd-icons-codicon "nf-cod-symbol_method")
                 :face font-lock-function-name-face)
                (function ,(nerd-icons-codicon "nf-cod-symbol_method")
                          :face font-lock-function-name-face)
                (module
                 ,(nerd-icons-codicon "nf-cod-file_submodule")
                 :face font-lock-preprocessor-face)
                (numeric
                 ,(nerd-icons-codicon "nf-cod-symbol_numeric")
                 :face font-lock-builtin-face)
                (operator
                 ,(nerd-icons-codicon "nf-cod-symbol_operator")
                 :face font-lock-comment-delimiter-face)
                (param
                 ,(nerd-icons-codicon "nf-cod-symbol_parameter")
                 :face default)
                (property
                 ,(nerd-icons-codicon "nf-cod-symbol_property")
                 :face font-lock-variable-name-face)
                (reference
                 ,(nerd-icons-codicon "nf-cod-references")
                 :face font-lock-variable-name-face)
                (snippet
                 ,(nerd-icons-codicon "nf-cod-symbol_snippet")
                 :face font-lock-string-face)
                (string
                 ,(nerd-icons-codicon "nf-cod-symbol_string")
                 :face font-lock-string-face)
                (struct
                 ,(nerd-icons-codicon "nf-cod-symbol_structure")
                 :face font-lock-variable-name-face)
                (text
                 ,(nerd-icons-codicon "nf-cod-text_size")
                 :face font-lock-doc-face)
                (typeparameter
                 ,(nerd-icons-codicon "nf-cod-list_unordered")
                 :face font-lock-type-face)
                (type-parameter
                 ,(nerd-icons-codicon "nf-cod-list_unordered")
                 :face font-lock-type-face)
                (unit
                 ,(nerd-icons-codicon "nf-cod-symbol_ruler")
                 :face font-lock-constant-face)
                (value
                 ,(nerd-icons-codicon "nf-cod-symbol_field")
                 :face font-lock-builtin-face)
                (variable
                 ,(nerd-icons-codicon "nf-cod-symbol_variable")
                 :face font-lock-variable-name-face)
                (t
                 ,(nerd-icons-codicon "nf-cod-code")
                 :face font-lock-warning-face)))

      (let* ((kind-func (lambda (cand) (company-call-backend 'kind cand)))
             (formatter
              (kind-icon-margin-formatter `((company-kind . ,kind-func)))))
        (defun sb/company-kind-icon-margin (cand _selected)
          (funcall formatter cand))
        (setopt company-format-margin-function
                #'sb/company-kind-icon-margin)))))

;; Show documentation popups
(use-package company-quickhelp
  :when (eq sb/in-buffer-completion 'company)
  :after company
  :when (display-graphic-p)
  :hook (prog-mode . company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :when (eq sb/in-buffer-completion 'company)
  :after company-quickhelp
  :unless (display-graphic-p)
  :hook (prog-mode . company-quickhelp-terminal-mode))

;; (use-package company-statistics
;;   :when (eq sb/in-buffer-completion 'company)
;;   :after company
;;   :init (company-statistics-mode 1))

;; By default, Unicode symbols backend (`company-math-symbols-unicode') is not
;; active in latex math environments and latex math symbols
;; (`company-math-symbols-latex') is not available outside of math latex
;; environments
(use-package company-math
  :after (:all tex-mode company)
  :demand t)

(use-package company-dict
  :after company
  :demand t
  :custom
  (company-dict-dir (expand-file-name "company-dict" user-emacs-directory))
  (company-dict-enable-yasnippet nil))

;; LATER: This package seems to trigger loading of `org-mode'
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

(use-package company-auctex
  :after (company tex)
  :commands
  (company-auctex-bibs
   company-auctex-environments
   company-auctex-labels
   company-auctex-macros
   company-auctex-symbols))

;; Uses RefTeX to complete label references and citations. When working with
;; multi-file documents, ensure that the variable `TeX-master' is appropriately
;; set in all files, so that RefTeX can find citations across documents.
(use-package company-reftex
  :after (company tex)
  :demand t
  :custom
  ;; https://github.com/TheBB/company-reftex/pull/13
  (company-reftex-labels-parse-all nil))

;; Notes on how to set up `company-backends'.

;; A few mode-agnostic backends are applicable to all modes:
;; `company-yasnippet', `company-ispell', `company-dabbrev-code', and
;; `company-dabbrev'. `company-yasnippet' is blocking. `company-dabbrev' returns
;; a non-nil prefix in almost any context (major mode, inside strings, or
;; comments). That is why it is better to put `company-dabbrev' at the end. The
;; ‘prefix’ bool command always returns non-nil for following backends even when
;; their ‘candidates’ list command is empty: `company-abbrev',
;; `company-dabbrev', `company-dabbrev-code'.

;; Most backends (e.g., `company-yasnippet') will not pass control to subsequent
;; backends. Only a few backends are specialized on certain major modes or
;; certain contexts (e.g. outside of strings and comments), and pass on control
;; to later backends when outside of that major mode or context.

;; The keyword ":with" helps to make sure the results from major/minor mode
;; agnostic backends (such as `company-yasnippet', `company-dabbrev-code') are
;; returned without preventing results from context-aware backends (such as
;; `company-capf' or `company-clang'). For this feature to work, put backends
;; dependent on a mode at the beginning of the grouped backends list, then put a
;; keyword ":with", and then put context agnostic backend(s).

;; Company does not support grouping of entirely arbitrary backends, they need
;; to be compatible in what `prefix' returns. If the group contains keyword
;; `:with', the backends listed after this keyword are ignored for the purpose
;; of the `prefix' command. If the group contains keyword `:separate', the
;; candidates that come from different backends are sorted separately in the
;; combined list. That is, with `:separate', the multi-backend-adapter will stop
;; sorting and keep the order of completions just like the backends returned
;; them.

;; Try completion backends in order until there is a non-empty completion list:
;; (setq company-backends '(company-xxx company-yyy company-zzz))

;; Merge completions of all the backends:
;; (setq company-backends '((company-xxx company-yyy company-zzz)))

;; (setq company-backends '((company-capf :with company-yasnippet)))

;; Merge completions of all the backends but keep the candidates organized in
;; accordance with the grouped backends order.
;; (setq company-backends '((company-xxx company-yyy company-zzz :separate)))

(with-eval-after-load "company"
  ;; Override `company-backends' for unhandled major modes.
  (setopt
   company-backends
   '(company-files
     (company-capf :with company-dabbrev-code company-yasnippet)
     ;; If we have `company-dabbrev' first, then other matches from
     ;; `company-ispell' will be ignored.
     company-dict company-ispell company-dabbrev)
   company-transformers '(delete-dups))

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
      (setq-local company-backends
                  '((company-reftex-citations company-auctex-bibs)
                    (company-reftex-labels company-auctex-labels)
                    (company-auctex-symbols
                     company-auctex-environments
                     company-auctex-macros
                     company-latex-commands
                     ;; Math latex tags
                     company-math-symbols-latex
                     ;; Math Unicode symbols and sub(super)scripts
                     company-math-symbols-unicode)
                    company-files
                    company-dict
                    company-ispell
                    company-dabbrev
                    company-yasnippet)))

    (add-hook 'LaTeX-mode-hook #'sb/company-latex-mode))

  (progn
    (defun sb/company-org-mode ()
      (set
       (make-local-variable 'company-backends)
       '(company-files
         company-org-block company-dict company-ispell company-dabbrev)))

    (add-hook 'org-mode-hook #'sb/company-org-mode))

  (progn
    (defun sb/company-text-mode ()
      "Add backends for `text-mode' completion in company mode."
      (set
       (make-local-variable 'company-backends)
       '(company-dict company-ispell company-files company-dabbrev)))

    ;; Extends to derived modes like `markdown-mode' and `org-mode'
    (add-hook
     'text-mode-hook
     (lambda ()
       (unless (derived-mode-p 'LaTeX-mode)
         (sb/company-text-mode)))))

  (progn
    (defun sb/company-yaml-mode ()
      (make-local-variable 'company-backends)
      (setq-local company-backends
                  '(company-files
                    (company-capf
                     :with
                     company-dabbrev-code ; Useful for variable names
                     company-yasnippet)
                    company-ispell company-dabbrev)))

    (dolist (mode '(yaml-mode-hook yaml-ts-mode-hook))
      (add-hook mode #'sb/company-yaml-mode)))

  (progn
    (defun sb/company-html-mode ()
      (set
       (make-local-variable 'company-backends)
       '(company-files
         company-capf company-dict company-ispell company-dabbrev)))

    (dolist (hook '(html-mode-hook html-ts-mode-hook))
      (add-hook hook #'sb/company-html-mode)))

  (progn
    (defun sb/company-prog-mode ()
      (setq-local company-backends
                  '(company-files
                    (company-capf
                     :separate
                     ;; company-citre-tags
                     company-c-headers company-ctags company-keywords
                     company-dabbrev-code ; Useful for variable names
                     :with company-yasnippet)
                    company-ispell company-dabbrev)))

    (add-hook
     'prog-mode-hook
     (lambda ()
       (unless (or (derived-mode-p 'emacs-lisp-mode)
                   (derived-mode-p 'lisp-data-mode)
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
                     company-yasnippet)
                    company-ispell company-dabbrev)))

    (dolist (hook '(emacs-lisp-mode-hook lisp-data-mode-hook))
      (add-hook hook #'sb/company-elisp-mode))))

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
      (corfu-history-mode 1)
      (corfu-echo-mode 1)
      (corfu-popupinfo-mode 1)
      ;; The right edge is getting cut off
      ;; (corfu-indexed-mode 1)
      )))
  :bind
  (:map
   corfu-map
   ("TAB" . corfu-quick-insert)
   ("<tab>" . corfu-quick-insert)
   ("M-d" . corfu-info-documentation)
   ("M-l" . corfu-info-location)
   ("M-n" . corfu-popupinfo-scroll-up)
   ("M-p" . corfu-popupinfo-scroll-down)
   ([remap corfu-show-documentation] . corfu-popupinfo-toggle))
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-prefix 3)
  (corfu-on-exact-match 'show)
  :config
  ;; Disable `orderless' completion for Corfu
  (add-hook
   'corfu-mode-hook
   (lambda ()
     (setq-local
      completion-styles '(basic)
      completion-category-defaults nil
      completion-category-overrides '((consult-location (styles . (orderless))))))))

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
  :config
  ;; TODO: Is this working correctly?
  (add-to-list 'completion-category-overrides '((cape-dict (styles (basic)))))

  ;; Make the capf composable
  (with-eval-after-load "lsp-mode"
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-buster))

  (with-eval-after-load "eglot"
    (advice-add
     #'eglot-completion-at-point
     :around #'cape-wrap-noninterruptible)
    (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
    (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster))

  ;; Initialize for all generic languages that are not specifically handled. The
  ;; order of the functions matters, unless they are merged, the first function
  ;; returning a result wins. Note that the list of buffer-local completion
  ;; functions takes precedence over the global list.

  ;; File completion with `cape-file' is available in comments and string
  ;; literals, but not in normal code.

  ;; `cape-capf-super' works for static completion functions like
  ;; `cape-dabbrev', `cape-keyword', and `cape-dict', but not for multi-step
  ;; completions like `cape-file'.

  (add-hook
   'prog-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  (cape-capf-inside-code
                   (cape-capf-super
                    #'cape-keyword (cape-capf-prefix-length #'cape-dabbrev 4)))
                  (cape-capf-inside-comment #'cape-dict)
                  #'cape-dabbrev
                  #'cape-file
                  #'yasnippet-capf))))

  ;; Override CAPFS for specific major modes
  (dolist (mode '(emacs-lisp-mode-hook lisp-data-mode-hook))
    (add-hook
     mode
     (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    (cape-capf-inside-code
                     (cape-capf-super
                      #'elisp-completion-at-point
                      #'cape-elisp-symbol
                      (cape-capf-prefix-length #'cape-dabbrev 4)))
                    (cape-capf-inside-comment #'cape-dict)
                    #'cape-dabbrev
                    #'cape-file
                    #'yasnippet-capf)))))

  ;; https://github.com/minad/cape/discussions/130
  ;; There is no mechanism to force deduplication if candidates from `cape-dict'
  ;; and `cape-dabbrev' are not exactly equal (equal string and equal text
  ;; properties).

  (setq completion-at-point-functions
        (list #'cape-dict #'cape-dabbrev #'cape-file))

  (add-hook
   'text-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  #'cape-dict #'cape-dabbrev #'cape-file #'yasnippet-capf))))

  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  #'cape-elisp-block
                  #'cape-dict
                  #'cape-dabbrev
                  #'cape-file
                  #'yasnippet-capf))))

  (when (eq sb/lsp-provider 'eglot)
    (dolist (mode '(LaTeX-mode-hook bibtex-mode-hook))
      (add-hook
       mode
       (lambda ()
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (setq-local
             completion-at-point-functions
             (list
              (cape-capf-super
               ;; Math latex tags
               (cape-company-to-capf #'company-math-symbols-latex)
               ;; Math Unicode symbols and sub(super)scripts
               (cape-company-to-capf #'company-math-symbols-unicode)
               (cape-company-to-capf #'company-latex-commands)
               ;; Used for Unicode symbols and not for the corresponding LaTeX
               ;; names.
               #'cape-tex)
              (cape-capf-super #'citar-capf #'bibtex-capf)
              #'cape-dict
              #'cape-dabbrev
              #'cape-file
              #'yasnippet-capf))))))))

  (when (eq sb/lsp-provider 'lsp-mode)
    (dolist (mode '(LaTeX-mode-hook bibtex-mode-hook))
      (add-hook
       mode
       (lambda ()
         (setq-local
          completion-at-point-functions
          (list
           (cape-capf-super
            ;; Math latex tags
            (cape-company-to-capf #'company-math-symbols-latex)
            ;; Math Unicode symbols and sub(super)scripts
            (cape-company-to-capf #'company-math-symbols-unicode)
            (cape-company-to-capf #'company-latex-commands)
            ;; Used for Unicode symbols and not for the corresponding LaTeX
            ;; names.
            #'cape-tex)
           (cape-capf-super #'citar-capf #'bibtex-capf)
           #'cape-dict
           #'cape-dabbrev
           #'cape-file
           #'yasnippet-capf))))))

  (with-eval-after-load "lsp-mode"
    (dolist (mode
             '(bash-ts-mode-hook
               c-mode-hook
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
               json-mode-hook
               json-ts-mode-hook
               jsonc-mode-hook
               makefile-mode
               python-mode-hook
               python-ts-mode-hook
               sh-mode-hook
               yaml-mode-hook
               yaml-ts-mode-hook))
      (add-hook
       mode
       (lambda ()
         (setq-local completion-at-point-functions
                     (list
                      (cape-capf-inside-code
                       (cape-capf-super
                        #'lsp-completion-at-point
                        #'citre-completion-at-point
                        #'cape-keyword
                        (cape-capf-prefix-length #'cape-dabbrev 4)))
                      (cape-capf-inside-comment #'cape-dict)
                      #'cape-dabbrev
                      #'cape-file
                      #'yasnippet-capf))))))

  (with-eval-after-load "eglot"
    (dolist (mode
             '(bash-ts-mode-hook
               c-mode-hook
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
               json-mode-hook
               json-ts-mode-hook
               jsonc-mode-hook
               makefile-mode-hook
               python-mode-hook
               python-ts-mode-hook
               sh-mode-hook
               yaml-mode-hook
               yaml-ts-mode-hook))
      (add-hook
       mode
       (lambda ()
         (setq-local completion-at-point-functions
                     (list
                      (cape-capf-inside-code
                       (cape-capf-super
                        #'eglot-completion-at-point
                        #'citre-completion-at-point
                        #'cape-keyword
                        (cape-capf-prefix-length #'cape-dabbrev 4)))
                      (cape-capf-inside-comment #'cape-dict)
                      #'cape-dabbrev
                      #'cape-file
                      #'yasnippet-capf)))))))

;; Prescient uses frecency (frequency + recency) for sorting. Recently used
;; commands should be sorted first. Only commands that have never been used
;; before will be sorted by length. Vertico does its own sorting based on
;; recency, Corfu has `corfu-history', and Company has `company-statistics'.
(use-package prescient
  :straight (:host github :repo "radian-software/prescient.el" :files (:defaults "/*.el"))
  :hook (emacs-startup . prescient-persist-mode)
  :custom (prescient-sort-full-matches-first t)
  :config
  (with-eval-after-load "corfu"
    (corfu-prescient-mode 1))
  (with-eval-after-load "vertico"
    (vertico-prescient-mode 1))
  (with-eval-after-load "company"
    (company-prescient-mode 1)))

;; It is tempting to use `eglot' because it is built in to Emacs. However,
;; `lsp-mode' offers several advantages. It allows connecting to multiple
;; servers simultaneously and provides helpers to install and uninstall servers.
(use-package lsp-mode
  :when (eq sb/lsp-provider 'lsp-mode)
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
  (lsp-eldoc-enable-hover nil "Do not show noisy hover info with mouse")
  (lsp-enable-dap-auto-configure nil "I do not use dap-mode")
  (lsp-enable-on-type-formatting nil "Reduce unexpected modifications to code")
  (lsp-enable-folding nil "I do not find the feature useful")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  ;; (lsp-enable-file-watchers nil "Avoid watcher warnings")
  (lsp-lens-enable nil "Lenses are intrusive")
  ;; I use `symbol-overlay' to include languages that do not have a language
  ;; server
  (lsp-enable-symbol-highlighting nil)
  ;; The workspace status icon on the terminal interface is misleading across
  ;; projects
  (lsp-modeline-workspace-status-enable nil)
  ;; (lsp-enable-snippet t)
  (lsp-enable-suggest-server-download nil)
  (lsp-inlay-hint-enable nil "Intrusive")
  ;; Enable integration of custom backends other than `capf'
  (lsp-completion-provider :none)
  ;; Show/hide completion metadata, e.g., "java.util.ArrayList"
  (lsp-completion-show-detail t)
  ;; Show/hide completion kind, e.g., interface/class
  (lsp-completion-show-kind t)
  ;; Show/hide description of completion candidates
  (lsp-completion-show-label-description t)
  (lsp-completion-default-behaviour :insert)
  (lsp-use-plists t)
  ;; I mostly SSH into the remote machine and launch Emacs, rather than using
  ;; Tramp which is slower
  (lsp-auto-register-remote-clients nil)
  (lsp-keep-workspace-alive nil)
  (lsp-progess-via-spinner nil)
  (lsp-imenu-sort-methods '(position) "More natural way of listing symbols")
  ;; Simpler to focus on the errors at hand
  (lsp-modeline-diagnostics-scope :file)
  ;; Sudden changes in the height of the echo area causes the cursor to lose
  ;; position, manually request via `lsp-signature-activate'.
  (lsp-signature-auto-activate nil)
  ;; Avoid annoying questions, we expect a server restart to succeed
  (lsp-restart 'auto-restart)
  ;; Avoid warning messages for unsupported modes like `csv-mode'
  (lsp-warn-no-matched-clients nil)
  ;; Client-specific configuration
  (lsp-clangd-version "19.1.2")
  (lsp-clients-clangd-args
   '("-j=4"
     "--all-scopes-completion"
     "--background-index"
     "--background-index-priority=low"
     "--clang-tidy"
     "--completion-style=detailed"
     "--fallback-style=LLVM"
     ;; Do not automatically insert #include statements when editing code
     "--header-insertion=never"
     "--header-insertion-decorators"
     "--log=error"
     ;; Unsupported options with Clangd 10: malloc-trim and enable-config
     "--malloc-trim" ; Release memory periodically
     "--enable-config"
     "--pch-storage=memory" ; Increases memory usage but can improve performance
     "--pretty"))
  (lsp-html-format-wrap-line-length fill-column)
  (lsp-html-format-end-with-newline t)
  (lsp-html-format-indent-inner-html t)
  (lsp-xml-logs-client nil)
  (lsp-pylsp-configuration-sources ["setup.cfg"])
  (lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-pylsp-plugins-preload-modules ["numpy" "csv" "pandas" "statistics"])
  (lsp-pylsp-plugins-pydocstyle-convention "pep257")
  (lsp-pylsp-plugins-pylint-enabled t)
  (lsp-pylsp-plugins-yapf-enabled t)
  (lsp-pylsp-plugins-flake8-enabled nil)
  (lsp-pylsp-plugins-isort-enabled t)
  (lsp-pylsp-plugins-mypy-enabled t)
  :config
  (when (display-graphic-p)
    (setopt lsp-modeline-code-actions-segments '(count icon name)))
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
  (defun sb/lsp-mode-disable-orderless ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(substring)))
  (add-hook 'lsp-completion-mode-hook #'sb/lsp-mode-disable-orderless)
  :diminish)

(use-package lsp-ui
  :when (eq sb/lsp-provider 'lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  ;; Disable intrusive on-hover dialogs, invoke with `lsp-ui-doc-show'
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-width 72 "150 (default) is too wide")
  (lsp-ui-doc-delay 0.75 "0.2 (default) is too naggy")
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-sideline-enable nil)
  ;; Enables understanding when to invoke code actions
  (lsp-ui-sideline-show-code-actions t)
  ;; Hide diagnostics when typing because they can be intrusive, Flycheck and
  ;; Flymake already highlight errors
  (lsp-ui-sideline-show-diagnostics nil)
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
  :when (eq sb/lsp-provider 'lsp-mode)
  :hook
  ((java-mode java-ts-mode)
   .
   (lambda ()
     (setq-local
      c-basic-offset 4
      c-set-style "java")
     (lsp-deferred)))
  :bind (:map lsp-mode-map ("y" . lsp-java-type-hierarchy))
  :custom
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-format-settings-url
   (concat
    "file://"
    (file-truename (locate-user-emacs-file "servers/eclipse-formatter.xml"))))
  (lsp-java-jdt-download-url
   "https://github.com/eclipse-jdtls/eclipse.jdt.ls/archive/refs/tags/v1.47.0.tar.gz"))

(use-package lsp-ltex-plus
  :straight (:host github :repo "emacs-languagetool/lsp-ltex-plus")
  :when (eq sb/lsp-provider 'lsp-mode)
  :init (setopt lsp-ltex-plus-version "18.5.1")
  :hook
  ((text-mode markdown-mode org-mode LaTeX-mode)
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
   '(:en-US
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

(use-package hl-todo
  :hook (emacs-startup . global-hl-todo-mode)
  :bind (("C-c p" . hl-todo-previous) ("C-c n" . hl-todo-next))
  :config
  (setopt
   hl-todo-highlight-punctuation ":"
   hl-todo-keyword-faces
   (append
    '(("LATER" . "#d0bf8f")
      ("IMP" . "#7cb8bb")
      ("TEST" . "tomato")
      ("WARNING" . "#cc0000"))
    hl-todo-keyword-faces)))

(use-package symbol-overlay
  :hook ((prog-mode conf-mode) . symbol-overlay-mode)
  :bind
  (("M-p" . symbol-overlay-jump-prev)
   ("M-n" . symbol-overlay-jump-next)
   :map
   symbol-overlay-map
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
  (with-eval-after-load "LaTeX-mode"
    (bind-key "<f10>" #'compile LaTeX-mode-map)
    (bind-key "<f11>" #'recompile LaTeX-mode-map)))

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
  ;; Allow Eldoc to trigger after completions
  (with-eval-after-load "company"
    (eldoc-add-command
     'company-complete-selection
     'company-complete-common
     'company-capf
     'company-abort))
  :diminish)

;; Tree-sitter provides advanced syntax highlighting features. Run
;; `tree-sitter-langs-install-grammar' to install the grammar files for
;; languages for tree-sitter. Many treesitter modes are derived from their based
;; modes since Emacs 30. For example, `(derived-mode-p 'c-mode)' will return t
;; in `c-ts-mode'. That means `.dir-locals.el' settings and yasnippets for
;; `c-mode' will work for `c-ts-mode' too. However, `c-ts-mode' still does not
;; run c-mode's major mode hooks. Also, there's still no major mode fallback.
(use-package treesit
  :straight (:type built-in)
  :when
  (and (executable-find "tree-sitter")
       (fboundp 'treesit-available-p)
       (treesit-available-p))
  :demand t
  :commands treesit-install-language-grammar
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
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
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
                     (treesit-language-available-p 'go)
                     (treesit-language-available-p 'html)
                     (treesit-language-available-p 'java)
                     (treesit-language-available-p 'javascript)
                     (treesit-language-available-p 'json)
                     (treesit-language-available-p 'kdl)
                     (treesit-language-available-p 'latex)
                     (treesit-language-available-p 'make)
                     (treesit-language-available-p 'markdown)
                     (treesit-language-available-p 'org)
                     (treesit-language-available-p 'perl)
                     (treesit-language-available-p 'php)
                     (treesit-language-available-p 'python)
                     (treesit-language-available-p 'toml)
                     (treesit-language-available-p 'tsx)
                     (treesit-language-available-p 'typescript)
                     (treesit-language-available-p 'rust)
                     (treesit-language-available-p 'yaml)))
    (mapc
     #'treesit-install-language-grammar
     (mapcar #'car treesit-language-source-alist)))

  (when (treesit-available-p)
    (setopt major-mode-remap-alist
            '((sh-mode . bash-ts-mode)
              (c-mode . c-ts-mode)
              (c++-mode . c++-ts-mode)
              (c-or-c++-mode . c-or-c++-ts-mode)
              (cmake-mode . cmake-ts-mode)
              (css-mode . css-ts-mode)
              (dockerfile-mode . dockerfile-ts-mode)
              (html-mode . html-ts-mode)
              (java-mode . java-ts-mode)
              (json-mode . json-ts-mode)
              (js2-mode . js-ts-mode)
              (kdl-mode . kdl-ts-mode)
              (python-mode . python-ts-mode)
              (toml-mode . toml-ts-mode)
              (conf-toml-mode . toml-ts-mode)
              (tsx-mode . tsx-ts-mode)
              (typescript-mode . typescript-ts-mode)
              (yaml-mode . yaml-ts-mode)))))

(use-package treesit-auto
  :after treesit
  :demand t
  :config
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; (with-eval-after-load "c++-ts-mode"
;;   (bind-key "C-M-a" #'treesit-beginning-of-defun c++-ts-mode-map)
;;   (bind-key "C-M-e" #'treesit-end-of-defun c++-ts-mode-map))

;; (with-eval-after-load "treesit"
;;   ;; Improves performance with large files without significantly diminishing
;;   ;; highlight quality
;;   (setq font-lock-maximum-decoration '((c-mode . 2) (c++-mode . 2) (t . t))))

(use-package cc-mode
  :straight (:type built-in)
  :hook
  (awk-mode
   .
   (lambda ()
     (setq-local c-basic-offset 4)
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred))))))

(use-package c-ts-mode
  :straight (:type built-in)
  :when
  (and (executable-find "tree-sitter")
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
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred)))))
  :bind
  (:map
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
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (progn
         (setq-local lsp-disabled-clients '(ltex-ls-plus))
         (lsp-deferred)))))))

(use-package python
  :straight (:type built-in)
  :mode
  (("SCon\(struct\|script\)$" . python-ts-mode)
   ("[./]flake8\\'" . conf-mode)
   ("/Pipfile\\'" . conf-mode))
  :hook
  ((python-mode python-ts-mode)
   .
   (lambda ()
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred)))))
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
      (setopt python-shell-interpreter
              (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks
   (list (lambda () (setopt python-shell-interpreter "python3")))))

(use-package cperl-mode
  :straight (:type built-in)
  :mode "latexmkrc\\'"
  :hook
  (cperl-mode
   .
   (lambda ()
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred)))))
  :config (fset 'perl-mode 'cperl-mode))

(use-package sh-script
  :straight (:type built-in)
  :mode ("\\bashrc\\'" . bash-ts-mode)
  :hook
  ((sh-mode bash-ts-mode)
   .
   (lambda ()
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred)))))
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
   .
   (lambda ()
     (add-hook 'before-save-hook #'fish_indent-before-save)
     ;; `lsp-mode' does not support fish yet
     (when (eq sb/lsp-provider 'eglot)
       (eglot-ensure))
     (when (eq sb/lsp-provider 'lsp-mode)
       (with-eval-after-load "lsp-mode"
         (lsp-register-client
          (make-lsp-client
           :new-connection
           (lsp-stdio-connection '("fish-lsp" "start"))
           :activation-fn (lsp-activate-on "fish")
           :server-id 'fish-lsp))
         (lsp-deferred))))))

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

(use-package ini-mode
  :commands (ini-mode))

(use-package conf-mode
  :straight (:type built-in)
  :mode
  "\\.cfg\\'"
  "\\.conf\\'")

(use-package toml-ts-mode
  :straight (:type built-in)
  :mode ("\\.toml\\'" "Cargo\\.lock\\'")
  :hook
  (toml-ts-mode
   .
   (lambda ()
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred))))))

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
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (progn
         (setq-local lsp-disabled-clients '(ltex-ls-plus))
         (lsp-deferred)))))))

(use-package yaml-imenu
  :hook ((yaml-mode yaml-ts-mode) . yaml-imenu-enable))

;; (use-package css-mode
;;   :straight (:type built-in)
;;   :hook
;;   ((css-mode css-ts-mode)
;;    .
;;    (lambda ()
;;      (cond
;;       ((eq sb/lsp-provider 'eglot)
;;        (eglot-ensure))
;;       ((eq sb/lsp-provider 'lsp-mode)
;;        (lsp-deferred)))))
;;   :custom (css-indent-offset 2))

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
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred))))))

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
     (cond
      ;; Eglot does not support multiple servers, so we use `ltex-ls-plus'.
      ;; ((eq sb/lsp-provider 'eglot)
      ;;  (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (progn
         (require 'lsp-marksman)
         (lsp-deferred))))))
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

;; (use-package markdown-ts-mode
;;   :mode ("\\.md\\'" . markdown-ts-mode)
;;   :config
;;   (add-to-list
;;    'treesit-language-source-alist
;;    '(markdown
;;      "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
;;      "split_parser"
;;      "tree-sitter-markdown/src"))
;;   (add-to-list
;;    'treesit-language-source-alist
;;    '(markdown-inline
;;      "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
;;      "split_parser"
;;      "tree-sitter-markdown-inline/src")))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf. Convert
;; `markdown' to `org': "pandoc -f markdown -t org -o output-file.org
;; input-file.md"
;; (use-package pandoc-mode
;;   :hook (markdown-mode . pandoc-mode)
;;   :config (pandoc-load-default-settings)
;;   :diminish)

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
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (progn
         (setq-local lsp-disabled-clients '(ltex-ls-plus))
         (lsp-deferred))))))
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
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred))))))

;; Links in org-mode by default are displayed as "descriptive" links, meaning
;; they hide their target URLs. While this looks great, it makes it a bit tricky
;; to figure out how you can edit their URL. There are two easy options: (i)
;; press "C-c C-l" (`org-insert-link') while your point is within a link and you
;; will be prompted to edit its URL in the minibuffer. You can use the same
;; command to create new links (when your point is not on an existing link).
;; (ii) You can convert the "descriptive" links to "literal" links by invoking
;; the command "M-x org-toggle-link-display". You can also toggle between the
;; two display modes for links. Use zero-width space "C-x 8 zero width space" to
;; treat Org markup as plain text.
;; https://orgmode.org/manual/Escape-Character.html
;; https://orgmode.org/manual/In_002dbuffer-Settings.html

(use-package org
  :defer 2
  :mode ("\\.org\\'" . org-mode)
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
  (org-agenda-files '("~/git/org" "~/git/org/client1" "~/git/client2"))
  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setopt
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

;; An alternate package is https://github.com/lorniu/org-expose-emphasis-markers.
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

;; Without auctex
(with-eval-after-load "tex-mode"
  (setopt tex-command "pdflatex"))

;; (use-package lsp-latex
;;   :when (eq sb/lsp-provider 'lsp-mode)
;;   :hook
;;   ((LaTeX-mode bibtex-mode)
;;    .
;;    (lambda ()
;;      (require 'lsp-latex)
;;      (lsp-deferred)))
;;   :custom
;;   (lsp-latex-bibtex-formatter "latexindent")
;;   (lsp-latex-latex-formatter "latexindent")
;;   (lsp-latex-bibtex-formatter-line-length fill-column)
;;   (lsp-latex-diagnostics-delay 2000)
;;   ;; Support forward search with Okular. Perform inverse search with Shift+Click
;;   ;; in the PDF.
;;   (lsp-latex-forward-search-executable "okular")
;;   (lsp-latex-forward-search-args '("--noraise --unique" "file:%p#src:%l%f"))
;;   :config
;;   (with-eval-after-load "tex-mode"
;;     (bind-key "C-c C-c" #'lsp-latex-build tex-mode-map)))

;; Auctex provides enhanced versions of `tex-mode' and `latex-mode', which
;; automatically replace the vanilla ones. Auctex provides `LaTeX-mode', which
;; is an alias to `latex-mode'. Auctex overrides the tex package. "P" in the
;; modeline highlighter "LaTeX/MPS" is due to `TeX-PDF-mode'.
(use-package auctex
  :hook
  ((LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . TeX-PDF-mode) ; Use `pdflatex'
   ;; Revert PDF buffer after TeX compilation has finished
   (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
   ;; Enable rainbow mode after applying styles to the buffer
   ;; (TeX-update-style . rainbow-delimiters-mode)
   (LaTeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . (lambda () (turn-on-reftex))))
  :custom
  ;; Enable parse on save, stores parsed information in an `auto' directory
  (TeX-auto-save t)
  (TeX-auto-untabify t "Remove all tabs before saving")
  (TeX-clean-confirm nil)
  ;; Automatically insert braces after typing ^ and _ in math mode
  (TeX-electric-sub-and-superscript t)
  ;; Inserting $ completes the math mode and positions the cursor
  ;; (TeX-electric-math t)
  (TeX-parse-self t "Parse documents")
  (TeX-save-query nil "Save buffers automatically when compiling")
  (LaTeX-item-indent 0 "Indent lists by two spaces")
  (LaTeX-fill-break-at-separators nil "Do not insert line-break at inline math")
  ;; Avoid raising of superscripts and lowering of subscripts
  (tex-fontify-script nil)
  ;; Avoid superscripts and subscripts from being displayed in a different font
  ;; size
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 1.0 "Avoid emphasizing section headers")
  :config
  ;; Make AUCTeX aware of the multifile document structure, always query for the
  ;; master file
  (setq-default
   TeX-master nil
   TeX-command-default "LaTexMk")
  (with-eval-after-load "tex-mode"
    (unbind-key "C-c ;" TeX-mode-map))

  ;; Enable correlation with synctex From Okular, press Shift + Left click to go
  ;; to the desired line.
  (setopt
   TeX-source-correlate-method 'synctex
   TeX-source-correlate-mode t
   TeX-source-correlate-start-server t
   ;; Enable synctex generation. Even though the command shows as "latex"
   ;; pdflatex is actually called
   LaTeX-command "latex -shell-escape=1 -synctex=1"))

(use-package tex
  :straight (:type built-in)
  :config
  (when (executable-find "okular")
    (add-to-list
     'TeX-view-program-list
     '("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
    (add-to-list 'TeX-view-program-selection '(output-pdf "Okular"))))

(use-package reftex
  :straight (:type built-in)
  :hook (LaTeX-mode . turn-on-reftex)
  :bind
  (("C-c [" . reftex-citation)
   ("C-c )" . reftex-reference)
   ("C-c (" . reftex-label)
   ("C-c &" . reftex-view-crossref))
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-enable-partial-scans t)
  (reftex-highlight-selection 'both)
  ;; Save parse info to avoid reparsing every time a file is visited
  (reftex-save-parse-info t)
  ;; Revisit files if necessary when browsing toc
  (reftex-revisit-to-follow t)
  (reftex-ref-macro-prompt nil) ; No unnecessary prompts
  (reftex-guess-label-type t "Try to guess the label type before prompting")
  (reftex-use-fonts t "Use nice fonts for TOC")
  ;; Cache selection buffers for faster access
  (reftex-use-multiple-selection-buffers t)
  :diminish)

(use-package bibtex
  :straight (:type built-in)
  :hook
  (bibtex-mode
   .
   (lambda ()
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred)))))
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-maintain-sorted-entries t)
  (bibtex-comma-after-last-field nil))

(use-package consult-reftex
  :straight (:host github :repo "karthink/consult-reftex")
  :after (consult LaTeX-mode)
  :commands (consult-reftex-insert-reference consult-reftex-goto-label))

;; LATER: This package seems to require `org'
;; Set `bibtex-capf-bibliography' in `.dir-locals.el'.
(use-package bibtex-capf
  :straight (:host github :repo "mclear-tools/bibtex-capf")
  :when (eq sb/in-buffer-completion 'corfu)
  :after tex
  :demand t)

(use-package math-delimiters
  :straight (:host github :repo "oantolin/math-delimiters")
  :after tex
  :demand t
  :commands (math-delimiters-no-dollars math-delimiters-toggle)
  :config
  (with-eval-after-load "LaTeX-mode"
    (bind-key "$" #'math-delimiters-insert LaTeX-mode-map)))

;; LATER: This package seems to require `org'
(use-package citar
  :when (eq sb/in-buffer-completion 'corfu)
  :after tex
  :demand t)

(use-package citar-embark
  :after (citar embark)
  :config (citar-embark-mode)
  :diminish)

;; (use-package auctex-latexmk
;;   :after tex
;;   :when (executable-find "latexmk")
;;   :demand t
;;   :custom
;;   ;; Pass the '-pdf' flag when `TeX-PDF-mode' is active
;;   (auctex-latexmk-inherit-TeX-PDF-mode t)
;;   :config
;;   ;; (setq-default TeX-command-default "LaTexMk")
;;   (auctex-latexmk-setup))

(use-package citre
  :preface
  (defun sb/citre-jump+ ()
    "Jump to the definition of the symbol at point using `citre-jump' first. Falls back to `xref-find-definitions' on failure."
    (interactive)
    (condition-case _
        (citre-jump)
      (error
       (let* ((xref-prompt-for-identifier nil))
         (call-interactively #'xref-find-definitions)))))
  :hook (prog-mode . citre-mode)
  :bind
  (("M-'" . sb/citre-jump+)
   ("C-x c j" . sb/citre-jump+)
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

  (when (eq sb/in-buffer-completion 'company)
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
                   (or (citre-get-symbol-at-point-for-backend ',backend)
                       'stop)))
             ('meta (citre-get-property 'signature arg))
             ('annotation (citre-get-property 'annotation arg))
             ('candidates
              (let ((citre-completion-backends '(,backend)))
                (all-completions arg (nth 2 (citre-completion-at-point)))))))))

    (citre-backend-to-company-backend tags))

  (defun sb/push-point-to-xref-marker-stack (&rest r)
    (xref-push-marker-stack (point-marker)))
  (dolist (func
           '(find-function consult-imenu
                           project-grep
                           deadgrep
                           counsel-rg
                           consult-lsp-file-symbols
                           citre-jump))
    (advice-add func :before 'sb/push-point-to-xref-marker-stack))
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
  :after (:all nerd-icons marginalia)
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
;; (use-package powerline
;;   :preface
;;   (defun sb/powerline-raw (str &optional face pad)
;;     "Render STR as mode-line data using FACE and optionally PAD import.
;; PAD can be left (`l') or right (`r')."
;;     (when str
;;       (let* ((rendered-str (format-mode-line str))
;;              (padded-str
;;               (concat
;;                (when (and (> (length rendered-str) 0) (eq pad 'l))
;;                  "")
;;                (if (listp str)
;;                    rendered-str
;;                  str)
;;                (when (and (> (length rendered-str) 0) (eq pad 'r))
;;                  ""))))
;;         (if face
;;             (pl/add-text-property padded-str 'face face)
;;           padded-str))))

;;   (defun sb/powerline-nano-theme ()
;;     "Setup a Nano-like modeline"
;;     (interactive)
;;     (setq-default mode-line-format
;;                   '("%e" (:eval
;;                      (let* ((active (powerline-selected-window-active))
;;                             (face0
;;                              (if active
;;                                  'powerline-active0
;;                                'powerline-inactive0))
;;                             (lhs
;;                              (list
;;                               (powerline-raw
;;                                (concat
;;                                 "GNU Emacs "
;;                                 (number-to-string emacs-major-version)
;;                                 "."
;;                                 (number-to-string emacs-minor-version))
;;                                nil 'l)))
;;                             (rhs
;;                              (list
;;                               (when which-function-mode
;;                                 (sb/powerline-raw which-func-format nil 'l))
;;                               (powerline-vc nil 'l)
;;                               (powerline-raw "")
;;                               (powerline-raw "%4l" nil 'l)
;;                               (powerline-raw ",")
;;                               (powerline-raw "%3c" nil 'r)
;;                               (if (buffer-modified-p)
;;                                   (powerline-raw " ⠾" nil 'r)
;;                                 (powerline-raw "  " nil 'r))))
;;                             (center (list (powerline-raw "%b" nil 'r))))
;;                        (concat
;;                         (powerline-render lhs)
;;                         (powerline-fill-center
;;                          nil (/ (powerline-width center) 2.0))
;;                         (powerline-render center)
;;                         (powerline-fill nil (powerline-width rhs))
;;                         (powerline-render rhs)))))))
;;   :when (eq sb/modeline-theme 'powerline)
;;   :hook (emacs-startup . sb/powerline-nano-theme)
;;   :custom
;;   ;; Visualization of the buffer position is not useful
;;   (powerline-display-hud nil)
;;   (powerline-display-buffer-size nil)
;;   (powerline-display-mule-info nil "File encoding information is not useful")
;;   (powerline-gui-use-vcs-glyph t)
;;   (powerline-height 20))

(use-package doom-modeline
  :when (eq sb/modeline-theme 'doom-modeline)
  :hook (emacs-startup . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'truncate-all)
  (doom-modeline-unicode-fallback t)
  ;; LSP state is wrong for non-LSP-managed files
  (doom-modeline-lsp nil)
  (doom-modeline-minor-modes t))

;; (use-package centaur-tabs
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
  :mode ("\\.kdl\\'" . kdl-ts-mode))

;; (use-package reformatter
;;   :after kdl-ts-mode
;;   :demand t
;;   :config
;;   (reformatter-define
;;    kdl-format
;;    :program "kdlfmt"
;;    :args '("format")
;;    :lighter " KDLFMT"
;;    :group 'reformatter)
;;   (add-hook 'kdl-ts-mode-hook #'kdl-format-on-save-mode))

;; Fontify ssh files
(use-package ssh-config-mode
  :mode ("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys\\'" . ssh-authorized-keys-mode))

(use-package asm-mode
  :straight (:type built-in)
  :hook
  (asm-mode
   .
   (lambda ()
     (cond
      ((eq sb/lsp-provider 'eglot)
       (eglot-ensure))
      ((eq sb/lsp-provider 'lsp-mode)
       (lsp-deferred))))))

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
  :hook (dired-mode . hl-line-mode)
  :custom
  ;; Restrict `hl-line-mode' highlighting to the current window
  (hl-line-sticky-flag nil))

(use-package xclip
  :when (or (executable-find "xclip") (executable-find "xsel"))
  :hook (emacs-startup . xclip-mode))

;; Send every kill from a TTY frame to the system clipboard
(use-package clipetty
  :hook (emacs-startup . global-clipetty-mode)
  :diminish)

;; Provides pixel-precise smooth scrolling which can keep up with the very high
;; event rates of modern trackpads and high-precision wheel mice.
;; (use-package ultra-scroll
;;   :straight (:host github :repo "jdtsmith/ultra-scroll")
;;   :custom
;;   (scroll-conservatively 101)
;;   (scroll-margin 0)
;;   :hook (find-file . ultra-scroll-mode))

;; Fold text using indentation levels
;; (use-package outline-indent
;;   :hook
;;   ((python-mode python-ts-mode yaml-mode yaml-ts-mode)
;;    .
;;    outline-indent-minor-mode)
;;   :custom
;;   (outline-indent-ellipsis " ▼ ")
;;   (outline-blank-line t)
;;   :diminish (outline-minor-mode outline-indent-minor-mode))

(use-package all-the-icons
  :commands all-the-icons-install-fonts
  :custom
  ;; Small icons look nicer
  (all-the-icons-scale-factor 0.9)
  (all-the-icons-faicon-scale-factor 0.9)
  (all-the-icons-wicon-scale-factor 0.9)
  (all-the-icons-octicon-scale-factor 0.9)
  (all-the-icons-fileicon-scale-factor 0.9)
  (all-the-icons-material-scale-factor 0.9)
  (all-the-icons-alltheicon-scale-factor 0.9)
  (all-the-icons-color-icons t))

;; Highlight the cursor position after the window scrolls
(use-package beacon
  :hook (emacs-startup . beacon-mode)
  :diminish)

;; Allows to easily identify the file path in a project. But does not support
;; imenu.
;; (use-package project-headerline
;;   :straight (:host github :repo "gavv/project-headerline")
;;   :hook (emacs-startup . global-project-headerline-mode)
;;   :custom
;;   (project-headerline-segment-separator " > ")
;;   (project-headerline-path-separator " > "))

(use-package breadcrumb
  :straight (:host github :repo "joaotavora/breadcrumb")
  :hook ((prog-mode conf-mode org-mode markdown-mode LaTeX-mode) . breadcrumb-mode)
  :config (breadcrumb-imenu-crumbs))

;; Hide a block with "C-c @ C-d", hide all folds with "C-c @ C-t", hide all
;; blocks below the current level with "C-c @ C-l", show a block with "C-c @
;; C-s", show all folds with "C-c @ C-a", and toggle hiding of a block with "C-c
;; @ C-c".
(use-package hideshow
  :preface
  (defun sb/toggle-fold ()
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding)))
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
  :custom (hs-isearch-open t "Open all folds while searching")
  :diminish hs-minor-mode)

(use-package kill-file-path
  :straight (:host github :repo "chyla/kill-file-path")
  :commands
  (kill-file-path-basename
   kill-file-path-basename-without-extension
   kill-file-path-dirname
   kill-file-path))

(use-package eglot
  :straight (:source (gnu-elpa-mirror))
  :when (eq sb/lsp-provider 'eglot)
  :hook
  ((dockerfile-ts-mode
    html-mode html-ts-mode LaTeX-mode markdown-mode org-mode text-mode)
   . eglot-ensure)
  :bind
  (("C-c l l" . eglot)
   ("C-c l q" . eglot-shutdown)
   ("C-c l Q" . eglot-shutdown-all)
   ("C-c l d" . eglot-find-declaration)
   ("C-c l i" . eglot-find-implementation)
   ("C-c l t" . eglot-find-typeDefinition)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format)
   ("C-c l F" . eglot-format-buffer)
   ("C-c l x" . eglot-code-actions)
   ("C-c l k" . eglot-code-action-quickfix)
   ("C-c l e" . eglot-code-action-extract)
   ("C-c l n" . eglot-code-action-inline)
   ("C-c l w" . eglot-code-action-rewrite)
   ("C-c l o" . eglot-code-action-organize-imports))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect nil "Do not block waiting to connect to the LSP")
  (eglot-send-changes-idle-time 3)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-config '(:size 0 :format full))
  (fset #'jsonrpc--log-event #'ignore)
  (eglot-ignored-server-capabilities
   '(:codeLensProvider
     :documentOnTypeFormattingProvider
     :inlayHintProvider ; Inlay hints are distracting
     ;; :executeCommandProvider
     ;; :hoverProvider ; Automatic documentation popups can be distracting
     ;; :foldingRangeProvider
     ;; :documentLinkProvider
     ;; :documentHighlightProvider
     ))
  (eglot-report-progress nil)
  (eglot-advertise-cancellation t)
  (eglot-mode-line-format '(eglot-mode-line-action-suggestion))
  (eglot-code-action-indications '(nearby mode-line margin))
  :config
  (setf (plist-get eglot-events-buffer-config :size) 0)
  (fset #'jsonrpc--log-event #'ignore)

  ;; Show all of the available Eldoc information when we want it. This way
  ;; Flymake errors don't just get clobbered by docstrings.
  (add-hook
   'eglot-managed-mode-hook
   (lambda ()
     (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)))

  (setopt eglot-server-programs nil)
  (add-to-list
   'eglot-server-programs
   '((org-mode markdown-mode text-mode) . ("ltex-ls-plus")))
  (add-to-list
   'eglot-server-programs
   '((autoconf-mode makefile-mode makefile-automake-mode makefile-gmake-mode)
     . ("autotools-language-server")))
  (add-to-list 'eglot-server-programs '(fish-mode . ("fish-lsp" "start")))
  (add-to-list
   'eglot-server-programs
   '((asm-mode fasm-mode masm-mode nasm-mode gas-mode) . ("asm-lsp")))
  (add-to-list
   'eglot-server-programs
   '((c++-mode c++-ts-mode c-mode c-ts-mode)
     .
     ("clangd"
      "-j=4"
      "--compile-commands-dir=./."
      "--all-scopes-completion"
      "--background-index"
      "--background-index-priority=low"
      "--clang-tidy"
      "--completion-style=detailed"
      "--fallback-style=LLVM"
      "--header-insertion=never"
      "--header-insertion-decorators"
      "--log=error"
      ;; Unsupported option with Clangd 10: malloc-trim and enable-config
      "--malloc-trim" ; Release memory periodically
      ;; Project config is from a .clangd file in the project directory
      "--enable-config"
      "--pch-storage=memory" ; Increases memory usage but can improve performance
      "--pretty")))
  (add-to-list 'eglot-server-programs '(awk-mode . ("awk-language-server")))
  (add-to-list
   'eglot-server-programs
   '((css-mode css-ts-mode) . ("vscode-css-language-server" "--stdio")))
  (add-to-list
   'eglot-server-programs
   '((html-mode html-ts-mode web-mode)
     .
     ("vscode-html-language-server" "--stdio")))
  (add-to-list
   'eglot-server-programs
   '((js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode)
     . ("typescript-language-server" "--stdio")))
  (add-to-list
   'eglot-server-programs
   '((js-json-mode json-mode json-ts-mode jsonc-mode)
     .
     ("vscode-json-language-server" "--stdio")))
  (add-to-list
   'eglot-server-programs
   '((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio")))
  (add-to-list
   'eglot-server-programs
   '((php-mode php-ts-mode) . ("intelephense" "--stdio")))
  (add-to-list
   'eglot-server-programs '((rust-ts-mode rust-mode) . ("rust-analyzer")))
  (add-to-list
   'eglot-server-programs
   '((cmake-mode cmake-ts-mode) . ("cmake-language-server")))
  (add-to-list
   'eglot-server-programs '((python-mode python-ts-mode) . ("pylsp")))
  (add-to-list
   'eglot-server-programs
   '((bash-ts-mode sh-mode) . ("bash-language-server" "start")))
  (add-to-list
   'eglot-server-programs
   '((java-mode java-ts-mode)
     .
     ("jdtls" "--illegal-access=warn" "-Xms2G" "-Xmx8G")))
  (add-to-list
   'eglot-server-programs
   '((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio")))
  (add-to-list
   'eglot-server-programs
   '((perl-mode cperl-mode)
     .
     ("perl" "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run")))
  ;; (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman" "server")))
  (add-to-list
   'eglot-server-programs
   '((toml-mode toml-ts-mode conf-toml-mode) . ("taplo" "lsp" "stdio")))
  (add-to-list 'eglot-server-programs '(bibtex-mode . ("texlab")))
  (add-to-list
   'eglot-server-programs
   `((nxml-mode xml-mode)
     .
     ("java" "-jar"
      ,(expand-file-name "servers/org.eclipse.lemminx-uber.jar"
                         user-emacs-directory))))

  ;; Eglot overwrites `company-backends' to only include `company-capf'
  (setopt eglot-stay-out-of '(flymake yasnippet company eldoc))

  ;; Translation between JSON and Eglot:
  ;; | true  | t           |
  ;; | false | :json-false |
  ;; | null  | nil         |
  ;; | {}    | eglot-{}    |
  (setq-default eglot-workspace-configuration
                '(:python.analysis
                  (:autoSearchPaths
                   t
                   :useLibraryCodeForTypes t
                   :typeCheckingMode "basic"
                   :diagnosticMode "openFilesOnly")
                  :pylsp
                  (:configurationSources
                   ["setup.cfg"]
                   :plugins
                   (:autopep8
                    (:enabled :json-false)
                    :black
                    (:enabled :json-false :line_length 80 :cache_config t)
                    :flake8
                    (:enabled :json-false :config t :maxLineLength 80)
                    :jedi
                    (:extra_paths [])
                    :jedi_completion
                    (:fuzzy
                     t
                     :include_params t
                     :include_class_objects t
                     :cache_for
                     ["pandas" "numpy" "matplotlib"])
                    :jedi_definition
                    (:enabled t :follow_imports t :follow_builtin_imports t)
                    :jedi_hover
                    (:enabled t)
                    :jedi_references
                    (:enabled t)
                    :jedi_signature_help
                    (:enabled t)
                    :jedi_symbols
                    (:enabled t :all_scopes t :include_import_symbols t)
                    :mccabe
                    (:enabled :json-false :threshold 15)
                    :mypy
                    (:enabled :json-false)
                    :preload
                    (:enabled t :modules ["pandas" "numpy" "matplotlib"])
                    :pycodestyle
                    (:enabled :json-false :maxLineLength 80)
                    :pydocstyle
                    (:enabled t :convention "numpy")
                    :pyflakes
                    (:enabled :json-false)
                    :pylint
                    (:enabled t)
                    :pylsp_black
                    (:enabled :json-false)
                    :pylsp_isort
                    (:enabled t)
                    :pylsp_mypy
                    (:enabled t :report_progress t :live_mode :json-false)
                    :rope_autoimport
                    (:code_actions
                     (:enabled t)
                     :completions
                     (:enabled t)
                     :enabled t)
                    :rope_completion
                    (:enabled t :eager :json-false)
                    :ruff
                    (:enabled :json-false :formatEnabled t :lineLength 80)
                    :yapf
                    (:enabled t)))
                  :basedpyright
                  (:checkOnlyOpenFiles
                   t
                   :reportDuplicateImport t
                   :typeCheckingMode "recommended"
                   :useLibraryCodeForTypes t)
                  :basedpyright.analysis
                  (:diagnosticSeverityOverrides
                   (:reportUnusedCallResult "none")
                   :inlayHints (:callArgumentNames :json-false))
                  :pyright
                  (:checkOnlyOpenFiles
                   t
                   :reportDuplicateImport t
                   :typeCheckingMode "recommended"
                   :useLibraryCodeForTypes t)
                  :ltex-ls-plus
                  (:language
                   "en-US"
                   :disabledRules
                   ["ELLIPSIS" "EN_QUOTES" "MORFOLOGIK_RULE_EN_US"]
                   :additionalRules (:enablePickyRules t))
                  :yaml
                  (:format
                   (:enable t :singleQuote nil :bracketSpacing t)
                   :validate t
                   :hover t
                   :completion t)
                  :vscode-json-language-server (:provideFormatter t)))

  (with-eval-after-load "eglot"
    (setq-default
     completion-category-defaults nil
     completion-category-overrides '((eglot (styles orderless)) (eglot-capf (styles orderless))))))

(use-package eglot-booster
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
  :when (executable-find "emacs-lsp-booster")
  :after eglot
  :demand t
  :config (eglot-booster-mode))

(use-package eglot-java
  :preface
  (defun sb/eglot-java-init-opts (server eglot-java-eclipse-jdt)
    "Custom options that will be merged with any default settings."
    '( ;;:workspaceFolders: ["file:///home/swarnendu/mavenproject"]
      :settings
      (:java
       (:home "/usr/lib/jvm/java-21-openjdk-amd64/")
       :configuration
       (:runtimes
        [(:name "JavaSE-17" :path "/usr/lib/jvm/openjdk-17/")
         (:name "JavaSE-21" :path "/usr/lib/jvm/openjdk-21/" :default t)])
       (:completion
        (:guessMethodArguments t)
        :format
        (:enabled
         t
         :comments (:enabled t)
         :onType (:enabled :json-false)
         :tabSize 4
         :insertSpaces t
         :settings
         (:url
          "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml")))
       :extendedClientCapabilities (:classFileContentsSupport t))))
  :when (eq sb/lsp-provider 'eglot)
  :hook
  (java-mode
   .
   (lambda ()
     (eglot-ensure)
     (eglot-java-mode)))
  :custom (eglot-java-user-init-opts-fn 'sb/eglot-java-init-opts))

(use-package eglot-hierarchy
  :straight (:host github :repo "dolmens/eglot-hierarchy"))

(use-package consult-eglot
  :when (eq sb/lsp-provider 'eglot)
  :after (consult eglot)
  :bind ("C-c l g" . consult-eglot-symbols))

(use-package flycheck-eglot
  :when (eq sb/lsp-provider 'eglot)
  :after (flycheck eglot)
  :init (global-flycheck-eglot-mode 1)
  :custom (flycheck-eglot-exclusive nil))

(use-package eglot-semantic-tokens
  :straight (:host github :repo "eownerdead/eglot-semantic-tokens")
  :when (eq sb/lsp-provider 'eglot)
  :after eglot
  :demand t
  :custom (eglot-enable-semantic-tokens t))

(use-package eglot-inactive-regions
  :when (eq sb/lsp-provider 'eglot)
  :after eglot
  :demand t
  :custom
  (eglot-inactive-regions-style 'darken-foreground)
  (eglot-inactive-regions-opacity 0.4)
  :config (eglot-inactive-regions-mode 1))

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
    ediff-meta-mode
    native-comp-limple-mode)
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

(defun sb/toggle-window-split ()
  "Switch between vertical and horizontal splits."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd
              (not
               (and (<= (car this-win-edges) (car next-win-edges))
                    (<= (cadr this-win-edges) (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges) (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd
              (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd
              (other-window 1))))))

;; Inside strings, special keys like tab or F1-Fn have to be written inside
;; angle brackets, e.g., "C-<up>". Standalone special keys (and some
;; combinations) can be written in square brackets, e.g. [tab] instead of
;; "<tab>".

;; ESC serves as a substitute for META, but there is no need to hold down ESC -
;; instead "M-something" keybindings can be triggered by pressing ESC and the
;; other key sequentially.

(bind-keys
 ("C-l" . goto-line)
 ("C-c z" . repeat) ; Repeat the last command
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

 ("C-x |" . sb/toggle-window-split)

 ("C-<left>" . backward-word)
 ("C-<right>" . forward-word)

 ("M-\\" . delete-horizontal-space)
 ("M-#" . cycle-spacing)

 ("C-M-b" . backward-sexp)
 ("C-M-f" . forward-sexp)
 ("C-M-k" . kill-sexp))

;; Originally bound to `abort-recursive-edit'. I use it as the prefix key for
;; Zellij.
(unbind-key "C-]")
;; (unbind-key "C-j") ; Bound to `electric-newline-and-maybe-indent'
(unbind-key "C-x f") ; Bound to `set-fill-column'
(unbind-key "M-'") ; Bound to `abbrev-prefix-mark'

(bind-key* "C-x s" #'scratch-buffer) ; Bound to `save-some-buffers'

(bind-keys
 ("M-<left>" . sb/previous-buffer)
 ("M-<right>" . sb/next-buffer)
 ("C-S-<iso-lefttab>" . sb/previous-buffer)
 ("C-<tab>" . sb/next-buffer))

;; Do not open the *Messages* buffer when clicking in the Echo area.
(unbind-key [mouse-1] minibuffer-inactive-mode-map)

;; ;; Make ESC quit everything
;; ;; Clear any previous ESC settings
;; (global-unset-key (kbd "<escape>"))
;; (define-key key-translation-map [escape] nil)
;; (define-key input-decode-map [escape] nil)

;; ;; Direct keyboard event interception
;; (defun sb/keyboard-quit-immediately ()
;;   "Quit immediately when ESC is pressed, regardless of context."
;;   (interactive)
;;   (keyboard-quit))

;; (define-key special-event-map [escape] 'sb/keyboard-quit-immediately)
;; (define-key function-key-map [escape] 'sb/keyboard-quit-immediately)
;; (global-set-key [escape] 'sb/keyboard-quit-immediately)

;; (use-package default-text-scale
;;   :when (display-graphic-p)
;;   :bind
;;   (("C-M-+" . default-text-scale-increase)
;;    ("C-M--" . default-text-scale-decrease)))

;; Show free bindings in current buffer
(use-package free-keys
  :commands free-keys)

;; Displays available keybindings following the currently entered incomplete
;; command/prefix in a popup.
(when (< emacs-major-version 30)
  (use-package which-key))
(add-hook 'emacs-startup-hook #'which-key-mode)
(with-eval-after-load "which-key"
  (diminish 'which-key-mode))

;; https://gist.github.com/mmarshall540/a12f95ab25b1941244c759b1da24296d
(which-key-add-key-based-replacements
 "<f1> 4"
 "help-other-win"
 "<f1>"
 "help"
 "<f2>"
 "2-column"
 "C-c"
 "mode-and-user"
 "C-h 4"
 "help-other-win"
 "C-h"
 "help"
 "C-x 4"
 "other-window"
 "C-x 5"
 "other-frame"
 "C-x 6"
 "2-column"
 "C-x 8"
 "insert-special"
 "C-x C-k C-q"
 "kmacro-counters"
 "C-x C-k C-r a"
 "kmacro-add"
 "C-x C-k C-r"
 "kmacro-register"
 "C-x C-k"
 "keyboard-macros"
 "C-x RET"
 "encoding/input"
 "C-x a i"
 "abbrevs-inverse-add"
 "C-x a"
 "abbrevs"
 "C-x n"
 "narrowing"
 "C-x p"
 "projects"
 "C-x r"
 "reg/rect/bkmks"
 "C-x t ^"
 "tab-bar-detach"
 "C-x t"
 "tab-bar"
 "C-x v M"
 "vc-mergebase"
 "C-x v b"
 "vc-branch"
 "C-x v"
 "version-control"
 "C-x w ^"
 "window-detach"
 "C-x w"
 "window-extras"
 "C-x x"
 "buffer-extras"
 "C-x"
 "extra-commands"
 "M-g"
 "goto-map"
 "M-s h"
 "search-highlight"
 "M-s"
 "search-map")

;; Upon loading, the built-in `page-ext' package turns "C-x C-p" into
;; a prefix-key.  If you know of other built-in packages that have
;; this behavior, please let me know, so I can add them.
(with-eval-after-load 'page-ext
  (which-key-add-key-based-replacements "C-x C-p" "page-extras"))

;; Support the Kitty Keyboard protocol in Emacs
(use-package kkp
  :hook (emacs-startup . global-kkp-mode)
  ;; :bind
  ;; ;; Should be remapped to "M-DEL"
  ;; ("M-<backspace>" . backward-kill-word)
  :config (define-key key-translation-map (kbd "M-S-4") (kbd "M-$")))

;;; init.el ends here

;; Local variables:
;; elisp-autofmt-load-packages-local: ("use-package-core")
;; end:
