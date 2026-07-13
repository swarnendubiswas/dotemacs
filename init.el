;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding: utf-8; fill-column: 80; -*-

;; Swarnendu Biswas

;;; Commentary:

;; My configuration mostly targets GNU Linux. I tend to switch between Company
;; and Corfu, and Eglot and Lsp-mode. Company provides finer-grained control
;; than Corfu. I generally prefer Eglot compared to lsp-mode because it feels
;; lightweight and I rarely use multiple servers simultaneously.

;;; Code:

(defgroup sb/emacs nil
  "Personal configuration for GNU Emacs."
  :group 'local)

(defcustom sb/extras-directory (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type 'string
  :group 'sb/emacs)

;; I now prefer dark themes. Modus-vivendi is the most complete, has good
;; contrast, and integrates well with all terminals.
(defcustom sb/theme
  (if (display-graphic-p)
      'standard-dark
    'standard-dark)
  "Specify which Emacs theme to use."
  :type
  '(radio
    (const :tag "modus-vivendi" modus-vivendi)
    (const :tag "matugen" matugen)
    (const :tag "standard-dark" standard-dark)
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme 'mini-echo
  "Specify the mode-line theme to use."
  :type '(radio (const :tag "mini-echo" mini-echo) (const :tag "none" none))
  :group 'sb/emacs)

(defconst sb/user-home-directory (getenv "HOME")
  "User HOME directory.")

(package-initialize)

(setopt
 use-package-always-ensure t
 use-package-vc-prefer-newest t
 use-package-enable-imenu-support t
 use-package-expand-minimally t
 use-package-always-defer t
 use-package-verbose nil
 use-package-minimum-reported-time 0 ; Show everything
 ;; Use "M-x use-package-report" to see results
 use-package-compute-statistics nil)

;; Where possible, it is better to avoid :preface, :config and
;; :init. Instead, prefer autoloading keywords such as :bind, :hook, and :mode,
;; as they will take care of setting up autoloads.

;; https://www.gnu.org/software/emacs/manual/html_node/use-package/Best-practices.html
;; https://batsov.com/articles/2025/04/17/using-use-package-the-right-way/

;; Check "use-package-keywords.org" for a suggested order of `use-package'
;; keywords.

;; "C-h b" lists all the bindings available in a buffer, "C-h m" shows the
;; keybindings for the major and the minor modes.

(use-package diminish)

(use-package no-littering
  :demand t

  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (custom-file (no-littering-expand-var-file-name "custom.el"))

  :config (no-littering-theme-backups))

;; Emacs launched in the terminal gets to see $PATH but the GUI app may not. Use
;; "~/.profile" for defining exports that modify $PATH, while use "~/.bashrc"
;; for defining aliases.

(use-package emacs
  :ensure nil

  :hook
  (emacs-startup
   .
   (lambda ()
     (global-auto-revert-mode 1)

     (savehist-mode 1)
     (column-number-mode 1)
     (save-place-mode 1)
     ;; (size-indication-mode 1) ; No benefit in seeing the file size
     (abbrev-mode 1)

     ;; `auto-save-mode' saves to a separate auto-save file, while
     ;; `auto-save-visited-mode' saves directly to the visited file and runs all
     ;; save-related hooks. We disable `auto-save-mode' and prefer
     ;; `auto-save-visited-mode' instead. Auto-save file-visiting buffers at
     ;; idle time intervals instead of based on the number of characters typed.
     (auto-save-visited-mode 1)

     ;; Typing with the mark active will overwrite the marked region
     (delete-selection-mode 1)

     ;; Use soft wraps, wrap lines without the ugly continuation marks
     (global-visual-line-mode 1)

     (pixel-scroll-precision-mode 1) ; Smooth scrolling

     ;; Continuation lines are displayed with proper indentation, as if the
     ;; text had been filled with M-q, but without modifying the buffer at all.
     (when (fboundp 'global-visual-wrap-prefix-mode)
       (global-visual-wrap-prefix-mode 1))

     ;; When you call `find-file', you do not need to clear the existing file
     ;; path before adding the new one. Just start typing the whole path and
     ;; Emacs will "shadow" the current one. For example, you are at
     ;; "~/Documents/notes/file.txt" and you want to go to "~/.emacs.d/init.el",
     ;; type the latter directly and Emacs will take you there.
     (file-name-shadow-mode 1)

     ;; This puts the buffer in read-only mode and disables font locking, revert
     ;; with "C-c C-c".
     (when (fboundp 'global-so-long-mode)
       (global-so-long-mode 1))

     (when (fboundp 'vc-auto-revert-mode)
       (vc-auto-revert-mode 1))

     (when (and (not (display-graphic-p)) (fboundp 'tty-tip-mode))
       (tty-tip-mode 1))))

  :bind
  (("<f1>" . execute-extended-command)

   ("<f2>" . ffap)
   ("C-x p o" . ff-find-other-file)

   ("<f7>" . previous-error) ; "M-g p" is the default keybinding
   ("<f8>" . next-error) ; "M-g n" is the default keybinding

   ("C-l" . goto-line) ; "M-g l" is the default keybinding
   ("C-c z" . repeat) ; Repeat the last command
   ("C-z" . undo)

   ;; In a line with comments, "C-u M-;" removes the comments altogether. That
   ;; means deleting the comment, NOT UNCOMMENTING but removing all commented
   ;; text and the comment marker itself.
   ("C-c n" . comment-region)
   ("C-c m" . uncomment-region)
   ("C-c b" . comment-box)

   ("<f3>" . switch-to-buffer)
   ("C-s" . save-buffer)
   ("C-x k" . kill-current-buffer)
   ("C-c x w" . write-file)
   ("C-c x r" . rename-file)
   ("C-c x a" . find-alternate-file)
   ("C-c x g" . revert-buffer-quick)
   ("C-c x b" . revert-buffer)

   ("C-<left>" . backward-word)
   ("C-<right>" . forward-word)

   ("M-\\" . delete-horizontal-space)
   ("M-#" . cycle-spacing)

   ("C-M-b" . backward-sexp)
   ("C-M-f" . forward-sexp)
   ("C-M-k" . kill-sexp)

   ("C-c d r" . restart-emacs)
   ("C-c d k" . describe-personal-keybindings)
   ("C-c d v" . view-echo-area-messages)
   ("C-c d l" .
    (lambda ()
      (interactive)
      (switch-to-buffer "*Messages*")))

   ("C-c s o" . occur))

  :bind*
  (("C-x s" . scratch-buffer) ; Bound to `save-some-buffers'
   ("C-c C-j" . imenu))

  :custom
  (auto-revert-verbose nil)
  (auto-revert-remote-files nil)
  ;; Automatically reread from disk if the underlying file changes
  (auto-revert-avoid-polling t)
  ;; This feature is supposed to be expensive
  (auto-revert-check-vc-info nil)

  ;; Revert `dired' buffers if the current directory contents change. Dired
  ;; buffers do not auto-revert as a result of changes in subdirectories, or in
  ;; the contents, size, modes, etc., of files.
  (global-auto-revert-non-file-buffers t)

  ;; ;; Reverting without confirmation is confusing, and hence it is better to be
  ;; ;; explicit
  ;; (revert-without-query '("\\.*") "Revert all files without asking")

  (abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory))
  (save-abbrevs 'silently)

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
     regexp-search-ring))

  ;; (ad-redefinition-action 'accept "Turn off warnings due to redefinitions")
  (auto-save-no-message t "Do not print frequent auto-save messages")
  ;; Disable auto-saving based on number of characters typed
  (auto-save-interval 0)
  (save-silently t)

  ;; ;; Save buffer to file after idling for 10s. The default of 5s may be too
  ;; ;; frequent since it runs all the save-related hooks.
  ;; (auto-save-visited-interval 10)

  ;; Save bookmark after every bookmark edit and also when Emacs is killed
  (bookmark-save-flag 1)

  ;; ;; Autofill comments in modes that define them
  ;; (comment-auto-fill-only-comments t)

  ;; Show the actual symbol name in the *customize* buffer
  (custom-unlispify-menu-entries nil)
  (custom-safe-themes t)

  (create-lockfiles nil)
  (backup-inhibited t "Disable backup for a per-file basis")
  (make-backup-files nil "Stop making backup `~' files")
  (delete-by-moving-to-trash t) ; Safe fallback

  ;; (apropos-do-all t "Make `apropos' search more extensively")

  ;; Prevents help command completion from triggering autoload.
  (help-enable-completion-autoload nil)
  (help-enable-autoload nil)
  (help-enable-symbol-autoload nil)

  (help-window-select t "Makes it easy to close the window")
  (switch-to-buffer-preserve-window-point t)
  ;; Make switching buffers more consistent
  (switch-to-buffer-obey-display-actions t)
  ;; (window-combination-resize t "Resize windows proportionally")

  (read-process-output-max (* 4 1024 1024)) ; 4 MB as recommended by `lsp-mode'

  (remote-file-name-inhibit-locks t)
  ;; Do not auto-save remote files using `auto-save-visited-mode'
  (remote-file-name-inhibit-auto-save-visited t)
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (remote-file-name-inhibit-auto-save t)

  (ring-bell-function 'ignore "Disable beeping sound")
  ;; (visible-bell nil)

  (save-interprogram-paste-before-kill t)
  (select-enable-clipboard t)
  (shift-select-mode nil)

  (history-delete-duplicates t)
  (kill-do-not-save-duplicates t "Do not save duplicates to kill ring")

  (sentence-end-double-space nil)
  (require-final-newline t "Always end a file with a newline")

  ;; (sort-fold-case nil "Do not ignore case when sorting")
  ;; (tags-case-fold-search nil "case-sensitive")

  (standard-indent 2)
  (view-read-only t "Use view mode for read-only buffers")

  ;; Allows showing all choices while importing with `lsp-mode'
  (max-mini-window-height 0.3)

  (x-gtk-use-system-tooltips nil "Do not use system tooltips")
  ;; Disable the warning "X and Y are the same file" in case of symlinks
  (find-file-suppress-same-file-warnings t)

  ;; (auto-mode-case-fold nil "Avoid a second pass through `auto-mode-alist'")

  ;; Prevent 'Active processes exist' when you quit Emacs
  (confirm-kill-processes nil)
  ;; (confirm-kill-emacs nil)

  (vc-handled-backends '(Git))
  ;; Disable version control for remote files to improve performance
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
  (vc-allow-rewriting-published-history t)
  (vc-dir-auto-hide-up-to-date 'revert)

  ;; Accelerate scrolling operations when non-nil. Only those portions of the
  ;; buffer which are actually going to be displayed get fontified.
  (fast-but-imprecise-scrolling t)
  (auto-window-vscroll nil)
  (scroll-preserve-screen-position t)
  ;; Number of lines of margin at the top and bottom of a window when automatic scrolling is triggered
  (scroll-margin 2)

  ;; (scroll-step 1)
  ;; (scroll-conservatively 10)
  ;; (scroll-error-top-bottom t)

  (hscroll-margin 2)
  (hscroll-step 1)

  ;; (fringes-outside-margins t)

  ;; Improve Emacs' responsiveness by delaying syntax highlighting during input
  (redisplay-skip-fontification-on-input t)
  (bidi-inhibit-bpa nil) ; Disabling BPA makes redisplay faster

  ;; ;; Show contextual lines around a match
  ;; (list-matching-lines-default-context-lines 1)

  (imenu-auto-rescan t)
  (imenu-use-popup-menu nil)
  (imenu-flatten t)

  (show-paren-when-point-inside-paren t)
  (whitespace-line-column fill-column)

  ;; Do not ping things that look like domain names
  (ffap-machine-p-known 'reject)

  :config
  (dolist (exts
           '(".aux"
             ".bbl"
             ".bcf"
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
  ;; ;; when no `ispell' dictionary is set.
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

   ;; ;; TAB first tries to indent the current line, and if the line was already
   ;; ;; indented, then try to complete the thing at point.
   ;; tab-always-indent 'complete

   bidi-display-reordering 'left-to-right
   bidi-paragraph-direction 'left-to-right)

  (setopt
   display-buffer-alist
   '(("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.5)
      (side . bottom)
      (slot . 0))
     ;; Allow *Help* buffers to use the full frame
     ("\\*\\([Hh]elp\\)\\*" (display-buffer-same-window))
     ("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
      (display-buffer-no-window)
      (allow-no-window . t))
     ("\\*compilation\\*" ; Compilation
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . 1)
      (window-height . 0.5))
     ("\\*\\(Flymake diagnostics\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 2))
     ("\\*\\(grep\\|xref\\|find\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.5)
      (side . bottom)
      (slot . 1))))

  (put 'overwrite-mode 'disabled t)

  ;; ;; Not a library/file, so `eval-after-load' does not work
  ;; (diminish 'auto-fill-function)
  (diminish 'auto-fill-mode)

  (advice-add 'risky-local-variable-p :override #'ignore)

  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))

  ;; Mark safe variables
  (put 'compilation-read-command 'safe-local-variable #'stringp)
  (put 'reftex-default-bibliography 'safe-local-variable #'stringp)

  ;; Originally bound to `abort-recursive-edit'. I use it as the prefix key for
  ;; Zellij.
  (unbind-key "C-]")
  (unbind-key "C-x f") ; Bound to `set-fill-column'
  (unbind-key "M-'") ; Bound to `abbrev-prefix-mark'

  ;; (unbind-key "C-j") ; Bound to `electric-newline-and-maybe-indent'

  :diminish (visual-line-mode auto-revert-mode))

(use-package recentf
  :ensure nil

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
   '(".*\\.gz\\'"
     ".*\\.xz\\'"
     ".*\\.zip\\'"
     ".*-autoloads.el\\'"
     "[/\\]elpa/"
     "[/\\]\\.git/"
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
  ;; Larger values help in lookup but slows it down
  (recentf-max-saved-items 250)

  :config
  ;; Abbreviate the home directory to make it easy to read the actual file name.
  (unless (> emacs-major-version 27)
    (setopt recentf-filename-handlers '(abbreviate-file-name)))

  (dolist (exclude
           `(,(recentf-expand-file-name no-littering-etc-directory)
             ,(recentf-expand-file-name no-littering-var-directory)))
    (add-to-list 'recentf-exclude exclude))

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent
  ;; list periodically after idling in case Emacs becomes unresponsive.
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
  (advice-add 'write-region :around #'sb/inhibit-message-call-orig-fun)

  ;; (advice-add 'write-file :around #'sb/inhibit-message-call-orig-fun)
  ;; (advice-add 'save-buffer :around #'sb/inhibit-message-call-orig-fun)

  (advice-add 'basic-save-buffer :around #'sb/inhibit-message-call-orig-fun))

(progn
  (defun sb/auto-save-wrapper (save-fn &rest args)
    "Hide 'Auto-saving...done' messages by calling the method.
  SAVE-FN with non-nil ARGS."
    (ignore args)
    (apply save-fn '(t)))

  (advice-add 'do-auto-save :around #'sb/auto-save-wrapper))

;; Highlight and open http links in strings and comments in buffers.
(use-package goto-addr
  :ensure nil

  :hook ((prog-mode . goto-address-prog-mode) (text-mode . goto-address-mode))

  :bind ("C-c C-o" . goto-address-at-point))

(use-package subword
  :ensure nil

  :hook ((LaTeX-mode prog-mode conf-unix-mode) . subword-mode)

  :diminish)

;; Use `ediff-regions-wordwise' for small regions and `ediff-regions-linewise'
;; for larger regions.
(use-package ediff
  :ensure nil

  :hook (ediff-startup . ediff-next-difference)

  :bind
  (("C-c e f" . ediff-files)
   ("C-c e d" . ediff-directories)
   ("C-c e b" . ediff-buffers))

  :custom
  ;; Put the control panel in the same frame as the diff windows
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split diffs side by side
  (ediff-split-window-function #'split-window-horizontally)
  ;; Prompt and kill file variants on quitting an Ediff session
  (ediff-keep-variants nil)

  :config (ediff-set-diff-options 'ediff-diff-options "-w"))

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
  :defer t

  :custom
  ;; Remote files are not updated outside of Tramp
  (remote-file-name-inhibit-cache nil)
  (tramp-verbose 1 "Only errors and warnings")
  (tramp-default-method "ssh")
  (tramp-copy-size-limit (* 2 1024 1024)) ; 2MB
  (tramp-use-scp-direct-remote-copying t)

  :config
  (when (boundp 'tramp-use-connection-share)
    (setopt tramp-use-connection-share nil))

  ;; Include "$HOME/.local/bin" directory in $PATH on remote
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path t)

  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  (setopt debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))

  ;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  ;; Newer versions of TRAMP will use SSH connection sharing for much faster
  ;; connections. These don’t require you to reenter your password each time you
  ;; connect. The compile command disables this feature, so we want to turn it
  ;; back on.
  (with-eval-after-load 'compile
    (remove-hook
     'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))

  (defun sb/cleanup-tramp ()
    (interactive)
    (ignore-errors
      (tramp-cleanup-all-buffers)
      (tramp-cleanup-all-connections)))
  (bind-key "C-c d q" #'sb/cleanup-tramp))

(use-package ibuffer
  :ensure nil

  :hook (ibuffer . ibuffer-auto-mode)

  :bind (("C-x C-b" . ibuffer) :map ibuffer-mode-map ("`" . ibuffer-switch-format))

  :custom
  (ibuffer-display-summary nil)
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-formats
   '((mark
      modified
      read-only
      locked
      " "
      (name 30 30 :left :elide)
      " "
      (mode 16 16 :left :elide)
      " "
      filename-and-process)
     (mark " " (name 30 -1) " " filename-and-process)))
  (ibuffer-never-show-predicates
   '("\\*Help\\*"
     "\\*Quick Help\\*"
     "\\*Calc Trail\\*"
     "\\*Compile-Log\\*"
     "\\*Async-native-compile-log\\*"
     "\\*Native-compile-log\\*"
     "\\*Calculator\\*"
     "\\*Calendar\\*"
     "\\*Org Help\\*"
     "\\*Ediff Registry\\*"
     "^magit.*"
     "\\*ltex-ls.*"
     "\\*bash-ls.*"
     "\\*marksman.*"
     "\\*yaml-ls.*"
     "\\*clangd.*"
     "\\*texlab.*"
     "\\*Completions\\*"))
  (ibuffer-human-readable-size t)

  :config (defalias 'list-buffers 'ibuffer))

;; By default buffers are grouped by `project-current' or by
;; `default-directory'.
(use-package ibuffer-project
  :commands ibuffer

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

  :custom (persistent-scratch-what-to-save '(point narrowing))

  :config
  (advice-add
   'persistent-scratch-setup-default
   :around #'sb/inhibit-message-call-orig-fun))

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

(use-package dired
  :ensure nil

  :hook
  ((dired-mode . auto-revert-mode) ; Auto refresh `dired' when files change
   (dired-mode . dired-hide-details-mode) (dired-mode . dired-omit-mode))

  :bind
  (("C-x C-j" . dired-jump)
   :map
   dired-mode-map
   ("M-<home>" . sb/dired-go-home)
   ("M-<up>" . sb/dired-jump-to-top)
   ("M-<down>" . sb/dired-jump-to-bottom)
   ("i" . find-file)
   ("_" . dired-create-empty-file))

  :custom
  ;; When there are two `dired' buffer windows in the same frame, Emacs will
  ;; select the other buffer as the target for copying or renaming files.
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
  (dired-free-space nil)
  (dired-omit-verbose nil "Do not show messages when omitting files")
  (dired-hide-details-hide-absolute-location t)

  :config
  (when (boundp 'dired-kill-when-opening-new-dired-buffer)
    (setopt dired-kill-when-opening-new-dired-buffer t))
  ;; Obsolete from Emacs 28+
  (unless (> emacs-major-version 27)
    (setopt dired-bind-jump t))

  (defun sb/dired-go-home ()
    "Go to home directory in Dired."
    (interactive)
    (dired sb/user-home-directory))

  (defun sb/dired-jump-to-top ()
    "Move to the topmost file in Dired."
    (interactive)
    (goto-char (point-min)) ; Faster than `(beginning-of-buffer)'
    (dired-next-line 1))

  (defun sb/dired-jump-to-bottom ()
    "Move to the last file in Dired."
    (interactive)
    (goto-char (point-max)) ; Faster than `(end-of-buffer)'
    (dired-next-line -1)))

(use-package dired-narrow
  :commands dired-narrow

  :bind (:map dired-mode-map ("/" . dired-narrow)))

;; In Emacs Lisp mode, `xref-find-definitions' will by default find only
;; functions and variables from Lisp packages which are loaded into the current
;; Emacs session or are auto-loaded.
(use-package xref
  :bind
  (("M-." . xref-find-definitions)
   ("M-," . xref-pop-marker-stack) ("M-?" . xref-find-references)
   ;; Find all identifiers whose name matches pattern
   ("C-M-." . xref-find-apropos))

  :custom (xref-search-program 'ripgrep))

;; Exclude project roots with `project-list-exclude'.
(use-package project
  :bind
  (("<f5>" . project-switch-project)
   ("<f6>" . project-find-file)
   ("C-x p c" . project-compile)
   ("C-x p k" . project-kill-buffers)
   ("C-x p f" . project-find-file)
   ("C-x p g" . project-find-regexp)
   ("C-x p r" . project-query-replace-regexp))

  :custom
  ;; Start `project-find-file' by default
  (project-switch-commands 'project-find-file)
  (project-vc-extra-root-markers '(".project" "pyproject.toml" "Cargo.toml")))

(use-package vertico
  :hook
  ((emacs-startup . vertico-mode)
   (minibuffer-setup . vertico-repeat-save)

   ;; Tidy or auto-hide shadowed file names. When you are in a sub-directory and
   ;; use, say, `find-file' to go to your home '~/' or root '/' directory,
   ;; Vertico will clear the old path to keep only your current input.
   (rfn-eshadow-update-overlay . vertico-directory-tidy))

  :bind
  (("C-c r" . vertico-repeat)
   ("M-r" . vertico-repeat-select)
   :map vertico-map
   ;; `vertico-exit' (RET) exits with the currently selected candidate, while
   ;; `vertico-exit-input' (M-RET) exits with the minibuffer input instead.
   ("M-<" . vertico-first)
   ("M->" . vertico-last)
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)
   ("C-q" . vertico-quick-insert)
   ("C-'" . vertico-quick-jump))

  :custom (vertico-cycle t)

  :config
  (let ((ext-dir
         (expand-file-name "extensions"
                           (file-name-directory (locate-library "vertico")))))
    (when (file-directory-p ext-dir)
      (add-to-list 'load-path ext-dir)))

  (require 'vertico-directory)
  (require 'vertico-repeat)
  (require 'vertico-quick)
  (require 'vertico-indexed)

  (vertico-indexed-mode 1)

  (when (eq sb/theme 'catppuccin)
    (set-face-attribute 'vertico-current nil
                        :background "#676767"
                        :foreground "#FFFFFF"))

  ;; Customize the display of the current candidate in the completion list. This
  ;; will prefix the current candidate with "» " to make it stand out.
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

(use-package vertico-timer
  :vc (:url "https://github.com/ventruvian/vertico-timer")

  :after vertico

  :hook (vertico-mode . vertico-timer-mode)

  :bind (:map vertico-map ("M-i" . vertico-timer-toggle-in-session))

  :diminish vertico-timer-mode)

(defconst sb/consult-buffer-filter
  '("^ "
    "\\` "
    "^:"
    "\\*Echo Area"
    "\\*Minibuf"
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
    "\\*Ediff Registry\\*"
    "TAGS"
    "\\*vc"
    "\\*tramp"
    "\\*citre.*"
    "\\*pylsp.*"
    "\\*pyright.*"
    "\\*ltex-ls"
    "\\*texlab"
    "\\*bash-ls.*"
    "\\*json-ls.*"
    "\\*yaml-ls.*"
    "\\*shfmt.*"
    "\\*clangd.*"
    "\\*semgrep.*"
    "\\*autotools.*"
    "\\*lsp-harper*"
    "\\*taplo*"
    "\\*ruff.*"
    "\\*marksman.*"
    "\\*html-ls.*")
  "Regexps to filter from `consult-buffer'.")

(use-package consult
  :after vertico

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
   ([remap list-bookmarks] . consult-bookmark)
   ([remap bookmark-bmenu-list] . consult-bookmark)
   ("M-g o" . consult-outline)
   ("C-c C-m" . consult-mark)
   ([remap imenu] . consult-imenu) ; "M-g i"
   ("C-c C-j" . consult-imenu)
   ([remap customize] . consult-customize)
   ([remap load-theme] . consult-theme)
   ([remap locate] . consult-locate)
   ("C-c s l" . consult-locate)
   ("C-c s f" . consult-fd)
   ;; Prefix argument "C-u" allows to specify the directory. You can pass
   ;; additional grep flags to `consult-grep' with the "--" separator. E.g.:
   ;; "foo bar -- -A3" to get matches with 3 lines of 'after' context.
   ([remap rgrep] . consult-grep)
   ([remap vc-git-grep] . consult-git-grep)
   ("<f4>" . consult-line)
   ("M-g l" . sb/consult-line-symbol-at-point)
   ("C-c s r" . consult-ripgrep)
   ([remap recentf-open-files] . consult-recent-file)
   ("M-g r" . consult-register)
   :map
   isearch-mode-map
   ("M-s e" . consult-isearch-history))

  :custom (consult-line-start-from-top t "Start search from the beginning")
  ;; Disable preview by default, enable for selected commands
  (consult-preview-key nil)
  (completion-in-region-function #'consult-completion-in-region "Complete M-:")

  ;; Having multiple other sources like `recentf' may make it difficult to
  ;; identify and switch quickly between only buffers, especially while wrapping
  ;; around.
  ;; (consult-buffer-sources '(consult--source-buffer))

  (consult-narrow-key "<")
  (consult-widen-key ">")

  ;; Do not filter buffers, they help to debug configuration errors 
  ;; (consult-buffer-filter sb/consult-buffer-filter)

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
  ;;   (consult-customize
  ;;    consult-line
  ;;  :add-history (seq-some #'thing-at-point '(region symbol)))
  ;; (defalias 'consult-line-thing-at-point 'consult-line)
  ;; (consult-customize
  ;;  consult-line-thing-at-point
  ;;  :initial (thing-at-point 'symbol))

  (defun sb/consult-line-symbol-at-point ()
    (interactive)
    (consult-line (or (thing-at-point 'symbol) ""))))

;; Easily add file and directory paths into the minibuffer.
(use-package consult-dir
  :commands consult-dir-jump-file

  :bind ("C-x C-d" . consult-dir)

  :config (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

;; Use `consult' to select Tramp targets. Supported completion sources are ssh
;; config, known hosts, and docker containers.
(use-package consult-tramp
  :vc (:url "https://github.com/Ladicle/consult-tramp" :rev :newest)

  :after consult

  :bind ("C-c d t" . consult-tramp))

(use-package ispell
  :ensure nil

  :bind ("M-$" . ispell-word)

  :custom
  (ispell-dictionary "en_US")
  (ispell-personal-dictionary (expand-file-name "spell" sb/extras-directory))
  (ispell-alternate-dictionary
   (expand-file-name "wordlist.5" sb/extras-directory))
  ;; Save a new word to personal dictionary without asking
  (ispell-silently-savep t)

  :config
  (when (boundp 'ispell-save-corrections-as-abbrevs)
    (setopt ispell-save-corrections-as-abbrevs t))

  (setq ispell-dictionary-alist
        (append
         '(("english"
            "[[:alpha:]]"
            "[^[:alpha:]]"
            "[']"
            nil
            ("-d" "en_US")
            nil
            utf-8)
           ("american"
            "[[:alpha:]]"
            "[^[:alpha:]]"
            "[']"
            nil
            ("-d" "en_US")
            nil
            utf-8))
         ispell-dictionary-alist))

  ;; Prefer hunspell over aspell on Linux platforms
  (cond
   ((executable-find "hunspell")
    (setenv "DICTIONARY" "en_US")

    (setenv "DICPATH" (expand-file-name "hunspell" user-emacs-directory))
    (let ((en-us-dict
           '(("en_US"
              "[[:alpha:]]"
              "[[^:alpha:]]"
              "[']"
              nil
              ("-d" "en_US")
              nil
              utf-8))))
      (setopt
       ispell-program-name "hunspell"
       ispell-local-dictionary-alist en-us-dict)
      (setq
       ispell-hunspell-dictionary-alist en-us-dict
       ispell-hunspell-dict-paths-alist
       `(("en_US"
          ,(expand-file-name "hunspell/en_US.dic" user-emacs-directory))))))
   ((executable-find "aspell")
    (setopt
     ispell-program-name "aspell"
     ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case"))))

  ;; Skip regions in `org-mode'
  (defun sb/org-ispell-setup ()
    (setq-local ispell-skip-region-alist
                (append
                 '(("^#\\+BEGIN_SRC" . "^#\\+END_SRC")
                   ("^#\\+BEGIN_EXAMPLE" . "^#\\+END_EXAMPLE")
                   ("~" . "~")
                   ("=" . "=")
                   ("\\:PROPERTIES\\:$" . "\\:END\\:$")
                   ;; Footnotes in org that have http links that are line
                   ;; breaked should not be ispelled
                   ("^http" . "\\]")
                   ("`" . "`")
                   ("cite:" . "[[:space:]]")
                   ("label:" . "[[:space:]]")
                   ("ref:" . "[[:space:]]")
                   ("\\\\begin{multline}" . "\\\\end{multline}")
                   ("\\\\begin{equation}" . "\\\\end{equation}")
                   ("\\\\begin{align}" . "\\\\end{align}"))
                 ispell-skip-region-alist)))

  (add-hook 'org-mode-hook #'sb/org-ispell-setup)

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
   :map helpful-mode-map ("q" . helpful-kill-buffers))

  :config
  (add-to-list
   'display-buffer-alist
   '("\\*helpful.*\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 0)
     (window-height . 0.5)
     (window-parameters . ((no-delete-other-windows . t))))))

;; Erase all consecutive white space characters in a given direction
(use-package hungry-delete
  :hook
  ((emacs-startup . global-hungry-delete-mode)
   (minibuffer-setup . (lambda () (hungry-delete-mode -1))))

  :diminish)

;; Move lines with "M-<up>" and "M-<down>"
(use-package move-text
  :bind (("M-<down>" . move-text-down) ("M-<up>" . move-text-up)))

;; Expand region increases the selected region by semantic units
(use-package expand-region
  :bind (("C-=" . er/expand-region) ("C-M-=" . er/contract-region)))

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  :hook (emacs-startup . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :hook (emacs-startup . whole-line-or-region-global-mode)

  :diminish whole-line-or-region-local-mode)

;; Keeps track of the point position over time and allows us to navigate back
;; and forward in history.
(use-package dogears
  :hook ((prog-mode text-mode) . dogears-mode)

  :bind
  (("M-g d" . dogears-go)
   ("M-g r" . dogears-remember)
   ("M-g b" . dogears-back)
   ("M-g f" . dogears-forward)
   ("M-g t" . dogears-list))

  :custom
  (dogears-message nil)
  (dogears-hooks
   '(imenu-after-jump-hook
     xref-after-jump-hook
     xref-after-return-hook
     consult-after-jump-hook
     before-save-hook
     isearch-mode-end-hook
     bookmark-after-jump-hook))
  (dogears-functions '(avy-goto-char-timer avy-goto-line))

  :config
  (dolist (mode
           '(elpaca-log-mode
             messages-buffer-mode helpful-mode completion-list-mode))
    (add-to-list 'dogears-ignore-modes mode))
  (with-eval-after-load 'git-commit
    (add-to-list 'dogears-ignore-modes 'git-commit-mode))
  (with-eval-after-load 'magit-status
    (add-to-list 'dogears-ignore-modes 'magit-status-mode))

  (add-to-list
   'display-buffer-alist
   '("\\*Dogears List\\*"
     (display-buffer-same-window) ; open in same window
     (inhibit-same-window . nil) ; allow reuse
     (inhibit-switch-frame . nil) ; allow switching frames
     (window-parameters . ((no-other-window . t)))
     ;; Make it full-frame
     (body-function . delete-other-windows))))

(use-package vundo
  :bind
  (([remap undo] . vundo)
   ("C-z" . vundo)
   :map vundo-mode-map ("C-a" . vundo-stem-root) ("C-e" . vundo-stem-end)
   ;; These are for horizontal movements.
   ("C-f" . vundo-forward) ("C-b" . vundo-backward)
   ;; These are for vertical movements.
   ("C-n" . vundo-next) ("C-p" . vundo-previous))

  :custom (vundo-compact-display t)
  ;; Use pretty Unicode glyphs to draw the tree
  (vundo-glyph-alist vundo-unicode-symbols))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :bind* ("C-." . iedit-mode))

;; Save a bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark,
;; use `bookmark-jump' ("C-x r b") or `bookmark-bmenu-list' ("C-x r l"). Rename
;; the bookmarked location in `bookmark-bmenu-mode' with `R'.
(use-package bm
  :init (setq bm-restore-repository-on-load t)

  :hook
  ((emacs-startup . bm-repository-load)
   ((find-file after-revert) . bm-buffer-restore)
   ((after-save kill-buffer vc-before-checkin) . bm-buffer-save)
   (kill-emacs
    .
    (lambda ()
      (bm-buffer-save-all)
      (bm-repository-save))))

  :bind (("C-<f1>" . bm-toggle) ("C-<f3>" . bm-next) ("C-<f2>" . bm-previous))

  :custom (bm-verbosity-level 0)

  :config (setq-default bm-buffer-persistence t))

(use-package crux
  :bind
  (("C-c d s" . crux-sudo-edit)
   ("C-<f9>" . crux-recentf-find-directory)
   ("C-<f11>" . crux-kill-other-buffers)
   ([remap keyboard-quit] . crux-keyboard-quit-dwim)
   ("C-c d i" . crux-ispell-word-then-abbrev))

  :bind* ("C-c C-d" . crux-duplicate-current-line-or-region))

;; Parsing parentheses for `LaTeX-mode' and `sh-mode' is difficult.
(use-package rainbow-delimiters
  :hook
  ((c-mode
    c-ts-mode
    c++-mode
    c++-ts-mode
    emacs-lisp-mode
    java-mode
    java-ts-mode
    json-mode
    json-ts-mode
    jsonc-mode
    lisp-data-mode
    python-mode
    python-ts-mode)
   . rainbow-delimiters-mode))

;; Allow GC to happen after a period of idle time
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)

  :diminish)

;; While searching, you can jump straight into `occur' with "M-s o". `isearch'
;; saves mark where the search started, so you can jump back to that point later
;; with "C-u C-SPC". Use "M-s M-<" to go to the first match and "M-s M->" to go
;; to the last match.
(use-package isearch
  :ensure nil

  :bind
  (("C-f" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regexp)
   ("C-c s f" . isearch-forward)
   ("C-c s b" . isearch-backward)
   ("C-c s h" . isearch-occur)
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
  :commands
  ( ;; Will not match substrings, so foo will not match foobar.
   isearch-forward-symbol
   isearch-backward-symbol-at-point)

  :bind
  ( ;; Starts an incremental search using the symbol under point as the initial
   ;; search string and searches forward by default unless `isearch-backward' was active.
   ("C-c s s" . isearch-symbol-at-point)
   ("C-c s w" . isearch-forward-symbol-at-point)))

(with-eval-after-load 'grep
  (setopt
   grep-command "grep --color -irHn "
   grep-highlight-matches t
   grep-scroll-output t)

  (when (executable-find "rg")
    (setopt grep-program "rg")
    (grep-apply-setting 'grep-find-command '("rg -n -H --no-heading -e" . 27)))

  (dolist (file '("*.iso" "*.xmp" "*.jpg" "*.mp4"))
    (push file grep-find-ignored-files))

  (dolist (dirs '(".cache" "elpa" "node_modules" "vendor" ".clangd"))
    (add-to-list 'grep-find-ignored-directories dirs)))

;; `consult-rg' provides live search, while `deadgrep' provides a buffer with
;; the search results. Visit the result in another buffer with "o", move between
;; search hits with "n" and "p", and move between files with "M-n" and "M-p".
;; Change the search term with "S" and enable incremental search with "I".
(use-package deadgrep
  :when (executable-find "rg")

  :commands deadgrep-edit-mode

  :bind ("C-c s d" . deadgrep)

  :custom
  (deadgrep-max-buffers 1)
  (deadgrep-display-buffer-function 'switch-to-buffer)
  (deadgrep-extra-arguments '()))

(use-package wgrep
  ;; These keybindings are also defined in `wgrep-mode-map'
  :bind
  (:map
   grep-mode-map
   ("C-x C-p" . wgrep-change-to-wgrep-mode)
   ("C-x C-s" . wgrep-finish-edit)
   ("C-x C-k" . wgrep-abort-changes)
   ("C-x C-q" . wgrep-exit))

  :custom (wgrep-auto-save-buffer t)

  :config
  (with-eval-after-load 'deadgrep
    (bind-key "e" #'wgrep-change-to-wgrep-mode deadgrep-mode-map)))

;; Allows to edit a `deadgrep' buffer and apply those changes to the file
;; buffer.
(use-package wgrep-deadgrep
  :hook (deadgrep-finished . wgrep-deadgrep-setup))

(use-package re-builder
  :ensure nil

  :commands re-builder

  :custom (reb-re-syntax 'string))

(use-package visual-replace
  :bind
  (([remap query-replace] . visual-replace)
   ([remap replace-string] . visual-replace)
   ([remap isearch-query-replace] . visual-replace-from-isearch)
   ([remap isearch-query-replace-regexp] . visual-replace-from-isearch))

  :custom (visual-replace-display-total t))

;; Magit often requires a newer version of transient.
(use-package transient
  :custom (transient-semantic-coloring t)

  :config (transient-bind-q-to-quit))

;; Use Emacsclient as the $EDITOR of child processes.
(use-package with-editor :diminish)

;; Use "M-p/n" to cycle between older commit messages.
(use-package magit
  :hook
  (git-commit-setup
   .
   (lambda ()
     (git-commit-save-message)
     (git-commit-turn-on-auto-fill)))

  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c M-g" . magit-file-dispatch))

  :custom
  ;; Open the status buffer in a full frame
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  (magit-section-initial-visibility-alist
   '((stashes . show) (untracked . show) (unpushed . show) (unpulled . show)))
  (magit-save-repository-buffers 'dontask)
  ;; Do not show the diff by default in the commit buffer.
  (magit-commit-show-diff nil)

  :config
  (with-eval-after-load 'magit-diff
    ;; Show fine differences for the current diff hunk only
    (setopt magit-diff-refine-hunk t)))

(use-package git-modes
  :mode ("dotgitconfig\''" . gitconfig-mode))

(use-package smerge-mode
  :ensure nil

  :bind
  (:map
   smerge-mode-map
   ("C-c ^ u" . smerge-keep-upper)
   ("C-c ^ l" . smerge-keep-lower)
   ("C-c ^ a" . smerge-keep-all)
   ("C-c ^ n" . smerge-next)
   ("C-c ^ p" . smerge-prev)))

;; "C-h m" or `describe-mode' shows all the active minor modes (and major mode)
;; and a brief description of each.

;; Discover key bindings for the current Emacs major mode.
(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major) ("C-c d m" . discover-my-mode)))

(use-package hl-todo
  :hook (emacs-startup . global-hl-todo-mode)

  ;; I use Flycheck integration (`previous-error' and `next-error') to navigate
  ;; among the highlighted lines.
  ;; :bind (("C-c p" . hl-todo-previous) ("C-c n" . hl-todo-next))

  :custom (hl-todo-highlight-punctuation ":"))

;; Jump to `hl-todo' keywords in current buffer.
(use-package consult-todo
  :after (consult hl-todo)

  :commands (consult-todo consult-todo-all))

;; Display ugly "^L" page breaks as tidy horizontal lines
(use-package page-break-lines
  :hook (emacs-startup . global-page-break-lines-mode)

  :diminish)

;; Basedpyright does not provide formatting feature. So, we cannot use
;; `lsp-format-buffer' or `eglot-format-buffer' with `basedpyright'.
(use-package apheleia
  :hook ((markdown-mode markdown-ts-mode python-mode python-ts-mode) . apheleia-mode)

  :bind ("C-x f" . apheleia-format-buffer)

  :custom (apheleia-formatters-respect-fill-column t)

  :config
  (setf (alist-get 'prettier apheleia-formatters) '("prettier"))
  (setf (alist-get 'shfmt apheleia-formatters) '("shfmt" "-i" "2" "-ci"))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  ;; (when (executable-find "kdlfmt")
  ;;   (setf (alist-get 'kdlfmt apheleia-formatters)
  ;;         '("kdlfmt" "format" "--stdin"))
  ;;   (setf (alist-get 'kdl-mode apheleia-mode-alist) 'kdlfmt)
  ;;   (setf (alist-get 'kdl-ts-mode apheleia-mode-alist) 'kdlfmt))

  :diminish apheleia-mode)

;; Auto-format Elisp code
(use-package elisp-autofmt
  :hook
  ((emacs-lisp-mode lisp-data-mode)
   .
   (lambda ()
     (when (and buffer-file-name
                (string-equal
                 (expand-file-name "~/.emacs.d/init.el")
                 (expand-file-name buffer-file-name)))
       (elisp-autofmt-mode 1))))

  :custom
  (elisp-autofmt-python-bin "python3")
  (elisp-autofmt-on-save-p 'always))

;; Provides indentation guide bars with optional `tree-sitter' support
(use-package indent-bars
  :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode) . indent-bars-mode)

  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists

  :config
  (when (and (fboundp 'treesit-available-p) (treesit-available-p))
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
        while_statement)
       (yaml block_mapping_pair comment)))))

;; `dabbrev-completion' finds all expansions in the current buffer and presents
;; suggestions for completion.
(use-package dabbrev
  :ensure nil

  :bind ("C-M-;" . dabbrev-completion)

  :custom
  (dabbrev-ignored-buffer-regexps
   '("^ " ;; internal or hidden buffers starting with space
     "\\.\\(?:jpe?g\\|png\\|pdf\\)\\'"
     "\\(TAGS\\|tags\\|ETAGS\\|etags\\|GTAGS\\|GRTAGS\\|GPATH\\)\\(<[0-9]+>\\)?"))
  (dabbrev-upcase-means-case-search t)

  :config
  (setopt dabbrev-ignored-buffer-modes
          (append
           '(doc-view-mode pdf-view-mode tags-table-mode)
           dabbrev-ignored-buffer-modes)))

(use-package hippie-exp
  :ensure nil

  :bind (("C-M-/" . hippie-expand) ([remap dabbrev-expand] . hippie-expand))

  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev ; current buffer
     try-expand-dabbrev-all-buffers ; any buffer
     try-complete-file-name-partially ; partial path
     try-expand-dabbrev-from-kill ; recent kills
     try-complete-file-name ; full path
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line))
  (hippie-expand-verbose nil))

;; "basic" matches only the prefix, "substring" matches the whole string.
;; "initials" matches acronyms and initialisms, e.g., can complete "M-x lch" to
;; "list-command-history". "partial-completion" style allows to use wildcards
;; for file completion and partial paths, e.g., "/u/s/l" for "/usr/share/local".
;; While "partial-completion" matches search terms must match in order,
;; "orderless" can match search terms in any order.
(use-package minibuffer
  :ensure nil

  :bind
  (("M-p" . minibuffer-previous-completion)
   ("M-n" . minibuffer-next-completion))

  :custom (enable-recursive-minibuffers t "Tracking the depth can be confusing")
  ;; Ignore case when reading a file name
  (read-file-name-completion-ignore-case t)
  ;; Ignore case when reading a buffer name
  (read-buffer-completion-ignore-case t)

  (completion-styles '(basic flex initials))
  (completion-category-defaults nil)
  ;; The "basic" completion style needs to be tried first for TRAMP hostname
  ;; completion to work. I also want substring matching for file names.
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-eager-update t)
  (completion-eager-display 'auto)
  ;; Never pop up the *Completions* buffer automatically
  (completion-auto-help nil)
  (completions-sort 'historical)

  (minibuffer-visible-completions 'up-down)

  :config
  ;; Show docstring description for completion candidates in commands like
  ;; `describe-function'.
  (when (boundp 'completions-detailed)
    (setopt completions-detailed t))
  ;; Emacs 31: partial-completion behaves like substring
  (when (boundp 'completion-pcm-leading-wildcard)
    (setopt completion-pcm-leading-wildcard t))

  (when (fboundp 'dabbrev-capf)
    (add-to-list 'completion-at-point-functions 'dabbrev-capf t))

  (defun sb/decrease-minibuffer-font ()
    "Decrease minibuffer font size."
    (setq-local face-remapping-alist '((default :height 0.95))))
  (add-hook 'minibuffer-setup-hook #'sb/decrease-minibuffer-font)

  ;; Do not open the *Messages* buffer when clicking in the Echo area.
  (unbind-key [mouse-1] minibuffer-inactive-mode-map))

;; https://www.reddit.com/r/emacs/comments/1qlngj1/completionatpoint_overwrites_following_text/

;; Insert completion without overwriting text right of cursor
(define-advice completion--capf-wrapper (:around (orig-fun fun which) nil -1)
  (save-restriction
    (narrow-to-region (point-min) (point))
    (funcall orig-fun fun which)))

;; It is recommended to load `yasnippet' before `eglot'
(use-package yasnippet
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)

  :hook ((prog-mode LaTeX-mode bibtex-mode org-mode markdown-mode) . yas-minor-mode)

  :custom
  (yas-verbosity 0)
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-wrap-around-region t) ; Allows snippets to wrap around selected text
  (yas-triggers-in-field t) ; Enable nested snippet expansion

  :config
  (with-eval-after-load 'hippie-expand
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand t))
  (unbind-key "<tab>" yas-minor-mode-map)
  (unbind-key "TAB" yas-minor-mode-map)

  :diminish yas-minor-mode)

(use-package yasnippet-snippets
  :after yasnippet

  :init (yasnippet-snippets-initialize))

(use-package consult-yasnippet
  :bind ("C-M-y" . consult-yasnippet))

;; Use "M-x company-diag" or the modeline status without diminish to see the
;; backend used for the last completion.
(use-package company
  :hook (emacs-startup . global-company-mode)

  :bind
  (:map
   company-active-map
   ("C-;" . company-other-backend) ; Invoke the next backend
   ("C-s" . company-search-candidates)
   ("C-f" . company-filter-candidates)
   ([escape] . company-abort)
   ("M-." . company-show-location)
   ("C-h" . company-show-doc-buffer)
   :map
   company-search-map
   ("C-s" . company-search-repeat-forward)
   ("C-r" . company-search-repeat-backward)
   ("C-g" . company-search-abort)
   ("DEL" . company-search-delete-char))

  :custom
  ;; ;; Avoid slowdown in case there are lot of buffers open
  ;; (company-dabbrev-other-buffers nil)
  ;; (company-dabbrev-code-other-buffers nil)

  (company-dabbrev-downcase nil "Do not downcase returned candidates")

  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-code-completion-styles '(basic))
  (company-ispell-dictionary
   (expand-file-name "wordlist.5" sb/extras-directory))

  ;; Speed up selecting a completion with quick access keys. Showing the access
  ;; keys on the left makes them easily discernible.
  (company-show-quick-access 'left)

  (company-global-modes
   '(not dired-mode
         magit-status-mode
         help-mode
         helpful-mode
         csv-mode
         minibuffer-inactive-mode))
  ;; Convenient to wrap around completion items at boundaries
  (company-selection-wrap-around t)

  ;; `company-pseudo-tooltip-unless-just-one-frontend' shows popup unless there
  ;; is only one candidate, `company-preview-frontend' shows the preview
  ;; in-place which is too intrusive, `company-preview-if-just-one-frontend'
  ;; shows in-place preview if there is only choice,
  ;; `company-echo-metadata-frontend' shows selected candidate docs in echo
  ;; area, and `company-pseudo-tooltip-frontend' which always shows the
  ;; candidates in an overlay. We do not want to use `company' for showing
  ;; selected candidate docs in echo area and hence remove
  ;; `company-echo-metadata-frontend'.
  (company-frontends '(company-pseudo-tooltip-frontend))

  ;; Setting this to true leads to candidates from `company-dabbrev-code' to be
  ;; unaligned.
  ;; (company-tooltip-align-annotations t)

  (company-tooltip-width-grow-only t) ; Avoid shrinking the company popup
  (company-format-margin-function nil)

  ;; Allow orderless-like behavior with Company, i.e., search candidates with
  ;; space-separated regexp
  ;; https://github.com/company-mode/company-mode/discussions/1211
  (company-search-regexp-function 'company-search-words-in-any-order-regexp)

  :config
  (setopt
   company-transformers
   '(delete-dups
     ;; Ignore matches from `company-dabbrev' that consist solely of numbers
     ;; https://github.com/company-mode/company-mode/issues/358
     (lambda (candidates)
       (cl-remove-if
        (lambda (c) (string-match-p "\\`[0-9]+\\'" c)) candidates))))

  ;; Disable code candidates in comments, otherwise text completions are not
  ;; offered with Eglot.
  ;; https://github.com/company-mode/company-mode/discussions/1498

  (defun sb/company-capf-around (orig-fun &rest args)
    "Custom advice for `company-capf--prefix' to restrict completions in comments."
    (let ((syntax-info (syntax-ppss)))
      (if (nth 4 syntax-info)
          nil
        (apply orig-fun args))))
  (advice-add 'company-capf--prefix :around #'sb/company-capf-around)

  (defun sb/company-abort-then-kill-word ()
    "If company popup is active, close it, then delete the next word."
    (interactive)
    (when (and (boundp 'company-mode) (company--active-p))
      (company-abort))
    (kill-word 1))
  (bind-key "M-d" #'sb/company-abort-then-kill-word)

  ;; This is useful for LaTeX completions with Texlab.
  (with-eval-after-load 'company-capf
    (bind-key "C-c p" #'company-capf))

  :diminish)

;; By default, the Unicode symbols backend `company-math-symbols-unicode' is not
;; active in latex math environments and latex math symbols
;; `company-math-symbols-latex' is not available outside of math latex
;; environments.
(use-package company-math
  :after company

  :commands (company-math-symbols-unicode company-math-symbols-latex))

(use-package company-dict
  :after company

  :custom
  (company-dict-dir (expand-file-name "company-dict" user-emacs-directory))
  (company-dict-enable-yasnippet nil))

;; Use "<" to trigger company completion of org blocks.
(defun sb/company-org-block-setup ()
  (autoload 'company-org-block "company-org-block")
  (setq-local company-backends
              '(company-files
                (company-org-block :separate company-dabbrev-code)
                (:separate company-dict company-ispell company-dabbrev))))

(add-hook 'org-mode-hook #'sb/company-org-block-setup)

;; (use-package company-org-block
;;   :after company

;;   :hook
;;   (org-mode
;;    .
;;    (lambda ()
;;      (require 'company-org-block)
;;      (setq-local company-backends
;;                  '(company-files
;;                    (company-org-block :separate company-dabbrev-code)
;;                    (:separate company-dict company-ispell company-dabbrev))))))

(use-package company-auctex
  :after tex

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
  :after reftex

  :custom
  ;; https://github.com/TheBB/company-reftex/pull/13
  (company-reftex-labels-parse-all nil))

(use-package company-try-hard
  :bind (:map company-active-map ("C-j" . company-try-hard)))

(with-eval-after-load 'company
  ;; Override `company-backends' for unhandled major modes.
  (setopt
   company-backends
   '(company-files
     ;; `company-capf' may not return all variable or type definitions, so we
     ;; also use `company-dabbrev-code' which is useful for local (e.g.,
     ;; variable) names. For example, `company-capf' is not complete for Elisp.
     ;; It will not suggest `doom-modeline' but suggests `doom-modeline-mode'.
     ;; `company-keywords' should not be required with LS support.
     ;; `company-yasnippet' is blocking.
     (company-capf
      :separate
      company-dabbrev-code
      company-keywords
      :with company-yasnippet)
     ;; If we have `company-dabbrev' first, then other matches from later
     ;; backends `company-ispell' or `company-dict' will be ignored.
     (:separate company-dict company-ispell company-dabbrev)))

  ;; `company-capf' with Texlab does not pass to later backends even if it does
  ;; not return any result. So it makes it difficult to complete non-LaTeX
  ;; commands (e.g., words) which is the majority. By combining it in a single
  ;; group with ":separate", the following code forces all listed backends to be
  ;; queried regardless of what `company-capf' returns.

  ;; Always query all the following backends by using ":separate".
  (defun sb/company-latex-backends-separate ()
    (setq-local
     company-backends
     '(company-files ; Have files first to allow completing paths
       (:separate
        company-capf
        company-reftex-citations ; will trigger inside \cite{}
        ;; Will trigger inside forms like \ref{}, \eqref{}, \auroref{}, etc.
        company-reftex-labels
        ;; LaTeX structure
        company-auctex-labels company-auctex-macros company-auctex-environments
        company-latex-commands ; `company-auctex-macros' seem to be better
        company-auctex-symbols
        company-math-symbols-latex ; Math latex tags
        ;; Math Unicode symbols and sub (super) scripts
        company-math-symbols-unicode company-dict company-ispell company-dabbrev
        :with company-yasnippet))))

  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     ;; Allow showing yasnippets auto-complete which often use two letters
     (setq-local company-minimum-prefix-length 2)
     (sb/company-latex-backends-separate)))

  (defun sb/company-text-mode ()
    "Add backends for `text-mode' completion in company mode."
    ;; Another way to make `company-backends' local.
    (set
     (make-local-variable 'company-backends)
     '(company-files (:separate company-dict company-ispell company-dabbrev))))

  ;; Extends to derived modes like `markdown-mode'. We use separate hooks for `org-mode' and `LaTeX-mode'.
  (add-hook
   'text-mode-hook
   (lambda ()
     (unless (or (derived-mode-p 'LaTeX-mode) (derived-mode-p 'org-mode))
       (sb/company-text-mode)))))

;; Prescient uses frecency (frequency + recency) for sorting. Recently used
;; commands should be sorted first. Only commands that have never been used
;; before will be sorted by length.

(use-package prescient
  :hook (emacs-startup . prescient-persist-mode)

  :custom (prescient-sort-full-matches-first t))

(use-package vertico-prescient
  :after vertico

  :init (vertico-prescient-mode 1))

(use-package company-prescient
  :after company

  :init (company-prescient-mode 1))

;; Highlight symbols on hover
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
  :ensure nil

  :bind (:map prog-mode-map ("<f10>" . compile) ("<f11>" . recompile))

  :custom
  (compile-command (format "make -k -j%s " (num-processors)))
  (compilation-always-kill t)
  (compilation-ask-about-save nil "Save all modified buffers without asking")

  ;; Use "t" to scroll the compilation buffer to follow output. We stop
  ;; scrolling when the first error appears.
  (compilation-scroll-output 'first-error)

  (compilation-auto-jump-to-first-error t)
  (compilation-max-output-line-length nil)

  ;; Skip warnings and info when navigating with next-error by setting the value
  ;; to 2. Set it to 1 to also stop at warnings but skip info. Set it to 0 to
  ;; stop at everything.
  (compilation-skip-threshold 2)

  :config
  (with-eval-after-load 'latex
    (bind-key "<f10>" #'compile LaTeX-mode-map)
    (bind-key "<f11>" #'recompile LaTeX-mode-map)))

(use-package fancy-compilation
  :after compile

  :init (fancy-compilation-mode 1)

  :custom (fancy-compilation-scroll-output 'first-error))

(use-package eldoc
  :ensure nil

  :custom
  (eldoc-area-prefer-doc-buffer t "Disable popups")
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (eldoc-help-at-pt t)

  :config
  ;; Allow Eldoc to trigger after completions
  (with-eval-after-load 'company
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

;; I am not very convinced with the usefulness of Treesitter for major modes where LSPs are available.

(use-package treesit
  :ensure nil

  :when (and (fboundp 'treesit-available-p) (treesit-available-p))

  :bind
  (("C-M-<up>" . treesit-up-list)
   ("C-M-<down>" . treesit-down-list)
   ("C-M-a" . treesit-beginning-of-defun)
   ("C-M-e" . treesit-end-of-defun))

  :custom
  (treesit-enabled-modes t)
  (treesit-auto-install-grammar 'always)

  ;;   ;; Increased default font locking may hurt performance
  ;;   (treesit-font-lock-level 4)

  ;;   (treesit-language-source-alist
  ;;    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
  ;;      (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
  ;;      (c "https://github.com/tree-sitter/tree-sitter-c")
  ;;      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
  ;;      (cmake "https://github.com/uyha/tree-sitter-cmake")
  ;;      (css "https://github.com/tree-sitter/tree-sitter-css")
  ;;      (cuda "https://github.com/tree-sitter-grammars/tree-sitter-cuda")
  ;;      (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
  ;;      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
  ;;      (go "https://github.com/tree-sitter/tree-sitter-go")
  ;;      (html "https://github.com/tree-sitter/tree-sitter-html")
  ;;      (java "https://github.com/tree-sitter/tree-sitter-java")
  ;;      (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
  ;;      (json "https://github.com/tree-sitter/tree-sitter-json")
  ;;      (kdl "https://github.com/tree-sitter-grammars/tree-sitter-kdl")
  ;;      (latex "https://github.com/latex-lsp/tree-sitter-latex")
  ;;      (make "https://github.com/alemuller/tree-sitter-make")
  ;;      (markdown
  ;;       "https://github.com/ikatyang/tree-sitter-markdown"
  ;;       "split_parser"
  ;;       "tree-sitter-markdown/src")
  ;;      (markdown-inline
  ;;       "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
  ;;       "split_parser"
  ;;       "tree-sitter-markdown-inline/src")
  ;;      (org "https://github.com/milisims/tree-sitter-org")
  ;;      (perl "https://github.com/tree-sitter-perl/tree-sitter-perl")
  ;;      (php "https://github.com/tree-sitter/tree-sitter-php")
  ;;      (python "https://github.com/tree-sitter/tree-sitter-python")
  ;;      (toml "https://github.com/tree-sitter/tree-sitter-toml")
  ;;      (tsx "https://github.com/tree-sitter/tree-sitter-typescript")
  ;;      (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
  ;;      (rust "https://github.com/tree-sitter/tree-sitter-rust")
  ;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;;   :config
  ;;   (setopt treesit-language-source-alist
  ;;           '((cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.22.0")))

  ;;   ;; Install grammars if missing
  ;;   (unless (seq-every-p
  ;;            #'treesit-language-available-p
  ;;            (mapcar #'car treesit-language-source-alist))
  ;;     (mapc
  ;;      #'treesit-install-language-grammar
  ;;      (mapcar #'car treesit-language-source-alist)))

  ;;   (setopt major-mode-remap-alist
  ;;           '((sh-mode . bash-ts-mode)
  ;;             (c-mode . c-ts-mode)
  ;;             (c++-mode . c++-ts-mode)
  ;;             (c-or-c++-mode . c-or-c++-ts-mode)
  ;;             (cmake-mode . cmake-ts-mode)
  ;;             (css-mode . css-ts-mode)
  ;;             (dockerfile-mode . dockerfile-ts-mode)
  ;;             (html-mode . html-ts-mode)
  ;;             (java-mode . java-ts-mode)
  ;;             (json-mode . json-ts-mode)
  ;;             (kdl-mode . kdl-ts-mode)
  ;;             (python-mode . python-ts-mode)
  ;;             (toml-mode . toml-ts-mode)
  ;;             (conf-toml-mode . toml-ts-mode)
  ;;             (yaml-mode . yaml-ts-mode))))
  )

;; (with-eval-after-load 'c++-ts-mode
;;   (bind-key "C-M-a" #'treesit-beginning-of-defun c++-ts-mode-map)
;;   (bind-key "C-M-e" #'treesit-end-of-defun c++-ts-mode-map))

;; Some systems may not have treesitter mode enabled.
(use-package cc-mode
  :ensure nil

  :mode ("\\.h\\'" . c-or-c++-ts-mode)

  :hook
  ((awk-mode
    .
    (lambda ()
      (setq-local c-basic-offset 4)
      (c-set-style "awk")
      (eglot-ensure)))
   ((c-mode c++-mode)
    .
    (lambda ()
      (setq-local c-basic-offset 2)
      (c-set-style "linux")
      (eglot-ensure)))))

(use-package c-ts-mode
  :ensure nil

  :when (and (fboundp 'treesit-available-p) (treesit-available-p))

  :mode (("\\.h\\'" . c-or-c++-ts-mode) ("\\.c\\'" . c++-ts-mode))

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
     (c-ts-mode-set-style "linux")
     (eglot-ensure)))

  :bind
  (:map
   c++-ts-mode-map
   ("C-M-a" . treesit-beginning-of-defun)
   ("C-M-e" . treesit-end-of-defun)))

;; Some systems may not have treesitter mode enabled.
(if (and (fboundp 'treesit-available-p) (treesit-available-p))
    (add-to-list 'auto-mode-alist '("\\.cu[h]?\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cu[h]?\\'" . c++-mode)))

(use-package opencl-c-mode
  :mode "\\.cl\\'")

(use-package cmake-mode
  :when (executable-find "cmake")

  :mode (("CMakeLists\\.txt\\'" . cmake-ts-mode) ("\\.cmake\\'" . cmake-ts-mode))

  :hook
  ((cmake-mode cmake-ts-mode)
   .
   (lambda ()
     ;; `cmake-mode' is derived from `text-mode', so disable grammar and spell
     ;; checking.
     (jinx-mode -1)
     (eglot-ensure))))

(use-package python
  :ensure nil

  :mode
  (("SCon\\(struct\\|script\\)\\'" . python-mode)
   ("[./]flake8\\'" . conf-mode)
   ("/Pipfile\\'" . conf-mode))

  :hook
  ((python-mode python-ts-mode)
   .
   (lambda ()
     (setq-local tab-width 4)
     (eglot-ensure)))

  :bind*
  (:map
   python-mode-map
   ("C-M-n" . python-nav-forward-defun)
   ("C-M-p" . python-nav-backward-defun))

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

(use-package pip-requirements
  :commands (pip-requirements-mode))

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

;; `cperl-mode' derives from `perl-mode' which derives from `prog-mode' which derives from `fundamental-mode'.
(use-package cperl-mode
  :ensure nil

  :mode ("latexmkrc\\'" . cperl-mode)

  :interpreter ("perl" . cperl-mode)

  :init (defalias 'perl-mode 'cperl-mode))

(use-package sh-script
  :ensure nil

  :mode ("\\bashrc\\'" . bash-ts-mode)

  :hook
  ((sh-mode bash-ts-mode)
   .
   (lambda ()
     ;; Apply formatting defaults for shfmt used by bash-language-server
     (setenv "SHFMT_OPTS" "-i 2 -ci")
     (eglot-ensure)))
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
     (eglot-ensure))))

(use-package lisp-mode
  :ensure nil

  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . lisp-data-mode))

(use-package elisp-mode
  :ensure nil

  :mode ("\\.el\\'" . emacs-lisp-mode))

(dolist (hook '(lisp-data-mode-hook emacs-lisp-mode-hook))
  (add-hook
   hook
   (lambda ()
     (when buffer-file-name
       (add-hook 'after-save-hook #'check-parens nil t)))))

(use-package ini-mode
  :commands ini-mode)

(use-package conf-mode
  :ensure nil

  :mode ("\\.cfg\\'" "\\.conf\\'" "\\.env\\..*\\'" "\\.env\\'"))

(use-package toml-ts-mode
  :ensure nil

  :mode ("\\.toml\\'" "Cargo\\.lock\\'")

  :hook (toml-ts-mode . eglot-ensure))

(use-package yaml-mode
  :mode
  (("\\.ya?ml\\'" . yaml-mode)
   ("\\.clang-format\\'" . yaml-mode)
   ("\\.clang-tidy\\'" . yaml-mode)
   ("\\.clangd\\'" . yaml-mode))

  :hook
  ((yaml-mode yaml-ts-mode)
   .
   (lambda ()
     ;; `yaml-mode' is derived from `text-mode', so disable grammar and spell
     ;; checking.
     (jinx-mode -1)
     (eglot-ensure))))

(use-package yaml-imenu
  :hook ((yaml-mode yaml-ts-mode) . yaml-imenu-enable))

(use-package web-mode
  :mode "\\.html?\\'"

  :hook (web-mode . eglot-ensure)

  :bind ("C-c C-d")

  :custom
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-quoting t)
  (web-mode-enable-block-face t)
  (web-mode-enable-css-colorization t)
  ;; Highlight the element under the cursor
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-markup-indent-offset 2) ; HTML
  (web-mode-css-indent-offset 2) ; CSS
  (web-mode-code-indent-offset 2) ; Script
  (web-mode-style-padding 2) ; For <style> tag
  (web-mode-script-padding 2) ; For <script> tag

  :config
  (with-eval-after-load 'html-mode
    (unbind-key "M-o" html-mode-map)
    (when (boundp 'html-ts-mode-map)
      (unbind-key "M-o" html-ts-mode-map))))

;; (use-package emmet-mode
;;   :hook ((web-mode css-mode css-ts-mode html-mode html-ts-mode) . emmet-mode)
;;   :custom
;;   (emmet-move-cursor-between-quote t)
;;   (emmet-self-closing-tag-style " /"))

(use-package css-mode
  :ensure nil

  :hook ((css-mode css-ts-mode) . eglot-ensure)

  :custom (css-indent-offset 2))

(use-package autoconf
  :ensure nil

  :hook (autoconf-mode . eglot-ensure))

(use-package make-mode
  :ensure nil

  :mode
  (("\\Makefile\\'" . makefile-mode)
   ("\\Makefile.common\\'" . makefile-mode)
   ("makefile\\.rules\\'" . makefile-mode))

  :hook
  (makefile-mode
   .
   (lambda ()
     (setq-local indent-tabs-mode t)
     (eglot-ensure))))

(use-package bison-mode
  :mode ("\\.flex\\'" . flex-mode)

  :mode ("\\.bison\\'" . bison-mode)

  :hook
  ((flex-mode bison-mode)
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

(use-package nxml-mode
  :preface
  (defun sb/nxml-setup ()
    ;; `xml-mode' is derived from `text-mode', so disable grammar and spell
    ;; checking.
    (jinx-mode -1)
    (eglot-ensure))

  :ensure nil

  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom\\'" "\\.drawio\\'")

  :hook (nxml-mode . sb/nxml-setup)

  :custom
  (nxml-auto-insert-xml-declaration-flag t)
  (nxml-slash-auto-complete-flag t)
  (nxml-sexp-element-flag t)

  :config (fset 'xml-mode 'nxml-mode))

(use-package json-mode
  :preface
  (defun sb/json-setup ()
    (setq-local js-indent-level 2)
    (eglot-ensure))

  :mode
  (("pyrightconfig\\.json\\'" . jsonc-mode)
   (".*/vscode/settings.json\\'" . jsonc-mode)
   (".*/\\.vscode/settings.json\\'" . jsonc-mode)
   ("User/settings\\.json\\'" . jsonc-mode)
   ("\\.jsonc\\'" . jsonc-mode)
   ("\\.htmlhintrc\\'" . json-mode)
   ("\\.json\\'" . json-mode))

  :hook ((json-mode json-ts-mode jsonc-mode) . sb/json-setup))

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

;; https://alexforsale.github.io/posts/org-mode-workflow/
;; https://github.com/james-stoup/org-mode-better-defaults
;; https://github.com/james-stoup/emacs-org-mode-tutorial

(use-package org
  :pin gnu

  :mode ("\\.org\\'" . org-mode)

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
   ("C-c C-n" . org-next-visible-heading)
   ("C-c C-p" . org-previous-visible-heading)
   ("C-c C-f" . org-backward-heading-same-level)
   ("C-c C-b" . outline-backward-same-level)
   ("C-c C-u" . outline-up-heading)
   ("M-{" . org-backward-element)
   ("M-}" . org-forward-element)
   ;; Insert empty structural blocks, such as ‘#+BEGIN_SRC’ . . . ‘#+END_SRC’,
   ("C-c C-," . org-insert-structure-template)
   ("C-c C-j" . consult-outline)
   ("C-c C-l" . org-store-link)
   ("C-c C-k" . org-insert-link)
   ("C-c ." . org-timestamp)
   ;; ("Shift-<left>" . org-timestamp-down-day)
   ;; ("Shift-<right>" . org-timestamp-up-day)
   ("C-c ;" . org-toggle-comment)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ;; Use a prefix argument to record a timestamp
   ("C-c C-t" . org-todo)
   ("C-c ," . org-priority)
   ;; ("S-UP" . org-priority-up)
   ;; ("S-DOWN" . org-priority-down)
   ("C-c C-e" . org-export-dispatch)
   ;; ("C-c C-e l l" . org-latex-export-to-latex)
   ;; ("C-c C-e l p" . org-latex-export-to-pdf)
   ;; ("C-c C-e h h" . org-html-export-to-html)
   ("C-c C-x f" . org-footnote-action)
   ;; Jump between definition and reference
   ("C-c C-c" . org-ctrl-c-ctrl-c))

  :custom
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t "Hide *, ~, and / in Org text unless you edit")
  (org-hide-leading-stars-before-indent-mode nil)
  (org-hide-leading-stars t)
  (org-src-preserve-indentation t)
  (org-src-tabs-acts-natively t "TAB behavior depends on the major mode")
  (org-src-window-setup 'current-window)
  (org-startup-indented t)
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
  (org-latex-pdf-process
   '("latexmk -pdf -shell-escape -interaction=nonstopmode -output-directory=%o -bibtex -f %f"))

  (org-agenda-files '("~/Dropbox/TODOs.org"))
  ;; The vertical bar separates states that need work with states that do not need any work.
  (org-todo-keywords
   '((sequence
      "TODO(t!)"
      "NEXT(n!)"
      "PROG(p!)"
      "WAIT(w!)"
      "HOLD(h!)"
      "|"
      "DONE(d!)"
      "CANCELED(c)")))
  (org-todo-keyword-faces
   '(("TODO" . "IndianRed1")
     ("PROG" . "DeepSkyBlue1")
     ("DONE" . "MediumSeaGreen")))
  (org-priority-highest ?A org-priority-lowest ?D org-priority-default ?B)
  (org-priority-faces
   '((?A . (:foreground "#bf616a" :weight bold :underline t))
     (?B . (:foreground "#d08770" :weight bold :underline t))
     (?C . (:foreground "#4c566a" :weight bold :underline t))
     (?D . (:foreground "#3b4252" :weight bold :underline t))))
  (org-use-tag-inheritance t)

  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  :diminish org-indent-mode)

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

;; Without auctex
(with-eval-after-load 'tex-mode
  (setopt tex-command "pdflatex"))

;; Auctex provides enhanced versions of `tex-mode' and `latex-mode', which
;; automatically replace the vanilla ones. Auctex provides `LaTeX-mode', which
;; is an alias to `latex-mode'. Auctex overrides the tex package. "P" in the
;; modeline highlighter "LaTeX/MPS" is due to `TeX-PDF-mode'.
(use-package latex
  :vc
  (:url
   "https://git.savannah.gnu.org/git/auctex.git"
   :branch "main"
   :lisp-dir "."
   :main-file "tex.el")

  :init (require 'tex-site)

  :hook
  (LaTeX-mode
   .
   (lambda ()
     (LaTeX-math-mode 1)
     (TeX-PDF-mode) ; Use `pdflatex'
     (turn-on-reftex)
     (TeX-source-correlate-mode)))

  :bind (:map LaTeX-mode-map ("C-c C-j" . consult-outline))

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
  (font-latex-script-display '()) ; super-/sub-script on baseline

  ;; Avoid different font styles and instead only use syntax color
  ;; (font-latex-fontify-sectioning 1.0 "Avoid emphasizing section headers")
  (font-latex-fontify-sectioning 'color)

  ;; Exclude bold/italic from keywords
  (font-latex-deactivated-keyword-classes
   '("italic-command" "bold-command" "italic-declaration" "bold-declaration"))

  :config
  ;; Make AUCTeX aware of the multifile document structure, always query for the
  ;; master file
  (setq-default
   TeX-master nil
   TeX-command-default "LaTeXMk")
  (with-eval-after-load 'tex-mode
    (unbind-key "C-c ;" TeX-mode-map))

  ;; Enable correlation with synctex From Okular, press Shift + Left click to go
  ;; to the desired line.
  (setopt
   TeX-source-correlate-method 'synctex
   TeX-source-correlate-mode t
   TeX-source-correlate-start-server t
   ;; Enable synctex generation. Even though the command shows as "latex"
   ;; pdflatex is actually called
   LaTeX-command "latex -shell-escape=1 -synctex=1")

  (when (executable-find "okular")
    (add-to-list
     'TeX-view-program-list
     '("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
    (add-to-list 'TeX-view-program-selection '(output-pdf "Okular"))))

(use-package reftex
  :ensure nil

  :hook (LaTeX-mode . turn-on-reftex)

  :bind
  (("C-c [" . reftex-citation)
   ("C-c )" . reftex-reference)
   ("C-c (" . reftex-label) ; Add a label
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

(use-package consult-reftex
  :vc (:url "https://github.com/karthink/consult-reftex" :rev :newest)

  :after (consult reftex)

  :commands (consult-reftex-insert-reference consult-reftex-goto-label))

(use-package bibtex
  :ensure nil

  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-maintain-sorted-entries t)
  (bibtex-comma-after-last-field nil))

;; (use-package math-delimiters
;;   :ensure (:host github :repo "oantolin/math-delimiters")
;;   :demand t
;;   :commands (math-delimiters-no-dollars math-delimiters-toggle)
;;   :bind
;;   (:map
;;    TeX-mode-map ("$" . math-delimiters-insert)
;;    :map LaTeX-mode-map ("$" . math-delimiters-insert)))

(use-package dumb-jump
  :after xref

  :demand t

  :commands (dumb-jump-go dumb-jump-back)

  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)

  :custom
  (dumb-jump-quiet t)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg))

(use-package citre
  :preface
  (defun sb/jump-citre-xref ()
    "Jump to the definition of the symbol at point using `citre-jump' first. Falls back to `xref-find-definitions' on failure."
    (interactive)
    (condition-case _
        (citre-jump)
      (error
       (let* ((xref-prompt-for-identifier nil))
         (call-interactively #'xref-find-definitions)))))

  (defun sb/jump-xref-citre ()
    "Jump to the definition of the symbol at point using `xref-find-definitions' first. Falls back to `citre-jump' on failure."
    (interactive)
    (let ((ofn
           (lambda ()
             (let* ((xref-prompt-for-identifier nil))
               (call-interactively #'xref-find-definitions)))))
      (condition-case _
          (citre-jump)
        (error
         (funcall ofn)))))

  (defun sb/jump-back-citre-xref ()
    "Go back to the position before last `citre-jump'.
Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error
       (if (fboundp #'xref-go-back)
           (call-interactively #'xref-go-back)
         (call-interactively #'xref-pop-marker-stack)))))

  :hook ((prog-mode LaTeX-mode) . citre-mode)

  :bind*
  (("M-." . sb/jump-xref-citre)
   ("M-," . sb/jump-back-citre-xref)
   ("C-c c j" . citre-jump)
   ("C-c c b" . citre-jump-back)
   ("C-c c p" . citre-peek)
   ("C-c c a" . citre-ace-peek)
   ("C-c c r" . citre-jump-to-reference)
   ("C-c c c" . citre-create-tags-file)
   ("C-c c u" . citre-update-tags-file)
   ("C-c c e" . citre-edit-tags-file-recipe)
   ("C-c c g" . citre-global-update-database))

  :custom (citre-default-create-tags-file-location 'in-dir)
  ;; Add exclude by: --exclude=target or by --exclude=@./.ctagsignore
  ;; Add dirs/files to scan here, one line per dir/file
  (citre-ctags-default-options
   (string-join
    '("-o %TAGSFILE%"
      "--languages=BibTeX,C,C++,CUDA,CMake,EmacsLisp,Java,Make,Python,Sh,TeX"
      "--kinds-all=*"
      "--fields=*"
      "--extras=*"
      "--recurse")
    " "))
  ;; Add Elisp to the backend lists.
  (citre-find-definition-backends '(elisp eglot tags global))
  (citre-find-reference-backends '(elisp eglot global))

  :config
  (setq-default
   citre-enable-imenu-integration nil ; Conflicts with Elisp imenu entries
   ;; Large tags file slows down completion
   citre-enable-capf-integration nil)

  ;; Use `citre' with Emacs Lisp
  (defvar citre-elisp-backend
    (citre-xref-backend-to-citre-backend
     ;; This is the xref backend name
     'elisp
     ;; A function to tell if the backend is usable
     (lambda () (derived-mode-p 'emacs-lisp-mode))))
  ;; Register the backend, which means to bind it with the symbol `elisp'.
  (citre-register-backend 'elisp citre-elisp-backend)

  ;; Integrate with `lsp-mode' and `eglot'
  (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
    (let ((fetcher (apply -fn -args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (apply -fn -args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))

  (defun sb/push-point-to-xref-marker-stack (&rest r)
    (xref-push-marker-stack (point-marker)))

  (dolist (func
           '(xref-find-definitions
             xref-find-references
             find-function
             consult-imenu
             project-grep
             deadgrep
             counsel-rg
             consult-lsp-file-symbols
             citre-jump))
    (advice-add func :before 'sb/push-point-to-xref-marker-stack))

  :diminish)

(use-package modus-themes
  :when (eq sb/theme 'modus-vivendi)

  :init (load-theme 'modus-vivendi t)

  :custom (modus-themes-mixed-fonts nil))

(use-package standard-themes
  :when (eq sb/theme 'standard-dark)

  :init (load-theme 'standard-dark t)

  :custom (modus-themes-mixed-fonts nil))

(use-package matugen-theme
  :ensure nil

  :load-path "themes"

  :when (eq sb/theme 'matugen)

  :init
  (require 'matugen-theme)
  (load-theme 'matugen t))

(use-package mini-echo
  :when (eq sb/modeline-theme 'mini-echo)

  :hook (emacs-startup . mini-echo-mode)

  :custom
  (mini-echo-buffer-status-style 'both)
  (mini-echo-right-padding 2)
  (mini-echo-persistent-rule
   '(:long
     ("remote-host"
      "selection-info"
      "flymake"
      "vcs"
      "buffer-position"
      "major-mode"
      "shrink-path")
     :short ("remote-host" "flymake" "vcs" "buffer-position" "shrink-path")))
  (mini-echo-temporary-rule
   '(:both ("selection-info" "narrow" "repeat" "text-scale" "wgrep")))

  :config
  (mini-echo-define-segment
   "shrink-path"
   "Return shrink path of current buffer in project or parent dir."
   :update-advice '((vc-refresh-state . :after))
   :fetch
   (concat
    (mini-echo-buffer-read-only)

    (propertize (let* ((filepath (buffer-file-name))
                       (project
                        (or mini-echo--project-root
                            (mini-echo-update-project-root)))
                       (dir
                        (thread-last
                         default-directory
                         (or (and (not (string-empty-p project)) project))
                         (directory-file-name)
                         (file-name-nondirectory))))
                  (cond
                   ((not filepath)
                    "")
                   ((string-empty-p project)
                    (propertize (concat dir "/") 'face 'shadow))
                   ((string-prefix-p project filepath)
                    (concat
                     (propertize dir 'face 'mini-echo-project) "/"
                     (when-let* ((p
                                  (butlast
                                   (split-string (string-remove-prefix
                                                  project filepath)
                                                 "/" t))))
                       (concat
                        (propertize (mapconcat (lambda (s) (substring s 0 1)) p
                                               "/")
                                    'face 'shadow)
                        "/"))))
                   (t
                    "")))
                'face '(:foreground "orange" :height 1.0))

    (propertize (mini-echo-buffer-name-with-status)
                'face
                '(:foreground "white" :height 1.0)))

   :update (mini-echo-update-project-root)))

;; Center the text environment
(use-package olivetti
  :hook ((text-mode prog-mode fundamental-mode conf-mode org-mode) . olivetti-mode)

  :bind (:map olivetti-mode-map ("C-c {") ("C-c }") ("C-c \\"))

  :diminish)

(use-package kdl-mode
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))

  :mode ("\\.kdl\\'" . kdl-mode))

(use-package asm-mode
  :ensure nil

  :hook (asm-mode . eglot-ensure))

;; ;; Combined clipboard integration for terminal & GUI. Sends every kill from a
;; ;; TTY frame to the system clipboard. Clipetty handles clipboard via OSC 52.
;; (use-package clipetty
;;   :hook (emacs-startup . global-clipetty-mode)

;;   :diminish)

;; Only enable xclip in TTY under X11
(use-package xclip
  :when
  (and (not (display-graphic-p)) ; only in TTY
       (not (getenv "WAYLAND_DISPLAY")) ; avoid Wayland
       (or (executable-find "xclip") (executable-find "xsel")))

  :hook (emacs-startup . xclip-mode))

(use-package kill-file-path

  :commands
  (kill-file-path-basename
   kill-file-path-basename-without-extension
   kill-file-path-dirname
   kill-file-path))

(use-package eglot
  :after project

  :pin gnu

  :hook
  ((html-mode html-ts-mode LaTeX-mode markdown-mode org-mode text-mode)
   .
   (lambda ()
     ;; Disable LSP for git commit message buffers which are usually ephemeral
     (unless (string-equal
              (file-name-nondirectory (or buffer-file-name ""))
              "COMMIT_EDITMSG")
       (eglot-ensure))))

  :bind
  (("M-'" . eglot-find-implementation)
   ("C-c l i" . eglot-find-implementation)
   ("C-c l d" . eglot-find-declaration)
   ("C-c l t" . eglot-find-typeDefinition)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format)
   ("C-c l x" . eglot-code-actions)
   ("C-c l k" . eglot-code-action-quickfix)
   ("C-c l o" . eglot-code-action-organize-imports))

  :custom
  ;; Disabling this helps avoid the race condition between closing a project and shutting down LSP servers.
  ;; (eglot-autoshutdown t)

  (eglot-sync-connect nil "Do not block waiting to connect to the LSP")
  (eglot-send-changes-idle-time 1)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities
   '(:codeLensProvider
     :documentHighlightProvider
     :documentOnTypeFormattingProvider
     :foldingRangeProvider
     :hoverProvider ; Automatic documentation popups can be distracting
     :inlayHintProvider ; Inlay hints are distracting
     :executeCommandProvider
     :documentLinkProvider))
  (eglot-report-progress nil)
  ;; Do not clutter the modeline
  (eglot-mode-line-format nil)
  (eglot-confirm-server-edits '((eglot-rename . nil) (t . diff)))

  :config
  ;; Reduce memory usage and avoid cluttering *EGLOT events* buffer
  ;; (setopt eglot-events-buffer-config '(:size 0 :format short))

  ;; (fset #'jsonrpc--log-event #'ignore)

  (setopt
   eglot-server-programs
   `(((toml-mode toml-ts-mode conf-toml-mode) . ("taplo" "lsp" "stdio"))
     ;; `harper-ls' is more efficient than `ltex-ls-plus' but does not support `LaTeX-mode' completely
     (LaTeX-mode . ("rass" "latex"))
     (text-mode . ("rass" "text"))
     ((markdown-mode markdown-ts-mode) . ("rass" "markdown"))
     (org-mode
      . ,(eglot-alternatives '(("harper-ls" "--stdio") "ltex-ls-plus")))
     ((autoconf-mode makefile-mode makefile-automake-mode makefile-gmake-mode)
      . ("autotools-language-server"))
     (fish-mode . ("fish-lsp" "start"))
     ((asm-mode fasm-mode masm-mode nasm-mode gas-mode) . ("asm-lsp"))
     ((c-mode c-ts-mode c++-mode c++-ts-mode c-or-c++-ts-mode c-or-c++-mode)
      .
      ("clangd"
       "-j=4"
       "--all-scopes-completion"
       "--background-index"
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
       "--inlay-hints=0"
       "--pretty"))
     (awk-mode . ("awk-language-server"))
     ((scss-mode css-mode css-ts-mode)
      .
      ("vscode-css-language-server" "--stdio"))
     ((web-mode html-mode html-ts-mode)
      .
      ("vscode-html-language-server" "--stdio"))
     ((json-mode json-ts-mode jsonc-mode)
      .
      ("vscode-json-language-server" "--stdio"))
     ((yaml-ts-mode yaml-mode) . ("yaml-language-server" "--stdio"))
     ((cmake-mode cmake-ts-mode)
      .
      ,(eglot-alternatives
        '(("neocmakelsp" "--stdio") "cmake-language-server")))
     ((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
     ;; Download the source from
     ;; https://github.com/eclipse-jdtls/eclipse.jdt.ls/tags. Build with "./mvnw
     ;; clean verify -U -DskipTests=true". Change the url in
     ;; "org.eclipse.jdt.ls.target/org.eclipse.jdt.ls.tp.target" if there is a
     ;; "No repository found" error.
     ((java-mode java-ts-mode)
      .
      ("jdtls" "--illegal-access=warn" "-Xms2G" "-Xmx4G"))
     ;; (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman" "server")))
     ((perl-mode cperl-mode)
      .
      ("perl" "-MPerl::LanguageServer" "-e" "Perl::LanguageServer::run"))
     ((dockerfile-mode dockerfile-ts-mode) . ("docker-langserver" "--stdio"))
     ;; Download the latest milestone from
     ;; https://github.com/eclipse-lemminx/lemminx and build with "./mvnw clean
     ;; verify -DskipTests=true". After successful compilation, the resulting
     ;; output "org.eclipse.lemminx-uber.jar" will be in the folder
     ;; "org.eclipse.lemminx/target".
     ((nxml-mode xml-mode)
      .
      ("java" "-jar"
       ,(expand-file-name "servers/org.eclipse.lemminx-uber.jar"
                          user-emacs-directory)))
     ((python-mode python-ts-mode) . ("rass" "python"))))

  ;; Eglot overwrites `company-backends' to only include `company-capf'
  (setq eglot-stay-out-of '(flymake yasnippet company eldoc))

  ;; `eglot-workspace-configuration' should be set as a directory-local
  ;; variable. `:json-false' is the correct way to send false to LSP servers
  ;; instead of nil, which would remove the key.
  (setq-default
   eglot-workspace-configuration
   '(:pylsp
     (:configurationSources
      ["pyproject.toml" "setup.cfg"]
      :plugins
      (:autopep8
       (:enabled :json-false)
       :black (:enabled :json-false)
       :flake8 (:enabled :json-false)
       :jedi
       (:auto_import_modules
        []
        :env_vars nil ; (:SOME_ENV_VAR "/some/path")
        :environment nil ; "./.venv/"
        :extra_paths [])
       :jedi_completion
       (:cache_for
        []
        :eager
        :json-false
        :enabled t
        :fuzzy t
        :include_class_objects
        :json-false
        :include_function_objects
        :json-false
        :include_params t
        :resolve_at_most 25)
       :jedi_definition
       (:enabled
        t
        :follow_builtin_definitions t
        :follow_builtin_imports t
        :follow_imports t)
       :jedi_hover (:enabled t)
       :jedi_references (:enabled t)
       :jedi_signature_help (:enabled t)
       :jedi_symbols
       (:all_scopes t :enabled t :include_import_symbols :json-false)
       :mccabe (:enabled t :threshold 15)
       :mypy (:enabled :json-false)
       :preload (:enabled :json-false :modules [])
       :pycodestyle (:enabled :json-false)
       :pydocstyle (:enabled :json-false)
       :pyflakes (:enabled :json-false)
       ;; We use "basedpyright" as the primary server which provides type hints.
       :pylint (:args [] :enabled :json-false)
       :pylsp_black (:enabled :json-false)
       :pylsp_isort (:enabled t)
       :pylsp_mypy
       (:enabled t :live_mode :json-false :report_progress :json-false)
       ;; We use ruff from `apheleia-mode' because `basedpyright' does not support formatting.
       :pylsp_ruff (:enabled t :formatEnabled :json-false :lineLength 80)
       :rope_autoimport
       (:code_actions
        (:enabled :json-false)
        :completions (:enabled :json-false)
        :enabled
        :json-false
        :memory
        :json-false)
       :rope_completion (:eager :json-false :enabled :json-false)
       :ruff
       (:enabled :json-false :formatEnabled :json-false :lineLength 80)
       :yapf (:enabled :json-false)
       :rope (:extensionModules nil :ropeFolder nil)))
     ;; A pyrightconfig.json or an entry in pyproject.toml gets priority over
     ;; LSP configuration for basedpyright.
     :basedpyright
     (:checkOnlyOpenFiles
      t
      :reportDuplicateImport t
      :typeCheckingMode "recommended"
      :useLibraryCodeForTypes t
      :analysis
      (:diagnosticSeverityOverrides
       (:reportUnusedCallResult "none" :reportInvalidCast :json-false)
       :inlayHints
       (:callArgumentNames
        :json-false
        :functionReturnTypes
        :json-false
        :variableTypes
        :json-false
        :genericTypes
        :json-false)))
     :ltex-ls-plus
     (:language
      "en-US"
      :disabledRules
      ["ELLIPSIS"
       "EN_QUOTES"
       "MORFOLOGIK_RULE_EN_US"
       "MORFOLOGIK_RULE_EN_GB"
       "HUNSPELL_RULE"
       "HUNSPELL_NO_SUGGEST_RULE"]
      ;; Keep grammar and style checking
      :additionalRules (:enablePickyRules t :motherTongue "en-IN"))
     :yaml
     (:format
      (:enable t :singleQuote nil :bracketSpacing t)
      :validate t
      :hover t
      :completion t)
     :json (:format (:enable t))
     ;; Harper uses four dictionaries: per-user, per-workspace, file-local, and a in-built static dictionary.
     :harper-ls
     (:userDictPath
      "~/.config/harper-ls/dictionary.txt"
      :workspaceDictPath "${workspaceFolder}/.harper-dictionary.txt"
      :fileDictPath ""
      :linters
      (:SpellCheck
       :json-false
       :SpelledNumbers
       :json-false
       :AnA t
       :UnclosedQuotes t
       :WrongApostrophe
       :json-false
       :LongSentences
       :json-false
       :RepeatedWords t
       :Spaces t
       :CorrectNumberSuffix t
       :SentenceCapitalization t)
      :codeActions (:ForceStable :json-false)
      :diagnosticSeverity "hint"
      :markdown (:IgnoreLinkTitle :json-false)
      :isolateEnglish
      :json-false
      :dialect "American")))

  (when (fboundp 'eglot-semantic-tokens-mode)
    (add-hook
     'eglot-managed-mode-hook
     (lambda ()
       (when (derived-mode-p 'c-mode 'c++-mode)
         (eglot-semantic-tokens-mode 1)))))

  ;; (setq-default completion-category-overrides
  ;;               '((eglot (styles hotfuzz basic substring orderless))
  ;;                 (eglot-capf (styles hotfuzz orderless))))

  ;; Avoid fuzzy or orderless completion for code
  (setq-default completion-category-overrides
                '((eglot (styles basic))
                  (eglot-capf (styles basic))
                  (capf (styles basic))))

  (add-to-list
   'display-buffer-alist
   '("\\*EGLOT workspace configuration\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 2)
     (window-height . 0.5))))

(use-package consult-eglot
  :after (consult eglot)

  :commands consult-eglot-symbols)

(use-package flymake
  :pin gnu

  :hook ((prog-mode text-mode) . flymake-mode)

  :bind
  (("C-c ! l" . flymake-show-buffer-diagnostics)
   :map
   flymake-mode-map
   ("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error))

  :config (setq flymake-diagnostic-format-alist '((t . (origin code message)))))

(defun sb/save-all-buffers ()
  "Save all modified buffers without prompting."
  (interactive)
  (save-some-buffers t))
(bind-key "C-S-s" #'sb/save-all-buffers)

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
(bind-key "C-c ;" #'sb/comment-line)

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
(bind-key "C-x |" #'sb/toggle-window-split)

;; Inside strings, special keys like tab or F1-Fn have to be written inside
;; angle brackets, e.g., "C-<up>". Standalone special keys (and some
;; combinations) can be written in square brackets, e.g. [tab] instead of
;; "<tab>".

;; ESC serves as a substitute for META, but there is no need to hold down ESC
;; while pressing the subsequent key. Instead "M-something" keybindings can be
;; triggered by pressing ESC and the other key sequentially.

;; ;; Allow scaling text across all Emacs frames unlike `text-scale-mode'.
;; (use-package default-text-scale
;;   :when (display-graphic-p)
;;   :bind
;;   (("C-M-+" . default-text-scale-increase)
;;    ("C-M--" . default-text-scale-decrease)))

;; Show free bindings in current buffer
(use-package free-keys
  :commands free-keys)

;; Support the Kitty keyboard protocol in Emacs
(use-package kkp
  :unless (display-graphic-p)

  :hook (emacs-startup . global-kkp-mode)

  ;; :bind
  ;; ("M-<backspace>" . backward-kill-word) ; Should be remapped to "M-DEL"

  :config
  ;; These workarounds are to help with Zellij.
  (define-key key-translation-map (kbd "M-S-;") (kbd "M-:"))
  (define-key key-translation-map (kbd "M-S-4") (kbd "M-$"))
  (define-key key-translation-map (kbd "M-S-/") (kbd "M-?")))

(define-key input-decode-map "\e[127;6u" (kbd "C-S-<backspace>"))
(define-key input-decode-map "\e[46;5u" (kbd "C-."))
(define-key input-decode-map "\e[47;5u" (kbd "C-/"))
;; (define-key input-decode-map "\e[61;5u" (kbd "C-="))

(add-hook
 'emacs-startup-hook
 (lambda ()
   (let ((gc-time (float-time gc-elapsed)))
     (message "Emacs ready (init time = %s, gc time = %.2fs, gc count = %d)."
              (emacs-init-time)
              gc-time
              gcs-done))))

;;; init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; End:
