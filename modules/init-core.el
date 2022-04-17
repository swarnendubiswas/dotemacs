(use-package warnings
  :straight nil
  :init
  ;; This is not a great idea, but I expect most warnings will arise from third-party packages.
  (setq warning-minimum-level :emergency))

(defvar apropos-do-all)
(defvar bookmark-save-flag)
(defvar compilation-always-kill)
(defvar compilation-scroll-output)
(defvar sort-fold-case)
(defvar help-enable-symbol-autoload)

(setq ad-redefinition-action 'accept ; Turn off warnings due to redefinitions
      apropos-do-all t ; Make `apropos' search more extensively
      auto-mode-case-fold nil ; Avoid a second pass through `auto-mode-alist'
      ;; Unlike `auto-save-mode', `auto-save-visited-mode' saves the buffer contents to the visiting
      ;; file and runs all save-related hooks. We disable `auto-save-mode' and prefer
      ;; `auto-save-visited-mode' instead.
      auto-save-default nil
      auto-save-no-message t ; Allows for debugging frequent autosave triggers if `nil'
      auto-save-interval 0 ; Disable autosaving based on number of characters typed
      ;; Save buffer to file after idling for some time, the default of 5s may be too frequent since
      ;; it runs all the save-related hooks.
      auto-save-visited-interval 30
      backup-inhibited t ; Disable backup for a per-file basis
      blink-matching-paren t ; Distracting
      bookmark-save-flag 1 ; Save bookmark after every bookmark edit and also when Emacs is killed
      case-fold-search t ; Searches and matches should ignore case
      comment-auto-fill-only-comments t
      compilation-always-kill t ; Kill a compilation process before starting a new one
      compilation-ask-about-save nil ; Save all modified buffers without asking
      ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first
      ;; error.
      compilation-scroll-output 'first-error
      completion-cycle-threshold 3 ; TAB cycle if there are only few candidates
      completion-ignore-case t ; Ignore case when completing
      completions-detailed t
      confirm-kill-emacs nil
      confirm-kill-processes nil ; Prevent "Active processes exist" when you quit Emacs
      confirm-nonexistent-file-or-buffer t
      create-lockfiles nil
      cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
      custom-safe-themes t
      delete-by-moving-to-trash t ; Use system trash to deal with mistakes while deleting
      echo-keystrokes 0.5 ; Show current key-sequence in minibuffer
      ;; enable-local-variables :all ; Avoid "defvar" warnings
      enable-recursive-minibuffers t ; Keeping track of the minibuffer nesting is difficult
      ;; The Emacs documentation warns about performance slowdowns with enabling remote directory
      ;; variables, but I edit files over Tramp a lot.
      enable-remote-dir-locals t
      ;; Expand truncated ellipsis:suspension points in the echo area, useful to see more
      ;; information
      eval-expression-print-length 500
      ;; Accelerate scrolling operations when non-nil. Only those portions of the buffer which are
      ;; actually going to be displayed get fontified.
      fast-but-imprecise-scrolling t
      ;; Disable the warning "X and Y are the same file" in case of symlinks
      find-file-suppress-same-file-warnings t
      find-file-visit-truename t ; Show true name, useful in case of symlinks
      frame-title-format (list '(buffer-file-name "%f" "%b") " - " invocation-name)
      help-enable-symbol-autoload t
      help-window-select t ; Makes it easy to close the window
      history-delete-duplicates t
      history-length 50 ; Reduce the state that is to be read
      indicate-buffer-boundaries nil
      kill-do-not-save-duplicates t
      kill-whole-line t ;; TODO: What is the utility of this variable?
      make-backup-files nil ; Stop making backup `~' files
      message-log-max 5000
      ;; mouse-drag-copy-region nil ; Mouse is disabled
      ;; mouse-yank-at-point t ; Yank at point with mouse instead of at click
      next-error-message-highlight t
      read-buffer-completion-ignore-case t ; Ignore case when reading a buffer name
      read-file-name-completion-ignore-case t ; Ignore case when reading a file name
      read-minibuffer-restore-windows t
      read-process-output-max (* 5 1024 1024) ; 5 MB, LSP suggests increasing it
      require-final-newline t ; Always end a file with a newline
      ring-bell-function 'ignore ; Disable beeping sound
      save-interprogram-paste-before-kill t
      save-silently t ; Error messages will still be printed
      ;; Enable use of system clipboard across Emacs and other applications, does not work on the
      ;; TUI
      select-enable-clipboard t
      sentence-end-double-space nil
      shift-select-mode nil ; Do not use `shift-select' for marking, use it for `windmove'
      sort-fold-case nil ; Do not ignore case when sorting
      standard-indent 2
      suggest-key-bindings t
      switch-to-buffer-preserve-window-point t
      use-dialog-box nil ; Do not use dialog boxes with mouse commands
      use-file-dialog nil
      vc-follow-symlinks t ; No need to ask
      ;; Disabling vc improves performance, the alternate option is '(Git) to show branch
      ;; information on the modeline
      vc-handled-backends '(Git)
      view-read-only t ; View mode for read-only buffers
      visible-bell nil
      x-gtk-use-system-tooltips nil ; Do not use system tooltips
      x-gtk-resize-child-frames 'resize-mode ; Always trigger an immediate resize of the child frame
      ;; Underline looks a bit better when drawn lower
      x-underline-at-descent-line t)

;; Changing buffer-local variables will only affect a single buffer. `setq-default' changes the
;; buffer-local variable's default value.
(setq-default fill-column sb/fill-column
              ;; electric-indent-inhibit nil
              indent-tabs-mode nil ; Spaces instead of tabs
              indicate-empty-lines nil
              major-mode 'text-mode ; Major mode to use for files that do no specify a major mode
              ;; TAB first tries to indent the current line, and if the line was already indented,
              ;; then try to complete the thing at point.
              tab-always-indent 'complete
              tab-width 4
              truncate-lines nil)

;; https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
(setq-default bidi-inhibit-bpa nil ; Disabling BPA makes redisplay faster
              bidi-paragraph-direction 'left-to-right)

(dolist (exts '(".aux"
                ".class"
                ".dll"
                ".elc"
                ".exe"
                ".fls"
                ".lof"
                ".o"
                ".pyc"
                ".rel"
                ".rip"
                ".so"
                ".toc"))
  (add-to-list 'completion-ignored-extensions exts))

;; Activate utf-8, these are needed (may not be all) for icons to work well in TUI
(set-language-environment    "UTF-8")
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(set-default-coding-systems  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system  'utf-8)

;; Scroll settings from Doom Emacs
(setq scroll-margin 5 ; Add margin lines when scrolling vertically to have a sense of continuity
      ;; Emacs spends too much effort recentering the screen if you scroll the cursor more than N
      ;; lines past window edges, where N is the setting of `scroll-conservatively'. This is
      ;; especially slow in larger files during large-scale scrolling commands. If kept over 100,
      ;; the window is never automatically recentered.
      scroll-conservatively 101
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines
      auto-window-vscroll nil
      mouse-wheel-follow-mouse 't ; Scroll window under mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      ;; Do not accelerate scrolling
      mouse-wheel-progressive-speed nil)

(fset 'display-startup-echo-area-message #'ignore)

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  ;; Type "y"/"n" instead of "yes"/"no"
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package autorevert ; Auto-refresh all buffers
  :straight nil
  :commands global-auto-revert-mode
  :diminish auto-revert-mode
  ;; :init (run-with-idle-timer 2 nil #'global-auto-revert-mode)
  :hook (after-init-hook . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 5 ; Faster (seconds) would mean less likely to use stale data
        ;; Emacs seems to hang with auto-revert and Tramp, disabling this should be okay if we only
        ;; use Emacs. Enabling auto-revert is always safe.
        auto-revert-remote-files t
        auto-revert-verbose nil
        ;; Revert only file-visiting buffers, set to non-nil value to revert dired buffers if the
        ;; contents of the directory changes
        global-auto-revert-non-file-buffers t))

;; Revert all (e.g., PDF) files without asking
(setq revert-without-query '("\\.*"))

;; We may open a file immediately after starting Emacs, hence we are using a hook instead of a
;; timer.
(use-package saveplace ; Remember cursor position in files
  :straight nil
  :hook (after-init-hook . save-place-mode))

(use-package savehist ; Save minibuffer history across sessions
  :straight nil
  :commands savehist-mode
  ;; :init (run-with-idle-timer 2 nil #'savehist-mode)
  :hook (after-init-hook . savehist-mode)
  :custom
  (savehist-additional-variables '(extended-command-history
                                   kill-ring
                                   regexp-search-ring
                                   search-ring)))

(use-package uniquify
  :straight nil
  :init
  (setq uniquify-after-kill-buffer-p t
        uniquify-buffer-name-style   'forward
        uniquify-ignore-buffers-re   "^\\*"
        uniquify-separator           "/"
        uniquify-strip-common-suffix t))

;; We open the "*scratch*" buffer in `text-mode', so enabling `abbrev-mode' early is useful
(use-package abbrev
  :straight nil
  :diminish
  :hook (after-init-hook . abbrev-mode)
  :custom
  ;; The "abbrev-defs" file is under version control
  (abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory))
  (save-abbrevs 'silently))

;; This puts the buffer in read-only mode and disables font locking, revert with "C-c C-c"
(use-package so-long
  :straight nil
  ;; :init (run-with-idle-timer 2 nil #'global-so-long-mode)
  :hook (after-init-hook . global-so-long-mode))

;; Edit remote file: "/method:user@host#port:filename". Shortcut "/ssh::" will connect to default
;; "user@host#port".
;; Edit local file with sudo: "C-x C-f /sudo::/etc/hosts".
;; Open a remote file with ssh + sudo: "C-x C-f /ssh:host|sudo:root:/etc/passwd".
;; Multihop syntax: "C-x C-f /ssh:bird@bastion|ssh:you@remotehost:/path"
;; Multihop with sudo: "C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file"
;; Multihop with sudo with custom user: "C-x C-f
;; /ssh:you@remotehost|sudo:them@remotehost:/path/to/file"

;; https://helpdeskheadesk.net/help-desk-head-desk/2021-05-19/ Use bookmarks to speed up remote file
;; access: upon visiting a location with TRAMP, save it as a bookmark with `bookmark-set' ("C-x r
;; m"). To revisit that bookmark, use `bookmark-jump' ("C-x r b") or `bookmark-bmenu-list' ("C-x r
;; l"). Rename the bookmarked location in `bookmark-bmenu-mode' with `R'.
(use-package tramp
  :straight nil
  :defines tramp-ssh-controlmaster-options
  :config
  (setq tramp-default-user user-login-name
        ;; Tramp uses SSH when connecting and when viewing a directory, but it will use SCP to copy
        ;; files which is faster than SSH.
        ;; tramp-default-method "ssh"
        tramp-default-remote-shell "/usr/bin/bash"
        remote-file-name-inhibit-cache nil ; Remote files are not updated outside of Tramp
        ;; Disable default options, reuse SSH connections by reading "~/.ssh/config" control master
        ;; settings
        ;; https://emacs.stackexchange.com/questions/22306/working-with-tramp-mode-on-slow-connection-emacs-does-network-trip-when-i-start
        ;; https://puppet.com/blog/speed-up-ssh-by-reusing-connections
        tramp-ssh-controlmaster-options ""
        tramp-verbose 1
        ;; Disable version control for remote files to improve performance
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                     vc-ignore-dir-regexp tramp-file-name-regexp))

  (defalias 'exit-tramp 'tramp-cleanup-all-buffers)

  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; https://www.gnu.org/software/tramp/
  (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  :bind ("C-S-q" . tramp-cleanup-all-buffers))

(use-package imenu
  :straight nil
  :after (:any markdown-mode yaml-mode prog-mode)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items 1000)
  ;; `t' will use a popup menu rather than a minibuffer prompt, `on-mouse' might be useful with
  ;; mouse support enabled
  (imenu-use-popup-menu nil)
  ;; `nil' implies no sorting and will list by position in the buffer
  (imenu-sort-function nil))

(use-package recentf
  :commands (recentf-mode recentf-add-file recentf-save-file
                          recentf-save-list
                          recentf-apply-filename-handlers
                          recentf-cleanup)
  :config
  (setq recentf-auto-cleanup 'never ; Do not stat remote files
        ;; Check the regex with `re-builder', use `recentf-cleanup' to update the list
        recentf-exclude '("[/\\]elpa/"
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
                          "/.autosaves/"
                          ".*/TAGS\\'"
                          "*.cache"
                          ".*/treemacs/persist.org")
        ;; https://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
        ;; Keep remote file without testing if they still exist
        recentf-keep '(file-remote-p file-readable-p)
        ;; Larger values help in lookup but takes more time to check if the files exist
        recentf-max-saved-items 100
        ;; Abbreviate the file name to make it easy to read the actual file name. Specifically,
        ;; `abbreviate-file-name' abbreviates the home directory to "~/" in the file list.
        recentf-filename-handlers (append '(abbreviate-file-name) recentf-filename-handlers))

  ;; Use the true file name and not the symlink name
  (dolist (exclude `(,(file-truename no-littering-etc-directory)
                     ,(file-truename no-littering-var-directory)))
    (add-to-list 'recentf-exclude exclude))

  (add-to-list 'recentf-exclude `(recentf-expand-file-name ,(straight--emacs-dir "straight")))

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
  ;; after idling for 30 seconds.
  (run-with-idle-timer 30 t #'recentf-save-list)

  ;; Adding many functions to `kill-emacs-hook' slows down Emacs exit, hence we are only using idle
  ;; timers.
  (run-with-idle-timer 60 t #'recentf-cleanup)
  :hook (after-init-hook . recentf-mode))

(use-package init-open-recentf
  :after recentf
  :straight t
  :demand t
  :disabled t
  :config (init-open-recentf))

(use-package whitespace
  :straight nil
  :disabled t
  :commands (whitespace-mode global-whitespace-mode
                             whitespace-buffer whitespace-cleanup
                             whitespace-turn-off)
  ;; :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  :hook (markdown-mode-hook . whitespace-mode)
  :config
  (setq show-trailing-whitespace t
        whitespace-line-column sb/fill-column
        whitespace-style '(face ; Visualize using faces
                           lines-tail
                           trailing ; Trailing whitespace
                           ;; tab-mark ; Mark any tabs
                           ;; empty ; Empty lines at beginning or end of buffer
                           ;; lines ; Lines that extend beyond `whitespace-line-column'
                           ;; indentation ; Wrong kind of indentation (tab when spaces and vice versa)
                           ;; space-before-tab space-after-tab ; Mixture of space and tab on the same line
                           )))

(use-package image-mode
  :straight nil
  :if (display-graphic-p)
  :commands image-get-display-property
  :mode "\\.svg$"
  :preface
  ;; http://emacs.stackexchange.com/a/7693/289
  (defun sb/show-image-dimensions-in-mode-line ()
    (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
           (width (car image-dimensions))
           (height (cdr image-dimensions)))
      (setq mode-line-buffer-identification
            (format "%s %dx%d" (propertized-buffer-identification "%12b") width height))))
  :custom
  ;;  Enable converting external formats (i.e., webp) to internal ones.
  (image-use-external-converter t)
  :hook (image-mode-hook . sb/show-image-dimensions-in-mode-line))

;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(progn
  (defun sb/auto-save-wrapper (save-fn &rest args)
    "Hide 'Auto-saving...done' messages by calling the method.
  SAVE-FN with non-nil ARGS."
    (ignore args)
    (apply save-fn '(t)))

  (advice-add 'do-auto-save :around #'sb/auto-save-wrapper))

;; Disable the unhelpful modes, ignore disabling for modes I am not bothered with
(dolist (mode '(tooltip-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Enable the following modes
(dolist (mode '(auto-save-visited-mode ; Autosave file-visiting buffers at idle time intervals
                blink-cursor-mode
                column-number-mode
                delete-selection-mode ; Typing with the mark active will overwrite the marked region
                ;; Use soft wraps, wrap lines without the ugly continuation marks
                global-visual-line-mode
                size-indication-mode))
  (when (fboundp mode)
    (funcall mode 1)))

;; Use "emacsclient -c -nw" to start a new frame.
(use-package server
  :straight nil
  :disabled t
  :unless (string-equal "root" (getenv "USER")) ; Only start server if not root
  :commands server-running-p
  :init
  (unless (server-running-p)
    (server-start)))

(use-package warnings
  :straight nil
  :init
  ;; This is not a great idea, but I expect most warnings will arise from third-party packages.
  (setq warning-minimum-level :emergency))

(defun sb/inhibit-message-call-orig-fun (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN, forward ARGS."
  (let ((inhibit-message t))
    (apply orig-fun args)))

;; Hide the "Wrote to recentf" message
(advice-add 'recentf-save-list :around #'sb/inhibit-message-call-orig-fun)
;; Hide the "Cleaning up the recentf list...done" message
(advice-add 'recentf-cleanup   :around #'sb/inhibit-message-call-orig-fun)

;; Hide the "Wrote ..." message
(advice-add 'write-region :around #'sb/inhibit-message-call-orig-fun)

(provide 'init-core)
