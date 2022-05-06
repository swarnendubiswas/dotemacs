;;; init-core.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar apropos-do-all)
(defvar bookmark-save-flag)
(defvar compilation-always-kill)
(defvar compilation-scroll-output)
(defvar sort-fold-case)
(defvar help-enable-symbol-autoload)
(defvar sb/fill-column)
(defvar sb/EMACS28+)
(defvar warning-minimum-level)

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
      ;; ISSUE: There is a known bug with Emacs upstream.
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=52292
      find-file-visit-truename nil ; Show true name, useful in case of symlinks
      frame-title-format (list '(buffer-file-name "%f" "%b") " - " invocation-name)
      help-enable-symbol-autoload t
      help-window-select t ; Makes it easy to close the window
      history-delete-duplicates t
      history-length 50 ; Reduce the state that is to be read
      indicate-buffer-boundaries nil
      kill-do-not-save-duplicates t
      kill-whole-line t ; TODO: What is the utility of this variable?
      make-backup-files nil ; Stop making backup `~' files
      message-log-max 5000
      ;; mouse-drag-copy-region nil ; Mouse is disabled
      ;; mouse-yank-at-point t ; Yank at point with mouse instead of at click
      read-buffer-completion-ignore-case t ; Ignore case when reading a buffer name
      read-file-name-completion-ignore-case t ; Ignore case when reading a file name
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
      view-read-only t ; View mode for read-only buffers
      visible-bell nil
      ;; This is not a great idea, but I expect most warnings will arise from third-party packages.
      warning-minimum-level :emergency
      x-gtk-use-system-tooltips nil ; Do not use system tooltips
      x-gtk-resize-child-frames 'resize-mode ; Always trigger an immediate resize of the child frame
      ;; Underline looks a bit better when drawn lower
      x-underline-at-descent-line t)

(when sb/EMACS28+
  (setq completions-detailed t
        next-error-message-highlight t
        read-minibuffer-restore-windows t))

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

(progn
  (if (boundp 'use-short-answers)
      (setq use-short-answers t)
    ;; Type "y"/"n" instead of "yes"/"no"
    (fset 'yes-or-no-p 'y-or-n-p))

  ;; Make RETURN key act the same way as "y" key for "y-or-n" prompts.
  (define-key y-or-n-p-map [return] 'act))

(when (bound-and-true-p enable-recursive-minibuffers)
  (minibuffer-depth-indicate-mode 1))

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

;; Auto-refresh all buffers
(progn
  (unless (fboundp 'global-auto-revert-mode)
    (autoload #'global-auto-revert-mode "autorevert" nil t))

  ;; (run-with-idle-timer 2 nil #'global-auto-revert-mode)
  (add-hook 'after-init-hook #'global-auto-revert-mode)

  (with-eval-after-load "autorevert"
    (defvar auto-revert-interval)
    (defvar auto-revert-remote-files)
    (defvar auto-revert-use-notify)
    (defvar auto-revert-verbose)
    (defvar global-auto-revert-non-file-buffers)
    (defvar auto-revert-check-vc-info)

    (setq auto-revert-interval 5 ; Faster (seconds) would mean less likely to use stale data
          ;; Emacs seems to hang with auto-revert and Tramp, disabling this should be okay if we only
          ;; use Emacs. Enabling auto-revert is always safe.
          auto-revert-remote-files t
          auto-revert-verbose nil
          ;; Revert only file-visiting buffers, set to non-nil value to revert dired buffers if the
          ;; contents of the "main" directory changes
          global-auto-revert-non-file-buffers t)

    (diminish 'auto-revert-mode))

  ;; Revert all (e.g., PDF) files without asking
  (setq revert-without-query '("\\.*")))

(progn
  (unless (fboundp 'save-place-mode)
    (autoload #'save-place-mode "saveplace" nil t))

  ;; We may open a file immediately after starting Emacs, hence we are using a hook instead of a
  ;; timer.
  (add-hook 'after-init-hook #'save-place-mode))


;; Save minibuffer history across sessions
(progn
  (unless (fboundp 'savehist-mode)
    (autoload #'savehist-mode "savehist" nil t))

  ;; (run-with-idle-timer 3 nil #'savehist-mode)
  (add-hook 'after-init-hook #'savehist-mode)

  (with-eval-after-load "savehist"
    (defvar savehist-additional-variables)
    (defvar savehist-file)
    (defvar savehist-save-minibuffer-history)

    (setq savehist-additional-variables '(extended-command-history
                                          kill-ring
                                          search-ring
                                          regexp-search-ring)
          savehist-save-minibuffer-history t)))

(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator "/"
      uniquify-strip-common-suffix t)

;; We open the "*scratch*" buffer in `text-mode', so enabling `abbrev-mode' early is useful
(progn
  (unless (fboundp 'abbrev-mode)
    (autoload #'abbrev-mode "abbrev" nil t))

  (add-hook 'after-init-hook #'abbrev-mode)

  (with-eval-after-load "abbrev"
    (setq abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory)
          save-abbrevs 'silently)

    (diminish 'abbrev-mode)))

(progn
  ;; This puts the buffer in read-only mode and disables font locking, revert with "C-c C-c"
  (unless (fboundp 'global-so-long-mode)
    (autoload #'global-so-long-mode "so-long" nil t))

  ;; (run-with-idle-timer 2 nil #'global-so-long-mode)
  (add-hook 'after-init-hook #'global-so-long-mode))

;; Edit remote file: "/method:user@host#port:filename". Shortcut "/ssh::" will connect to default
;; "user@host#port".
;; Edit local file with sudo: "C-x C-f /sudo::/etc/hosts".
;; Open a remote file with ssh + sudo: "C-x C-f /ssh:host|sudo:root:/etc/passwd".
;; Multihop syntax: "C-x C-f /ssh:bird@bastion|ssh:you@remotehost:/path"
;; Multihop with sudo: "C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file"
;; Multihop with sudo with custom user: "C-x C-f
;; /ssh:you@remotehost|sudo:them@remotehost:/path/to/file"

;; Use bookmarks to speed up remote file access: upon visiting a location with TRAMP, save it as a
;; bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark, use `bookmark-jump' ("C-x r
;; b") or `bookmark-bmenu-list' ("C-x r l"). Rename the bookmarked location in `bookmark-bmenu-mode'
;; with `R'.
;; https://helpdeskheadesk.net/help-desk-head-desk/2021-05-19/

(defvar tramp-default-user)
(defvar tramp-default-remote-shell)
(defvar tramp-verbose)
(defvar tramp-remote-path)
(defvar tramp-ssh-controlmaster-options)

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

(with-eval-after-load "tramp"
  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; https://www.gnu.org/software/tramp/
(setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))

(declare-function tramp-cleanup-connection "tramp")
(bind-key "C-S-q" #'tramp-cleanup-connection)

(with-eval-after-load "imenu"
  (defvar imenu-auto-rescan)
  (defvar imenu-max-items)
  (defvar imenu-use-popup-menu)

  (setq imenu-auto-rescan t
        imenu-max-items 1000
        ;; `t' will use a popup menu rather than a minibuffer prompt, `on-mouse' might be useful with
        ;; mouse support enabled
        imenu-use-popup-menu nil
        ;; `nil' implies no sorting and will list by position in the buffer
        imenu-sort-function nil))

(progn
  (declare-function recentf-save-file "recentf")
  (declare-function recentf-save-list "recentf")
  (declare-function recentf-cleanup "recentf")

  (unless (fboundp 'recentf-mode)
    (autoload #'recentf-mode "recentf" nil t))
  (unless (fboundp 'recentf-add-file)
    (autoload #'recentf-add-file "recentf" nil t))
  (unless (fboundp 'recentf-save-file)
    (autoload #'recentf-save-file "recentf" nil t))
  (unless (fboundp 'recentf-cleanup)
    (autoload #'recentf-cleanup "recentf" nil t))
  (unless (fboundp 'recentf-save-list)
    (autoload #'recentf-save-list "recentf" nil t))
  (unless (fboundp 'recentf-apply-filename-handlers)
    (autoload #'recentf-apply-filename-handlers "recentf" nil t))

  ;; Load immediately after start since I use it often
  (add-hook 'after-init-hook #'recentf-mode)

  (with-eval-after-load "recentf"
    (defvar recentf-auto-cleanup)
    (defvar recentf-exclude)
    (defvar recentf-max-saved-items)
    (defvar recentf-menu-filter)
    (defvar recentf-save-file)
    (defvar recentf-keep)
    (defvar recentf-filename-handlers)

    (setq recentf-auto-cleanup 'never ; Do not stat remote files
          ;; Check regex with `re-builder', use `recentf-cleanup' to update the list
          recentf-exclude '("[/\\]elpa/"
                            "[/\\]\\.git/"
                            ".*\\.gz\\'"
                            ".*\\.xz\\'"
                            ".*\\.zip\\'"
                            "[/\\]archive-contents\\'"
                            "[/\\]\\.loaddefs\\.el\\'"
                            "[/\\]tmp/.*"
                            ".*/recentf\\'"
                            ".*/recentf-save.el\\'"
                            ".*/init.el\\'"
                            "~$"
                            "/.autosaves/"
                            ".*/TAGS\\'"
                            "*.cache"
                            ".*/treemacs/persist.org")
          ;; https://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
          ;; Keep remote file without testing if they still exist
          recentf-keep '(file-remote-p file-readable-p)
          recentf-max-saved-items 100 ; Larger values help in lookup
          ;; recentf-menu-filter 'recentf-sort-descending
          recentf-filename-handlers (append '(abbreviate-file-name)
                                            recentf-filename-handlers))

    ;; Use the true file name and not the symlink name
    (dolist (exclude `(,(file-truename no-littering-etc-directory)
                       ,(file-truename no-littering-var-directory)))
      (add-to-list 'recentf-exclude exclude))

    (when (bound-and-true-p sb/disable-package.el)
      (add-to-list 'recentf-exclude `(recentf-expand-file-name ,(straight--emacs-dir "straight"))))

    ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
    ;; after idling for 30 seconds.
    (run-with-idle-timer 30 t #'recentf-save-list)

    ;; Adding many functions to `kill-emacs-hook' slows down Emacs exit, hence we are only using
    ;; idle timers.
    (run-with-idle-timer 60 t #'recentf-cleanup)))

(when nil
  (progn
    (declare-function whitespace-buffer "whitespace")
    (declare-function whitespace-turn-off "whitespace")

    (unless (fboundp 'whitespace-mode)
      (autoload #'whitespace-mode "whitespace" nil t))
    (unless (fboundp 'global-whitespace-mode)
      (autoload #'global-whitespace-mode "whitespace" nil t))
    (unless (fboundp 'whitespace-buffer)
      (autoload #'whitespace-buffer "whitespace" nil t))
    (unless (fboundp 'whitespace-cleanup)
      (autoload #'whitespace-cleanup "whitespace" nil t))
    (unless (fboundp 'whitespace-turn-off)
      (autoload #'whitespace-turn-off "whitespace" nil t))

    (add-hook 'markdown-mode-hook #'whitespace-mode)

    (with-eval-after-load "whitespace"
      (defvar whitespace-line-column)
      (defvar whitespace-style)

      (setq show-trailing-whitespace t
            whitespace-line-column sb/fill-column
            whitespace-style '(face ; Visualize using faces
                               lines-tail
                               trailing ; Trailing whitespace
                               ;; tab-mark ; Mark any tabs
                               ;; empty ; Empty lines at beginning or end of buffer
                               ;; lines ; Lines that extend beyond `whitespace-line-column'
                               ;; indentation ; Wrong indentation (tab when spaces and vice versa)
                               ;; space-before-tab ; Mixture of space and tab on the same line
                               ;; space-after-tab ; Mixture of space and tab on the same line
                               ))

      (diminish 'global-whitespace-mode)
      (diminish 'whitespace-mode)
      (diminish 'whitespace-newline-mode))))

(progn
  (eval-and-compile
    ;; http://emacs.stackexchange.com/a/7693/289
    (defun sb/show-image-dimensions-in-mode-line nil
      (let* ((image-dimensions (image-size (image-get-display-property) :pixels))
             (width (car image-dimensions))
             (height (cdr image-dimensions)))
        (setq mode-line-buffer-identification
              (format "%s %dx%d" (propertized-buffer-identification "%12b") width height)))))

  (when (display-graphic-p)
    (unless (fboundp 'image-mode)
      (autoload #'image-mode "image-mode" nil t))
    (unless (fboundp 'sb/show-image-dimensions-in-mode-line)
      (autoload #'sb/show-image-dimensions-in-mode-line "image-mode" nil t))
    (unless (fboundp 'image-get-display-property)
      (autoload #'image-get-display-property "image-mode" nil t))

    ;; Enable converting external formats (i.e., webp) to internal ones.
    (setq image-use-external-converter t)

    (add-hook 'image-mode-hook #'sb/show-image-dimensions-in-mode-line)

    (add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))))

;; Use "emacsclient -c -nw" to start a new frame.
;; https://andreyorst.gitlab.io/posts/2020-06-29-using-single-emacs-instance-to-edit-files/
(progn
  (when (fboundp 'server-running-p)
    (autoload #'server-running-p "server" nil t))

  (when nil
    (unless (string-equal "root" (getenv "USER")) ; Only start server mode if not root
      (unless (and (fboundp 'server-running-p) (server-running-p))
        (server-start)))))

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

;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(progn
  (defun sb/auto-save-wrapper (save-fn &rest args)
    "Hide 'Auto-saving...done' messages by calling the method.
  SAVE-FN with non-nil ARGS."
    (ignore args)
    (apply save-fn '(t)))

  (advice-add 'do-auto-save :around #'sb/auto-save-wrapper))

(provide 'init-core)

;;; init-core.el ends here
