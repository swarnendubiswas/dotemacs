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
(defvar sb/extras-directory)
(defvar no-littering-etc-directory)
(defvar no-littering-var-directory)

(declare-function straight--emacs-dir "straight")

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
      completion-cycle-threshold 3 ; TAB cycle if there are only few candidates
      completion-ignore-case t ; Ignore case when completing
      confirm-kill-emacs nil
      confirm-kill-processes nil ; Prevent "Active processes exist" when you quit Emacs
      confirm-nonexistent-file-or-buffer t
      create-lockfiles nil
      custom-safe-themes t
      delete-by-moving-to-trash t ; Use system trash to deal with mistakes while deleting
      ;; enable-local-variables :all ; Avoid "defvar" warnings
      echo-keystrokes 0.5 ; Show current key-sequence in minibuffer
      ;; Allow doing a command that requires candidate-selection when you are already in the middle
      ;; of candidate-selection. But keeping track of the minibuffer nesting is difficult.
      enable-recursive-minibuffers t
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
      ;; If you have something on the system clipboard, and then kill something in Emacs, then by
      ;; default whatever you had on the system clipboard is gone and there is no way to get it
      ;; back. Setting the following option makes it so that when you kill something in Emacs,
      ;; whatever was previously on the system clipboard is pushed into the kill ring. This way, you
      ;; can paste it with `yank-pop'.
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
      window-combination-resize t ; Resize windows proportionally
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
(setq-default cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
              fill-column sb/fill-column
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

(dolist (exts '(".dll"
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

;; Activate utf-8, these are needed (may not be all) for icons to work well in TUI
(set-language-environment    "UTF-8")
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(prefer-coding-system        'utf-8) ; Add utf-8 at the front for automatic detection
(set-default-coding-systems  'utf-8) ; Set default value of various coding systems
(set-keyboard-coding-system  'utf-8) ; Set coding system for keyboard input on TERMINAL
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system  'utf-8) ; Set coding system of terminal output

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
                column-number-mode
                delete-selection-mode ; Typing with the mark active will overwrite the marked region
                ;; Use soft wraps, wrap lines without the ugly continuation marks
                global-visual-line-mode
                size-indication-mode))
  (when (fboundp mode)
    (funcall mode 1)))

(use-package autorevert ; Auto-refresh all buffers
  :straight (:type built-in)
  :diminish auto-revert-mode
  :hook (after-init-hook . global-auto-revert-mode)
  :config
  (setq auto-revert-interval 5 ; Faster (seconds) would mean less likely to use stale data
        ;; Emacs seems to hang with auto-revert and Tramp, disabling this should be okay if we only
        ;; use Emacs. Enabling auto-revert is always safe.
        auto-revert-remote-files t
        auto-revert-verbose nil
        ;; Revert only file-visiting buffers, set to non-nil value to revert dired buffers if the
        ;; contents of the directory changes
        global-auto-revert-non-file-buffers t)

  ;; Revert all (e.g., PDF) files without asking
  (setq revert-without-query '("\\.*")))

;; We may open a file immediately after starting Emacs, hence we are using a hook instead of a
;; timer.
(use-package saveplace ; Remember cursor position in files
  :straight (:type built-in)
  :hook (after-init-hook . save-place-mode))

;; Save minibuffer history across sessions
(use-package savehist ; Save minibuffer history across sessions
  :straight (:type built-in)
  :commands savehist-mode
  :hook (after-init-hook . savehist-mode)
  :custom
  (savehist-additional-variables '(extended-command-history
                                   command-history
                                   bookmark-history
                                   file-name-history
                                   kill-ring
                                   search-ring
                                   regexp-search-ring)))

(use-package uniquify
  :straight (:type built-in)
  :init
  (setq uniquify-after-kill-buffer-p t
        uniquify-buffer-name-style   'forward
        uniquify-ignore-buffers-re   "^\\*"
        uniquify-separator           "/"
        uniquify-strip-common-suffix t))

;; We open the "*scratch*" buffer in `text-mode', so enabling `abbrev-mode' early is useful
(use-package abbrev
  :straight (:type built-in)
  :diminish
  :hook (after-init-hook . abbrev-mode)
  :custom
  ;; The "abbrev-defs" file is under version control
  (abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory))
  (save-abbrevs 'silently))

;; This puts the buffer in read-only mode and disables font locking, revert with "C-c C-c"
(use-package so-long
  :straight (:type built-in)
  :hook (after-init-hook . global-so-long-mode))

(use-package imenu
  :straight (:type built-in)
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
  :straight (:type built-in)
  :commands (recentf-add-file recentf-save-file
                              recentf-save-list
                              recentf-apply-filename-handlers
                              recentf-cleanup)
  :custom
  (recentf-auto-cleanup 'never "Do not stat remote files")
  ;; Check the regex with `re-builder', use `recentf-cleanup' to update the list
  (recentf-exclude '("[/\\]elpa/"
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
                     ".*/treemacs/persist.org"))
  ;; https://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
  ;; Keep remote file without testing if they still exist
  (recentf-keep '(file-remote-p file-readable-p))
  ;; Larger values help in lookup but takes more time to check if the files exist
  (recentf-max-saved-items 150)
  :config
  ;; Abbreviate the file name to make it easy to read the actual file name. Specifically,
  ;; `abbreviate-file-name' abbreviates the home directory to "~/" in the file list.
  (setq recentf-filename-handlers (append '(abbreviate-file-name) recentf-filename-handlers))

  ;; Use the true file name and not the symlink name
  (dolist (exclude `(,(file-truename no-littering-etc-directory)
                     ,(file-truename no-littering-var-directory)))
    (add-to-list 'recentf-exclude exclude))

  (when (bound-and-true-p sb/disable-package.el)
    (add-to-list 'recentf-exclude `,(recentf-expand-file-name (straight--emacs-dir "straight"))))

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
  ;; after idling for 30 seconds.
  (run-with-idle-timer 30 t #'recentf-save-list)

  ;; Adding many functions to `kill-emacs-hook' slows down Emacs exit, hence we are only using idle
  ;; timers.
  (run-with-idle-timer 60 t #'recentf-cleanup)
  :hook (after-init-hook . recentf-mode))

(use-package image-mode
  :straight (:type built-in)
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

;; Use "emacsclient -c -nw" to start a new frame.
;; https://andreyorst.gitlab.io/posts/2020-06-29-using-single-emacs-instance-to-edit-files/
(use-package server
  :straight (:type built-in)
  :unless (string-equal "root" (getenv "USER")) ; Only start server if not root
  :commands server-running-p
  :hook
  (after-init-hook . (lambda ()
                       ;; Only start server mode if not root
                       (unless (string-equal "root" (getenv "USER"))
                         (unless (and (fboundp 'server-running-p) (server-running-p))
                           (server-start)))))
  :config
  ;; Hide "When done with a buffer, type C-x 5" message
  (when (boundp 'server-client-instructions)
    (setq server-client-instructions nil)))

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

;; TODO: We use the keybindings for moving around windows in tmux
(use-package windmove ; "Shift + direction" arrows
  :straight (:type built-in)
  :commands windmove-default-keybindings
  :init (windmove-default-keybindings)
  :custom
  (windmove-wrap-around t "Wrap around at edges"))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-mode 1))

(use-package solar
  :straight (:type built-in)
  :custom
  (calendar-latitude 26.50)
  (calendar-location-name "Kanpur, UP, India")
  (calendar-longitude 80.23))

;; `text-mode' is the parent mode for `LaTeX-mode' and `org-mode', and so any hooks defined will
;; also get run for all modes derived from a basic mode such as `text-mode'.

;; Enabling `autofill-mode' makes it difficult to include long instructions verbatim, since they get
;; wrapped around automatically.
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
;; Auto-create missing directories on a file create and save
(defun sb/auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'sb/auto-create-missing-dirs)

(use-package compile
  :straight (:type built-in)
  :preface
  (defun sb/colorize-compilation-buffer ()
    "Colorize compile mode output."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-compile.el
  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  ;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
  (defun sb/bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings "
    (when (and
           (buffer-live-p buffer)
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (not
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))

  ;; https://github.com/malb/emacs.d/blob/master/malb.org
  (defun sb/compilation-exit-autoclose (status code msg)
    ;; If M-x compile exists with a 0
    (when (and (eq status 'exit) (zerop code))
      ;; and delete the *compilation* window
      (let ((compilation-window (get-buffer-window (get-buffer "*compilation*"))))

        (when (and (not (window-at-side-p compilation-window 'top))
                   (window-at-side-p compilation-window 'left)
                   (window-at-side-p compilation-window 'right))
          (delete-window compilation-window))))
    ;; Always return the anticipated result of compilation-exit-message-function
    (cons msg code))
  :hook
  ((compilation-filter-hook . sanityinc/colourise-compilation-buffer)
   ;; (compilation-filter-hook . sb/colorize-compilation-buffer)
   ;; (compilation-finish-functions . sb/bury-compile-buffer-if-successful)
   )
  :custom
  (compilation-always-kill t "Kill a compilation process before starting a new one")
  (compilation-ask-about-save nil "Save all modified buffers without asking")
  (compilation-exit-message-function #'sb/compilation-exit-autoclose)
  ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first
  ;; error.
  (compilation-scroll-output 'first-error))

;; Enable commands that are disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init-core)

;;; init-core.el ends here
