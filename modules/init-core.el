;;; init-core.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar apropos-do-all)
(defvar bookmark-save-flag)
(defvar sort-fold-case)
(defvar help-enable-symbol-autoload)
(defvar sb/fill-column)
(defvar sb/EMACS28+)
(defvar warning-minimum-level)
(defvar sb/extras-directory)
(defvar no-littering-etc-directory)
(defvar no-littering-var-directory)

(setq
  ad-redefinition-action 'accept ; Turn off warnings due to redefinitions
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
  blink-matching-paren t
  bookmark-save-flag 1 ; Save bookmark after every bookmark edit and also when Emacs is killed
  case-fold-search t ; Searches and matches should ignore case
  ;; Useful in `prog-mode'
  comment-auto-fill-only-comments t
  confirm-kill-emacs nil
  confirm-kill-processes nil ; Prevent "Active processes exist" when you quit Emacs
  confirm-nonexistent-file-or-buffer t
  create-lockfiles nil
  custom-safe-themes t
  delete-by-moving-to-trash t ; Use system trash to deal with mistakes while deleting
  ;; enable-local-variables :all ; Avoid "defvar" warnings
  echo-keystrokes 0.1 ; Show current key-sequence in minibuffer
  ;; Allow doing a command that requires candidate-selection when you are already in the middle
  ;; of candidate-selection. But keeping track of the minibuffer nesting is difficult.
  enable-recursive-minibuffers t
  ;; The Emacs documentation warns about performance slowdowns with enabling remote directory
  ;; variables. I edit remote files mostly via TUI+SSH instead of Tramp.
  enable-remote-dir-locals nil
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
  kill-do-not-save-duplicates t ; Do not save duplicates to kill ring
  kill-whole-line t ; TODO: What is the utility of this variable?
  make-backup-files nil ; Stop making backup `~' files
  message-log-max 5000
  ;; mouse-drag-copy-region nil ; Mouse is disabled
  ;; mouse-yank-at-point t ; Yank at point with mouse instead of at click
  read-process-output-max (* 5 1024 1024) ; 5 MB, LSP suggests increasing it
  remote-file-name-inhibit-locks t
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
  switch-to-buffer-preserve-window-point t
  use-dialog-box nil ; Do not use dialog boxes with mouse commands
  use-file-dialog nil
  view-read-only t ; View mode for read-only buffers
  visible-bell nil
  ;; This is not a great idea, but I expect most warnings will arise from third-party packages.
  warning-minimum-level
  :error
  window-combination-resize t ; Resize windows proportionally
  x-gtk-use-system-tooltips nil ; Do not use system tooltips
  x-gtk-resize-child-frames 'resize-mode ; Always trigger an immediate resize of the child frame
  ;; Underline looks a bit better when drawn lower
  x-underline-at-descent-line t)

(when sb/EMACS28+
  (setq
    next-error-message-highlight t
    read-minibuffer-restore-windows t)

  ;; Hide commands in "M-x" in Emacs 28 which do not work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(when (boundp 'help-window-keep-selected)
  (setq help-window-keep-selected t))

(when (boundp 'find-sibling-rules)
  (setq find-sibling-rules
    '
    (("\\([^/]+\\)\\.c\\'" "\\1.h")
      ("\\([^/]+\\)\\.cpp\\'" "\\1.h")
      ("\\([^/]+\\)\\.h\\'" "\\1.c")
      ("\\([^/]+\\)\\.hpp\\'" "\\1.cpp"))))

(when (symbol-value 'sb/IS-WINDOWS)
  (setq w32-get-true-file-attributes nil))

;; Changing buffer-local variables will only affect a single buffer. `setq-default' changes the
;; buffer-local variable's default value.
(setq-default
  cursor-in-non-selected-windows nil ; Hide the cursor in inactive windows
  fill-column sb/fill-column
  ;; electric-indent-inhibit nil
  indent-tabs-mode nil ; Spaces instead of tabs
  indicate-empty-lines nil
  ;; Major mode to use for files that do no specify a major mode. Setting this to
  ;; `text-mode' causes LSP to run for unrelated files.
  ;; major-mode 'text-mode
  ;; TAB first tries to indent the current line, and if the line was already indented,
  ;; then try to complete the thing at point.
  tab-always-indent 'complete
  tab-width 4
  truncate-lines nil)

;; https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
(setq-default
  bidi-inhibit-bpa nil ; Disabling BPA makes redisplay faster
  bidi-paragraph-direction 'left-to-right)

;; Activate utf-8, these are needed (may not be all) for icons to work well in TUI
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8) ; Add utf-8 at the front for automatic detection
(set-default-coding-systems 'utf-8) ; Set default value of various coding systems
(set-keyboard-coding-system 'utf-8) ; Set coding system for keyboard input on TERMINAL
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8) ; Set coding system of terminal output

;; Scroll settings from Doom Emacs
(setq
  scroll-margin 5 ; Add margin lines when scrolling vertically to have a sense of continuity
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

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-mode 1))

(fset 'display-startup-echo-area-message #'ignore)

;; Type "y"/"n" instead of "yes"/"no"
(progn
  (if (boundp 'use-short-answers)
    (setq use-short-answers t)
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
(dolist
  (mode
    '
    (auto-save-visited-mode ; Autosave file-visiting buffers at idle time intervals
      column-number-mode
      delete-selection-mode ; Typing with the mark active will overwrite the marked region
      ;; TODO: This package is probably causing the jumping behavior with corfu-terminal-mode.
      global-visual-line-mode ; Use soft wraps, wrap lines without the ugly continuation marks
      size-indication-mode))
  (when (fboundp mode)
    (funcall mode 1)))

(diminish 'visual-line-mode)

(use-package
  autorevert ; Auto-refresh all buffers
  :straight (:type built-in)
  :hook (emacs-startup-hook . global-auto-revert-mode)
  :custom (auto-revert-interval 5 "Faster (seconds) would mean less likely to use stale data")
  ;; Emacs seems to hang with auto-revert and Tramp, disabling this should be okay if we only
  ;; use Emacs. Enabling auto-revert is always safe.
  (auto-revert-remote-files t)
  (auto-revert-verbose nil)
  ;; Revert only file-visiting buffers, set to non-nil value to revert dired buffers if the
  ;; contents of the directory changes
  (global-auto-revert-non-file-buffers t)
  (revert-without-query '("\\.*") "Revert all (e.g., PDF) files without asking")
  :diminish auto-revert-mode)

;; We may open a file immediately after starting Emacs, hence we are using a hook instead of a
;; timer.
(use-package
  saveplace ; Remember cursor position in files
  :straight (:type built-in)
  :hook (emacs-startup-hook . save-place-mode))

(use-package
  savehist ; Save minibuffer history across sessions
  :straight (:type built-in)
  :hook (emacs-startup-hook . savehist-mode)
  :custom
  (savehist-additional-variables
    '
    (extended-command-history
      command-history
      bookmark-history
      file-name-history
      kill-ring
      search-ring
      regexp-search-ring
      compile-command
      compile-history))
  (savehist-autosave-interval 60))

(use-package
  uniquify
  :straight (:type built-in)
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "/")
  (uniquify-strip-common-suffix t))

(use-package
  abbrev
  :straight (:type built-in)
  :hook (emacs-startup-hook . abbrev-mode)
  :custom
  (abbrev-file-name (expand-file-name "abbrev-defs" sb/extras-directory))
  (save-abbrevs 'silently)
  :diminish)

;; This puts the buffer in read-only mode and disables font locking, revert with "C-c C-c"
(use-package
  so-long
  :straight (:type built-in)
  :if sb/EMACS28+
  :hook (emacs-startup-hook . global-so-long-mode))

(use-package
  imenu
  :straight (:type built-in)
  :after (:any markdown-mode org-mode yaml-mode prog-mode)
  :custom (imenu-auto-rescan t) (imenu-max-items 1000)
  ;; `t' will use a popup menu rather than a minibuffer prompt, `on-mouse' might be useful with
  ;; mouse support enabled
  (imenu-use-popup-menu nil)
  ;; `nil' implies no sorting and will list by position in the buffer
  (imenu-sort-function nil))

(use-package
  recentf
  :straight (:type built-in)
  :functions straight--emacs-dir
  :hook (emacs-startup-hook . recentf-mode)
  :bind ("<f9>" . recentf-open-files)
  :custom (recentf-auto-cleanup 'never "Do not stat remote files")
  (recentf-exclude
    '
    ("[/\\]elpa/"
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
      "*[/\\]straight/repos/*"
      ".*/treemacs/persist.org"))
  ;; https://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
  ;; Keep remote file without testing if they still exist
  (recentf-keep '(file-remote-p file-readable-p))
  ;; Larger values help in lookup but takes more time to check if the files exist
  (recentf-max-saved-items 250)
  ;; Abbreviate the file name to make it easy to read the actual file name. Specifically,
  ;; `abbreviate-file-name' abbreviates the home directory to "~/" in the file list.
  ;; (recentf-filename-handlers '(abbreviate-file-name))
  :config
  ;; Use the true file name and not the symlink name
  (dolist
    (exclude
      `
      (,(recentf-expand-file-name no-littering-etc-directory)
        ,(recentf-expand-file-name no-littering-var-directory)))
    (add-to-list 'recentf-exclude exclude))

  (when (bound-and-true-p sb/disable-package.el)
    (add-to-list 'recentf-exclude `,(recentf-expand-file-name (straight--emacs-dir "straight"))))

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
  ;; after idling for a few seconds.
  (run-with-idle-timer 30 t #'recentf-save-list)

  ;; Adding many functions to `kill-emacs-hook' slows down Emacs exit, hence we are only using idle
  ;; timers.
  (run-with-idle-timer 60 t #'recentf-cleanup))

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

;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(progn
  (defun sb/auto-save-wrapper (save-fn &rest args)
    "Hide 'Auto-saving...done' messages by calling the method.
  SAVE-FN with non-nil ARGS."
    (ignore args)
    (apply save-fn '(t)))

  (advice-add 'do-auto-save :around #'sb/auto-save-wrapper))

;; NOTE: We use the "Shift+direction" keybindings for moving around windows in tmux which is okay
;; since I do not split Emacs frames often.

;; (use-package windmove ; "Shift + direction" arrows
;;   :straight (:type built-in)
;;   :init (windmove-default-keybindings)
;;   :custom (windmove-wrap-around t "Wrap around at edges"))

;; (use-package solar
;;   :straight (:type built-in)
;;   :custom
;;   (calendar-latitude 26.50)
;;   (calendar-location-name "Kanpur, UP, India")
;;   (calendar-longitude 80.23))

;; NOTE: `text-mode' is the parent mode for `LaTeX-mode' and `org-mode', and so any hooks defined
;; will also get run for all modes derived from a basic mode such as `text-mode'.

;; Enabling `autofill-mode' makes it difficult to include long instructions verbatim, since they get
;; wrapped around automatically.

;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Not a library/file, so `eval-after-load' does not work
(diminish 'auto-fill-function)

;; Enable commands that are disabled by default. I prefer upcase and downcase to work on the first
;; characters in a word instead of the character at point.

;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)

;; Binds "C-x C-f" to `find-file-at-point' which will continue to work like `find-file' unless a
;; prefix argument is given. Then it will find file at point.
(use-package
  ffap
  :straight (:type built-in)
  ;; Vertico does not provide intelligent file lookup, unlike `counsel'.
  :if (eq sb/minibuffer-completion 'vertico)
  :bind
  (("<f2>" . ffap)
    ([remap find-file] . find-file-at-point)
    ([remap find-file-read-only] . ffap-read-only)
    ([remap find-alternate-file] . ffap-alternate-file)
    ([remap dired] . dired-at-point)
    ([remap list-directory] . ffap-list-directory))
  :custom (ffap-machine-p-known 'reject "Do not ping things that look like domain names"))

;; (use-package doc-view
;;   :straight (:type built-in)
;;   :bind
;;   (:map
;;     doc-view-mode-map
;;     ("=" . doc-view-enlarge)
;;     ("-" . doc-view-shrink)
;;     ("n" . doc-view-next-page)
;;     ("p" . doc-view-previous-page)
;;     ("0" . doc-view-scale-reset)
;;     ("M-<" . doc-view-first-page)
;;     ("M->" . doc-view-last-page)
;;     ("C-l" . doc-view-goto-page))
;;   :custom
;;   (doc-view-continuous t)
;;   (doc-view-resolution 120))

;; Highlight and allow to open http links in strings and comments in buffers.
(use-package
  goto-addr
  :straight (:type built-in)
  :hook ((prog-mode-hook . goto-address-prog-mode) (text-mode-hook . goto-address-mode))
  :bind ("C-c RET" . goto-address-at-point))

(use-package
  ediff
  :straight (:type built-in)
  :defines (ediff-window-setup-function)
  :commands (ediff ediff3)
  :custom
  ;; Change default ediff style: do not start another frame with `ediff-setup-windows-default'
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split windows horizontally in ediff (instead of vertically)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  :config (ediff-set-diff-options 'ediff-diff-options "-w"))

;; https://emacs.stackexchange.com/questions/10983/remember-permission-to-execute-risky-local-variables/44604#44604
(advice-add 'risky-local-variable-p :override #'ignore)

;; To edit remote files, use "/method:user@host#port:filename". The shortcut "/ssh::" will connect
;; to default "user@host#port". To edit a local file with sudo, use "C-x C-f /sudo::/etc/hosts". To
;; open a remote file with ssh + sudo, use "C-x C-f /ssh:host|sudo:root:/etc/passwd".

;; Multihop syntax: "C-x C-f /ssh:bird@bastion|ssh:you@remotehost:/path"
;; Multihop with sudo: "C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file"
;; Multihop with sudo with custom user: "C-x C-f
;; /ssh:you@remotehost|sudo:them@remotehost:/path/to/file"

;; Sudo over ssh: "emacs -nw /ssh:user@172.16.42.1\|sudo:172.16.42.1:/etc/hosts"

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
(defvar sb/minibuffer-completion)

(use-package
  tramp
  :straight (:type built-in)
  :bind ("C-S-q" . tramp-cleanup-connection)
  :custom (tramp-default-user user-login-name)
  ;; Tramp uses SSH when connecting and when viewing a directory, but it will use SCP to copy
  ;; files which is faster than SSH.
  ;; (tramp-default-method "ssh")
  (tramp-default-remote-shell "/usr/bin/bash")
  (remote-file-name-inhibit-cache nil "Remote files are not updated outside of Tramp")
  ;; Disable default options, reuse SSH connections by reading "~/.ssh/config" control master
  ;; settings
  ;; https://emacs.stackexchange.com/questions/22306/working-with-tramp-mode-on-slow-connection-emacs-does-network-trip-when-i-start
  ;; https://puppet.com/blog/speed-up-ssh-by-reusing-connections
  (tramp-ssh-controlmaster-options "")
  (tramp-verbose 1)
  :config (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))

  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  ;; https://stackoverflow.com/questions/26630640/tramp-ignores-tramp-remote-path#26649558
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Recommended to connect with Bash
  (setenv "SHELL" shell-file-name)

  ;; https://www.gnu.org/software/tramp/
  (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))

;; LATER: Can we shorten long Tramp file names? This does not work with Tramp.
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/data/swarnendu/" . "/vindhya/data/swarnendu/"))
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/home/swarnendu/" . "/vindhya/home/swarnendu/"))

;; (use-package password-cache
;;   :after tramp
;;   :custom (password-cache-expiry nil))

;; (use-package whitespace
;;   :hook
;;   (markdown-mode-hook
;;     .
;;     (lambda ()
;;       (setq
;;         show-trailing-whitespace t
;;         whitespace-style
;;         '
;;         (face ; Visualize using faces
;;           ;; tabs
;;           ;; spaces
;;           trailing ; Trailing whitespace
;;           ;; newline
;;           ;; tab-mark ; Mark any tabs
;;           ;; empty ; Empty lines at beginning or end of buffer
;;           ;; lines ; Lines that extend beyond `whitespace-line-column'
;;           ;; space-mark ; Wrong kind of indentation (e.g., tab when spaces)
;;           ;; space-before-tab ; Mixture of space and tab on the same line
;;           ;; space-after-tab ; Mixture of space and tab on the same line
;;           ;; empty
;;           ;; newline-mark
;;           missing-newline-at-eof))
;;       (whitespace-mode 1)))
;;   :custom (whitespace-line-column sb/fill-column)
;;   :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode))

;; "M-x delete-trailing-whitespace" deletes trailing lines. This is different from
;; `whitespace-cleanup-mode' since this is unconditional.
(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (setq delete-trailing-lines t)

  (add-hook 'write-file-functions #'delete-trailing-whitespace)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

;; When you call `find-file', you do not need to clear the existing file path before adding the new
;; one. Just start typing the whole path and Emacs will "shadow" the current one. For example, you
;; are at "~/Documents/notes/file.txt" and you want to go to "~/.emacs.d/init.el", type the latter
;; directly and Emacs will take you there.
(file-name-shadow-mode 1)

(provide 'init-core)

;;; init-core.el ends here
