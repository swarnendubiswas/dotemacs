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
  :type
  '(radio
    (const :tag "server" server) (const :tag "daemon" daemon) (const :tag "standalone" standalone))
  :group 'sb/emacs)

(defcustom sb/debug-init-file nil
  "Enable features to debug errors and performance bottlenecks."
  :type 'boolean
  :group 'sb/emacs)

;; A dark theme has better contrast and looks good with the TUI.
(defcustom sb/theme 'doom-nord
  "Specify which Emacs theme to use, unless we are using `circadian'."
  :type
  '(radio
    (const :tag "doom-one" doom-one)
    (const :tag "doom-nord" doom-nord)
    (const :tag "modus-operandi" modus-operandi)
    (const :tag "modus-vivendi" modus-vivendi)
    (const :tag "nano-dark" nano-dark)
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme 'powerline
  "Specify the mode-line theme to use."
  :type
  '(radio
    (const :tag "powerline" powerline)
    (const :tag "doom-modeline" doom-modeline)
    (const :tag "nano-modeline" nano-modeline)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Corfu is easy to configure, integrates nicely with `orderless', and provides better completion
;; for elisp symbols. But `corfu-terminal-mode' has a potential rendering problem with TUI Emacs,
;; for completion popups appearing near the right edges. The completion entries wrap around, and
;; sometimes messes up the completion. Company works better with Windows and TUI Emacs, and has more
;; extensive LaTeX support than Corfu. `company-ispell' is configurable, and we can set up a custom
;; file containing completions with `company-dict'. However, `company-ispell' does not keep prefix
;; case when used as a grouped backend.
(defcustom sb/capf 'company
  "Choose the framework to use for completion at point."
  :type '(radio (const :tag "corfu" corfu) (const :tag "company" company) (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/window-split 'vertical
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type
  '(radio
    ;; Split into two windows one above the other (`split-window-below')
    (const :tag "vertical" vertical)
    ;; Split into two side-by-side windows (`split-window-right')
    (const :tag "horizontal" horizontal))
  :group 'sb/emacs)

;; Large values make reading difficult when the window is split side-by-side, 100 is also a stretch
;; for smaller screens.
(defcustom sb/fill-column 100
  "Column beyond which lines should not extend."
  :type 'number
  :group 'sb/emacs)

(defconst sb/user-home-directory (getenv "HOME")
  "User HOME directory.")

(defconst sb/user-config-directory
  (or (getenv "XDG_CONFIG_HOME") (expand-file-name ".config" sb/user-home-directory))
  "Path to user's local config store.")

(defconst sb/user-tmp-directory (expand-file-name "tmp" sb/user-home-directory)
  "User temp directory.
This location is used for temporary installations and files.")

;; Helper const variables

(defconst sb/EMACS27 (= emacs-major-version 27)
  "Non-nil if Emacs version is 27.")

(defconst sb/EMACS27+ (> emacs-major-version 26)
  "Non-nil if Emacs version is 27 and above.")

(defconst sb/EMACS28 (= emacs-major-version 28)
  "Non-nil if Emacs version is 28.")

(defconst sb/EMACS28+ (> emacs-major-version 27)
  "Non-nil if Emacs version is 28 and above.")

(defconst sb/EMACS29+ (> emacs-major-version 28)
  "Non-nil if Emacs version is 29 and above.")

(defconst sb/IS-LINUX (eq system-type 'gnu/linux)
  "Non-nil if the OS is GNU/Linux.")

(defconst sb/IS-WINDOWS (eq system-type 'windows-nt)
  "Non-nil if the OS is Windows.")

;; "straight.el" makes it easy to install packages from arbitrary sources like GitHub. Bootstrap
;; `straight.el'.
(setq
 straight-build-dir
 (format "build/%d%s%d" emacs-major-version version-separator emacs-minor-version)
 ;; Do not check packages on startup to reduce load time
 straight-check-for-modifications '(check-on-save find-when-checking)
 straight-use-package-by-default t
 ;; There is no need to download the whole Git history, and a single branch often suffices.
 straight-vc-git-default-clone-depth '(1 single-branch))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
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
(straight-use-package
 '(use-package :source
    melpa))

(cond
 ((eq sb/op-mode 'daemon)
  (setq
   use-package-always-demand t
   use-package-expand-minimally nil
   use-package-always-defer nil
   use-package-minimum-reported-time 0 ; Show everything
   use-package-verbose t))
 ((eq sb/op-mode 'standalone)
  (if (bound-and-true-p sb/debug-init-file)
      (progn
        (setq
         debug-on-error nil
         debug-on-event 'sigusr2
         use-package-compute-statistics t ; Use "M-x use-package-report" to see results
         use-package-verbose t
         use-package-minimum-reported-time 0 ; Show everything
         use-package-always-demand t))
    (progn
      (setq
       use-package-always-defer t
       ;; Disable error checks during macro expansion because the configuration just works
       use-package-expand-minimally t
       use-package-compute-statistics nil
       use-package-verbose nil)))))

;; Check "use-package-keywords.org" for a suggested order of `use-package' keywords.

(use-package diminish
  :demand t)

;; Package `bind-key' provides macros `bind-key', `bind-key*', and `unbind-key' which provides a
;; much prettier API for manipulating keymaps than `define-key' and `global-set-key'. "C-h b" lists
;; all the bindings available in a buffer, "C-h m" shows the keybindings for the major and the minor
;; modes.
(use-package bind-key
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package no-littering
  :demand t
  :custom (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(defcustom sb/custom-file (no-littering-expand-var-file-name "custom.el")
  "File to write Emacs customizations."
  :type 'string
  :group 'sb/emacs)

;; NOTE: Make a symlink to "private.el" in "$HOME/.emacs.d/etc".
(defcustom sb/private-file (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type 'string
  :group 'sb/emacs)

;; Get PATH with "(getenv "PATH")". Set PATH with "(setenv "PATH" (concat (getenv "PATH")
;; ":/home/swarnendu/bin"))".

;; These are alternative ways to manipulate the `exec-path'.
;; "(setq exec-path (append exec-path (expand-file-name "node_modules/.bin" sb/user-tmp-directory)))"
;; "(add-to-list 'exec-path (expand-file-name "node_modules/.bin" sb/user-tmp-directory))"

(use-package exec-path-from-shell
  :when (symbol-value 'sb/IS-LINUX)
  :init
  (setq
   exec-path-from-shell-check-startup-files nil
   exec-path-from-shell-variables '("PATH" "JAVA_HOME" "TERM" "PYTHONPATH" "LANG" "LC_CTYPE" "XAUTHORITY" "LSP_USE_PLISTS")
   exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package emacs
  :hook
  ((after-init . garbage-collect)
   (prog-mode
    .
    (lambda ()
      (auto-fill-mode 1) ; Autofill comments
      ;; Native from Emacs 27+, disable in TUI since the line characters also get copied.
      (when (or (display-graphic-p) (daemonp))
        (display-fill-column-indicator-mode 1)))))
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
  ;; Accelerate scrolling operations when non-nil. Only those portions of the buffer which are
  ;; actually going to be displayed get fontified.
  (fast-but-imprecise-scrolling t)
  (help-window-select t "Makes it easy to close the window")
  (history-delete-duplicates t)
  (read-process-output-max (* 5 1024 1024) "`lsp-mode' suggests increasing the value")
  (remote-file-name-inhibit-locks t)
  (ring-bell-function 'ignore "Disable beeping sound")
  ;; If you have something on the system clipboard, and then kill something in Emacs, then by
  ;; default whatever you had on the system clipboard is gone and there is no way to get it
  ;; back. Setting the following option makes it so that when you kill something in Emacs,
  ;; whatever was previously on the system clipboard is pushed into the kill ring. This way, you
  ;; can paste it with `yank-pop'.
  (save-interprogram-paste-before-kill t)
  (save-silently t "Error messages will still be printed")
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
  ;; This is not a great idea, but I expect most warnings will arise from third-party packages.
  (warning-minimum-level :error)
  (window-combination-resize t "Resize windows proportionally")
  (x-gtk-use-system-tooltips nil "Do not use system tooltips")
  (x-gtk-resize-child-frames 'resize-mode "Always trigger an immediate resize of the child frame")
  (x-underline-at-descent-line t "Underline looks a bit better when drawn lower")
  (read-buffer-completion-ignore-case t "Ignore case when reading a buffer name")
  (kill-do-not-save-duplicates t "Do not save duplicates to kill ring")
  (tags-add-tables nil)
  (tags-case-fold-search nil "case-sensitive")
  ;; Do not ask before rereading the "TAGS" files if they have changed
  (tags-revert-without-query t)
  ;; Changing height of the echo area is jarring, but limiting the height makes it difficult to see
  ;; useful information.
  ;; (resize-mini-windows nil)
  (max-mini-window-height 0.35)
  ;; Disable the warning "X and Y are the same file" in case of symlinks
  (find-file-suppress-same-file-warnings t)
  ;; ISSUE: There is a known bug with Emacs upstream.
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=52292
  (find-file-visit-truename nil "Show true name, useful in case of symlinks")
  (large-file-warning-threshold (* 500 1024 1024))
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
  (revert-without-query '("\\.*") "Revert all (e.g., PDF) files without asking")
  :config
  (dolist (exts
           '(".dll" ".exe" ".fdb_latexmk" ".fls" ".lof" ".pyc" ".rel" ".rip" ".synctex.gz" "TAGS"))
    (add-to-list 'completion-ignored-extensions exts))

  (when sb/EMACS28+
    (setq
     next-error-message-highlight t
     read-minibuffer-restore-windows t
     ;; Hide commands in "M-x" in Emacs 28 which do not work in the current mode.
     read-extended-command-predicate #'command-completion-default-include-p
     ;; Type "y/n" instead of "yes"/"no", although it is not recommended to prevent from wrong answers
     ;; being typed in a hurry.
     use-short-answers t))

  (when sb/EMACS29+
    (setq
     help-window-keep-selected t
     find-sibling-rules
     '(("\\([^/]+\\)\\.c\\'" "\\1.h")
       ("\\([^/]+\\)\\.cpp\\'" "\\1.h")
       ("\\([^/]+\\)\\.h\\'" "\\1.c")
       ("\\([^/]+\\)\\.hpp\\'" "\\1.cpp"))))

  (when sb/IS-WINDOWS
    (setq w32-get-true-file-attributes nil))

  ;; Disable unhelpful modes, ignore disabling for modes I am not bothered with
  (dolist (mode '(tooltip-mode))
    (when (fboundp mode)
      (funcall mode -1)))

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

  ;; vertical - Split the selected window into two windows (e.g., `split-window-below'), one above the
  ;; other.
  (when (eq sb/window-split 'vertical)
    (setq
     split-width-threshold nil
     split-height-threshold 0))

  ;; horizontal - Split the selected window into two side-by-side windows (e.g.,
  ;; `split-window-right').
  (when (eq sb/window-split 'horizontal)
    (setq
     split-height-threshold nil
     split-width-threshold 0))

  (setq
   ;; Scroll settings from Doom Emacs
   scroll-preserve-screen-position t
   scroll-margin 5 ; Add margin lines when scrolling vertically to have a sense of continuity
   ;; Emacs spends too much effort recentering the screen if you scroll the cursor more than N lines
   ;; past window edges, where N is the setting of `scroll-conservatively'. This is especially slow
   ;; in larger files during large-scale scrolling commands. If kept over 100, the window is never
   ;; automatically recentered.
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
  (diminish 'visual-line-mode))

;; Auto-refresh all buffers
(use-package autorevert
  :straight (:type built-in)
  :hook (emacs-startup . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  ;; Revert only file-visiting buffers, set to non-nil value to revert dired buffers if the contents
  ;; of the directory changes
  (global-auto-revert-non-file-buffers t)
  :diminish auto-revert-mode)

;; Remember cursor position in files
(use-package saveplace
  :straight (:type built-in)
  :hook (emacs-startup . save-place-mode))

;; Save minibuffer history across sessions
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

;; This puts the buffer in read-only mode and disables font locking, revert with "C-c C-c".
(use-package so-long
  :straight (:type built-in)
  :when sb/EMACS28+
  :hook (emacs-startup . global-so-long-mode))

(use-package imenu
  :straight (:type built-in)
  :after (:any markdown-mode org-mode yaml-mode yaml-ts-mode prog-mode)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items 1000)
  (imenu-use-popup-menu nil))

(use-package recentf
  :straight (:type built-in)
  :hook (emacs-startup . recentf-mode)
  :bind ("<f9>" . recentf-open-files)
  :custom
  (recentf-auto-cleanup 60)
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
     "/.autosaves/"
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
    (setq recentf-filename-handlers '(abbreviate-file-name)))

  ;; Use the true file name and not the symlink name
  (dolist (exclude
           `(,(recentf-expand-file-name no-littering-etc-directory)
             ,(recentf-expand-file-name no-littering-var-directory)
             ,(recentf-expand-file-name (straight--emacs-dir "straight"))))
    (add-to-list 'recentf-exclude exclude))

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
  ;; after idling for a few seconds.
  (run-with-idle-timer 30 t #'recentf-save-list))

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

(progn
  (defun sb/auto-save-wrapper (save-fn &rest args)
    "Hide 'Auto-saving...done' messages by calling the method.
  SAVE-FN with non-nil ARGS."
    (ignore args)
    (apply save-fn '(t)))

  (advice-add 'do-auto-save :around #'sb/auto-save-wrapper))

;; Use "Shift + direction" arrows for moving around windows. I also use the "Shift+direction"
;; keybindings for moving around windows in tmux which is okay because I do not split Emacs frames
;; often.
(use-package windmove
  :straight (:type built-in)
  :when (display-graphic-p)
  :init (windmove-default-keybindings)
  :custom (windmove-wrap-around t "Wrap around at edges"))

;; Consult does not provide intelligent file lookup. `ffap' binds "C-x C-f" to `find-file-at-point'
;; which will continue to work like `find-file' unless a prefix argument is given. Then it will find
;; file at point.
(use-package ffap
  :straight (:type built-in)
  :bind
  (("<f2>" . ffap)
   ([remap find-file] . find-file-at-point)
   ([remap find-file-read-only] . ffap-read-only)
   ([remap find-alternate-file] . ffap-alternate-file)
   ("C-x p o" . ff-find-other-file))
  :custom (ffap-machine-p-known 'reject "Do not ping things that look like domain names"))

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

;; Highlight and allow to open http links in strings and comments in buffers.
(use-package goto-addr
  :straight (:type built-in)
  :hook ((prog-mode . goto-address-prog-mode) (text-mode . goto-address-mode))
  :bind ("C-c RET" . goto-address-at-point))

(use-package ediff
  :straight (:type built-in)
  :hook (ediff-cleanup . (lambda () (ediff-janitor nil nil)))
  :custom
  ;; Put the control panel in the same frame as the diff windows
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split diffs side by side, i.e., show windows horizontally (instead of vertically)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-keep-variants nil "Kill file variants upon quitting an Ediff session")
  :config (ediff-set-diff-options 'ediff-diff-options "-w"))

(advice-add 'risky-local-variable-p :override #'ignore)

;; To edit remote files, use "/method:user@host#port:filename". The shortcut "/ssh::" will connect
;; to default "user@host#port". To edit a local file with sudo, use "C-x C-f /sudo::/etc/hosts". To
;; open a remote file with ssh + sudo, use "C-x C-f /ssh:host|sudo:root:/etc/passwd".
;; Multihop syntax: "C-x C-f /ssh:bird@bastion|ssh:you@remotehost:/path"
;; Multihop with sudo: "C-x C-f /ssh:you@remotehost|sudo:remotehost:/path/to/file"
;; Multihop with sudo with custom user: "C-x C-f
;; /ssh:you@remotehost|sudo:them@remotehost:/path/to/file"
;; Sudo over ssh: "emacs -nw /ssh:user@172.16.42.1\|sudo:172.16.42.1:/etc/hosts"

;; Use bookmarks to speed up remote file access: upon visiting a location with Tramp, save it as a
;; bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark, use `bookmark-jump' ("C-x r
;; b") or `bookmark-bmenu-list' ("C-x r l"). Rename the bookmarked location in `bookmark-bmenu-mode'
;; with `R'.
(use-package tramp
  :straight (:type built-in)
  :custom
  (remote-file-name-inhibit-cache nil "Remote files are not updated outside of Tramp")
  (tramp-verbose 1)
  :config
  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Recommended to connect with Bash
  (setenv "SHELL" shell-file-name)
  (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))

(use-package whitespace
  :hook
  (markdown-mode
   .
   (lambda ()
     (setq
      show-trailing-whitespace t
      whitespace-style
      '(face ; Visualize using faces
        ;; tabs
        ;; spaces
        trailing ; Trailing whitespace
        ;; newline
        ;; tab-mark ; Mark any tabs
        ;; empty ; Empty lines at beginning or end of buffer
        ;; lines ; Lines that extend beyond `whitespace-line-column'
        ;; space-mark ; Wrong kind of indentation (e.g., tab when spaces)
        ;; space-before-tab ; Mixture of space and tab on the same line
        ;; space-after-tab ; Mixture of space and tab on the same line
        ;; empty
        ;; newline-mark
        missing-newline-at-eof))
     (whitespace-mode 1)))
  :custom (whitespace-line-column sb/fill-column)
  :config (setq-default whitespace-action '(cleanup auto-cleanup))
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode))

(use-package ibuffer
  :straight (:type built-in)
  :hook (ibuffer . ibuffer-auto-mode)
  :bind (("C-x C-b" . ibuffer-jump) :map ibuffer-mode-map ("`" . ibuffer-switch-format))
  :custom
  (ibuffer-display-summary nil)
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-show-empty-filter-groups nil "Do not show empty groups if there are no buffers")
  :config (defalias 'list-buffers 'ibuffer))

;; Provides ibuffer filtering and sorting functions to group buffers by function or regexp applied
;; to `default-directory'. By default buffers are grouped by `project-current' or by
;; `default-directory'.
(use-package ibuffer-project
  :hook
  (ibuffer
   .
   (lambda ()
     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative))))
  :custom (ibuffer-project-use-cache t "Avoid calculating project root, use cache")
  :config
  ;; Remote buffers will be grouped by protocol and host
  (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote")))

;; Immediately respawn the *scratch* buffer when it is killed
(use-package immortal-scratch
  :hook (emacs-startup . immortal-scratch-mode))

;; Helps to make the data in the "*scratch*" buffer persistent
(use-package persistent-scratch
  :hook
  (emacs-startup
   .
   (lambda ()
     (ignore-errors
       (persistent-scratch-setup-default))))
  :config (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

;; Windows of temporary buffers are shown as a popup window, and we can close them by typing "C-g".
;; It is useful because it does not split frames.
(use-package popwin
  :hook (emacs-startup . popwin-mode)
  :config
  ;;   (push '("*Help*"              :noselect t)   popwin:special-display-config)
  ;;   (push '(compilation-mode      :noselect t)   popwin:special-display-config)
  ;;   (push '("*Compile-Log*"       :noselect t)   popwin:special-display-config)
  ;;   (push '("*manage-minor-mode*" :noselect t)   popwin:special-display-config)
  ;;   (push '("*Selection Ring:")                  popwin:special-display-config)
  ;;   (push '("*Flycheck checkers*" :noselect nil) popwin:special-display-config)
  ;;   (push '(flycheck-error-list-mode :noselect nil) popwin:special-display-config)
  ;;   (push '("*ripgrep-search*"    :noselect nil) popwin:special-display-config)
  ;;   (push '("^\*magit:.+\*$"      :noselect nil) popwin:special-display-config)
  ;;   (push '("*xref*"              :noselect nil) popwin:special-display-config)
  (push '(helpful-mode :noselect t :position bottom :height 20) popwin:special-display-config)
  ;;   (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
  ;;   (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
  ;;   (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  ;;   (add-to-list 'popwin:special-display-config '("*Apropos*"))
  ;;   (add-to-list 'popwin:special-display-config '("*Warnings*"))
  ;;   (add-to-list 'popwin:special-display-config '("*prettier errors*"))
  ;;   (add-to-list 'popwin:special-display-config '(deadgrep-mode :noselect nil))
  ;;   (add-to-list 'popwin:special-display-config '("*lsp session*"))
  ;;   (add-to-list 'popwin:special-display-config '("*rg*" :noselect nil))
  )

;; `ace-window' replaces `other-window' by assigning each window a short, unique label.
(use-package ace-window
  :bind (([remap other-window] . ace-window) ("M-o" . ace-window))
  :custom (aw-minibuffer-flag t)
  :config
  (add-to-list 'aw-ignored-buffers "*toc*")
  (ace-window-display-mode 1))

;; The keybinding will be hidden if we use Emacs with Tmux with its default prefix key, and we will
;; need to press twice.
(use-package ace-jump-buffer
  :bind ("C-b" . ace-jump-buffer)
  :custom
  (ajb-bs-configuration "files-and-scratch")
  (ajb-max-window-height 30)
  (ajb-sort-function 'bs--sort-by-filename "Always predictable"))

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
  ;; Guess a default target directory. When there are two dired buffers, Emacs will select another
  ;; buffer as the target (e.g., target for copying files).
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
  (unless sb/EMACS28+
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

  ;; We can also configure `dired-omit-extensions'

  ;; ":diminish dired-omit-mode" does not work
  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Remove 'Omit' from the modeline."
    (diminish 'dired-omit-mode)
    dired-mode-map))

;; Narrow `dired' to match filter
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;; Alternate: https://github.com/Anoncheg1/dired-hist
(use-package dired-hist
  :straight (:host github :repo "karthink/dired-hist")
  :hook (dired-mode . dired-hist-mode)
  :bind (:map dired-mode-map ("l" . dired-hist-go-back) ("r" . dired-hist-go-forward)))

(use-package vertico
  :straight
  (vertico
   :files (:defaults "extensions/*")
   :includes (vertico-directory vertico-repeat vertico-quick))
  :hook (emacs-startup . vertico-mode)
  :bind (:map vertico-map ("M-<" . vertico-first) ("M->" . vertico-last) ("C-M-j" . vertico-exit-input))
  :custom (vertico-cycle t))

;; More convenient directory navigation commands
(use-package vertico-directory
  :straight nil
  :after vertico
  :hook
  ;; Tidy shadowed file names. That is, when using a command for selecting a file in the minibuffer,
  ;; the following fixes the path so the selected path does not have prepended junk left behind.
  ;; This works with `file-name-shadow-mode' enabled. When you are in a sub-directory and use, say,
  ;; `find-file' to go to your home '~/' or root '/' directory, Vertico will clear the old path to
  ;; keep only your current input.
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map
   vertico-map
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-repeat
  :straight nil
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-c r" . vertico-repeat-last) ("M-r" . vertico-repeat-select))
  :config
  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history)))

(use-package vertico-quick
  :straight nil
  :after vertico
  :bind (:map vertico-map ("C-c q" . vertico-quick-insert) ("C-'" . vertico-quick-jump)))

;; Press "SPC" to show ephemeral buffers, "b SPC" to filter by buffers, "f SPC" to filter by
;; files, "p SPC" to filter by projects. If you press "DEL" afterwards, the full candidate list
;; will be shown again.
(use-package consult
  :after vertico
  :commands consult-fd
  :bind
  (("<f1>" . execute-extended-command)
   ([remap switch-to-buffer] . consult-buffer)
   ("<f3>" . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ([remap apropos] . consult-apropos)
   ([remap goto-line] . consult-goto-line)
   ("M-g e" . consult-compile-error)
   ("M-g o" . consult-outline)
   ("C-c C-m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ([remap imenu] . consult-imenu) ; "M-g i"
   ("C-c C-j" . consult-imenu)
   ([remap customize] . consult-customize)
   ([remap load-theme] . consult-theme)
   ("C-c s f" . consult-find)
   ([remap locate] . consult-locate)
   ("C-c s l" . consult-locate)
   ;; Prefix argument "C-u" allows to specify the directory
   ([remap rgrep] . consult-grep)
   ("C-c s g" . consult-grep)
   ([remap vc-git-grep] . consult-git-grep)
   ("C-c s G" . consult-git-grep)
   ("C-c s r" . consult-ripgrep)
   ("C-c s h" . consult-isearch-history)
   ("<f4>" . consult-line)
   ([remap multi-occur] . consult-multi-occur)
   ;; ("M-s m" . consult-multi-occur)
   ([remap recentf-open-files] . consult-recent-file)
   ("M-g r" . consult-register)
   :map
   isearch-mode-map
   ("M-s e" . consult-isearch-history)
   :map
   minibuffer-local-map
   ("M-s" . consult-history))
  :custom
  (consult-line-start-from-top t "Start search from the beginning")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-preview-key nil "Disable preview by default, enable for selected commands")
  (completion-in-region-function #'consult-completion-in-region "Complete M-:")
  ;; Having multiple other sources like recentf makes it difficult to identify and switch quickly
  ;; between only buffers, especially while wrapping around.
  (consult-buffer-sources '(consult--source-buffer))
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

;; Provide context-dependent actions similar to a content menu.
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
  :config (define-key embark-identifier-map "y" #'symbol-overlay-put)

  ;; (define-key embark-identifier-map "-" #'string-inflection-cycle)
  ;; (add-to-list 'embark-repeat-actions #'string-inflection-cycle)

  (with-eval-after-load "vertico"
    (bind-keys :map vertico-map ("C-`" . embark-act) ("C-c C-e" . embark-export)))

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets)
                       "…"
                     "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding) (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator embark-highlight-indicator embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter :around #'embark-hide-which-key-indicator)

  (defvar embark--target-mode-timer nil)
  (defvar embark--target-mode-string "")

  (defun embark--target-mode-update ()
    (setq embark--target-mode-string
          (if-let (targets
                   (embark--targets))
            (format "[%s%s] "
                    (propertize (symbol-name (plist-get (car targets) :type)) 'face 'bold)
                    (mapconcat (lambda (x) (format ", %s" (plist-get x :type))) (cdr targets) ""))
            "")))

  (define-minor-mode embark-target-mode
    "Shows the current targets in the modeline."
    :global
    t
    (setq mode-line-misc-info (assq-delete-all 'embark-target-mode mode-line-misc-info))
    (when embark--target-mode-timer
      (cancel-timer embark--target-mode-timer)
      (setq embark--target-mode-timer nil))
    (when embark-target-mode
      (push '(embark-target-mode (:eval embark--target-mode-string)) mode-line-misc-info)
      (setq embark--target-mode-timer (run-with-idle-timer 0.1 t #'embark--target-mode-update)))))

;; Adds support for exporting a list of search results to a `grep-mode' buffer, on which you can use
;; `wgrep'
(use-package embark-consult
  :after (embark consult))

;; Enriches the completion display with annotations, e.g., documentation strings or file
;; information.
(use-package marginalia
  :after vertico
  :init (marginalia-mode 1)
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :config
  (setq marginalia-annotator-registry (assq-delete-all 'file marginalia-annotator-registry))
  (add-to-list 'marginalia-annotator-registry '(symbol-help marginalia-annotate-variable))
  (add-to-list 'marginalia-annotator-registry '(project-buffer marginalia-annotate-project-buffer)))

;; ":after consult" prevents `consult-tramp' keybinding from being registered
(use-package consult-tramp
  :straight (:host github :repo "Ladicle/consult-tramp")
  :bind ("C-c d t" . consult-tramp))

(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map
   vertico-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-flyspell
  :after (consult flyspell)
  :bind ("C-c f l" . consult-flyspell)
  :config
  (setq consult-flyspell-select-function
        (lambda ()
          (flyspell-correct-at-point)
          (consult-flyspell))))

(use-package consult-flycheck
  :after (consult flycheck)
  :bind (:map flycheck-command-map ("!" . consult-flycheck)))

(use-package consult-lsp
  :after (consult lsp)
  :commands (consult-lsp-diagnostics consult-lsp-file-symbols)
  :bind (:map lsp-mode-map ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package consult-yasnippet
  :bind ("C-M-y" . consult-yasnippet))

(use-package ispell
  :straight (:type built-in)
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
      (setq
       ispell-program-name "hunspell"
       ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
       ispell-hunspell-dictionary-alist ispell-local-dictionary-alist
       ispell-hunspell-dict-paths-alist `(("en_US" ,(concat user-emacs-directory "hunspell/en_US.aff"))))))
   ((and (symbol-value 'sb/IS-LINUX) (executable-find "aspell"))
    (progn
      (setq
       ispell-program-name "aspell"
       ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90"))))
   ((and (symbol-value 'sb/IS-WINDOWS) (executable-find "hunspell"))
    (progn
      (setenv "LANG" "en_US")
      (setenv "DICTIONARY" "en_US")
      (setenv "DICPATH" `,(concat user-emacs-directory "hunspell"))
      (setq
       ispell-program-name "hunspell"
       ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
       ispell-hunspell-dictionary-alist ispell-local-dictionary-alist
       ispell-hunspell-dict-paths-alist `(("en_US" ,(concat user-emacs-directory "hunspell/en_US.aff")))))))

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

(use-package flyspell
  :straight (:type built-in)
  :hook
  ( ;; Enabling `flyspell-prog-mode' does not seem to be very useful and highlights links and
   ;; language-specific words. Furthermore, it is supposedly slow.
   (prog-mode . flyspell-prog-mode)
   (text-mode . flyspell-mode))
  :bind (("C-c f f" . flyspell-mode) ("C-c f b" . flyspell-buffer))
  :custom
  (flyspell-abbrev-p t "Add corrections to abbreviation table")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :diminish)

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; As of Emacs 29, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.

;; "M-$" triggers correction for the misspelled word before point, "C-u M-$" triggers correction for
;; the entire buffer.
(use-package jinx
  :when (symbol-value 'sb/IS-LINUX)
  :hook (text-mode . jinx-mode)
  :bind (([remap ispell-word] . jinx-correct) ("C-M-$" . jinx-languages))
  :custom (jinx-languages "en_US")
  :diminish)

;; (use-package transient
;;   :commands transient-bind-q-to-quit
;;   :custom (transient-semantic-coloring t)
;;   :config
;;   ;; Allow using `q' to quit out of popups in addition to `C-g'
;;   (transient-bind-q-to-quit))

;; The built-in `describe-function' includes both functions and macros. `helpful-function' is only
;; for functions, so we use `helpful-callable' as a replacement.
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
  ((minibuffer-setup . (lambda () (hungry-delete-mode -1)))
   (emacs-startup . global-hungry-delete-mode))
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
  :hook (emacs-startup . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :hook (emacs-startup . whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode)

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

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

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :bind* ("C-." . iedit-mode))

(use-package hl-todo
  :hook (emacs-startup . global-hl-todo-mode)
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
      ("BEWARE" . "#aa0000")
      ("REFACTOR" . "#cc9393"))
    hl-todo-keyword-faces)))

;; (use-package highlight-numbers
;;   :hook
;;   (
;;     (prog-mode
;;       yaml-mode
;;       yaml-ts-mode
;;       conf-mode
;;       css-mode
;;       css-ts-mode
;;       html-mode
;;       html-ts-mode
;;       web-mode)
;;     . highlight-numbers-mode))

;; ;; Display ugly "^L" page breaks as tidy horizontal lines
;; (use-package page-break-lines
;;   :hook (emacs-startup . global-page-break-lines-mode)
;;   :diminish)

(use-package wc-mode
  :straight (:type built-in)
  :commands wc-mode)

;; Save a bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark, use `bookmark-jump'
;; ("C-x r b") or `bookmark-bmenu-list' ("C-x r l"). Rename the bookmarked location in
;; `bookmark-bmenu-mode' with `R'.
(use-package bm
  :init
  ;; Must be set before `bm' is loaded
  (setq
   bm-verbosity-level 1
   bm-modeline-display-total t)
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
  :custom
  (bm-buffer-persistence t "Save bookmarks")
  (bm-highlight-style 'bm-highlight-line-and-fringe))

(use-package crux
  :bind
  (("C-c d i" . crux-ispell-word-then-abbrev)
   ("<f12>" . crux-kill-other-buffers)
   ("C-c d s" . crux-sudo-edit))
  :bind* ("C-c C-d" . crux-duplicate-current-line-or-region))

(use-package rainbow-mode
  :hook ((LaTeX-mode css-mode css-ts-mode html-mode html-ts-mode web-mode help-mode) . rainbow-mode)
  :diminish)

;; Temporarily highlight the region involved in certain operations like `kill-line' and `yank'.
(use-package volatile-highlights
  :hook (emacs-startup . volatile-highlights-mode)
  :diminish volatile-highlights-mode)

(use-package xclip
  :when (or (executable-find "xclip") (executable-find "xsel"))
  :hook (emacs-startup . xclip-mode))

;; (use-package fix-word
;;   :bind (("M-u" . fix-word-upcase) ("M-l" . fix-word-downcase) ("M-c" . fix-word-capitalize)))

;; Allow GC to happen after a period of idle time
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :diminish)

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :diminish)

;; Both project.el and projectile are unable to remember remote projects.
(use-package project
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
   ("r" . project-query-replace-regexp))
  :custom (project-switch-commands 'project-find-file "Start `project-find-file' by default"))

(use-package project-x
  :straight (:host github :repo "karthink/project-x")
  :after project
  :demand t
  :config (project-x-mode 1))

(use-package isearch
  :straight (:type built-in)
  :bind
  ;; Change the bindings for `isearch-forward-regexp' and `isearch-repeat-forward'
  (("C-s")
   ("C-M-f") ; Was bound to `isearch-forward-regexp', but we use it for `forward-sexp'
   ("C-f" . isearch-forward-regexp) ("C-r" . isearch-backward-regexp)
   :map isearch-mode-map ("C-s") ("C-f" . isearch-repeat-forward) ("C-c C-o" . isearch-occur))
  :custom (isearch-lazy-count t "Show match count"))

;; Auto populate `isearch' with the symbol at point
(use-package isearch-symbol-at-point
  :after isearch
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

(when (executable-find "fd")
  (setq find-program "fd"))

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

;; `consult-rg' provides a live search feature, while `deadgrep' provides a resulting search buffer.
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
   ("C-'" . avy-isearch))
  :custom (avy-background t "Provides better contrast")
  :config (avy-setup-default))

;; Package `visual-regexp' provides an alternate version of `query-replace' which highlights matches
;; and replacements as you type.
(use-package visual-regexp
  :bind
  ([remap query-replace] . vr/query-replace)
  ([remap replace-regex] . vr/replace))

(use-package vc-hooks
  :straight (:type built-in)
  :custom
  (vc-handled-backends '(Git))
  (vc-follow-symlinks t "No need to ask")
  ;; Disable version control for remote files to improve performance
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))

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
  :config
  (require 'magit-diff)
  (setq
   magit-diff-refine-hunk t ; Show fine differences for the current diff hunk only
   magit-diff-highlight-trailing nil)

  (with-eval-after-load "project"
    (define-key project-prefix-map "m" #'magit-project-status)))

(use-package difftastic
  :commands (difftastic-files difftastic-dired-diff difftastic-magit-diff)
  :bind
  (:map
   difftastic-mode-map
   ("e" . difftastic-leave)
   ("g" . difftastic-rerun)
   ("p" . difftastic-previous-chunk)
   ("n" . difftastic-next-chunk)
   ("q" . difftastic-quit)
   :map
   magit-blame-read-only-mode-map
   ("D" . difftastic-magit-show)
   ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix
      'magit-diff '(-1 -1)
      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
       ("S" "Difftastic show" difftastic-magit-show)])))

(use-package git-modes
  :mode ("dotgitconfig" . gitconfig-mode)
  :mode ("/\\.gitignore\\'" . gitignore-mode)
  :mode ("/\\.gitattributes\\'" . gitattributes-mode))

;; Diff-hl looks nicer than git-gutter, and is based on `vc'
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
   (dired-mode . diff-hl-dired-mode-unless-remote) (emacs-startup . global-diff-hl-mode))
  :bind (("C-x v [" . diff-hl-previous-hunk) ("C-x v ]" . diff-hl-next-hunk))
  :custom
  (diff-hl-draw-borders nil "Highlight without a border looks nicer")
  (diff-hl-disable-on-remote t "Disable in remote buffers")
  :config
  (diff-hl-flydiff-mode 1) ; For unsaved buffers

  ;; Display in the margin since the fringe is unavailable in TTY
  ;; (unless (display-graphic-p)
  ;;   (diff-hl-margin-mode 1))

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
     (git-commit-turn-on-flyspell)))
  :custom (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

;; Use the minor mode `smerge-mode' to move between conflicts and resolve them. Since Emacs 25.1,
;; `vc-git-find-file-hook' enables smerge for files with conflicts.
(use-package smerge-mode
  :straight (:type built-in)
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
  :straight (:type built-in)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package elec-pair
  :straight (:type built-in)
  :hook
  ((emacs-startup . electric-pair-mode)
   ;; Disable pairs when entering minibuffer
   (minibuffer-setup . (lambda () (electric-pair-local-mode -1))))
  :custom
  ;; Avoid balancing parentheses since they can be both irritating and slow
  (electric-pair-preserve-balance nil)
  ;; Allow always pairing matching delimiters
  (electric-pair-inhibit-predicate 'ignore)
  :config
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
  :straight (:host github :repo "jdtsmith/mode-minder")
  :commands mode-minder)

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
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
    (setq flycheck-mode-line nil))

  (setq-default
   flycheck-markdown-markdownlint-cli-config
   (expand-file-name ".markdownlint.json" sb/user-home-directory)
   flycheck-pylintrc '("setup.cfg" "pylintrc")
   flycheck-python-pylint-executable "python3"
   flycheck-shellcheck-follow-sources nil)

  ;; Add support for textidote
  (flycheck-define-checker
   tex-textidote
   "A LaTeX grammar/spelling checker using textidote.
  See https://github.com/sylvainhalle/textidote."
   :modes (LaTeX-mode)
   :command
   ("java"
    "-jar"
    (eval (expand-file-name (no-littering-expand-etc-file-name "textidote.jar")))
    "--read-all"
    "--output"
    "singleline"
    "--no-color"
    "--check"
    (eval
     (if ispell-current-dictionary
         (substring ispell-current-dictionary 0 2)
       "en"))
    ;; Try to honor local aspell dictionary and replacements if they exist
    "--dict"
    (eval (expand-file-name ispell-personal-dictionary))
    "--replace"
    (eval (expand-file-name "~/.aspell.en.prepl"))
    "--ignore"
    "lt:en:MORFOLOGIK_RULE_EN_US:WANT:EN_QUOTES:EN_DIACRITICS_REPLACE,lt:en:WORD_CONTAINS_UNDERSCORE"
    ;; Using source ensures that a single temporary file in a different dir is created
    ;; such that textidote won't process other files. This serves as a hacky workaround for
    ;; https://github.com/sylvainhalle/textidote/issues/200.
    source)
   :error-patterns
   ((warning
     line-start
     (file-name)
     "(L"
     line
     "C"
     column
     "-"
     (or (seq "L" end-line "C" end-column) "?")
     "): "
     (message (one-or-more (not "\"")))
     (one-or-more not-newline)
     line-end)))
  ;; (add-to-list 'flycheck-checkers 'tex-textidote)

  ;; https://github.com/flycheck/flycheck/issues/1833
  (add-to-list 'flycheck-hooks-alist '(after-revert-hook . flycheck-buffer))

  ;; Exclude directories and files from being checked
  ;; https://github.com/flycheck/flycheck/issues/1745

  ;; (defvar sb/excluded-directory-regexps '(".git" "elpa" ".cache" ".clangd"))
  ;; (defun sb/flycheck-may-check-automatically (&rest _conditions)
  ;;   (or (null buffer-file-name)
  ;;     (let ((bufname (file-truename buffer-file-name)))
  ;;       (not (seq-some (lambda (re) (string-match-p re bufname)) sb/excluded-directory-regexps)))))
  ;; (advice-add 'flycheck-may-check-automatically :after-while #'sb/flycheck-may-check-automatically)

  ;; Chain flycheck checkers with lsp. We prefer to use per-project directory local variables
  ;; instead of defining here.

  ;; https://github.com/flycheck/flycheck/issues/1762
  (defvar-local sb/flycheck-local-checkers nil)
  (defun sb/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker sb/flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get))

;; Use for major modes which do not provide a formatter.
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
                  ("HTML" tidy)
                  ("LaTeX" latexindent)
                  ("Markdown" (prettier "--print-width" "100"))
                  ("Perl"
                   perltidy
                   "--quiet"
                   "--standard-error-output"
                   "--perl-best-practices"
                   "-l=100")
                  ("Python" (yapf "--style" "file") isort)
                  ("Shell" (shfmt "-i" "4" "-ci"))
                  ("XML" tidy)
                  ("YAML" prettier "--print-width" "100")))
  (with-eval-after-load "markdown-mode"
    (bind-key "C-x f" #'format-all-buffer markdown-mode-map))
  (with-eval-after-load "auctex"
    (bind-key "C-x f" #'format-all-buffer LaTeX-mode-map))

  ;; The cursor position is not saved in `LaTeX-mode-hook', so we invoke explicitly.
  ;; (add-hook 'LaTeX-mode-hook (lambda () (run-with-idle-timer 30 t #'format-all-buffer)))
  :diminish)

;; https://languagetool.org/download/LanguageTool-stable.zip
;; The "languagetool" folder should include all files in addition to the ".jar" files.

;; (use-package langtool
;;   :commands (langtool-check langtool-check-done langtool-show-message-at-point langtool-correct-buffer)
;;   :init
;;   (setq
;;     langtool-default-language "en-US"
;;     languagetool-java-arguments '("-Dfile.encoding=UTF-8")
;;     languagetool-console-command (no-littering-expand-etc-file-name "languagetool/languagetool-commandline.jar")
;;     languagetool-server-command (no-littering-expand-etc-file-name "languagetool/languagetool-server.jar")
;;     langtool-language-tool-jar (no-littering-expand-etc-file-name "languagetool/languagetool-commandline.jar")
;;     langtool-disabled-rules
;;     '
;;     ("MORFOLOGIK_RULE_EN_US"
;;       ;; "WHITESPACE_RULE"
;;       ;; "EN_QUOTES"
;;       ;; "DASH_RULE"
;;       ;; "COMMA_PARENTHESIS_WHITESPACE"
;;       ;; "OXFORD_SPELLING_ISE_VERBS"
;;       ;; "OXFORD_SPELLING_NOUNS")
;;       )))

;; (use-package highlight-indentation
;;   :hook
;;   ((yaml-mode yaml-ts-mode python-mode python-ts-mode)
;;     .
;;     highlight-indentation-mode)
;;   :diminish (highlight-indentation-current-column-mode highlight-indentation-mode))

;; (use-package
;;   indent-bars
;;   :straight (:host github :repo "jdtsmith/indent-bars")
;;   :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode) . indent-bars-mode)
;;   :custom
;;   (indent-bars-treesit-support t)
;;   (indent-bars-no-descend-string t)
;;   (indent-bars-treesit-ignore-blank-lines-types '("module"))
;;   (indent-bars-treesit-wrap
;;     '
;;     (
;;       (python
;;         argument_list
;;         parameters ; for python, as an example
;;         list
;;         list_comprehension
;;         dictionary
;;         dictionary_comprehension
;;         parenthesized_expression
;;         subscript))))

;; `format-all-the-code' just runs Emacs' built-in `indent-region' for `emacs-lisp'.
(use-package elisp-autofmt
  :commands (elisp-autofmt-buffer)
  :hook ((emacs-lisp-mode lisp-data-mode) . elisp-autofmt-mode)
  :custom (elisp-autofmt-python-bin "python3")
  :config (setq-default elisp-autofmt-load-packages-local '("use-package")))

(use-package shfmt
  :hook ((sh-mode bash-ts-mode) . shfmt-on-save-mode)
  :custom
  ;; p: Posix, ci: indent case labels, i: indent with spaces
  (shfmt-arguments '("-i" "4" "-ln" "bash" "-ci")))

(use-package flycheck-hl-todo
  :straight (:host github :repo "alvarogonzalezsotillo/flycheck-hl-todo")
  :after flycheck
  :init (flycheck-hl-todo-setup))

;; "basic" matches only the prefix, "substring" matches the whole string. "initials" matches
;; acronyms and initialisms, e.g., can complete "M-x lch" to "list-command-history".
;; "partial-completion" style allows to use wildcards for file completion and partial paths, e.g.,
;; "/u/s/l" for "/usr/share/local". While "partial-completion" matches search terms must match in
;; order, "orderless" can match search terms in any order.
(use-package minibuffer
  :straight (:type built-in)
  :bind
  (("M-p" . minibuffer-previous-completion)
   ("M-n" . minibuffer-next-completion)
   ("M-RET" . minibuffer-choose-completion))
  :custom
  (completions-format 'vertical)
  (read-file-name-completion-ignore-case t "Ignore case when reading a file name")
  (completion-cycle-threshold 3 "TAB cycle if there are only few candidates")
  (completion-category-defaults nil)
  :config
  ;; Show docstring description for completion candidates in commands like `describe-function'.
  (when sb/EMACS28+
    (setq completions-detailed t))

  (when (fboundp 'dabbrev-capf)
    (add-to-list 'completion-at-point-functions 'dabbrev-capf t))

  (with-eval-after-load "orderless"
    ;; substring is needed to complete common prefix, orderless does not
    (setq completion-styles '(orderless substring basic)))

  (with-eval-after-load "hotfuzz"
    (setq completion-styles '(hotfuzz substring basic)))

  ;; The "basic" completion style needs to be tried first for TRAMP hostname completion to
  ;; work. I also want substring matching for file names.
  (setq completion-category-overrides '((file (styles basic substring partial-completion)))))

;; Use "C-M-;" for `dabbrev-completion' which finds all expansions in the current buffer and
;; presents suggestions for completion.
(use-package dabbrev
  :straight (:type built-in)
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

;; Use "M-SPC" for space-separated completion lookups, works with Corfu.
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
  :hook (emacs-startup . yas-global-mode)
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
  :when (eq sb/capf 'company)
  :hook (emacs-startup . global-company-mode)
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
  (company-idle-delay 0.05 "Start autocompletion faster")
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-code-completion-styles '(basic flex))
  (company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory))
  (company-require-match nil "Allow typing input characters that do not match candidates")
  (company-show-quick-access t "Speed up selecting a completion")
  ;; Align additional metadata, like type signatures, to the right-hand side because it looks better
  (company-tooltip-align-annotations t)
  ;; Choices are: `company-pseudo-tooltip-unless-just-one-frontend' shows popup unless there is only
  ;; one candidate, `company-preview-frontend' shows the preview in-place which is too intrusive,
  ;; `company-preview-if-just-one-frontend' shows in-place preview if there is only choice,
  ;; `company-echo-metadata-frontend' shows selected candidate docs in echo area, and
  ;; `company-pseudo-tooltip-frontend' which always shows the candidates in an overlay.
  ;; (company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (company-global-modes
   '(not dired-mode magit-status-mode help-mode csv-mode minibuffer-inactive-mode))
  (company-format-margin-function nil "Disable icons")
  (company-selection-wrap-around t "Convenient to wrap around completion items at boundaries")
  ;; (company-tooltip-flip-when-above t "Flip the tooltip when it is close to the bottom")
  :diminish)

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
  :after (:all tex-mode (:any company corfu))
  :demand t)

(use-package company-math
  :straight math-symbols
  :straight t
  :after (:all tex-mode (:any company corfu))
  :demand t)

;; Uses RefTeX to complete label references and citations. When working with multi-file documents,
;; ensure that the variable `TeX-master' is appropriately set in all files, so that RefTeX can find
;; citations across documents.
(use-package company-reftex
  :after (:all tex-mode (:any company corfu))
  :demand t
  :custom
  ;; https://github.com/TheBB/company-reftex/pull/13
  (company-reftex-labels-parse-all nil))

;; Complete in the middle of words
(use-package company-anywhere
  :straight (:host github :repo "zk-phi/company-anywhere")
  :after company
  :demand t)

(use-package company-dict
  :after (:any company corfu)
  :demand t
  :custom
  (company-dict-dir (expand-file-name "company-dict" user-emacs-directory))
  (company-dict-enable-yasnippet nil))

;; Better replacement for `company-files'
(use-package company-dirfiles
  :straight (:host codeberg :repo "cwfoo/company-dirfiles")
  :after company
  :demand t)

(use-package company-org-block
  :after (company org)
  :demand t)

(use-package company-c-headers
  :after (company cc-mode)
  :demand t
  :custom (company-c-headers-path-system '("/usr/include/c++/11" "/usr/include" "/usr/local/include")))

(use-package company-web
  :after (:any company corfu)
  :demand t
  :config (require 'company-web-html))

(use-package company-wordfreq
  :straight (:host github :repo "johannes-mueller/company-wordfreq.el")
  :after (:any company corfu)
  :demand t)

;; Try completion backends in order untill there is a non-empty completion list:
;; (setq company-backends '(company-xxx company-yyy company-zzz))

;; Merge completions of all the backends:
;; (setq company-backends '((company-xxx company-yyy company-zzz)))

;; Merge completions of all the backends but keep the candidates organized in accordance with the
;; grouped backends order.
;; (setq company-backends '((company-xxx company-yyy company-zzz :separate)))

;; Another keyword :with helps to make sure the results from major/minor mode agnostic backends
;; (such as company-yasnippet, company-dabbrev-code) are returned without preventing results from
;; context-aware backends (such as company-capf or company-clang). For this feature to work, put
;; backends dependent on a mode at the beginning of the grouped backends list, then put a keyword
;; :with, and only then put context agnostic backend(s).
;; (setq company-backends '((company-capf :with company-yasnippet)))

;; Most backends will not pass control to the following backends (e.g., `company-yasnippet' and
;; `company-tempo'). Only a few backends are specialized on certain major modes or certain contexts
;; (e.g. outside of strings and comments), and pass on control to later backends when outside of
;; that major mode or context.

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
   '(company-dirfiles
     (company-capf :with company-dabbrev-code company-yasnippet)
     ;; If we have `company-dabbrev' first, then other matches from `company-ispell' will be
     ;; ignored.
     company-ispell company-dict company-dabbrev)
   ;; Options: `company-sort-prefer-same-case-prefix', `company-sort-by-occurrence',
   ;; `company-sort-by-statistics', `company-sort-by-length', `company-sort-by-backend-importance',
   ;; `delete-dups'.
   company-transformers
   '(delete-dups
     ;; company-sort-by-backend-importance
     ;; company-sort-by-occurrence
     company-sort-by-statistics
     company-sort-prefer-same-case-prefix))

  ;; (add-to-list 'company-transformers 'delete-dups)
  ;; (add-to-list 'company-transformers 'company-sort-by-backend-importance)
  ;; (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)

  ;; Ignore matches that consist solely of numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  (push (apply-partially #'cl-remove-if (lambda (c) (string-match-p "\\`[0-9]+\\'" c)))
        company-transformers)

  (progn
    (defun sb/company-latex-mode ()
      "Add backends for `latex-mode' completion in company mode."
      (make-local-variable 'company-backends)

      ;; Example: company-backends: https://github.com/TheBB/company-reftex/issues/10

      ;; `company-reftex' should be considerably more powerful than `company-auctex' backends for
      ;; labels and citations. `company-reftex-labels' is expected to be better than
      ;; `company-auctex-labels'. `company-reftex-citations' is better than `company-bibtex' and
      ;; `company-auctex-bibs'.

      ;; `company-capf' does not pass to later backends with Texlab, so we have it last
      (setq company-backends
            '(company-dirfiles
              (company-math-symbols-latex ; Math latex tags
               company-latex-commands
               company-reftex-labels
               company-reftex-citations
               company-auctex-environments
               company-auctex-macros
               company-auctex-labels
               ;; Math unicode symbols and sub(super)scripts
               company-math-symbols-unicode
               company-auctex-symbols
               company-auctex-bibs
               company-ispell
               company-dict
               company-dabbrev
               company-wordfreq
               company-capf))))

    (add-hook 'LaTeX-mode-hook (lambda () (sb/company-latex-mode))))

  (progn
    (defun sb/company-org-mode ()
      "Add backends for org completion in company mode."
      (set
       (make-local-variable 'company-backends)
       '(company-dirfiles company-org-block company-ispell company-dict company-dabbrev)))

    (add-hook 'org-mode-hook (lambda () (sb/company-org-mode))))

  (progn
    (defun sb/company-text-mode ()
      "Add backends for `text-mode' completion in company mode."
      (set
       (make-local-variable 'company-backends)
       '(company-dirfiles (company-wordfreq company-ispell company-dict company-dabbrev))))

    ;; Extends to derived modes like `markdown-mode' and `org-mode'
    (add-hook
     'text-mode-hook
     (lambda ()
       (unless (derived-mode-p 'LaTeX-mode)
         (sb/company-text-mode)

         ;; (defun sb/company-after-completion-hook (&rest _ignored)
         ;;   (just-one-space))
         ;; (setq-local company-after-completion-hook #'sb/company-after-completion-hook)
         ))))

  (progn
    (defun sb/company-yaml-mode ()
      "Add backends for `yaml-mode' completion in company mode."
      (make-local-variable 'company-backends)
      (setq company-backends
            '(company-dirfiles
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
      "Add backends for html completion in company mode."
      (setq-local company-minimum-prefix-length 2)

      (set
       (make-local-variable 'company-backends)
       '(company-dirfiles
         (company-capf company-web-html) company-ispell company-dict company-dabbrev)))

    (dolist (hook '(html-mode-hook html-ts-mode-hook))
      (add-hook hook (lambda () (sb/company-html-mode)))))

  (progn
    (defun sb/company-prog-mode ()
      "Add backends for `prog-mode' completion in company mode."
      ;; Typing short prefixes help with faster completion and a more responsive UI
      (setq-local company-minimum-prefix-length 2)

      (make-local-variable 'company-backends)

      ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
      (setq company-backends
            '(company-dirfiles
              (company-capf
               company-citre-tags company-c-headers
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
      "Add backends for `emacs-lisp-mode' completion in company mode."
      ;; Typing short prefixes help with faster completion and a more responsive UI
      (setq-local company-minimum-prefix-length 2)

      (make-local-variable 'company-backends)

      (setq company-backends
            '(company-dirfiles
              (company-capf
               company-citre-tags
               :with company-keywords
               company-dabbrev-code ; Useful for variable names
               company-yasnippet
               :separate)
              company-dict company-ispell company-dabbrev)))

    (dolist (hook '(emacs-lisp-mode-hook lisp-data-mode-hook))
      (add-hook hook (lambda () (sb/company-elisp-mode))))))

;; Corfu is not a completion framework, it is a front-end for `completion-at-point'.
(use-package corfu
  :straight
  (corfu
   :files (:defaults "extensions/*")
   :includes (corfu-echo corfu-popupinfo corfu-history corfu-info))
  :when (eq sb/capf 'corfu)
  :hook (emacs-startup . global-corfu-mode)
  :bind
  (:map
   corfu-map
   ("ESCAPE" . corfu-quit)
   ([escape] . corfu-quit)
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous))
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0.05 "Recommended to not use zero for performance reasons")
  (corfu-exclude-modes
   '(dired-mode inferior-python-mode magit-status-mode help-mode csv-mode minibuffer-inactive-mode))
  :config
  ;; The goal is to use a smaller prefix for programming languages to get faster auto-completion,
  ;; but the popup wraps around with `corfu-terminal-mode' on TUI Emacs. This mostly happens with
  ;; longish completion entries. Hence, a larger prefix can limit to more precise and smaller
  ;; entries.
  (add-hook 'prog-mode-hook (lambda () (setq-local corfu-auto-prefix 2))))

(use-package corfu-info
  :straight nil
  :after corfu
  :bind (:map corfu-map ("M-d" . corfu-info-documentation) ("M-l" . corfu-info-location)))

(use-package corfu-quick-access
  :straight (:host codeberg :repo "spike_spiegel/corfu-quick-access.el")
  :when (eq sb/capf 'corfu)
  :hook
  (corfu-mode
   .
   (lambda ()
     (ignore-errors
       (corfu-quick-access-mode)))))

(use-package corfu-history
  :straight nil
  :when (eq sb/capf 'corfu)
  :hook (corfu-mode . corfu-history-mode)
  :config
  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package corfu-echo
  :straight nil
  :when (eq sb/capf 'corfu)
  :hook (corfu-mode . corfu-echo-mode))

(use-package corfu-popupinfo
  :straight nil
  :when (eq sb/capf 'corfu)
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind
  (:map
   corfu-map
   ("M-n" . corfu-popupinfo-scroll-up)
   ("M-p" . corfu-popupinfo-scroll-down)
   ([remap corfu-show-documentation] . corfu-popupinfo-toggle)))

(use-package popon
  :straight (:host codeberg :repo "akib/emacs-popon")
  :when (and (eq sb/capf 'corfu) (not (display-graphic-p))))

(use-package corfu-terminal
  :straight (:host codeberg :repo "akib/emacs-corfu-terminal")
  :when (and (eq sb/capf 'corfu) (not (display-graphic-p)))
  :hook (corfu-mode . corfu-terminal-mode)
  :custom
  ;; TODO: This is supposedly a bug, report to the maintainer.
  (corfu-terminal-position-right-margin 5 "Prevent wraparound at the right edge"))

(use-package yasnippet-capf
  :after (yasnippet corfu)
  :demand t
  :straight (:host github :repo "elken/yasnippet-capf"))

;; Here is a snippet to show how to support `company' backends with `cape'.
;; https://github.com/minad/cape/issues/20
;; (fset #'cape-path (cape-company-to-capf #'company-files))
;; (add-hook 'completion-at-point-functions #'cape-path)

;; `cape-capf-super' works only well for static completion functions like `cape-dabbrev',
;; `cape-keyword', `cape-dict', etc., but not for complex multi-step completions like `cape-file'.
(use-package cape
  :after corfu
  :demand t
  :init
  ;; Initialize for all generic languages that are not specifically handled
  (add-to-list 'completion-at-point-functions #'cape-keyword 'append)
  (add-to-list 'completion-at-point-functions #'cape-file 'append)
  (add-to-list 'completion-at-point-functions (cape-capf-super #'cape-dabbrev #'cape-dict) 'append)
  :custom
  (cape-dabbrev-min-length 3)
  (cape-dict-grep nil "Load the word files in memory for better performance")
  (cape-dict-file
   `(,(expand-file-name "wordlist.5" sb/extras-directory)
     ,(expand-file-name "company-dict/text-mode" user-emacs-directory)))
  (cape-dabbrev-check-other-buffers 'some)
  :config
  ;; Make these capfs composable
  (with-eval-after-load "lsp-mode"
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))

  ;; Override CAPFS for specific major modes
  (dolist (mode '(emacs-lisp-mode-hook lisp-data-mode-hook))
    (add-hook
     mode
     (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    #'cape-file #'yasnippet-capf
                    (cape-capf-super
                     #'elisp-completion-at-point
                     #'citre-completion-at-point
                     #'cape-elisp-symbol
                     (cape-company-to-capf #'company-yasnippet))
                    (cape-capf-super #'cape-dabbrev #'cape-dict))))))

  (dolist (mode '(flex-mode-hook bison-mode-hook))
    (add-hook
     mode
     (lambda ()
       (setq-local completion-at-point-functions
                   (list #'cape-file #'cape-keyword #'cape-dabbrev #'cape-dict)))))

  (add-hook
   'text-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  #'cape-file
                  ;; Merge dabbrev and dict candidates
                  (cape-capf-properties
                   (cape-capf-super
                    #'cape-dabbrev
                    #'cape-dict
                    (cape-company-to-capf #'company-ispell)
                    (cape-company-to-capf #'company-wordfreq))
                   :sort t)))))

  ;; `cape-tex' is used for Unicode symbols and not for the corresponding LaTeX names.
  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  #'cape-file
                  (cape-capf-super
                   (cape-company-to-capf #'company-math-symbols-latex) ; Math latex tags
                   (cape-company-to-capf #'company-latex-commands)
                   (cape-company-to-capf #'company-reftex-labels)
                   (cape-company-to-capf #'company-reftex-citations)
                   (cape-company-to-capf #'company-auctex-environments)
                   (cape-company-to-capf #'company-auctex-macros)
                   (cape-company-to-capf #'company-auctex-labels)
                   ;; Math unicode symbols and sub(super)scripts
                   (cape-company-to-capf #'company-math-symbols-unicode)
                   (cape-company-to-capf #'company-auctex-symbols)
                   #'cape-dabbrev
                   #'cape-dict)
                  #'yasnippet-capf))))

  (with-eval-after-load "lsp-mode"
    (dolist (mode
             '(c-mode-hook
               c-ts-mode-hook
               c++-mode-hook
               c++-ts-mode-hook
               java-mode-hook
               java-ts-mode-hook
               python-mode-hook
               python-ts-mode-hook
               sh-mode-hook
               bash-ts-mode-hook
               cmake-mode-hook
               cmake-ts-mode-hook
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
                      #'cape-file #'yasnippet-capf
                      (cape-capf-super
                       #'lsp-completion-at-point
                       #'citre-completion-at-point
                       #'cape-keyword
                       (cape-company-to-capf #'company-yasnippet))
                      (cape-capf-super #'cape-dabbrev #'cape-dict))))))))

;; Registering `lsp-format-buffer' makes sense only if the server is active. We may not always want
;; to format unrelated files and buffers (e.g., commented YAML files in out-of-project locations).
(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :bind
  (:map
   lsp-command-map
   ("=")
   ("w")
   ("g")
   ("G")
   ("F")
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
  (lsp-completion-show-detail nil "Disable completion metadata, e.g., java.util.ArrayList")
  (lsp-completion-show-kind nil "Show completion kind, e.g., interface/class")
  (lsp-completion-show-label-description nil "Show description of completion candidates")
  (lsp-eldoc-enable-hover nil "Do not show noisy hover info with mouse")
  (lsp-enable-dap-auto-configure nil "I do not use dap-mode")
  (lsp-enable-on-type-formatting nil "Reduce unexpected modifications to code")
  (lsp-enable-folding nil "I do not find the feature useful")
  (lsp-headerline-breadcrumb-enable nil "Breadcrumb is not useful for all modes")
  ;; Do not customize breadcrumb faces based on errors
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-html-format-wrap-line-length sb/fill-column)
  (lsp-html-format-end-with-newline t)
  (lsp-html-format-indent-inner-html t)
  (lsp-imenu-sort-methods '(position) "More natural way of listing symbols")
  (lsp-lens-enable nil "Lenses are intrusive")
  (lsp-modeline-diagnostics-enable nil "We have Flycheck, and the modeline gets congested")
  (lsp-modeline-diagnostics-scope :file "Simpler to focus on the errors at hand")
  (lsp-modeline-workspace-status-enable t)
  ;; Sudden changes in the height of the echo area causes the cursor to lose position, manually
  ;; request via `lsp-signature-activate'.
  (lsp-signature-auto-activate nil)
  (lsp-restart 'auto-restart "Avoid annoying questions, we expect a server restart to succeed")
  (lsp-xml-logs-client nil)
  (lsp-yaml-print-width sb/fill-column)
  (lsp-warn-no-matched-clients nil "Avoid warning messages for unsupported modes like csv-mode")
  (lsp-keep-workspace-alive nil)
  (lsp-enable-file-watchers nil "Avoid watcher warnings")
  ;; I am using symbol-overlay for languages that do not have a server
  (lsp-enable-symbol-highlighting nil)
  (lsp-pylsp-configuration-sources ["setup.cfg"])
  (lsp-pylsp-plugins-mccabe-enabled nil)
  ;; We can also set this per-project
  (lsp-pylsp-plugins-preload-modules ["numpy" "csv" "pandas" "statistics" "json"])
  (lsp-pylsp-plugins-pycodestyle-enabled nil)
  (lsp-pylsp-plugins-pycodestyle-max-line-length sb/fill-column)
  (lsp-pylsp-plugins-pydocstyle-convention "pep257")
  (lsp-pylsp-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213")))
  (lsp-pylsp-plugins-pyflakes-enabled nil)
  (lsp-pylsp-plugins-pylint-enabled t)
  (lsp-pylsp-plugins-yapf-enabled t)
  (lsp-pylsp-plugins-flake8-enabled nil)
  (lsp-pylsp-plugins-black-enabled nil)
  (lsp-pylsp-plugins-jedi-use-pyenv-environment nil)
  (lsp-pylsp-plugins-pylint-args
   (vconcat
    (list
     "-j 2" (concat "--rcfile=" (expand-file-name ".config/pylintrc" sb/user-home-directory)))))
  (lsp-pylsp-plugins-isort-enabled t)
  (lsp-pylsp-plugins-mypy-enabled t)
  (lsp-use-plists t)
  (lsp-enable-snippet nil)
  :config
  ;; I am explicitly setting company backends and cape capfs for corfu, and do not want lsp-mode to
  ;; interfere with `completion-at-point-functions'
  (when (eq sb/capf 'corfu)
    ;; (setq lsp-completion-enable nil)

    (defun sb/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(flex)))
    (add-hook 'lsp-completion-mode-hook #'sb/lsp-mode-setup-completion))

  (when (or (display-graphic-p) (daemonp))
    (setq lsp-modeline-code-actions-segments '(count icon name)))

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

  ;; (add-to-list 'lsp-language-id-configuration '(flex-mode . "cpp"))
  :diminish)

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

;; We need to enable lsp workspace to allow `lsp-grammarly' to work, which makes it ineffective for
;; temporary text files. `lsp-grammarly' supports PRO Grammarly accounts. If there are failures,
;; then try logging out of Grammarly and logging in again. Make sure to run "M-x keytar-install".
(use-package lsp-grammarly
  :hook ((text-mode markdown-mode org-mode LaTeX-mode) . lsp-deferred)
  :custom
  (lsp-grammarly-suggestions-oxford-comma t)
  (lsp-grammarly-suggestions-passive-voice t)
  (lsp-grammarly-suggestions-informal-pronouns-academic t)
  (lsp-grammarly-suggestions-preposition-at-the-end-of-sentence t)
  (lsp-grammarly-suggestions-conjunction-at-start-of-sentence t)
  (lsp-grammarly-user-words '(Swarnendu Biswas)))

;; The ":after" clause does not work with the ":hook", `lsp-mode' is not started automatically
(use-package lsp-ltex
  :hook ((text-mode markdown-mode org-mode LaTeX-mode) . lsp-deferred)
  :custom
  ;; https://valentjn.github.io/ltex/settings.html#ltexlanguage
  (lsp-ltex-language "en" "Recommended to set a generic language to disable spell check")
  (lsp-ltex-check-frequency "save")
  (lsp-ltex-java-path "/usr/lib/jvm/java-17-openjdk-amd64")
  (lsp-ltex-version "16.0.0")
  (lsp-ltex-dictionary (expand-file-name "company-dict/text-mode" user-emacs-directory))
  :config
  ;; Disable spell checking since we cannot get `lsp-ltex' to work with custom dict words.
  ;; Furthermore, we also use `flyspell' and `jinx'.
  (setq lsp-ltex-disabled-rules
        #s(hash-table
           size 30 data ("en-US" ["MORFOLOGIK_RULE_EN_US,WANT,EN_QUOTES,EN_DIACRITICS_REPLACE"])))

  ;; (setq lsp-ltex-disabled-rules
  ;;       (json-parse-string
  ;;        "{\"en-US\": [\"MORFOLOGIK_RULE_EN_US\"]}"))
  )

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
  (lsp-latex-chktex-on-open-and-save t)
  ;; Delay time in milliseconds before reporting diagnostics
  (lsp-latex-diagnostics-delay 2000)
  ;; Support forward search with Okular. Perform inverse search with Shift+Click in the PDF.
  (lsp-latex-forward-search-executable "okular")
  (lsp-latex-forward-search-args '("--unique" "file:%p#src:%l%f"))
  :config
  (add-to-list 'lsp-latex-build-args "-c")
  (add-to-list 'lsp-latex-build-args "-pvc"))

;; (use-package lsp-snippet-yasnippet
;;   :straight (lsp-snippet-yasnippet :type git :host github :repo "svaante/lsp-snippet")
;;   :after (lsp-mode yasnippet)
;;   :demand t
;;   :config (lsp-snippet-yasnippet-lsp-mode-init))

(use-package subword
  :straight (:type built-in)
  :hook (prog-mode . subword-mode)
  :diminish)

;; Highlight symbol under point
(use-package symbol-overlay
  :hook ((prog-mode html-mode html-ts-mode yaml-ts-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind (("M-p" . symbol-overlay-jump-prev) ("M-n" . symbol-overlay-jump-next))
  :custom (symbol-overlay-idle-time 2 "Delay highlighting to allow for transient cursor placements")
  :diminish)

(use-package compile
  :straight (:type built-in)
  :bind
  ;; "<f10>" and "<f11>" may conflict with Gnome window manager keybindings
  (("<f10>" . compile) ("<f11>" . recompile))
  :custom
  (compile-command (format "make -k -j%s " (num-processors)))
  (compilation-always-kill t "Kill a compilation process before starting a new one")
  (compilation-ask-about-save nil "Save all modified buffers without asking")
  ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first error.
  (compilation-scroll-output 'first-error))

(use-package fancy-compilation
  :after compile
  :init (fancy-compilation-mode 1))

(use-package rainbow-delimiters
  :hook ((prog-mode LaTeX-mode org-src-mode) . rainbow-delimiters-mode))

;; Tree-sitter provides advanced syntax highlighting features.
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

;; (use-package yasnippet-treesitter-shim
;;   :straight (:host github :repo "fbrosda/yasnippet-treesitter-shim" :files ("snippets/*"))
;;   :after (treesit-auto yasnippet)
;;   :demand t
;;   :no-require t
;;   :config (add-to-list 'yas-snippet-dirs (straight--build-dir "yasnippet-treesitter-shim")))

(use-package eldoc
  :straight (:type built-in)
  :hook (prog-mode . turn-on-eldoc-mode)
  :custom (eldoc-area-prefer-doc-buffer t "Disable popups")
  ;; The variable-height minibuffer and extra eldoc buffers are distracting. We can limit ElDoc
  ;; messages to one line which prevents the echo area from resizing itself unexpectedly when point
  ;; is on a variable with a multiline docstring, but then it cuts of useful information.
  ;; (eldoc-echo-area-use-multiline-p nil)
  :config
  ;; Allow eldoc to trigger after completions
  (with-eval-after-load "company"
    (eldoc-add-command
     'company-complete-selection 'company-complete-common 'company-capf 'company-abort))
  :diminish)

(use-package cc-mode
  :straight (:type built-in)
  :mode (("\\.h\\'" . c++-mode) ("\\.c\\'" . c++-mode))
  :hook
  ((c-mode c-ts-mode c++-mode c++-ts-mode)
   .
   (lambda ()
     (setq-local
      ;; Available C styles: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Built_002din-Styles
      c-set-style "cc-mode"
      c-basic-offset 2
      c-auto-newline nil ; Disable electric indentation and on-type formatting
      ;; c-electric-brace nil
      c-electric-flag nil
      ;; c-electric-indent nil
      c-enable-auto-newline nil
      c-syntactic-indentation nil)
     (lsp-deferred)))
  :bind (:map c-mode-base-map ("C-c C-d")))

(use-package c-ts-mode
  :straight (:type built-in)
  :hook
  ((c-ts-mode c++-ts-mode)
   .
   (lambda ()
     (setq-local
      c-ts-mode-indent-style 'linux
      c-ts-mode-indent-offset 2
      c-ts-mode-toggle-comment-style -1
      c-auto-newline nil ; Disable electric indentation and on-type formatting
      ;; c-electric-brace nil
      c-electric-flag nil
      ;; c-electric-indent nil
      c-enable-auto-newline nil
      c-syntactic-indentation nil)
     (lsp-deferred)))
  :bind
  (:map
   c-ts-mode-map ("C-M-a" . treesit-beginning-of-defun) ("C-M-e" . treesit-end-of-defun)
   :map c++-ts-mode-map ("C-M-a" . treesit-beginning-of-defun) ("C-M-e" . treesit-end-of-defun)))

(use-package cuda-mode
  :mode (("\\.cu\\'" . c++-mode) ("\\.cuh\\'" . c++-mode)))

(use-package opencl-mode
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
  :straight (:type built-in)
  :mode
  (("SCon\(struct\|script\)$" . python-ts-mode)
   ("[./]flake8\\'" . conf-mode)
   ("/Pipfile\\'" . conf-mode))
  :hook ((python-mode python-ts-mode) . lsp-deferred)
  :bind
  ;; Assigning a keybinding such as "C-[" is involved, "[" is treated as `meta'
  ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c

  ;; TODO: Bind other functions suitably: python-nav-beginning-of-block, python-nav-end-of-block,
  ;; python-nav-backward-defun, python-nav-forward-defun, python-nav-backward-statement,
  ;; python-nav-forward-statement
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
  :mode "latexmkrc\\'"
  :hook (cperl-mode . lsp-deferred)
  :config
  ;; Prefer CPerl mode to Perl mode
  (fset 'perl-mode 'cperl-mode))

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
  :hook (fish-mode . (lambda () (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package highlight-doxygen
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode) . highlight-doxygen-mode))

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

(use-package yaml-mode
  :mode
  (("\\.yml\\'" . yaml-ts-mode)
   ("\\.yaml\\'" . yaml-ts-mode)
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
     (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
     (lsp-deferred))))

(use-package yaml-imenu
  :hook ((yaml-mode yaml-ts-mode) . yaml-imenu-enable))

;; (use-package css-mode
;;   :hook ((css-mode css-ts-mode) . lsp-deferred)
;;   :custom (css-indent-offset 2))

(use-package make-mode
  :straight (:type built-in)
  :mode
  (("\\Makefile\\'" . makefile-mode)
   ("\\Makefile.common\\'" . makefile-mode)
   ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
   ("makefile\\.rules\\'" . makefile-mode))
  :hook ((makefile-mode make-ts-mode) . (lambda () (setq-local indent-tabs-mode t))))

;; ;; Align fields with "C-c C-a"
;; ;; (use-package csv-mode
;; ;;   :disabled
;; ;;   :hook
;; ;;   (csv-mode
;; ;;     .
;; ;;     (lambda ()
;; ;;       (make-local-variable 'lsp-disabled-clients)
;; ;;       (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
;; ;;       (when (fboundp 'flyspell-mode)
;; ;;         (flyspell-mode -1))
;; ;;       (when (fboundp 'jinx-mode)
;; ;;         (jinx-mode -1))))
;; ;;   :custom (csv-separators '("," ";" "|" " ")))

(use-package antlr-mode
  :straight (:type built-in)
  :mode "\\.g4\\'")

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
  ;; :init
  ;; Looks good, but hiding markup makes it difficult to be consistent while editing
  ;; (setq-default markdown-hide-markup t)
  :mode
  ;; The order is important to associate "README.md" with `gfm-mode'
  (("\\.md\\'" . markdown-mode) ("\\.markdown\\'" . markdown-mode) ("README\\.md\\'" . gfm-mode))
  :bind
  (:map
   markdown-mode-map ("C-c C-d") ("C-c C-j")
   ;; Enable live preview
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
  ;; (markdown-make-gfm-checkboxes-buttons nil)
  (markdown-hide-urls t))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf. Convert `markdown' to `org': "pandoc
;; -f markdown -t org -o output-file.org input-file.md"
(use-package pandoc-mode
  :hook (markdown-mode . pandoc-mode)
  :config (pandoc-load-default-settings)
  :diminish)

;; (use-package emmet-mode
;;   :hook ((web-mode css-mode css-ts-mode html-mode html-ts-mode) . emmet-mode)
;;   :custom (emmet-move-cursor-between-quote t))

(use-package nxml-mode
  :straight (:type built-in)
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
     (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
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
     (setq js-indent-level 2)
     (lsp-deferred))))

;; Links in org-mode by default are displayed as "descriptive" links, meaning they hide their target
;; URLs. While this looks great, it makes it a bit tricky to figure out how you can edit their URL.
;; There are two easy options: (i) press "C-c C-l" (`org-insert-link') while your point is within a
;; link and you will be prompted to edit its URL in the minibuffer. You can use the same command to
;; create new links (when your point is not on an existing link). (ii) You can convert the
;; "descriptive" links to "literal" links by invoking the command "M-x org-toggle-link-display". You
;; can also toggle between the two display modes for links via the mode's menu (under "Hyperlinks").

;; Use zero-width space "C-x 8 zero width space" to treat Org markup as plain text.
;; https://orgmode.org/manual/Escape-Character.html
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-unicode.el

;; https://orgmode.org/manual/In_002dbuffer-Settings.html
(use-package org
  :defer 2
  :custom
  (org-fontify-whole-heading-line nil)
  (org-fontify-emphasized-text t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t "Hide *, ~, and / in Org text unless you edit")
  (org-hide-leading-stars nil "Show every star as it helps identify the indentation level")
  (org-hide-leading-stars-before-indent-mode nil)
  ;; Code block fontification using the major-mode of the code
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tabs-acts-natively t "TAB behavior depends on the major mode")
  (org-src-window-setup 'current-window)
  ;; There is a lot of visible distortion with `org-indent-mode' enabled. Emacs performance
  ;; feels better with the mode disabled.
  (org-startup-indented nil "Indentation looks nice, but is sometimes misaligned")
  (org-startup-truncated nil)
  ;; https://orgmode.org/manual/Initial-visibility.html
  (org-startup-folded 'showeverything)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
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
  (org-adapt-indentation nil)
  (org-odd-levels-only t "Use odd levels to add more indentation")
  (org-export-with-smart-quotes t "#+OPTIONS ':t")
  (org-export-with-section-numbers nil "#+OPTIONS num:nil")
  ;; #+OPTIONS toc:nil, use "#+TOC: headlines 2" or similar if you need a headline
  (org-export-with-toc nil)
  (org-export-with-sub-superscripts nil "#+OPTIONS ^:{}")
  ;; This exports broken links as [BROKEN LINK %s], so we can actually find them. The default value
  ;; nil just aborts the export process with an error message "Unable to resolve link: nil". This
  ;; doesn't give any hint on which line the broken link actually is.
  (org-export-with-broken-links 'mark)
  (org-indent-indentation-per-level 1)
  (org-latex-listings 'minted "Syntax coloring is more extensive than listings")
  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

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
   ;; Was bound to `org-goto', I prefer to use it for `imenu' and its variants
   ("C-c C-j")
   ;; Was bound to `org-forward-paragraph', I prefer to use it for `forward-sentence'
   ("M-e")
   ("<tab>" . org-indent-item)
   ("<backtab>" . org-outdent-item)
   ("M-{" . org-backward-element)
   ("M-}" . org-forward-element)
   ("C-c C-," . org-insert-structure-template)))

;; Make invisible parts of Org elements appear visible
(use-package org-appear
  :straight (:host github :repo "awth13/org-appear")
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
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions)
  :custom (org-block-capf-edit-style 'inline))

;; Auctex provides enhanced versions of `tex-mode' and `latex-mode', which automatically replace the
;; vanilla ones. Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides
;; the tex package.
(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook
  ((LaTeX-mode . LaTeX-math-mode)
   (LaTeX-mode . TeX-PDF-mode) ; Use `pdflatex'
   ;; Revert PDF buffer after TeX compilation has finished
   (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
   ;; Enable rainbow mode after applying styles to the buffer
   (TeX-update-style . rainbow-delimiters-mode) (LaTeX-mode . lsp-deferred))
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
    (bind-key "C-c C-m" TeX-insert-macro LaTeX-mode-map)))

(use-package bibtex
  :straight (:type built-in)
  :hook (bibtex-mode . lsp-deferred)
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-maintain-sorted-entries t)
  (bibtex-comma-after-last-field nil))

(use-package consult-reftex
  :straight (:host github :repo "karthink/consult-reftex")
  :after (consult tex-mode)
  :demand t
  :commands (consult-reftex-insert-reference consult-reftex-goto-label)
  :bind (("C-c [" . consult-reftex-insert-reference) ("C-c )" . consult-reftex-goto-label)))

(use-package auctex-latexmk
  :after tex-mode
  :when (executable-find "latexmk")
  :demand t
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t "Pass the '-pdf' flag when `TeX-PDF-mode' is active")
  :config
  (setq-default TeX-command-default "LatexMk")
  (auctex-latexmk-setup))

;; (with-eval-after-load "latex"
;;   (unbind-key "C-j" LaTeX-mode-map)
;;   ;; Disable `LaTeX-insert-item' in favor of `imenu'
;;   (unbind-key "C-c C-j" LaTeX-mode-map)
;;   (bind-key "C-c x q" #'TeX-insert-quote LaTeX-mode-map))

;; TODO: Try `citar' https://github.com/emacs-citar/citar

;; Set `bibtex-capf-bibliography' in `.dir-locals.el'.
(use-package bibtex-capf
  :straight (:host github :repo "mclear-tools/bibtex-capf")
  :when (eq sb/capf 'corfu)
  :hook ((LaTeX-mode reftex-mode) . bibtex-capf-mode))

(use-package math-delimiters
  :straight (:host github :repo "oantolin/math-delimiters")
  :after tex
  :bind (:map TeX-mode-map ("$" . math-delimiters-insert)))

;; In Emacs Lisp mode, `xref-find-definitions' will by default find only functions and variables
;; from Lisp packages which are loaded into the current Emacs session or are auto-loaded.
(use-package xref
  :bind
  (("M-." . xref-find-definitions)
   ("M-?" . xref-find-references)
   ("C-M-." . xref-find-apropos) ; Find all identifiers whose name matches pattern
   ("M-," . xref-go-back)
   :map
   xref--xref-buffer-mode-map
   ("C-o" . xref-show-location-at-point)
   ("<tab>" . xref-quit-and-goto-xref)
   ("r" . xref-query-replace-in-results))
  :custom (xref-search-program 'ripgrep))

;; https://github.com/universal-ctags/citre/wiki/Use-Citre-together-with-lsp-mode
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

  (defun sb/citre-jump-back+ ()
    "Go back to the position before last `citre-jump'.
Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
        (citre-jump-back)
      (error
       (if (fboundp #'xref-go-back)
           (call-interactively #'xref-go-back)
         (call-interactively #'xref-pop-marker-stack)))))

  (defun sb/push-point-to-xref-marker-stack (&rest r)
    (xref-push-marker-stack (point-marker)))

  (defun sb/lsp-citre-capf-function ()
    "A capf backend that tries lsp first, then Citre."
    (let ((lsp-result
           (cond
            (and (fboundp #'lsp-completion-at-point)
                 (lsp-completion-at-point)))))
      (if (and lsp-result
               (try-completion
                (buffer-substring (nth 0 lsp-result) (nth 1 lsp-result)) (nth 2 lsp-result)))
          lsp-result
        (citre-completion-at-point))))

  (defun sb/enable-lsp-citre-capf-backend ()
    "Enable the lsp + Citre capf backend in current buffer."
    (add-hook 'completion-at-point-functions #'sb/lsp-citre-capf-function nil t))
  :hook
  ;; Using "(require citre-config)" will enable `citre-mode' for all files as long as it finds a
  ;; tags backend, which is not desired for plain text files.
  (prog-mode . citre-mode)
  :bind
  (("C-x c j" . citre-jump)
   ("M-'" . sb/citre-jump+)
   ("C-x c b" . sb/citre-jump-back+)
   ("C-x c p" . citre-peek)
   ("C-x c c" . citre-create-tags-file)
   ("C-x c u" . citre-update-this-tags-file)
   ("C-x c U" . citre-update-tags-file)
   ("C-x c e" . citre-edit-tags-file-recipe))
  :custom
  (citre-use-project-root-when-creating-tags t)
  (citre-default-create-tags-file-location 'project-cache)
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  ;; Enabling this breaks imenu for Elisp files, it cannot identify `use-package' definitions
  (citre-enable-imenu-integration nil)
  (citre-edit-cmd-buf-default-cmd
   "ctags
-o
%TAGSFILE%
;; Edit the relevant programming languages to keep the tags file size reasonable
--languages=BibTeX,C,C++,CUDA,CMake,EmacsLisp,Java,Make,Python,Sh,TeX
--kinds-all=*
--fields=*
--extras=*
-R
;; -e
--exclude=@./.ctagsignore
;; add exclude by: --exclude=target
;; add dirs/files to scan here, one line per dir/file")
  :config (add-hook 'citre-mode-hook #'sb/enable-lsp-citre-capf-backend)

  (dolist (func '(find-function citre-jump))
    (advice-add func :before 'sb/push-point-to-xref-marker-stack))

  ;; Try lsp first, then use Citre
  (with-no-warnings
    (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
      (let ((fetcher (apply -fn -args))
            (citre-fetcher
             (let ((xref-backend-functions '(citre-xref-backend t)))
               (apply -fn -args))))
        (lambda ()
          (or (with-demoted-errors "%s, fallback to citre"
                (funcall fetcher))
              (funcall citre-fetcher))))))

  (with-eval-after-load "company"
    (defmacro citre-backend-to-company-backend (backend)
      "Create a company backend from Citre completion backend BACKEND.
The result is a company backend called
`company-citre-<backend>' (like `company-citre-tags') and can be
used in `company-backends'."
      (let ((backend-name (intern (concat "company-citre-" (symbol-name backend))))
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
              (and
               (bound-and-true-p citre-mode)
               (citre-backend-usable-p ',backend)
               ;; We shouldn't use this as it's defined for getting definitions/references. But the
               ;; Citre completion backend design is not fully compliant with company's design so
               ;; there's no simple "right" solution, and this works for tags/global backends.
               (or (citre-get-symbol-at-point-for-backend ',backend) 'stop)))
             ('meta (citre-get-property 'signature arg))
             ('annotation (citre-get-property 'annotation arg))
             ('candidates
              (let ((citre-completion-backends '(,backend)))
                (all-completions arg (nth 2 (citre-completion-at-point)))))))))

    (citre-backend-to-company-backend tags))
  :diminish)

(use-package breadcrumb
  :straight (:host github :repo "joaotavora/breadcrumb")
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
    java-ts-mode)
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

;; Configure appearance-related settings at the end

(progn
  (defun sb/decrease-minibuffer-font ()
    "Decrease minibuffer font size."
    (set (make-local-variable 'face-remapping-alist) '((default :height 0.90))))

  (add-hook 'minibuffer-setup-hook #'sb/decrease-minibuffer-font))

(use-package doom-themes
  :when (or (eq sb/theme 'doom-one) (eq sb/theme 'doom-nord))
  :init
  (cond
   ((eq sb/theme 'doom-one)
    (load-theme 'doom-one t))
   ((eq sb/theme 'doom-nord)
    (load-theme 'doom-nord t)))
  :config
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-themes
  :when (or (eq sb/theme 'modus-operandi) (eq sb/theme 'modus-vivendi))
  :init
  (cond
   ((eq sb/theme 'modus-operandi)
    (load-theme 'modus-operandi t))
   ((eq sb/theme 'modus-vivendi)
    (load-theme 'modus-vivendi t))))

(use-package nano-theme
  :straight (:host github :repo "rougier/nano-theme")
  :when (eq sb/theme 'nano-dark)
  :init (load-theme 'nano-dark t)
  :custom
  (nano-window-divider-show t)
  (nano-fonts-use t))

;; Powerline theme for Nano looks great, and takes less space on the modeline. It does not show lsp
;; status, flycheck information, and Python virtualenv information on the modeline. The package is
;; not being actively maintained.
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

  ;; https://github.com/dgellow/config/blob/master/emacs.d/modules/01-style.el
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
  :hook (emacs-startup . sb/powerline-nano-theme)
  :custom
  (powerline-display-hud nil "Visualization of the buffer position is not useful")
  (powerline-display-buffer-size nil)
  (powerline-display-mule-info nil "File encoding information is not useful")
  (powerline-gui-use-vcs-glyph t)
  (powerline-height 20))

(use-package doom-modeline
  :when (eq sb/modeline-theme 'doom-modeline)
  :hook (emacs-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 28 "Respected only in GUI")
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-checker-simple-format nil)
  (doom-modeline-buffer-file-name-style 'file-name "Reduce space on the modeline")
  (doom-modeline-unicode-fallback t "Use Unicode instead of ASCII when not using icons"))

(use-package nano-modeline
  :when (eq sb/modeline-theme 'nano-modeline)
  :hook
  ((prog-mode . nano-modeline-prog-mode)
   (text-mode . nano-modeline-text-mode)
   (org-mode . nano-modeline-org-mode)
   (pdf-view-mode . nano-modeline-pdf-mode)
   (messages-buffer-mode . nano-modeline-message-mode))
  :custom (nano-modeline-position #'nano-modeline-footer))

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

 ("C-<f11>" . delete-other-windows)

 ("<f7>" . previous-error) ; "M-g p" is the default keybinding
 ("<f8>" . next-error) ; "M-g n" is the default keybinding

 ("C-x x a" . find-alternate-file)
 ("C-x x g" . revert-buffer-quick)
 ("C-x x f" . rename-file)
 ("C-x x r" . rename-buffer)

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
 ("C-M-@" . mark-sexp))

(unbind-key "C-]") ; Bound to `abort-recursive-edit'

;; This was bound to `electric-newline-and-maybe-indent'. However, this causes problems when
;; pressing "C-c C-j" quickly for `imenu'.
;; (unbind-key "C-j")

(unbind-key "C-x s") ; Bound to `save-some-buffers'
(bind-key "C-x s" #'scratch-buffer)

(global-set-key [remap next-buffer] #'sb/next-buffer)
(global-set-key [remap previous-buffer] #'sb/previous-buffer)

(bind-keys
 ("M-<left>" . sb/previous-buffer)
 ("C-S-<iso-lefttab>" . sb/previous-buffer)
 ("M-<right>" . sb/next-buffer)
 ("C-<tab>" . sb/next-buffer))

(use-package default-text-scale
  :when (display-graphic-p)
  :bind (("C-M-+" . default-text-scale-increase) ("C-M--" . default-text-scale-decrease)))

(use-package free-keys
  :commands free-keys)

;; Show help popups for prefix keys
(use-package which-key
  :hook (emacs-startup . which-key-mode)
  :diminish)

;; (use-package pixel-scroll
;;   :straight (:type built-in)
;;   :bind
;;   ([remap scroll-up-command] . pixel-scroll-interpolate-down)
;;   ([remap scroll-down-command] . pixel-scroll-interpolate-up)
;;   :custom (pixel-scroll-precision-interpolate-page t)
;;   :init
;;   (cond
;;     ((fboundp 'pixel-scroll-precision-mode)
;;       (pixel-scroll-precision-mode 1))
;;     ((fboundp 'pixel-scroll-mode)
;;       (pixel-scroll-mode 1))))

;; Hide "When done with a buffer, type C-x 5" message
(when (boundp 'server-client-instructions)
  (setq server-client-instructions nil))

;; (when (or (eq sb/op-mode 'server) (eq sb/op-mode 'daemon))
;;   ;; Start server if not root user
;;   (unless (string-equal "root" (getenv "USER"))
;;     (when (and (fboundp 'server-running-p) (not (server-running-p)))
;;       (server-start))))

(setq custom-file sb/custom-file)

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))
(when (file-exists-p sb/private-file)
  (load sb/private-file 'noerror 'nomessage))

;; Mark safe variables
(put 'compilation-read-command 'safe-local-variable #'stringp)
(put 'reftex-default-bibliography 'safe-local-variable #'stringp)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (let ((gc-time (float-time gc-elapsed)))
     (message "Emacs ready (init time = %s, gc time = %.2fs, gc count = %d)."
              (emacs-init-time)
              gc-time
              gcs-done))))

;;; init.el ends here

;; Local variables:
;; elisp-autofmt-load-packages-local: ("use-package")
;; End:
