;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary: My configuration is mostly targeted toward GNU Linux.

;;; Code:

;; Splitting the configuration across multiple files is much easier to maintain, and looks less
;; cluttered. The downside is that more files need to be loaded during startup, affecting startup
;; performance.

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
  '
  (radio
    (const :tag "server" server)
    (const :tag "daemon" daemon)
    (const :tag "standalone" standalone))
  :group 'sb/emacs)

(defcustom sb/debug-init-file nil
  "Enable features to debug errors and performance bottlenecks."
  :type 'boolean
  :group 'sb/emacs)

;; "straight.el" makes it easy to install packages from arbitrary sources like GitHub.
(defcustom sb/disable-package.el t
  "Disable package.el.
Prefer the straight.el package manager instead."
  :type 'boolean
  :group 'sb/emacs)

;; A dark theme has better contrast and looks good with the TUI.
(defcustom sb/theme 'doom-nord
  "Specify which Emacs theme to use, unless we are using `circadian'."
  :type
  '
  (radio
    (const :tag "doom-one" doom-one)
    (const :tag "doom-nord" doom-nord)
    (const :tag "modus-operandi" modus-operandi)
    (const :tag "modus-vivendi" modus-vivendi)
    (const :tag "leuven" leuven)
    (const :tag "leuven-dark" leuven-dark)
    (const :tag "nano-dark" nano-dark)
    (const :tag "nordic-night" nordic-night)
    (const :tag "customized" sb/customized) ; Customizations over the default theme
    ;; No customization
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme 'doom-modeline
  "Specify the mode-line theme to use."
  :type
  '
  (radio
    ;; Powerline theme for Nano looks great, and takes less space on the modeline. It does not show
    ;; lsp status and flycheck information.
    (const :tag "powerline" powerline)
    (const :tag "doom-modeline" doom-modeline)
    (const :tag "nano-modeline" nano-modeline)
    ;; No customization
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/window-split 'vertical
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type
  '
  (radio
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

(defcustom sb/delete-trailing-whitespace-p nil
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.
Sometimes we do not want to unnecessarily add differences due to
  whitespaces."
  :type 'boolean
  :group 'sb/emacs)

(defconst sb/user-home-directory (getenv "HOME")
  "User HOME directory.")

(defconst sb/user-config-directory
  (or (getenv "XDG_CONFIG_HOME") (expand-file-name ".config" sb/user-home-directory))
  "Path to user's local config store.")

(defconst sb/user-tmp-directory (expand-file-name "tmp" sb/user-home-directory)
  "User temp directory.
This location is used for temporary installations and files.")

;; `pyls' and `mspyls' are not actively maintained. Improvements to `pylsp' is slow but I prefer it
;; over `pyright'.
(defcustom sb/python-langserver 'pylsp
  "Choose the Python Language Server implementation."
  :type '(radio (const :tag "pylsp" pylsp) (const :tag "pyright" pyright) (const :tag "none" none))
  :group 'sb/emacs)

;; Ivy is no longer maintained
(defcustom sb/minibuffer-completion 'vertico
  "Choose the framework to use for narrowing and selection."
  :type '(radio (const :tag "vertico" vertico) (const :tag "ivy" ivy) (const :tag "none" none))
  :group 'sb/emacs)

;; Corfu is easy to configure, integrates nicely with `orderless', and provides better completion
;; for elisp symbols. But `corfu-terminal-mode' has a potential rendering problem with TUI Emacs,
;; for completion popups appearing near the right edges. The completion entries wrap around, and
;; sometimes messes up the completion. Corfu does not work as well as `company' with LaTeX for me.
;; Company works better with Windows and TUI Emacs, and has more extensive LaTeX support.
;; `company-ispell' is configurable, and we can set up a custom file containing completions with
;; `company-dict'. However, `company-ispell' does not keep prefix case when used as a grouped
;; backend.
(defcustom sb/capf 'company
  "Choose the framework to use for completion at point."
  :type '(radio (const :tag "corfu" corfu) (const :tag "company" company) (const :tag "none" none))
  :group 'sb/emacs)

;; Icons look good and help to distinguish completion categories.
(defcustom sb/corfu-icons 'none
  "Choose the provider for Corfu icons."
  :type
  '
  (radio
    (const :tag "kind-icon" kind-icon)
    (const :tag "kind-all-the-icons" kind-all-the-icons)
    (const :tag "nerd-icons" nerd-icons)
    (const :tag "none" none))
  :group 'sb/emacs)

;; I do not find any difference in terms of the features I require. However, packages like
;; `centaur-tabs' only support `projectile'.
(defcustom sb/project-handler 'project
  "Choose the handler for projects."
  :type '(radio (const :tag "project.el" project) (const :tag "projectile" projectile))
  :group 'sb/emacs)

;; Improvements and bug fixes to `centaur-tabs' are slow.
(defcustom sb/tab-bar-handler nil
  "Choose the handler for tabs."
  :type '(radio (const :tag "centaur-tabs" centaur-tabs) (const :tag "none" nil))
  :group 'sb/emacs)

;; `all-the-icons' only supports GUI, while `nerd-icons' supports both GUI and TUI. We keep icons
;; disabled for better performance and because using icons sometimes lead to visual misalignment in
;; lists.
(defcustom sb/icons-provider 'none
  "Choose the provider for icons."
  :type
  '
  (radio
    (const :tag "all-the-icons" all-the-icons)
    (const :tag "nerd-icons" nerd-icons)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Eglot does not allow multiple servers to connect to a major mode. For example, I can use
;; `texlab', `grammarly', and `lsp-ltex' together with LaTeX files. Eglot also does not support
;; semantic tokens. However, configuring Eglot is simpler and I expect it to receive significant
;; improvements now that it is in the Emacs core.
(defcustom sb/lsp-provider 'lsp-mode
  "Choose between Lsp-mode and Eglot."
  :type '(radio (const :tag "lsp-mode" lsp-mode) (const :tag "eglot" eglot) (const :tag "none" none))
  :group 'sb/emacs)

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

;; We are not using any packages in these paths.
;; (dolist (dir '("extras" "modules"))
;;   (push (expand-file-name dir user-emacs-directory) load-path))

;; Bootstrap `straight.el'
(when (bound-and-true-p sb/disable-package.el)
  (setq
    straight-build-dir
    (format "build/%d%s%d" emacs-major-version version-separator emacs-minor-version)
    ;; Do not check packages on startup to reduce load time
    straight-check-for-modifications '(check-on-save find-when-checking)
    straight-use-package-by-default t
    ;; There is no need to download the whole Git history, and a single branch often suffices.
    straight-vc-git-default-clone-depth '(1 single-branch))

  (let
    (
      (bootstrap-file
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
    '
    (use-package :source
      melpa)))

;; (unless (bound-and-true-p sb/disable-package.el)
;;   (with-eval-after-load 'package
;;     (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;     (add-to-list 'package-archives '("celpa" . "https://celpa.conao3.com/packages/") t)
;;     (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))

;;   ;; Initialise the package management system. Another option is to construct the `load-path'
;;   ;; manually, e.g., "(add-to-list 'load-path (concat package-user-dir "magit-20170715.1731"))".
;;   (package-initialize)

;;   (unless (package-installed-p 'use-package)
;;     (package-refresh-contents)
;;     (package-install 'use-package))

;;   ;; Avoid manual installations whenever I modify package installations
;;   (setq
;;     use-package-always-ensure t
;;     ;; These variables need to be set before loading `use-package'
;;     use-package-enable-imenu-support t
;;     use-package-hook-name-suffix nil)

;;   (eval-when-compile
;;     (require 'use-package)))

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

;; (use-package benchmark-init
;;   :when (and (eq sb/op-mode 'standalone) (bound-and-true-p sb/debug-init-file))
;;   :init (benchmark-init/activate)
;;   :hook (emacs-startup . benchmark-init/deactivate))

(use-package no-littering
  :demand t
  :custom (auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; (use-package package
;;   :unless (bound-and-true-p sb/disable-package.el)
;;   ;; "no-littering" places "package-quickstart.el" in `no-littering-expand-var-file-name'.
;;   :after no-littering
;;   :bind (("C-c d p" . package-quickstart-refresh) ("C-c d l" . package-list-packages))
;;   :custom (package-quickstart t))

(defcustom sb/custom-file (no-littering-expand-var-file-name "custom.el")
  "File to write Emacs customizations."
  :type 'string
  :group 'sb/emacs)

;; NOTE: Make a symlink to "private.el" in "$HOME/.emacs.d/etc".
(defcustom sb/private-file (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type 'string
  :group 'sb/emacs)

;; Asynchronously byte compile packages installed with `package.el'
;; (use-package async
;;   :straight (:host github :repo "jwiegley/emacs-async")
;;   :unless (bound-and-true-p sb/disable-package.el)
;;   :commands async-bytecomp-package-mode
;;   :init (async-bytecomp-package-mode 1))

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
  (prog-mode
    .
    (lambda ()
      (auto-fill-mode 1) ; Autofill comments
      ;; Native from Emacs 27+, disable in TUI since the line characters also get copied.
      (when (or (display-graphic-p) (daemonp))
        (display-fill-column-indicator-mode 1))))
  :custom
  (ad-redefinition-action 'accept "Turn off warnings due to redefinitions")
  (apropos-do-all t "Make `apropos' search more extensively")
  (auto-save-no-message t "Allows for debugging frequent autosave triggers if `nil'")
  (auto-save-interval 0 "Disable autosaving based on number of characters typed")
  (bookmark-save-flag 1 "Save bookmark after every bookmark edit and also when Emacs is killed")
  (case-fold-search t "Searches and matches should ignore case")
  (comment-auto-fill-only-comments t "Autofill comments modes that define them")
  (create-lockfiles nil)
  (custom-safe-themes t)
  (delete-by-moving-to-trash t "Use system trash to deal with mistakes while deleting")
  ;; (enable-local-variables :all "Avoid `defvar' warnings")
  (echo-keystrokes 0.1 "Show current key-sequence in minibuffer")
  ;; Allow invoking a command that requires candidate-selection when are already in the middle of
  ;; candidate-selection.
  (enable-recursive-minibuffers t)
  ;; Expand truncated ellipsis:suspension points in the echo area, useful to see more information
  (eval-expression-print-length 500)
  (frame-title-format (list '(buffer-file-name "%f" "%b") " - " invocation-name))
  (help-enable-symbol-autoload t)
  ;; Accelerate scrolling operations when non-nil. Only those portions of the buffer which are
  ;; actually going to be displayed get fontified.
  (fast-but-imprecise-scrolling t)
  (help-window-select t "Makes it easy to close the window")
  (history-delete-duplicates t)
  (history-length 50 "Reduce the state that is to be read")
  (indicate-buffer-boundaries nil)
  (message-log-max 5000)
  ;; (mouse-drag-copy-region nil "Mouse is disabled")
  ;; (mouse-yank-at-point t "Yank at point with mouse instead of at click")
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
  (completion-ignore-case t "Ignore case when completing")
  (read-buffer-completion-ignore-case t "Ignore case when reading a buffer name")
  (kill-do-not-save-duplicates t "Do not save duplicates to kill ring")
  (blink-matching-paren t)
  (suggest-key-bindings t)
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
  (scroll-error-top-bottom t)
  ;; The Emacs documentation warns about performance slowdowns with enabling remote directory
  ;; variables. I edit remote files mostly via TUI+SSH instead of Tramp.
  (enable-remote-dir-locals nil)
  ;; Unlike `auto-save-mode', `auto-save-visited-mode' saves the buffer contents to the visiting
  ;; file and runs all save-related hooks. We disable `auto-save-mode' and prefer
  ;; `auto-save-visited-mode' instead.
  (auto-save-default nil)
  ;; Save buffer to file after idling for some time, the default of 5s may be too frequent since
  ;; it runs all the save-related hooks.
  (auto-save-visited-interval 30)
  :config
  ;; Keep track of the minibuffer nesting
  (when (bound-and-true-p enable-recursive-minibuffers)
    (minibuffer-depth-indicate-mode 1))

  (dolist
    (exts '(".dll" ".exe" ".fdb_latexmk" ".fls" ".lof" ".pyc" ".rel" ".rip" ".synctex.gz" "TAGS"))
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
      '
      (("\\([^/]+\\)\\.c\\'" "\\1.h")
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
      '
      (auto-save-visited-mode ; Auto-save file-visiting buffers at idle time intervals
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

  ;; (when (display-graphic-p)
  ;;   ;; Show dividers on the right of each window, more prominent than the default
  ;;   (add-hook 'emacs-startup-hook #'window-divider-mode)

  ;;   ;; Default is 8 pixels, fringes do not work on the TUI. Having a fringe on the RHS seems
  ;;   ;; pointless.
  ;;   (fringe-mode '(10 . 0))

  ;;   ;; Cursor customizations do not work with TUI Emacs because the cursor style then is controlled by
  ;;   ;; the terminal application.
  ;;   (setq-default cursor-type 'box)
  ;;   (set-cursor-color "#ffffff") ; Set cursor color to white
  ;;   ;; Use a blinking bar for the cursor style to help identify it easily.
  ;;   (blink-cursor-mode 1))
  )

(setq
  kill-whole-line t ; TODO: What is the utility of this variable?
  ;; Scroll settings from Doom Emacs
  scroll-preserve-screen-position t
  scroll-margin 5 ; Add margin lines when scrolling vertically to have a sense of continuity
  ;; Emacs spends too much effort recentering the screen if you scroll the cursor more than N lines
  ;; past window edges, where N is the setting of `scroll-conservatively'. This is especially slow
  ;; in larger files during large-scale scrolling commands. If kept over 100, the window is never
  ;; automatically recentered.
  scroll-conservatively 101
  ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll' for tall lines
  auto-window-vscroll nil
  mouse-wheel-follow-mouse 't ; Scroll window under mouse
  mouse-wheel-progressive-speed nil ; Do not accelerate scrolling
  mouse-wheel-scroll-amount '(5 ((shift) . 2)))


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
  tab-width 4
  ;; TAB first tries to indent the current line, and if the line was already indented,
  ;; then try to complete the thing at point.
  tab-always-indent 'complete
  truncate-lines nil
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

(diminish 'visual-line-mode)
;; Not a library/file, so `eval-after-load' does not work
(diminish 'auto-fill-function)

;; Auto-refresh all buffers
(use-package autorevert
  :straight (:type built-in)
  :hook (emacs-startup . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-interval 5 "Faster (seconds) would mean less likely to use stale data")
  ;; Emacs seems to hang with auto-revert and Tramp, disabling this should be okay if we only
  ;; use Emacs. Enabling auto-revert is always safe.
  (auto-revert-remote-files t)
  ;; Revert only file-visiting buffers, set to non-nil value to revert dired buffers if the
  ;; contents of the directory changes
  (global-auto-revert-non-file-buffers t)
  (revert-without-query '("\\.*") "Revert all (e.g., PDF) files without asking")
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

(use-package uniquify
  :straight (:type built-in)
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "/")
  (uniquify-strip-common-suffix t))

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
  ;; `t' will use a popup menu rather than a minibuffer prompt, `on-mouse' might be useful with
  ;; mouse support enabled.
  (imenu-use-popup-menu nil)
  ;; `nil' implies no sorting and will list by position in the buffer
  (imenu-sort-function nil))

(use-package recentf
  :straight (:type built-in)
  :hook (emacs-startup . recentf-mode)
  :bind ("<f9>" . recentf-open-files)
  :custom
  (recentf-auto-cleanup 'never "Do not stat remote files")
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
  ;; Keep remote file without testing if they still exist
  (recentf-keep '(file-remote-p file-readable-p))
  ;; Larger values help in lookup but takes more time to check if the files exist
  (recentf-max-saved-items 250)
  :config
  ;; Abbreviate the home directory to "~/" to make it easy to read the actual file name.
  (unless sb/EMACS28+
    (setq recentf-filename-handlers '(abbreviate-file-name)))

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

(progn
  (defun sb/auto-save-wrapper (save-fn &rest args)
    "Hide 'Auto-saving...done' messages by calling the method.
  SAVE-FN with non-nil ARGS."
    (ignore args)
    (apply save-fn '(t)))

  (advice-add 'do-auto-save :around #'sb/auto-save-wrapper))

;; I use the "Shift+direction" keybindings for moving around windows in tmux which is okay because I
;; do not split Emacs frames often.
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

;; `text-mode' is the parent mode for `LaTeX-mode' and `org-mode', and so any hooks defined
;; will also get run for all modes derived from a basic mode such as `text-mode'.

;; Enabling `autofill-mode' makes it difficult to include long instructions verbatim, since they get
;; wrapped around automatically.
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Binds "C-x C-f" to `find-file-at-point' which will continue to work like `find-file' unless a
;; prefix argument is given. Then it will find file at point.
(use-package ffap
  :straight (:type built-in)
  ;; Consult does not provide intelligent file lookup, unlike `counsel'.
  :when (eq sb/minibuffer-completion 'vertico)
  :bind
  (("<f2>" . ffap)
    ([remap find-file] . find-file-at-point)
    ([remap find-file-read-only] . ffap-read-only)
    ([remap find-alternate-file] . ffap-alternate-file)
    ([remap dired] . dired-at-point)
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
;; (use-package goto-addr
;;   :straight (:type built-in)
;;   :hook ((prog-mode . goto-address-prog-mode) (text-mode . goto-address-mode))
;;   :bind ("C-c RET" . goto-address-at-point))

(use-package ediff
  :straight (:type built-in)
  :commands (ediff)
  :custom
  ;; Change default ediff style: do not start another frame with `ediff-setup-windows-default'
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split windows horizontally in ediff (instead of vertically)
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
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
  (tramp-default-user user-login-name)
  (tramp-default-remote-shell "/usr/bin/bash")
  (remote-file-name-inhibit-cache nil "Remote files are not updated outside of Tramp")
  (tramp-verbose 1)
  :config (defalias 'exit-tramp 'tramp-cleanup-all-buffers)
  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  ;; Include this directory in $PATH on remote
  (add-to-list 'tramp-remote-path (expand-file-name ".local/bin" (getenv "HOME")))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;; Recommended to connect with Bash
  (setenv "SHELL" shell-file-name)
  (setq debug-ignored-errors (cons 'remote-file-error debug-ignored-errors)))

;; LATER: Can we shorten long Tramp file names? This does not work with Tramp.
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/data/swarnendu/" . "/vindhya/data/swarnendu/"))
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/home/swarnendu/" . "/vindhya/home/swarnendu/"))

(use-package whitespace
  :hook
  (markdown-mode
    .
    (lambda ()
      (setq
        show-trailing-whitespace t
        whitespace-style
        '
        (face ; Visualize using faces
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

;; "M-x delete-trailing-whitespace" deletes trailing lines. This is different
;; from `whitespace-cleanup-mode' since this is unconditional.

;; (when (bound-and-true-p sb/delete-trailing-whitespace-p)
;;   (setq delete-trailing-lines t)
;;   (add-hook 'write-file-functions #'delete-trailing-whitespace)
;;   (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package ibuffer
  :straight (:type built-in)
  :hook (ibuffer . ibuffer-auto-mode)
  :bind (("C-x C-b" . ibuffer-jump) :map ibuffer-mode-map ("`" . ibuffer-switch-format))
  :custom
  (ibuffer-display-summary nil)
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-use-header-line t)
  (ibuffer-show-empty-filter-groups nil "Do not show empty groups if there are no buffers")
  :config (defalias 'list-buffers 'ibuffer))

;; Provides ibuffer filtering and sorting functions to group buffers by function or regexp applied
;; to `default-directory'. By default buffers are grouped by `project-current' or by
;; `default-directory'.
(use-package ibuffer-project
  :when (eq sb/project-handler 'project)
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

;; Group buffers by Projectile project
(use-package ibuffer-projectile
  :when (eq sb/project-handler 'projectile)
  :after projectile
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))

;; (use-package vlf ; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
;;   :demand t
;;   :commands vlf
;;   :init
;;   (setq vlf-application 'dont-ask)
;;   (require 'vlf-setup))

;; When the *scratch* buffer is killed, immediately respawn it
(use-package immortal-scratch
  :hook (emacs-startup . immortal-scratch-mode))

;; Helps to make the data in the "*scratch*" buffer persist
(use-package persistent-scratch
  :hook
  (emacs-startup
    .
    (lambda ()
      (ignore-errors
        (persistent-scratch-setup-default))))
  :config (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

(use-package popwin
  :hook (emacs-startup . popwin-mode)
  :config
  ;;   (push '("*Help*"              :noselect t)   popwin:special-display-config)
  ;;   (push '(compilation-mode      :noselect t)   popwin:special-display-config)
  ;;   (push '("*Compile-Log*"       :noselect t)   popwin:special-display-config)
  ;;   (push '("*manage-minor-mode*" :noselect t)   popwin:special-display-config)
  ;;   (push '("*Paradox Report*"    :noselect t)   popwin:special-display-config)
  ;;   (push '("*Selection Ring:")                  popwin:special-display-config)
  ;;   (push '("*Flycheck checkers*" :noselect nil) popwin:special-display-config)
  ;;   (push '(flycheck-error-list-mode :noselect nil) popwin:special-display-config)
  ;;   (push '("*ripgrep-search*"    :noselect nil) popwin:special-display-config)
  ;;   (push '("^\*magit:.+\*$"      :noselect nil) popwin:special-display-config)
  ;;   (push '("*xref*"              :noselect nil) popwin:special-display-config)
  (push '(helpful-mode :noselect t :position bottom :height 20) popwin:special-display-config)
  ;;   (push "*Shell Command Output*"               popwin:special-display-config)
  ;;   (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
  ;;   (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
  ;;   (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  ;;   (add-to-list 'popwin:special-display-config '("*Apropos*"))
  ;;   (add-to-list 'popwin:special-display-config '("*Warnings*"))
  ;;   (add-to-list 'popwin:special-display-config '("*prettier errors*"))
  ;;   (add-to-list 'popwin:special-display-config '("*explain-pause-top*"))
  ;;   (add-to-list 'popwin:special-display-config '(ivy-occur-grep-mode))
  (add-to-list 'popwin:special-display-config '(deadgrep-mode :noselect nil))
  ;;   (add-to-list 'popwin:special-display-config '("*lsp session*"))
  (add-to-list 'popwin:special-display-config '(comint-mode :noselect t))
  (add-to-list 'popwin:special-display-config '("*rg*" :noselect nil)))

;; (add-to-list 'display-buffer-alist '("\\magit:" (display-buffer-same-window)))
;; (add-to-list 'display-buffer-alist '("\\*Help" (display-buffer-same-window)))
;; (add-to-list 'display-buffer-alist '("\\*helpful" (display-buffer-same-window)))
;; (add-to-list
;;   'display-buffer-alist
;;   '
;;   ("\\*\\(Backtrace\\|Compile-log\\|Messages\\|Warnings\\)\\*"
;;     (display-buffer-in-side-window)
;;     (side . bottom)
;;     (slot . 0)
;;     (window-height . 0.33)
;;     (window-parameters (no-delete-other-windows . nil))))

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

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `before-save-hook' and `after-save-hook' and leads to auto-formatters being invoked more
;; frequently.
;; (use-package super-save
;;   :hook (emacs-startup . super-save-mode)
;;   :custom
;;   (super-save-remote-files nil "Ignore remote files, can cause Emacs to hang")
;;   (super-save-silent t)
;;   (super-save-delete-trailing-whitespace nil)
;;   ;; Enable deleting trailing white spaces before saving
;;   (super-save-exclude '(".gpg"))
;;   (super-save-auto-save-when-idle t)
;;   :config
;;   (add-to-list 'super-save-triggers 'ace-window)
;;   (add-to-list 'super-save-hook-triggers 'find-file-hook)
;;   :diminish)

(use-package dired
  :preface
  (defun sb/dired-go-home ()
    (interactive)
    (dired sb/user-home-directory))

  (defun sb/dired-jump-to-top ()
    (interactive)
    (goto-char (point-min)) ; Faster than `(beginning-of-buffer)'
    (dired-next-line 2))

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

;; Load `dired-x' when `dired' is loaded
(use-package dired-x
  :straight (:type built-in)
  :hook
  (dired-mode
    .
    (lambda ()
      (require 'dired-x)
      (dired-omit-mode)))
  :bind ("C-x C-j" . dired-jump)
  :custom
  (dired-cleanup-buffers-too t)
  (dired-omit-verbose nil "Do not show messages when omitting files")
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

;; Do not create multiple dired buffers

;; (use-package dired+
;;   :straight (:host github :repo "emacsmirror/dired-plus")
;;   :commands diredp-toggle-find-file-reuse-dir
;;   :init
;;   ;; Set before the module is loaded
;;   (setq diredp-bind-problematic-terminal-keys nil)
;;   (setq-local font-lock-maximum-decoration nil)
;;   :hook
;;   (dired-mode
;;     .
;;     (lambda ()
;;       (when sb/EMACS27
;;         (diredp-toggle-find-file-reuse-dir 1))))
;;   :custom
;;   (diredp-hide-details-initially-flag nil)
;;   (diredp-hide-details-propagate-flag nil))

;; (use-package dired-async
;;   :straight async
;;   :after (dired async)
;;   :hook (dired-mode . dired-async-mode)
;;   :diminish)

;; (use-package dired-rsync
;;   :after dired
;;   :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

;; (use-package dired-rsync-transient
;;   :after dired
;;   :bind (:map dired-mode-map ("C-c C-x" . dired-rsync-transient)))

;; (use-package diredfl :hook (dired-mode . diredfl-mode))

(use-package dired-hist
  :straight (:host github :repo "karthink/dired-hist")
  :hook (dired-mode . dired-hist-mode)
  :bind (:map dired-mode-map ("l" . dired-hist-go-back) ("r" . dired-hist-go-forward)))

;; Sort most-used commands and show keyboard shortcuts. `amx-major-mode-commands' limits to commands
;; that are relevant to the current major mode, `amx-show-unbound-commands' shows frequently used
;; commands that have no key bindings.
;; (use-package amx
;;   :when (eq sb/minibuffer-completion 'ivy) ; `amx' is not needed with `vertico'.
;;   :hook (emacs-startup . amx-mode))

;; (use-package ivy
;;   :when (eq sb/minibuffer-completion 'ivy)
;;   :hook (emacs-startup . ivy-mode)
;;   :bind
;;   (("C-c r" . ivy-resume)
;;     :map
;;     ivy-minibuffer-map
;;     ("RET" . ivy-alt-done) ; Continue completion
;;     ("<left>" . ivy-previous-line)
;;     ("<right>" . ivy-next-line)
;;     ([escape] . minibuffer-keyboard-quit))
;;   :custom
;;   (ivy-count-format "(%d/%d) " "Helps identify wrap around")
;;   (ivy-extra-directories nil "Hide . and ..")
;;   ;; Make the height of the minibuffer proportionate to the screen
;;   ;; (ivy-height-alist '((t lambda (_caller) (/ (frame-height) 2))))
;;   (ivy-fixed-height-minibuffer t "Distracting if the height keeps changing")
;;   (ivy-truncate-lines t) ; `counsel-flycheck' output gets truncated
;;   (ivy-wrap t "Easy to navigate")
;;   (ivy-initial-inputs-alist nil "Do not start searches with ^")
;;   (ivy-use-virtual-buffers nil "Do not show recent files in `switch-buffer'")
;;   (ivy-use-selectable-prompt t "Easier to edit names with common prefixes")
;;   (ivy-sort-max-size 50000 "Increase the limit to allow sorting")
;;   (ivy-flx-limit 1500 "Increase the limit to allow flex sorting")
;;   :config
;;   (dolist
;;     (buffer
;;       '
;;       ("TAGS" "magit-process" "*emacs*" "*xref*" "^\\*.+Completions\\*$" "^\\*Compile-Log\\*$"
;;         ;; "*eldoc for use-package*" "^\\*Help\\*$" "^\\*Ibuffer\\*$" "*Warnings*"
;;         ;; "^\\*Backtrace\\*$"
;;         ;; "*flycheck-posframe-buffer*" "^\\*prettier" "^\\*json*" "^\\*texlab*"
;;         ;; "^\\*clangd*" "^\\*shfmt*" "*company-documentation*"
;;         ))
;;     (add-to-list 'ivy-ignore-buffers buffer))

;;   ;; Other options: ivy--regex-ignore-order
;;   ;; (setq ivy-re-builders-alist '((counsel-rg        . ivy--regex-plus)
;;   ;;                               (counsel-M-x       . ivy--regex-fuzzy)
;;   ;;                               (counsel-find-file . ivy--regex-fuzzy)
;;   ;;                               (t                 . ivy--regex-plus)))

;;   ;; Ignore `dired' buffers from `ivy-switch-buffer'
;;   ;; https://github.com/abo-abo/swiper/wiki/Hiding-dired-buffers

;;   ;; (progn
;;   ;;   (defun sb/ignore-dired-buffers (str)
;;   ;;     "Return non-nil if STR names a Dired buffer.
;;   ;; This function is intended for use with `ivy-ignore-buffers'."
;;   ;;     (let ((buf (get-buffer str)))
;;   ;;       (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

;;   ;;   (add-to-list 'ivy-ignore-buffers #'sb/ignore-dired-buffers))

;;   (with-eval-after-load "savehist"
;;     (add-to-list 'savehist-additional-variables 'ivy-views))
;;   :diminish)

;; (use-package all-the-icons-ivy
;;   :when (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p))
;;   :after ivy
;;   :hook (emacs-startup . all-the-icons-ivy-setup))

;; (use-package counsel
;;   :preface
;;   ;; https://emacs.stackexchange.com/questions/33332/recursively-list-all-files-and-sub-directories
;;   (defun sb/counsel-all-files-recursively (dir-name)
;;     "List all files recursively in DIR-NAME."
;;     (interactive "DDirectory: ")
;;     (let*
;;       ((cands (split-string (shell-command-to-string (format "find %s -type f" dir-name)) "\n" t)))
;;       (ivy-read "File: " cands :action #'find-file :caller 'sb/counsel-all-files-recursively)))
;;   :when (eq sb/minibuffer-completion 'ivy)
;;   :hook (ivy-mode . counsel-mode)
;;   :bind
;;   ( ;; Counsel can use the sorting from `amx' for `counsel-M-x'.
;;     ([remap execute-extended-command] . counsel-M-x)
;;     ("<f1>" . counsel-M-x)
;;     ([remap completion-at-point] . counsel-company)
;;     ("C-M-i" . counsel-company)
;;     ([remap find-file] . counsel-find-file)
;;     ("<f2>" . counsel-find-file)
;;     ([remap dired] . counsel-dired)
;;     ([remap recentf-open-files] . counsel-recentf)
;;     ("<f9>" . counsel-recentf)
;;     ("C-c d m" . counsel-minor)
;;     ("C-c s g" . counsel-git)
;;     ("C-c s G" . counsel-git-grep)
;;     ("C-c s r" . counsel-rg)
;;     ("<f4>" . counsel-grep-or-swiper)
;;     ([remap locate] . counsel-locate)
;;     ("C-c s l" . counsel-locate)
;;     ([remap yank-pop] . counsel-yank-pop)
;;     ("M-y" . counsel-yank-pop)
;;     ("C-c C-m" . counsel-mark-ring)
;;     ("<f3>" . counsel-switch-buffer)
;;     ;; ("S-<f3>" . counsel-switch-buffer)
;;     ([remap imenu] . counsel-imenu)
;;     ("C-c C-j" . counsel-imenu)
;;     ([remap bookmark-jump] . counsel-bookmark)
;;     ([remap apropos] . counsel-apropos)
;;     ("M-g o" . counsel-outline)
;;     ([remap load-theme] . counsel-theme)
;;     ([remap load-library] . counsel-load-library)
;;     ("C-x j" . sb/counsel-all-files-recursively)
;;     ([remap compile] . counsel-compile)
;;     :map
;;     minibuffer-local-map
;;     ("C-c h" . counsel-minor-history))
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   (counsel-find-file-at-point t "Identify file at point")
;;   (counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
;;   (counsel-mode-override-describe-bindings t)
;;   (counsel-preselect-current-file t)
;;   (counsel-switch-buffer-preview-virtual-buffers nil)
;;   (counsel-yank-pop-preselect-last t)
;;   (counsel-yank-pop-separator "\n---------------------------------------------------\n")
;;   (ivy-height-alist
;;     '
;;     ((counsel-M-x . 15)
;;       (counsel-switch-buffer . 15)
;;       (counsel-yank-pop . 15)
;;       (swiper . 15)
;;       (swiper-isearch . 15)
;;       (counsel-recentf . 15)))
;;   :config
;;   (with-eval-after-load "savehist"
;;     (add-to-list 'savehist-additional-variables 'counsel-compile-history))
;;   :diminish)

;; Enable before `ivy-rich-mode' for better performance. The new transformers (file permissions)
;; seem an overkill, and it hides long file names.
;; (use-package all-the-icons-ivy-rich
;;   :hook (ivy-mode . all-the-icons-ivy-rich-mode)
;;   :custom
;;   (all-the-icons-ivy-rich-icon nil "Disable icons")
;;   (all-the-icons-ivy-rich-icon-size 0.9)
;;   :config
;;   (plist-put
;;     all-the-icons-ivy-rich-display-transformers-list 'counsel-recentf
;;     '
;;     (:columns
;;       ((all-the-icons-ivy-rich-file-icon)
;;         (all-the-icons-ivy-rich-file-name (:width 0.70))
;;         (all-the-icons-ivy-rich-file-id
;;           (:width 10 :face all-the-icons-ivy-rich-file-owner-face :align right))
;;         (ivy-rich-file-last-modified-time (:face all-the-icons-ivy-rich-time-face)))
;;       :delimiter "\t"))

;;   (plist-put
;;     all-the-icons-ivy-rich-display-transformers-list 'counsel-find-file
;;     '
;;     (:columns
;;       ((all-the-icons-ivy-rich-file-icon)
;;         (all-the-icons-ivy-rich-file-name (:width 0.4))
;;         (all-the-icons-ivy-rich-file-id
;;           (:width 15 :face all-the-icons-ivy-rich-file-owner-face :align right)))
;;       :delimiter "\t"))

;;   (plist-put
;;     all-the-icons-ivy-rich-display-transformers-list 'ivy-switch-buffer
;;     '
;;     (:columns
;;       ((all-the-icons-ivy-rich-buffer-icon)
;;         (ivy-rich-candidate (:width 30))
;;         (ivy-rich-switch-buffer-indicators
;;           (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
;;         (all-the-icons-ivy-rich-switch-buffer-major-mode
;;           (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
;;         (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
;;         (ivy-rich-switch-buffer-path
;;           (:width
;;             (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))
;;             :face all-the-icons-ivy-rich-path-face)))
;;       :predicate (lambda (cand) (get-buffer cand))
;;       :delimiter "\t"))

;;   (with-eval-after-load "projectile"
;;     (plist-put
;;       all-the-icons-ivy-rich-display-transformers-list 'projectile-completing-read
;;       '
;;       (:columns
;;         ((all-the-icons-ivy-rich-file-icon)
;;           (all-the-icons-ivy-rich-project-find-file-transformer (:width 0.4))
;;           (all-the-icons-ivy-rich-project-file-id
;;             (:width 15 :face all-the-icons-ivy-rich-file-owner-face :align right)))
;;         :delimiter "\t"))))

;; Icons and columns are sometimes unaligned
;; (use-package nerd-icons-ivy-rich
;;   :hook (ivy-mode . nerd-icons-ivy-rich-mode)
;;   :custom
;;   (nerd-icons-ivy-rich-icon
;;     (if (eq sb/icons-provider 'nerd-icons)
;;       t
;;       nil))
;;   (nerd-icons-ivy-rich-icon-size 1.0)
;;   :config
;;   (plist-put
;;     nerd-icons-ivy-rich-display-transformers-list 'counsel-recentf
;;     '
;;     (:columns
;;       ((nerd-icons-ivy-rich-file-icon)
;;         (nerd-icons-ivy-rich-file-name (:width 0.75))
;;         (nerd-icons-ivy-rich-file-id
;;           (:width 10 :face nerd-icons-ivy-rich-file-owner-face :align right))
;;         (ivy-rich-file-last-modified-time (:face nerd-icons-ivy-rich-time-face)))
;;       :delimiter "\t"))

;;   (plist-put
;;     nerd-icons-ivy-rich-display-transformers-list 'counsel-find-file
;;     '
;;     (:columns
;;       ((nerd-icons-ivy-rich-file-icon)
;;         (nerd-icons-ivy-rich-file-name (:width 0.6))
;;         (nerd-icons-ivy-rich-file-id
;;           (:width 15 :face nerd-icons-ivy-rich-file-owner-face :align right))
;;         (ivy-rich-file-last-modified-time (:face nerd-icons-ivy-rich-time-face)))
;;       :delimiter "\t"))

;;   (plist-put
;;     nerd-icons-ivy-rich-display-transformers-list 'ivy-switch-buffer
;;     '
;;     (:columns
;;       ((nerd-icons-ivy-rich-buffer-icon)
;;         (ivy-rich-candidate (:width 30))
;;         (ivy-rich-switch-buffer-indicators
;;           (:width 4 :face nerd-icons-ivy-rich-indicator-face :align right))
;;         (nerd-icons-ivy-rich-switch-buffer-major-mode
;;           (:width 18 :face nerd-icons-ivy-rich-major-mode-face))
;;         (ivy-rich-switch-buffer-project (:width 0.12 :face nerd-icons-ivy-rich-project-face))
;;         (ivy-rich-switch-buffer-path
;;           (:width
;;             (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))
;;             :face nerd-icons-ivy-rich-path-face)))
;;       :predicate (lambda (cand) (get-buffer cand))
;;       :delimiter "\t"))

;;   (with-eval-after-load "projectile"
;;     (plist-put
;;       nerd-icons-ivy-rich-display-transformers-list 'projectile-completing-read
;;       '
;;       (:columns
;;         ((nerd-icons-ivy-rich-file-icon)
;;           (nerd-icons-ivy-rich-project-find-file-transformer (:width 0.6))
;;           (nerd-icons-ivy-rich-project-file-id
;;             (:width 15 :face nerd-icons-ivy-rich-file-owner-face :align right))
;;           (nerd-icons-ivy-rich-project-file-size (:width 7 :face nerd-icons-ivy-rich-size-face)))
;;         :delimiter "\t")))

;;   (nerd-icons-ivy-rich-reload))

;; (use-package ivy-rich
;;   :preface
;;   (defun sb/ivy-rich-file-size (candidate)
;;     "Displays the file size of the candidate for ivy-rich."
;;     (let ((candidate (expand-file-name candidate ivy--directory)))
;;       (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
;;         ""
;;         (let ((size (file-attribute-size (file-attributes candidate))))
;;           (cond
;;             ((> size 1000000)
;;               (format "%.1fM " (/ size 1000000.0)))
;;             ((> size 1000)
;;               (format "%.1fk " (/ size 1000.0)))
;;             (t
;;               (format "%d " size)))))))

;;   (defun sb/ivy-rich-file-user (candidate)
;;     "Displays the file user of the candidate for ivy-rich."
;;     (let ((candidate (expand-file-name candidate ivy--directory)))
;;       (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
;;         ""
;;         (let*
;;           (
;;             (user-id (file-attribute-user-id (file-attributes candidate)))
;;             (user-name (user-login-name user-id)))
;;           (format "%s" user-name)))))
;;   :after counsel
;;   :init (ivy-rich-mode 1)
;;   :custom (ivy-rich-parse-remote-buffer nil)
;;   :config (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow-line)

;;   (ivy-rich-project-root-cache-mode 1)

;;   (ivy-rich-set-columns
;;     'counsel-find-file
;;     '
;;     ((ivy-rich-candidate (:width 0.80))
;;       (sb/ivy-rich-file-size (:width 10 :align right :face font-lock-doc-face))))

;;   ;; Increase the width to see the major mode clearly
;;   (ivy-rich-modify-columns
;;     'ivy-switch-buffer
;;     '
;;     ((ivy-rich-switch-buffer-size (:align right))
;;       (ivy-rich-switch-buffer-major-mode (:width 16 :face error))
;;       (ivy-rich-switch-buffer-project (:width 0.24 :face success))))

;;   ;; (ivy-rich-modify-columns
;;   ;;   'counsel-switch-buffer
;;   ;;   '
;;   ;;   ((ivy-rich-switch-buffer-size (:align right))
;;   ;;     (ivy-rich-switch-buffer-major-mode (:width 16 :face error))
;;   ;;     (ivy-rich-switch-buffer-project (:width 0.20 :face success))))

;;   (ivy-rich-set-columns
;;     'counsel-recentf
;;     '((file-name-nondirectory (:width 0.30)) (ivy-rich-candidate (:width 0.80)))))

;; (use-package counsel-fd
;;   :when (and (eq sb/minibuffer-completion 'ivy) (executable-find "fd"))
;;   :bind
;;   (("C-x d" . counsel-fd-dired-jump) ; Jump to a directory below the current directory
;;     ;; Jump to a file below the current directory
;;     ("C-x f" . counsel-fd-file-jump)))

;; This package adds a "C-'" binding to the Ivy minibuffer that uses Avy

;; (use-package ivy-avy
;;   :after ivy
;;   :bind (:map ivy-minibuffer-map ("C-'" . ivy-avy)))

;; (use-package ivy-yasnippet
;;   :when (eq sb/minibuffer-completion 'ivy)
;;   :after (ivy yasnippet)
;;   :bind ("C-M-y" . ivy-yasnippet))

;; (use-package lsp-ivy
;;   :after (lsp-mode ivy)
;;   :bind (:map lsp-command-map ("G" . lsp-ivy-global-workspace-symbol) ("W" . lsp-ivy-workspace-symbol)))

;; (use-package ivy-xref
;;   :after (ivy xref)
;;   :demand t
;;   :custom
;;   (xref-show-definitions-function #'ivy-xref-show-defs)
;;   (xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package counsel-tramp
;;   :after counsel
;;   :bind ("C-c d t" . counsel-tramp))

;; (use-package ivy-bibtex
;;   :when (eq sb/minibuffer-completion 'ivy)
;;   :after latex
;;   :commands ivy-bibtex
;;   ;; :bind ("C-c x b" . ivy-bibtex)
;;   :custom (ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
;;   :config (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

;;   (require 'bibtex-completion)

;;   (setq
;;     bibtex-completion-cite-default-as-initial-input t
;;     bibtex-completion-cite-prompt-for-optional-arguments nil
;;     bibtex-completion-display-formats '((t . "${author:24} ${title:*} ${=key=:16} ${=type=:12}"))))

;; "M-n" will search for the word under cursor, "C-s" will search for the next occurrence, "C-s"
;; will search for a previously searched string.

;; (use-package swiper
;;   :when (eq sb/minibuffer-completion 'ivy)
;;   :commands (swiper swiper-isearch swiper-isearch-thing-at-point)
;;   :custom
;;   (swiper-action-recenter t)
;;   (swiper-verbose nil))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*") :includes (vertico-directory vertico-repeat))
  :when (eq sb/minibuffer-completion 'vertico)
  :hook (emacs-startup . vertico-mode)
  :bind (:map vertico-map ("M-<" . vertico-first) ("M->" . vertico-last) ("C-M-j" . vertico-exit-input))
  :custom (vertico-cycle t)
  ;; :config
  ;; (cond
  ;;  ;;  ((eq sb/theme 'modus-vivendi)
  ;;  ;;   (set-face-attribute 'vertico-current nil :background "#384551" :inherit t))
  ;;  ((eq sb/theme 'modus-operandi)
  ;;   (set-face-attribute 'vertico-current nil :background "#E6F2FF" :inherit t))
  ;;  ;;  ((eq sb/theme 'standard-light)
  ;;  ;;   (set-face-attribute 'vertico-current nil :background "#9AB8C4" :inherit t))
  ;;  (t (set-face-attribute 'vertico-current nil :background "#3A3F5A" :inherit t))
  ;;  )
  )

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

;; (use-package vertico-indexed ; Select candidates by number with "C-u number RET"
;;   :straight nil
;;   :after vertico
;;   :init (vertico-indexed-mode 1))

(use-package vertico-quick
  :straight nil
  :after vertico
  :bind (:map vertico-map ("C-c q" . vertico-quick-insert) ("C-'" . vertico-quick-jump)))

;; (use-package vertico-multiform
;;   :straight nil
;;   :after vertico
;;   :init (vertico-multiform-mode 1)
;;   :custom
;;   (vertico-multiform-categories '((embark-keybinding grid)))
;;   (vertico-multiform-commands
;;     '
;;     ( ;; (execute-extended-command indexed)
;;       ;; (completion-at-point vertical)
;;       ;; (consult-imenu buffer indexed)
;;       ;; (ffap flat (vertico-cycle . t))
;;       ;; (consult-projectile-switch-project grid)
;;       ;; (consult-yank-pop indexed)
;;       ;; (embark-bindings buffer)
;;       ;; (xref-find-references buffer)
;;       ;; (find-file-at-point (vertico-sort-function . sort-directories-first))
;;       ;; (consult-line buffer)
;;       ;; (consult-grep buffer)
;;       ;; (consult-git-grep buffer)
;;       ;; (consult-ripgrep buffer)
;;       )))

;; Press "SPC" to show ephemeral buffers, "b SPC" to filter by buffers, "f SPC" to filter by
;; files, "p SPC" to filter by projects. If you press "DEL" afterwards, the full candidate list
;; will be shown again.
(use-package consult
  :after vertico
  :commands consult-fd
  ;; Enable automatic preview at point in the *Completions* buffer. This is relevant when you use
  ;; the default completion UI.
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (("<f1>" . execute-extended-command)
    ;; ("C-x M-:" . consult-complex-command)
    ;; ([remap repeat-complex-command] . consult-complex-command)
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
    ;; ("M-g I" . consult-imenu-multi)
    ;; ([remap flymake-show-diagnostic] . consult-flymake)
    ;; ([remap flymake-show-buffer-diagnostics] . consult-flymake)
    ;; ([remap flymake-show-diagnostics-buffer] . consult-flymake)
    ([remap customize] . consult-customize)
    ([remap load-theme] . consult-theme)
    ;; ("C-c h" . consult-history)
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
    ;; ([remap isearch-forward] . consult-line)
    ("<f4>" . consult-line)
    ([remap multi-occur] . consult-multi-occur)
    ;; ("M-s m" . consult-multi-occur)
    ([remap recentf-open-files] . consult-recent-file)
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
      (buffer-substring-no-properties (region-beginning) (region-end))))

  (with-eval-after-load "projectile"
    (setq consult-project-function (lambda (_) (projectile-project-root)))
    (bind-key [remap projectile-ripgrep] #'consult-ripgrep)
    (bind-key [remap projectile-grep] #'consult-grep)))

;; Adds support for exporting a list of search results to a `grep-mode' buffer, on which you can use
;; `wgrep'
(use-package embark-consult
  :after consult
  :demand t)

;; Provide context-dependent actions similar to a content menu. Embark is likely not required with
;; ivy.
(use-package embark
  :bind
  (([remap describe-bindings] . embark-bindings)
    ("C-`" . embark-act)
    ;; ("C-`" . embark-dwim)
    :map
    minibuffer-local-map
    ("C-`" . embark-act)
    ("C-c C-;" . embark-export)
    ("C-c C-l" . embark-collect)
    :map
    minibuffer-local-completion-map
    ("C-`" . embark-act)
    :map
    embark-file-map
    ("s" . sudo-edit)
    ("l" . vlf))
  :custom
  ;; Replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  :config (define-key embark-identifier-map "y" #'symbol-overlay-put)

  (define-key embark-identifier-map "-" #'string-inflection-cycle)
  (add-to-list 'embark-repeat-actions #'string-inflection-cycle)

  (with-eval-after-load "vertico"
    (bind-keys :map vertico-map ("M-o" . embark-act) ("C-c C-l" . embark-export)))

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
      (if-let
        (
          targets
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

(use-package consult-eglot
  :when (eq sb/lsp-provider 'eglot)
  :after (consult eglot)
  :commands consult-eglot-symbols)

;; (use-package consult-project-extra
;;   :after (project consult)
;;   :bind (:map project-prefix-map ("z" . consult-project-extra-find))
;;   :config
;;   ;; (add-to-list 'project-switch-commands '(consult-project-extra-find "Find file" ?f))
;;   ;; (add-to-list 'project-switch-commands '(consult-project-buffer "Buffer"))
;;   (setq project-switch-commands 'consult-project-extra-find))

(use-package consult-jump-project
  :straight (:host github :repo "jdtsmith/consult-jump-project")
  :when (and (eq sb/minibuffer-completion 'vertico) (eq sb/project-handler 'project))
  :custom (consult-jump-direct-jump-modes '(dired-mode))
  :bind (("C-x p j" . consult-jump-project) ("<f6>" . consult-jump-project)))

(use-package consult-dir
  :after consult
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
  :after consult
  :bind ("C-M-y" . consult-yasnippet))

;; ":after (consult projectile)" prevents binding `consult-projectile-switch-project'
(use-package consult-projectile
  :when (and (eq sb/minibuffer-completion 'vertico) (eq sb/project-handler 'projectile))
  :bind
  (("<f5>" . consult-projectile-switch-project)
    ("<f6>" . consult-projectile)
    ([remap projectile-recentf] . consult-projectile-recentf)
    ([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer)
    ([remap projectile-find-file] . consult-projectile-find-file)
    ([remap projectile-find-dir] . consult-projectile-find-dir)
    ([remap projectile-switch-project] . consult-projectile-switch-project))
  :config (consult-customize consult-projectile :preview-key nil))

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
          ispell-local-dictionary "en_US"
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
          ispell-local-dictionary "en_US"
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
    ;; `find-file-hook' will not work for buffers with no associated files. I have this commented
    ;; because the `*scratch*' buffer is currently not using `text-mode'.
    ;; (emacs-startup
    ;;   .
    ;;   (lambda ()
    ;;     (when (string= (buffer-name) "*scratch*")
    ;;       (flyspell-mode 1))))
    (text-mode . flyspell-mode))
  :bind (("C-c f f" . flyspell-mode) ("C-c f b" . flyspell-buffer))
  :custom
  (flyspell-abbrev-p t "Add corrections to abbreviation table")
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :diminish)

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; (use-package flyspell-popup
;;   :after flyspell
;;   :bind
;;   (:map flyspell-mode-map
;;         ("C-;" . flyspell-popup-correct))
;;   :custom (flyspell-popup-correct-delay 0.1))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-at-point) ("C-," . flyspell-correct-previous)))

;; As of Emacs 29, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.

;; (use-package spell-fu
;;   :when (or (executable-find "aspell") (executable-find "hunspell"))
;;   :commands spell-fu-mode
;;   :init
;;   (add-hook
;;     'text-mode-hook
;;     (lambda ()
;;       (setq spell-fu-faces-exclude
;;         '
;;         (hl-line
;;           ;; `nxml-mode' is derived from `text-mode'
;;           nxml-attribute-local-name))
;;       (spell-fu-mode)))

;;   (add-hook
;;     'org-mode-hook
;;     (lambda ()
;;       (setq spell-fu-faces-exclude
;;         '
;;         (org-block
;;           org-block-begin-line
;;           org-block-end-line
;;           org-cite
;;           org-cite-key
;;           org-code
;;           org-date
;;           org-footnote
;;           org-formula
;;           org-latex-and-related
;;           org-link
;;           org-meta-line
;;           org-property-value
;;           org-ref-cite-face
;;           org-special-keyword
;;           org-tag
;;           org-todo
;;           org-todo-keyword-done
;;           org-todo-keyword-habt
;;           org-todo-keyword-kill
;;           org-todo-keyword-outd
;;           org-todo-keyword-todo
;;           org-todo-keyword-wait
;;           org-verbatim
;;           org-modern-tag
;;           hl-line))
;;       (spell-fu-mode)))

;;   (add-hook
;;     'markdown-mode-hook
;;     (lambda ()
;;       (setq spell-fu-faces-exclude
;;         '
;;         (markdown-blockquote-face
;;           markdown-code-face
;;           markdown-html-attr-name-face
;;           markdown-html-attr-value-face
;;           markdown-html-tag-name-face
;;           markdown-inline-code-face
;;           markdown-link-face
;;           markdown-markup-face
;;           markdown-plain-url-face
;;           markdown-reference-face
;;           markdown-url-face
;;           pandoc-citation-key-face
;;           hl-line))
;;       (spell-fu-mode)))

;;   (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
;;     (add-hook
;;       hook
;;       (lambda ()
;;         (setq spell-fu-faces-exclude
;;           '
;;           (font-latex-math-face
;;             font-latex-sedate-face
;;             font-lock-function-name-face
;;             font-lock-keyword-face
;;             font-lock-variable-name-face
;;             hl-line))
;;         (spell-fu-mode))))
;;   :bind
;;   (("C-c f n" . spell-fu-goto-next-error)
;;     ("C-c f p" . spell-fu-goto-previous-error)
;;     ("C-c f a" . spell-fu-word-add))
;;   :custom (spell-fu-directory (expand-file-name "spell-fu" no-littering-var-directory))
;;   :config
;;   ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/64
;;   (add-to-list 'spell-fu-faces-include 'font-lock-string-face)
;;   (add-to-list 'spell-fu-faces-include 'font-lock-doc-face)
;;   (add-to-list 'spell-fu-faces-include 'font-lock-comment-face)
;;   (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:comment)
;;   (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:doc)
;;   (add-to-list 'spell-fu-faces-include 'tree-sitter-hl-face:string)

;;   ;; Ignore read-only buffers
;;   (setq global-spell-fu-ignore-buffer (lambda (buf) (buffer-local-value 'buffer-read-only buf))))

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

;; This does not seem to be useful given that I am also using whole-line-or-region.
;; (use-package expand-line
;;   :bind ("M-i" . turn-on-expand-line-mode)
;;   :diminish)

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  :hook (emacs-startup . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :hook (emacs-startup . whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode)

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

;; The real beginning and end of buffers (i.e., `point-min' and `point-max') are accessible by
;; pressing the keys "M-<" and "M->" keys again.
;; (use-package beginend
;;   :hook (emacs-startup . beginend-global-mode)
;;   :config
;;   (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
;;     (diminish mode)))

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
      '
      (("LATER" . "#d0bf8f")
        ("IMP" . "#7cb8bb")
        ("ISSUE" . "#ff8c00")
        ("DEBUG" . "#ff8c00")
        ("TEST" . "tomato")
        ("WARNING" . "#cc0000")
        ("BEWARE" . "#aa0000")
        ("REFACTOR" . "#cc9393"))
      hl-todo-keyword-faces)))

(use-package highlight-numbers
  :hook
  (
    (prog-mode
      yaml-mode
      yaml-ts-mode
      conf-mode
      css-mode
      css-ts-mode
      html-mode
      html-ts-mode
      web-mode)
    . highlight-numbers-mode))

;; Display ugly "^L" page breaks as tidy horizontal lines
(use-package page-break-lines
  :hook (emacs-startup . global-page-break-lines-mode)
  :diminish)

;; https://emacs.stackexchange.com/questions/19686/how-to-use-pdf-tools-pdf-view-mode-in-emacs
;; Use `isearch', `swiper' will not work

;; (use-package pdf-tools
;;   :when (display-graphic-p)
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
;; (use-package define-word
;;   :commands define-word
;;   :bind ("C-c w" . define-word-at-point))

;; (use-package esup
;;   :when (bound-and-true-p sb/debug-init-file)
;;   :commands (esup))

;; (use-package bug-hunter
;;   :commands (bug-hunter-init-file bug-hunter-file))

;; Save a bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark, use `bookmark-jump'
;; ("C-x r b") or `bookmark-bmenu-list' ("C-x r l"). Rename the bookmarked location in
;; `bookmark-bmenu-mode' with `R'.
(use-package bm
  :init
  ;; Must be set before `bm' is loaded
  (setq
    bm-restore-repository-on-load t
    bm-verbosity-level 1
    bm-modeline-display-total t)
  :hook
  (
    (kill-emacs
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
  :hook ((LaTeX-mode css-mode css-mode-hook html-mode html-ts-mode web-mode help-mode) . rainbow-mode)
  :diminish)

;; (use-package volatile-highlights
;;   :hook (emacs-startup . volatile-highlights-mode)
;;   :diminish volatile-highlights-mode)

;; (use-package unfill
;;   :commands (unfill-region unfill-paragraph unfill-toggle))

(use-package xclip
  :when (or (executable-find "xclip") (executable-find "xsel"))
  :hook (emacs-startup . xclip-mode))

(use-package fix-word
  :bind (("M-u" . fix-word-upcase) ("M-l" . fix-word-downcase) ("M-c" . fix-word-capitalize)))

(use-package string-inflection
  ;;   :bind (:map prog-mode-map ("C-c C-u" . string-inflection-all-cycle))
  )

;; Allow GC to happen after a period of idle time
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :diminish)

(use-package kill-file-path
  :straight (:host github :repo "chyla/kill-file-path")
  :commands
  (kill-file-path-basename
    kill-file-path-basename-without-extension
    kill-file-path-dirname
    kill-file-path))

;; (use-package change-inner
;;   :commands (change-inner change-outer yank-inner yank-outer))

;; (use-package link-hint
;;   :bind ("C-c C-l" . link-hint-open-link))

;; Call `whitespace-cleanup' only if the initial buffer was clean. This mode works on the entire
;; file unlike `ws-butler'. To enable the mode for an entire project, set `whitespace-cleanup-mode'
;; to `t' in the `.dir-locals.el' file.

;; (use-package whitespace-cleanup-mode
;;   :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
;;   :custom
;;   (whitespace-cleanup-mode-preserve-point t)
;;   (whitespace-cleanup-mode-only-if-initially-clean t)
;;   :config (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)
;;   :diminish)

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :diminish)

;; Both project.el and projectile are unable to remember remote projects.
(use-package project
  :when (eq sb/project-handler 'project)
  :bind-keymap ("C-c p" . project-prefix-map)
  :bind
  (("<f5>" . project-switch-project)
    ;; ("<f6>" . project-find-file)
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
  :custom (project-switch-commands 'project-find-file "Start `project-find-file' by default"))

(use-package projection
  :when (eq sb/project-handler 'project)
  :after project
  :init (global-projection-hook-mode 1))

;; The contents of ".projectile" are ignored and files are not sorted when using the `alien' project
;; indexing.
(use-package projectile
  :when (eq sb/project-handler 'projectile)
  ;; We can open a project file without enabling projectile via `bind-keys'
  :hook (emacs-startup . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind
  (([remap project-switch-to-buffer] . projectile-switch-to-buffer)
    ([remap project-compile] . projectile-compile-project)
    ([remap project-find-dir] . projectile-find-dir)
    ([remap project-dired] . projectile-dired)
    ([remap project-find-file] . projectile-find-file)
    ([remap project-kill-buffers] . projectile-kill-buffers)
    ([remap project-switch-project] . projectile-switch-project)
    ([remap project-vc-dir] . projectile-vc)
    ([remap project-forget-project] . projectile-remove-known-project)
    ("C-c p A" . projectile-add-known-project)
    ("C-c p F" . projectile-find-other-file)
    :map
    projectile-command-map
    ("A" . projectile-add-known-project)
    ("F" . projectile-find-other-file))
  :custom
  (projectile-file-exists-remote-cache-expire nil)
  (projectile-mode-line-prefix "" "Save modeline space")
  (projectile-require-project-root t "Use only in desired directories, too much noise otherwise")
  ;; No sorting is faster. Files are not sorted if `projectile-indexing-method' is set to `alien'.
  (projectile-sort-order 'recently-active)
  (projectile-verbose nil)
  ;; The topmost file in a hierarchy has precedence
  (projectile-project-root-files
    '
    ("GTAGS"
      "TAGS"
      "setup.py"
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

  (defun sb/projectile-do-not-visit-tags-table ()
    "Do not visit the tags table automatically even if it is present."
    nil)
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
        ".swp"
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
    ("C-M-f") ; Was bound to `isearch-forward-regexp', but we use it for `forward-sexp'
    ("C-f" . isearch-forward-regexp) ("C-r" . isearch-backward-regexp)
    :map isearch-mode-map ("C-s") ("C-f" . isearch-repeat-forward) ("C-c C-o" . isearch-occur))
  :custom
  (search-highlight t "Highlight incremental search")
  (isearch-lazy-highlight t)
  (isearch-lazy-count t "Show match count next to the minibuffer prompt"))

;; Auto populate `isearch' with the symbol at point
(use-package isearch-symbol-at-point
  :after isearch
  :commands (isearch-forward-symbol-at-point isearch-backward-symbol-at-point)
  :bind (("M-s ." . isearch-symbol-at-point) ("M-s _" . isearch-forward-symbol)))

(use-package cc-isearch-menu
  :bind (:map isearch-mode-map ("<f2>" . cc-isearch-menu-transient)))

;; (use-package anzu
;;   :init
;;   (setq
;;     anzu-search-threshold 10000
;;     anzu-minimum-input-length 2)
;;   (global-anzu-mode 1)
;;   :bind ([remap query-replace-regexp] . anzu-query-replace-regexp)
;;   :diminish anzu-mode)

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

(use-package deadgrep
  :bind ("C-c s d" . deadgrep)
  :custom (deadgrep-max-buffers 1))

;; `avy-setup-default' will bind `avy-isearch' to "C-'" in `isearch-mode-map', so that you can
;; select one of the currently visible `isearch' candidates using `avy'.
(use-package avy
  :bind
  (("C-\\" . avy-goto-word-1)
    ("C-'" . avy-goto-char-timer)
    ("C-/" . avy-goto-line)
    ("C-M-g" . avy-copy-line)
    ("C-M-G" . avy-move-line)
    :map isearch-mode-map
    ;; Use "C-'" in `isearch-mode-map' to use `avy-isearch' to select one of the many currently
    ;; visible `isearch' candidates.
    ("C-'" . avy-isearch))
  :custom (avy-background t "Provides better contrast"))

(use-package re-builder
  :commands re-builder
  :custom (reb-re-syntax 'string))

;; Package `visual-regexp' provides an alternate version of `query-replace' which highlights matches
;; and replacements as you type.
(use-package visual-regexp
  :bind
  ([remap query-replace] . vr/query-replace)
  ([remap replace-regex] . vr/replace))

;; (use-package ripgrep
;;   :commands (ripgrep-regexp projectile-ripgrep))

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
  :custom
  ;; Disabling vc is said to improve performance. However, I find it useful to show branch
  ;; information on the modeline and highlight modifications in the current file.
  (vc-handled-backends '(Git))
  (vc-follow-symlinks t "No need to ask")
  ;; Disable version control for remote files to improve performance
  (vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)))

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

(use-package magit-todos
  :after magit
  :demand t
  :config (magit-todos-mode 1))

(use-package difftastic
  :after magit
  :demand t
  :bind (:map magit-blame-read-only-mode-map ("D" . difftastic-magit-show) ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '
    (transient-append-suffix
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
  :custom
  (diff-hl-draw-borders nil "Highlight without a border looks nicer")
  (diff-hl-disable-on-remote t)
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

;; (use-package with-editor :diminish)

(use-package paren
  :straight (:type built-in)
  :custom
  (show-paren-style 'parenthesis) ; `mixed' may lead to performance problems
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package elec-pair
  :straight (:type built-in)
  :hook
  ((emacs-startup . electric-pair-mode)
    ;; Disable pairs when entering minibuffer
    (minibuffer-setup . (lambda () (electric-pair-local-mode -1)))
    ;; Re-enable pairs when existing minibuffer
    ;; (minibuffer-exit . (lambda () (electric-pair-mode 1)))
    )
  :custom
  ;; Avoid balancing parentheses since they can be both irritating and slow
  (electric-pair-preserve-balance nil)
  :config
  (defvar sb/markdown-pairs '((?` . ?`))
    "Electric pairs for `markdown-mode'.")

  (defun sb/add-markdown-pairs ()
    "Add custom pairs to `markdown-mode'."
    (setq-local electric-pair-pairs (append electric-pair-pairs sb/markdown-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (add-hook 'markdown-mode-hook #'sb/add-markdown-pairs))

;; `sp-cheat-sheet' will show you all the commands available, with examples.
;; (use-package smartparens
;;   :hook
;;   ((minibuffer-setup . turn-on-smartparens-strict-mode)
;;     (emacs-startup
;;       .
;;       (lambda ()
;;         (smartparens-global-mode 1)
;;         (show-smartparens-global-mode 1))))
;;   :bind
;;   (("C-M-a" . sp-beginning-of-sexp) ; "foo ba_r" -> "_foo bar"
;;     ("C-M-e" . sp-end-of-sexp) ; "f_oo bar" -> "foo bar_"
;;     ("C-M-u" . sp-up-sexp) ; "f_oo bar" -> "foo bar"_
;;     ("C-M-w" . sp-down-sexp) ; "foo ba_r" -> "_foo bar"
;;     ("C-M-f" . sp-forward-sexp) ; "foo ba_r" -> "foo bar"_
;;     ("C-M-b" . sp-backward-sexp) ; "foo ba_r" -> "_foo bar"
;;     ("C-M-n" . sp-next-sexp) ; ))" -> ((foo) (bar))"
;;     ("C-M-p" . sp-previous-sexp) ; "(foo (b|ar baz))" -> "(foo| (bar baz))"
;;     ("C-S-b" . sp-backward-symbol) ; "foo bar| baz" -> "foo |bar baz"
;;     ("C-S-f" . sp-forward-symbol) ; "|foo bar baz" -> "foo| bar baz"
;;     ("C-M-k" . sp-splice-sexp) ; "(foo bar)" -> "foo bar"
;;     ;; "foo(2,3)" -> "foo[2,3]"
;;     ("C-M-r" . sp-rewrap-sexp))

;; Discover key bindings for the current Emacs major mode
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

(use-package mode-minder
  :straight (:host github :repo "jdtsmith/mode-minder")
  :commands mode-minder)

;; Identify weasel words, passive voice, and duplicate words.
;; (use-package writegood-mode
;;   :when (executable-find "writegood")
;;   :hook (text-mode . writegood-duplicates-turn-on)
;;   :config
;;   (let
;;     (
;;       (sb/weasel-words
;;         '
;;         ("actionable"
;;           "actually"
;;           "basically"
;;           "clearly"
;;           "easily"
;;           "easy"
;;           "it turns out that"
;;           "In this regard"
;;           "In this sense"
;;           "With this in mind"
;;           "With the above in mind"
;;           "may have"
;;           "often"
;;           "simple"
;;           "probably"
;;           "simply"
;;           "specifically")))
;;     (cl-union writegood-weasel-words sb/weasel-words))
;;   :diminish)

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  ;; Remove newline checks, since they would trigger an immediate check when we want the
  ;; `flycheck-idle-change-delay' to be in effect while editing.
  (flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change))
  (flycheck-checker-error-threshold 1500)
  (flycheck-idle-buffer-switch-delay 2 "Increase the time (s) to allow for quick transitions")
  (flycheck-idle-change-delay 2 "Increase the time (s) to allow for transient edits")
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-global-modes '(not csv-mode conf-mode))
  (flycheck-indication-mode 'left-fringe)
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
      "lt:en:MORFOLOGIK_RULE_EN_US,lt:en:WORD_CONTAINS_UNDERSCORE"
      ;; Using source ensures that a single temporary file in a different dir is created
      ;; such that textidote won't process other files. This serves as a hacky workaround for
      ;; https://github.com/sylvainhalle/textidote/issues/200.
      source)
    :error-patterns
    (
      (warning
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
  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get)

  (with-eval-after-load "counsel"
    (bind-key "C-c ! !" #'counsel-flycheck flycheck-mode-map)))

;; Use for major modes which do not provide a formatter.
(use-package format-all
  :hook
  ((format-all-mode . format-all-ensure-formatter)
    ;; The cursor position is not saved in `LaTeX-mode-hook', so we invoke explicitly.
    ((markdown-mode markdown-ts-mode) . format-all-mode))
  :config
  (setq-default format-all-formatters
    '
    (("Assembly" asmfmt)
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
      ("Perl" perltidy "--quiet" "--standard-error-output" "--perl-best-practices" "-l=100")
      ("Python" (yapf "--style" "file") isort)
      ("Shell" (shfmt "-i" "4" "-ci"))
      ("XML" tidy)
      ("YAML" prettier "--print-width" "100")))
  (with-eval-after-load "markdown-mode"
    (bind-key "C-x f" #'format-all-buffer markdown-mode-map))
  (with-eval-after-load "auctex"
    (bind-key "C-x f" #'format-all-buffer LaTeX-mode-map))
  :diminish)

;; The advantage with `flycheck-grammarly' over `lsp-grammarly' is that you need not set up lsp
;; support, so you can use it anywhere. But `flycheck-grammarly' does not support a PRO Grammarly
;; account. We only need this package for checking text in "*scratch*" buffer.

;; (use-package flycheck-grammarly
;;   :after (flycheck persistent-scratch)
;;   :init (flycheck-grammarly-setup)
;;   :custom (flycheck-grammarly-check-time 3)
;;   :config
;;   ;; Remove from the beginning of the list and append to the end
;;   ;; (add-to-list 'flycheck-checkers (pop flycheck-checkers) 'append)
;;   (setq flycheck-checkers (delete 'grammarly flycheck-checkers))
;;   (add-to-list 'flycheck-checkers 'grammarly t))

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

;; https://languagetool.org/download/LanguageTool-stable.zip
;; The "languagetool" folder should include all files in addition to the ".jar" files.

;; (use-package flycheck-languagetool
;;   :after (flycheck persistent-scratch)
;;   :init (flycheck-languagetool-setup)
;;   :custom
;;   (flycheck-languagetool-server-jar
;;     (no-littering-expand-etc-file-name "languagetool/languagetool-server.jar"))
;;   :config
;;   ;; (add-to-list 'flycheck-checkers (pop flycheck-checkers) t)
;;   (setq flycheck-checkers (delete 'languagetool flycheck-checkers))
;;   (add-to-list 'flycheck-checkers 'languagetool t))

;; Most likely, `text', `org', `markdown', and `latex' files will be in directories that can use LSP
;; support. We enable `flycheck' support for the "*scratch*" buffer if it is in `text-mode'.

;; (run-with-idle-timer
;;   3 nil
;;   (lambda ()
;;     (add-hook
;;       'text-mode-hook
;;       (lambda ()
;;         (when (string= (buffer-name) "*scratch*")
;;           (progn
;;             (flycheck-select-checker 'grammarly)
;;             (flycheck-add-next-checker 'grammarly 'languagetool)))))))

;; (use-package highlight-indentation
;;   :hook
;;   ((yaml-mode yaml-ts-mode python-mode python-ts-mode)
;;     .
;;     highlight-indentation-mode)
;;   :diminish (highlight-indentation-current-column-mode highlight-indentation-mode))

(use-package indent-bars
  :straight (:host github :repo "jdtsmith/indent-bars")
  :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode) . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap
    '
    (
      (python
        argument_list
        parameters ; for python, as an example
        list
        list_comprehension
        dictionary
        dictionary_comprehension
        parenthesized_expression
        subscript))))

;; `format-all-the-code' just runs Emacs' built-in `indent-region' for `emacs-lisp'.
(use-package elisp-autofmt
  :commands (elisp-autofmt-buffer)
  :hook ((emacs-lisp-mode lisp-data-mode) . elisp-autofmt-mode)
  :custom
  (elisp-autofmt-style 'fixed)
  (elisp-autofmt-python-bin "python3")
  :config (setq-default elisp-autofmt-load-packages-local '("use-package")))

(use-package flycheck-eglot
  :straight (:host github :repo "intramurz/flycheck-eglot")
  :when (eq sb/lsp-provider 'eglot)
  :after (flycheck eglot)
  :init (global-flycheck-eglot-mode 1))

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

  (with-eval-after-load "orderless"
    ;; substring is needed to complete common prefix, orderless does not
    (setq
      completion-styles '(orderless substring basic)
      ;; The "basic" completion style needs to be tried first for TRAMP hostname completion to
      ;; work. I also want substring matching for file names.
      completion-category-overrides
      '
      ((file (styles basic substring partial-completion))
        ;; (buffer (styles basic substring flex))
        ;; (project-file (styles basic substring flex))
        ;; (minibuffer (orderless flex))
        ))))

;; Use "C-M-;" for `dabbrev-completion' which finds all expansions in the current buffer and
;; presents suggestions for completion.
(use-package dabbrev
  :straight (:type built-in)
  :bind ("C-M-;" . dabbrev-completion)
  :custom (dabbrev-completion-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package hippie-exp
  :straight (:type built-in)
  :custom
  (hippie-expand-try-functions-list
    '
    (try-expand-dabbrev
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
  :custom
  ;; Allow escaping space with backslash
  (orderless-component-separator 'orderless-escapable-split-on-space)
  :config
  ;; (with-eval-after-load "ivy"
  ;;   (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  ;;   ;; `counsel-rg' fails with `orderless'
  ;;   (add-to-list
  ;;     'ivy-highlight-functions-alist
  ;;     '(orderless-ivy-re-builder . orderless-ivy-highlight)))

  (with-eval-after-load "company"
    (defun sb/just-one-face (fn &rest args)
      (let ((orderless-match-faces [completions-common-part]))
        (apply fn args)))
    (advice-add 'company-capf--candidates :around #'sb/just-one-face)))

;; It is recommended to load `yasnippet' before `eglot'
(use-package yasnippet
  ;; :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (emacs-startup . yas-global-mode)
  :custom (yas-verbosity 0)
  :config
  (with-eval-after-load "hippie-expand"
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  (unbind-key "<tab>" yas-minor-mode-map)
  :diminish yas-minor-mode)

;; YASnippet no longer bundles snippets directly
(use-package yasnippet-snippets
  :after yasnippet
  :init (yasnippet-snippets-initialize))

;; Prescient uses frecency (frequency + recency) for sorting. recently used commands should be
;; sorted first. Only commands that have never been used before will be sorted by length. Vertico
;; does its own sorting based on recency, and Corfu has corfu-history. Company has
;; company-statistics. Ivy is not actively supported with prescient.

;; (use-package prescient
;;   :straight (:host github :repo "radian-software/prescient.el" :files (:defaults "/*.el"))
;;   :hook (emacs-startup . prescient-persist-mode)
;;   :custom (prescient-sort-full-matches-first t)
;;   :config
;;   (with-eval-after-load "corfu"
;;     (corfu-prescient-mode 1))
;;   (with-eval-after-load "vertico"
;;     (vertico-prescient-mode 1))
;;   (with-eval-after-load "company"
;;     (company-prescient-mode 1))
;;   (with-eval-after-load "counsel"
;;     (ivy-prescient-mode 1)))

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
  ;; (company-dabbrev-other-buffers t "Search in other buffers with the same major mode")
  ;; (company-dabbrev-ignore-case t "Ignore case when *collecting* completion candidates")
  ;; (company-dabbrev-downcase nil "Do not downcase returned candidates")
  (company-idle-delay 0.05 "Start autocompletion faster")
  (company-dabbrev-code-completion-styles '(basic flex))
  (company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory))
  (company-minimum-prefix-length 3 "Small words can be faster to type")
  (company-require-match nil "Allow typing input characters that do not match candidates")
  (company-show-quick-access t "Speed up selecting a completion")
  ;; Align additional metadata, like type signatures, to the right-hand side if non-nil.
  (company-tooltip-align-annotations nil)
  ;; Choices are: `company-pseudo-tooltip-unless-just-one-frontend' shows popup unless there is only
  ;; one candidate, `company-preview-frontend' shows the preview in-place which is too intrusive,
  ;; `company-preview-if-just-one-frontend' shows in-place preview if there is only choice,
  ;; `company-echo-metadata-frontend' shows selected candidate docs in echo area, and
  ;; `company-pseudo-tooltip-frontend' which always shows the candidates in an overlay.
  ;; (company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (company-global-modes
    '
    (not dired-mode
      erc-mode
      message-mode
      comint-mode
      inferior-python-mode
      vterm-mode
      magit-status-mode
      help-mode
      gud-mode
      eshell-mode
      shell-mode
      csv-mode
      minibuffer-inactive-mode))
  ;; (company-format-margin-function nil "Disable icons")
  (company-selection-wrap-around t "Convenient to wrap around completion items at boundaries")
  ;; (company-tooltip-flip-when-above t "Flip the tooltip when it is close to the bottom")
  :config
  ;; Options: `company-sort-prefer-same-case-prefix', `company-sort-by-occurrence',
  ;; `company-sort-by-statistics', `company-sort-by-length', `company-sort-by-backend-importance',
  ;; `delete-dups'.

  ;; Ignore matches that consist solely of numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  (push
    (apply-partially #'cl-remove-if (lambda (c) (string-match-p "\\`[0-9]+\\'" c)))
    company-transformers)

  ;; (add-to-list 'company-transformers 'delete-dups)
  ;; (add-to-list 'company-transformers 'company-sort-by-backend-importance)
  ;; (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix)
  )

;; Posframes do not have unaligned rendering issues with variable `:height' unlike an overlay.
;; However, posframes do not work with TUI, and the width of the frame popup is often not enough and
;; the right side gets cut off. https://github.com/company-mode/company-mode/issues/1010

;; (use-package company-posframe
;;   :when (display-graphic-p)
;;   :hook (company-mode . company-posframe-mode)
;;   :custom
;;   (company-posframe-show-metadata t "Difficult to distinguish the help text from completions")
;;   (company-posframe-show-indicator nil "The backend display in the posframe modeline gets cut")
;;   (company-posframe-quickhelp-delay nil "Disable showing the help frame")
;;   :diminish)

(use-package company-quickhelp
  :after company
  :when (display-graphic-p)
  :hook (prog-mode . company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :after company
  :unless (display-graphic-p)
  :hook (prog-mode . company-quickhelp-terminal-mode))

(use-package company-statistics
  :after company
  :init (company-statistics-mode 1))

;; We should enable `company-fuzzy-mode' at the very end of configuring `company'. Nice feature but
;; slows completions.

;; (use-package company-fuzzy
;;   :straight flx
;;   :straight t
;;   :after company
;;   :commands (global-company-fuzzy-mode company-fuzzy-mode)
;;   :custom
;;   (company-fuzzy-sorting-backend 'alphabetic) ; Using "flx" slows down completion significantly
;;   ;; (company-fuzzy-passthrough-backends '(company-capf))
;;   (company-fuzzy-show-annotation t "The right-hand side may get cut off")
;;   ;; We should not need this with "flx" sorting because the "flx" sorting accounts for the prefix.
;;   ;; Disabling the requirement may help with performance.
;;   (company-fuzzy-prefix-on-top t))

(use-package company-auctex
  :after tex-mode
  :demand t)

;; Required by `ac-math' and `company-math'
(use-package math-symbols
  :after tex-mode
  :demand t)

(use-package company-math
  :after tex-mode
  :demand t)

;; Uses RefTeX to complete label references and citations. When working with multi-file documents,
;; ensure that the variable `TeX-master' is appropriately set in all files, so that RefTeX can find
;; citations across documents.
(use-package company-reftex
  :after tex-mode
  :demand t
  :custom
  ;; https://github.com/TheBB/company-reftex/pull/13
  (company-reftex-labels-parse-all nil))

;; (use-package company-bibtex
;;   :after tex-mode
;;   :demand t)

;; Complete in the middle of words
(use-package company-anywhere
  :straight (:host github :repo "zk-phi/company-anywhere")
  :after company
  :demand t)

(use-package company-dict
  :after company
  :demand t
  :custom
  (company-dict-dir (expand-file-name "company-dict" user-emacs-directory))
  (company-dict-enable-fuzzy nil)
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

;; (use-package company-makefile
;;   :straight (:host github :repo "nverno/company-makefile")
;;   :after (company make-mode)
;;   :demand t)

;; (use-package company-spell
;;   :straight (:host github :repo "enzuru/company-spell")
;;   :after company
;;   :demand t
;;   :config (setf company-aspell-command "hunspell"))

(use-package company-try-hard
  :bind (("C-j" . company-try-hard) :map company-active-map ("C-j" . company-try-hard)))

(use-package company-web
  :after company
  :demand t
  :config (require 'company-web-html))

(use-package company-wordfreq
  :straight (:host github :repo "johannes-mueller/company-wordfreq.el")
  :after company
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
    '
    (company-dirfiles
      (company-capf :with company-dabbrev-code company-yasnippet)
      ;; If we have `company-dabbrev' first, then other matches from `company-ispell' will be
      ;; ignored.
      company-ispell company-dict company-dabbrev)
    company-transformers
    '
    (delete-dups ; company-sort-by-backend-importance
      ; company-sort-by-occurrence
      company-sort-by-statistics
      company-sort-prefer-same-case-prefix))

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
        '
        (company-dirfiles
          (company-math-symbols-latex ; Math latex tags
            company-latex-commands
            company-reftex-labels
            company-reftex-citations
            company-auctex-environments
            company-auctex-macros
            ;; Math unicode symbols and sub(super)scripts
            company-math-symbols-unicode
            company-auctex-symbols
            ;; company-bibtex
            ;; :separate
            company-ispell
            company-dict
            company-dabbrev
            company-wordfreq
            company-capf))))

    (add-hook
      'LaTeX-mode-hook
      (lambda ()
        (sb/company-latex-mode)
        ;; `company-capf' does not pass to later backends with Texlab, so we use
        ;; `company-fuzzy-mode' to merge results from all backends.
        ;; (company-fuzzy-mode 1)
        ;; (diminish 'company-fuzzy-mode)
        )))

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
        '
        (company-dirfiles
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
        '
        (company-dirfiles
          (company-capf company-web-html)
          company-ispell
          company-dict
          company-dabbrev)))

    (dolist (hook '(html-mode-hook html-ts-mode-hook))
      (add-hook hook (lambda () (sb/company-html-mode)))))

  (progn
    (defun sb/company-prog-mode ()
      "Add backends for `prog-mode' completion in company mode."
      ;; Typing short prefixes help with faster completion and a more responsive UI
      (setq-local company-minimum-prefix-length 2)

      (make-local-variable 'company-backends)

      ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names

      (cond
        ((eq sb/lsp-provider 'eglot)
          (setq company-backends
            '
            (company-dirfiles
              (company-capf
                company-c-headers
                :with company-keywords
                company-dabbrev-code ; Useful for variable names
                company-yasnippet
                :separate)
              company-dict company-ispell company-dabbrev)))
        ((eq sb/lsp-provider 'lsp-mode)
          (setq company-backends
            '
            (company-dirfiles
              (company-capf
                company-citre-tags company-c-headers
                :with company-keywords
                company-dabbrev-code ; Useful for variable names
                company-yasnippet
                :separate)
              company-dict company-ispell company-dabbrev)))))

    (add-hook
      'prog-mode-hook
      (lambda ()
        (unless
          (or (derived-mode-p 'emacs-lisp-mode)
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
        '
        (company-dirfiles
          (company-capf
            company-elisp company-citre-tags
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
    '
    (dired-mode
      erc-mode
      message-mode
      comint-mode
      inferior-python-mode
      vterm-mode
      magit-status-mode
      help-mode
      gud-mode
      eshell-mode
      shell-mode
      csv-mode
      minibuffer-inactive-mode))
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

;; (use-package corfu-quick
;;   :straight nil
;;   :after corfu
;;   :bind (:map corfu-map ("C-'" . corfu-quick-insert)))

(use-package corfu-quick-access
  :straight (:host codeberg :repo "spike_spiegel/corfu-quick-access.el")
  :when (eq sb/capf 'corfu)
  :hook
  (corfu-mode
    .
    (lambda ()
      (ignore-errors
        (corfu-quick-access-mode)))))

;; We do not need this if we use prescient-based sorting.
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

;; (use-package kind-icon
;;   :when (eq sb/corfu-icons 'kind-icon)
;;   :after corfu
;;   :demand t
;;   :commands kind-icon-margin-formatter
;;   :custom (kind-icon-default-face 'corfu-default "Compute blended backgrounds correctly")
;;   ;; Prefer smaller icons and a more compact popup
;;   (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.6))
;;   ;; (kind-icon-blend-background nil)
;;   :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;;   ;; (when (eq sb/corfu-icons 'nerd-icons)
;;   ;;   (with-eval-after-load "nerd-icons"
;;   ;;     (setq kind-icon-use-icons nil)
;;   ;;     (setq kind-icon-mapping
;;   ;;       `
;;   ;;       ((array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
;;   ;;         (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
;;   ;;         (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
;;   ;;         (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
;;   ;;         (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
;;   ;;         (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
;;   ;;         (constructor
;;   ;;           ,(nerd-icons-codicon "nf-cod-triangle_right")
;;   ;;           :face font-lock-function-name-face)
;;   ;;         (enummember
;;   ;;           ,(nerd-icons-codicon "nf-cod-symbol_enum_member")
;;   ;;           :face font-lock-builtin-face)
;;   ;;         (enum-member
;;   ;;           ,(nerd-icons-codicon "nf-cod-symbol_enum_member")
;;   ;;           :face font-lock-builtin-face)
;;   ;;         (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
;;   ;;         (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
;;   ;;         (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
;;   ;;         (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
;;   ;;         (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
;;   ;;         (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
;;   ;;         (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
;;   ;;         (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
;;   ;;         (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
;;   ;;         (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
;;   ;;         (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
;;   ;;         (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
;;   ;;         (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
;;   ;;         (operator
;;   ;;           ,(nerd-icons-codicon "nf-cod-symbol_operator")
;;   ;;           :face font-lock-comment-delimiter-face)
;;   ;;         (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
;;   ;;         (property
;;   ;;           ,(nerd-icons-codicon "nf-cod-symbol_property")
;;   ;;           :face font-lock-variable-name-face)
;;   ;;         (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
;;   ;;         (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
;;   ;;         (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
;;   ;;         (struct
;;   ;;           ,(nerd-icons-codicon "nf-cod-symbol_structure")
;;   ;;           :face font-lock-variable-name-face)
;;   ;;         (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
;;   ;;         (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
;;   ;;         (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
;;   ;;         (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
;;   ;;         (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
;;   ;;         (variable
;;   ;;           ,(nerd-icons-codicon "nf-cod-symbol_variable")
;;   ;;           :face font-lock-variable-name-face)
;;   ;;         (group ,(nerd-icons-codicon "nf-cod-variable_group") :face font-lock-variable-name-face)
;;   ;;         (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))))
;;   )

(use-package nerd-icons-corfu
  :straight (:host github :repo "LuigiPiucco/nerd-icons-corfu")
  :when (eq sb/corfu-icons 'nerd-icons)
  :after corfu
  :demand t
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package kind-all-the-icons
;;   :straight (:host github :repo "Hirozy/kind-all-the-icons")
;;   :when (and (eq sb/corfu-icons 'kind-all-the-icons) (display-graphic-p))
;;   :after corfu
;;   :demand t
;;   :config (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))

(use-package yasnippet-capf
  :after corfu
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
    `
    (,(expand-file-name "wordlist.5" sb/extras-directory)
      ,(expand-file-name "company-dict/text-mode" user-emacs-directory)))
  (cape-dabbrev-check-other-buffers 'some)
  :config
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
              #'cape-elisp-symbol)
            (cape-capf-properties (cape-capf-super #'cape-dabbrev #'cape-dict) :sort t))))))

  (add-hook
    'flex-mode-hook
    (lambda ()
      (setq-local completion-at-point-functions
        (list #'cape-file #'cape-keyword #'cape-dabbrev #'cape-dict))))

  (add-hook
    'text-mode-hook
    (lambda ()
      (setq-local completion-at-point-functions
        (list
          #'cape-file
          ;; Merge dabbrev and dict candidates
          (cape-capf-properties (cape-capf-super #'cape-dabbrev #'cape-dict) :sort t)))))

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
            ;; Math unicode symbols and sub(super)scripts
            (cape-company-to-capf #'company-math-symbols-unicode)
            (cape-company-to-capf #'company-auctex-symbols)
            #'cape-dabbrev
            #'cape-dict)
          #'yasnippet-capf))))

  (with-eval-after-load "lsp-mode"
    (dolist
      (mode
        '
        (c-mode-hook
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
              #'cape-file
              #'yasnippet-capf
              (cape-capf-super #'lsp-completion-at-point #'citre-completion-at-point #'cape-keyword)
              (cape-capf-super #'cape-dabbrev #'cape-dict)))))))

  (with-eval-after-load "eglot"
    (dolist
      (mode
        '
        (c-mode-hook
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
                #'eglot-completion-at-point
                #'citre-completion-at-point
                #'cape-keyword)
              (cape-capf-super #'cape-dabbrev #'cape-dict))))))))

;; I work a lot over SSH, and `lsp-mode' is poor over Tramp. The alternative I used was to use TUI
;; Emacs. Eglot works better than `lsp-mode' over Tramp, which allows me to continue using GUI
;; Emacs. However, Eglot does not support multiple servers for a major-mode. For example, it will be
;; nice to have TexLab and Grammarly with LaTeX files.

;; Registering `lsp-format-buffer' makes sense only if the server is active. We may not always want
;; to format unrelated files and buffers (e.g., commented YAML files in out-of-project locations).
(use-package lsp-mode
  :when (eq sb/lsp-provider 'lsp-mode)
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind
  (:map
    lsp-command-map
    ("=")
    ("w")
    ("g")
    ("G")
    ("a")
    ("F")
    ("l" . lsp)
    ("L" . lsp)
    ("q" . lsp-disconnect)
    ("Q" . lsp-workspace-shutdown)
    ("H" . lsp-describe-session)
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
    '
    ("-j=4"
      "--all-scopes-completion"
      "--background-index"
      "--clang-tidy"
      "--completion-style=detailed"
      "--fallback-style=LLVM"
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
  (lsp-diagnostics-provider :auto "Prefer Flycheck, otherwise use Flymake")
  (lsp-html-format-wrap-line-length sb/fill-column)
  (lsp-html-format-end-with-newline t)
  (lsp-html-format-indent-inner-html t)
  (lsp-imenu-sort-methods '(position) "More natural way of listing symbols")
  (lsp-lens-enable nil "Lenses are intrusive")
  (lsp-modeline-diagnostics-enable nil "We have Flycheck, and the modeline gets congested")
  (lsp-modeline-diagnostics-scope :file "Simpler to focus on the errors at hand")
  (lsp-modeline-code-actions-enable t "Useful to show code actions on the modeline")
  (lsp-modeline-workspace-status-enable t)
  ;; Sudden changes in the height of the echo area causes the cursor to lose position, manually
  ;; request via `lsp-signature-activate'.
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation t "Show the function documentation along with the prototype")
  (lsp-restart 'auto-restart "Avoid annoying questions, we expect a server restart to succeed")
  (lsp-xml-logs-client nil)
  (lsp-yaml-print-width sb/fill-column)
  (lsp-warn-no-matched-clients nil "Avoid warning messages for unsupported modes like csv-mode")
  (lsp-keep-workspace-alive nil)
  (lsp-enable-file-watchers nil "Avoid watcher warnings")
  ;; Useful to identify disabled code but not all language servers support this feature. Recent
  ;; versions of clangd do.
  (lsp-semantic-tokens-enable nil)
  ;; I am using symbol-overlay for languages that do not have a server
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-snippet t)
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
        "-j 2"
        (concat "--rcfile=" (expand-file-name ".config/pylintrc" sb/user-home-directory)))))
  (lsp-pylsp-plugins-isort-enabled t)
  (lsp-use-plists t)
  :config
  ;; I am explicitly setting company backends and cape capfs for corfu, and do not want lsp-mode to
  ;; interfere with `completion-at-point-functions'
  (cond
    ((eq sb/capf 'company)
      (setq lsp-completion-enable t))
    ((eq sb/capf 'corfu)
      (setq lsp-completion-enable nil)))

  (when (or (display-graphic-p) (daemonp))
    (setq lsp-modeline-code-actions-segments '(count icon name)))

  (dolist
    (ignore-dirs
      '
      ("/build\\'"
        "/\\.metadata\\'"
        "/\\.recommenders\\'"
        "/\\.clangd\\'"
        "/\\.cache\\'"
        "/__pycache__\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories ignore-dirs))

  (with-eval-after-load "lsp-lens"
    (diminish 'lsp-lens-mode))

  :diminish)

(use-package lsp-ui
  :when (eq sb/lsp-provider 'lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil "Disable intrusive on-hover dialogs, invoke with `lsp-ui-doc-show'")
  (lsp-ui-doc-include-signature t)
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-sideline-show-code-actions t "Enables understanding when to invoke code actions")
  (lsp-ui-sideline-enable nil "Noisy to show symbol information in the sideline")
  ;; Hide diagnostics when typing because they can be intrusive
  (lsp-ui-sideline-show-diagnostics nil "Flycheck/flymake already highlights errors")
  (lsp-ui-doc-max-width 72 "150 (default) is too wide")
  (lsp-ui-doc-delay 0.75 "0.2 (default) is too naggy")
  (lsp-ui-peek-enable nil))

;; Sync workspace folders and treemacs projects

;; (use-package lsp-treemacs
;;   :when (eq sb/lsp-provider 'lsp-mode)
;;   :commands (lsp-treemacs-errors-list lsp-treemacs-sync-mode)
;;   :config (lsp-treemacs-sync-mode 1)
;;   :bind
;;   (:map
;;     lsp-command-map
;;     ("S" . lsp-treemacs-symbols)
;;     ("F" . lsp-treemacs-references)
;;     ("Y" . lsp-treemacs-sync-mode)
;;     ("C" . lsp-treemacs-call-hierarchy)
;;     ("T" . lsp-treemacs-type-hierarchy)
;;     ("E" . lsp-treemacs-errors-list)))

;; Try to delete `lsp-java-workspace-dir' if the JDTLS fails
(use-package lsp-java
  :when (eq sb/lsp-provider 'lsp-mode)
  :hook
  ((java-mode java-ts-mode)
    .
    (lambda ()
      (setq-local
        c-basic-offset 4
        c-set-style "java")
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :custom
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-format-settings-profile "Swarnendu")
  (lsp-java-format-settings-url
    (expand-file-name "github/dotfiles/java/eclipse-format-swarnendu.xml" sb/user-home-directory)))

;; We need to enable lsp workspace to allow `lsp-grammarly' to work, which makes it ineffective for
;; temporary text files. `lsp-grammarly' supports PRO Grammarly accounts. If there are failures,
;; then try logging out of Grammarly and logging in again. Make sure to run "M-x keytar-install".
(use-package lsp-grammarly
  :when (eq sb/lsp-provider 'lsp-mode)
  :hook ((text-mode markdown-mode org-mode LaTeX-mode) . lsp-deferred)
  :custom
  (lsp-grammarly-suggestions-oxford-comma t)
  (lsp-grammarly-suggestions-passive-voice t)
  (lsp-grammarly-suggestions-informal-pronouns-academic t)
  (lsp-grammarly-suggestions-preposition-at-the-end-of-sentence t)
  (lsp-grammarly-suggestions-conjunction-at-start-of-sentence t)
  (lsp-grammarly-user-words '(Swarnendu Biswas))
  ;; :config (defvar lsp-grammarly-active-modes)
  ;; (lsp-register-client
  ;;   (make-lsp-client
  ;;     :new-connection (lsp-tramp-connection #'lsp-grammarly--server-command)
  ;;     :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-grammarly-active-modes))
  ;;     :priority -1
  ;;     :remote? t
  ;;     :add-on? t
  ;;     :server-id 'grammarly-r
  ;;     :download-server-fn
  ;;     (lambda (_client callback error-callback _update?)
  ;;       (lsp-package-ensure 'grammarly-ls callback error-callback))
  ;;     :after-open-fn #'lsp-grammarly--init
  ;;     :async-request-handlers
  ;;     (ht
  ;;       ("$/getCredentials" #'lsp-grammarly--get-credentials)
  ;;       ("$/getToken" #'lsp-grammarly--get-token)
  ;;       ("$/storeToken" #'lsp-grammarly--store-token)
  ;;       ("$/showError" #'lsp-grammarly--show-error)
  ;;       ("$/updateDocumentState" #'lsp-grammarly--update-document-state))))
  )

;; The ":after" clause does not work with the ":hook", `lsp-mode' is not started automatically
(use-package lsp-ltex
  :when (eq sb/lsp-provider 'lsp-mode)
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
  (setq lsp-ltex-disabled-rules #s(hash-table size 30 data ("en-US" ["MORFOLOGIK_RULE_EN_US"])))

  ;; (setq lsp-ltex-disabled-rules
  ;;       (json-parse-string
  ;;        "{\"en-US\": [\"MORFOLOGIK_RULE_EN_US\"]}"))

  ;; (defvar lsp-ltex-active-modes)

  ;; (lsp-register-client
  ;;   (make-lsp-client
  ;;     :new-connection
  ;;     (lsp-tramp-connection
  ;;       (expand-file-name "lsp/server/ltex-ls/latest/bin/ltex-ls" no-littering-var-directory))
  ;;     :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-ltex-active-modes))
  ;;     :priority -2
  ;;     :add-on? t
  ;;     :remote? t
  ;;     :server-id 'ltex-r
  ;;     :download-server-fn
  ;;     (lambda (_client _callback error-callback _update?)
  ;;       (lsp-package-ensure
  ;;         'ltex-ls
  ;;         (lambda ()
  ;;           (let ((dest (f-dirname (lsp-ltex--downloaded-extension-path))))
  ;;             (unless
  ;;               (lsp-ltex--execute "tar" "-xvzf" (lsp-ltex--downloaded-extension-path) "-C" dest)
  ;;               (error "Error during the unzip process: tar"))))
  ;;         error-callback))))
  )

;; (use-package dap-mode
;;   :after lsp-mode
;;   :commands (dap-continue dap-debug)
;;   :init (dap-auto-configure-mode)
;;   :hook ((lsp-mode . dap-ui-mode) (lsp-mode . dap-mode))
;;   ;; :bind
;;   ;; ("<f7>" . dap-step-in)
;;   ;; ("<f8>" . dap-next)
;;   ;; ("<f9>" . dap-continue)
;;   )

;; Install with "python3 -m pip install -U pyright --user". Create stubs for a package with "pyright
;; --createstub pandas".

;; (use-package lsp-pyright
;;   :when
;;   (and (eq sb/lsp-provider 'lsp-mode)
;;     (eq sb/python-langserver 'pyright)
;;     (executable-find "pyright"))
;;   :commands (lsp-pyright-locate-python lsp-pyright-locate-venv)
;;   :hook ((python-mode python-ts-mode) . (lambda () (require 'lsp-pyright)))
;;   :custom
;;   (lsp-pyright-python-executable-cmd "python3")
;;   (lsp-pyright-typechecking-mode "basic")
;;   (lsp-pyright-auto-import-completions t)
;;   (lsp-pyright-auto-search-paths t)
;;   ;; :config
;;   ;; (lsp-register-client
;;   ;;   (make-lsp-client
;;   ;;     :new-connection
;;   ;;     (lsp-tramp-connection
;;   ;;       (lambda () (cons "pyright-langserver" lsp-pyright-langserver-command-args)))
;;   ;;     :major-modes '(python-mode)
;;   ;;     :remote? t
;;   ;;     :server-id 'pyright-r
;;   ;;     :multi-root lsp-pyright-multi-root
;;   ;;     :priority 3
;;   ;;     :initialization-options
;;   ;;     (lambda ()
;;   ;;       (ht-merge (lsp-configuration-section "pyright") (lsp-configuration-section "python")))
;;   ;;     :initialized-fn
;;   ;;     (lambda (workspace)
;;   ;;       (with-lsp-workspace
;;   ;;         workspace
;;   ;;         (lsp--set-configuration
;;   ;;           (ht-merge (lsp-configuration-section "pyright") (lsp-configuration-section "python")))))
;;   ;;     :download-server-fn
;;   ;;     (lambda (_client callback error-callback _update?)
;;   ;;       (lsp-package-ensure 'pyright callback error-callback))
;;   ;;     :notification-handlers
;;   ;;     (lsp-ht
;;   ;;       ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
;;   ;;       ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
;;   ;;       ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))
;;   )

;; `lsp-tex' provides minimal settings for Texlab, `lsp-latex' supports full features of Texlab.
(use-package lsp-latex
  :when (eq sb/lsp-provider 'lsp-mode)
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

  ;; Support forward search with Evince. Inverse search is already configured with evince-synctex,
  ;; use Ctrl+Click in the PDF document.
  ;; (lsp-latex-forward-search-executable "evince-synctex")
  ;; “%f” is replaced with "The path of the current TeX file", "%p" with "The path of the current
  ;; PDF file", "%l" with "The current line number" by texlab
  ;; (lsp-latex-forward-search-args '("-f" "%l" "%p" "\"emacsclient +%l %f\""))

  ;; Support forward search with Okular. Perform inverse search with Shift+Click in the PDF.
  (lsp-latex-forward-search-executable "okular")
  (lsp-latex-forward-search-args '("--unique" "file:%p#src:%l%f"))
  :config
  (add-to-list 'lsp-latex-build-args "-c")
  (add-to-list 'lsp-latex-build-args "-pvc")

  ;; (lsp-register-client
  ;;   (make-lsp-client
  ;;     :new-connection (lsp-tramp-connection "texlab")
  ;;     :major-modes '(tex-mode latex-mode LaTeX-mode bibtex-mode)
  ;;     :remote? t
  ;;     :server-id 'texlab-r))
  )

(use-package eglot
  :straight (:source (gnu-elpa-mirror))
  :when (eq sb/lsp-provider 'eglot)
  ;; :init (put 'eglot-server-programs 'safe-local-variable 'listp)
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
    ("C-c l x" . eglot-code-actions))
  :hook
  ( ;; (eglot-managed-mode . eglot-inlay-hints-mode) ; Inlay hints are distracting
    (
      (c-mode
        c-ts-mode
        c++-mode
        c++-ts-mode
        python-mode
        python-ts-mode
        css-mode
        css-ts-mode
        markdown-mode
        sh-mode
        bash-ts-mode
        LaTeX-mode
        bibtex-mode
        html-mode
        html-ts-mode
        json-mode
        json-ts-mode
        dockerfile-ts-mode
        perl-mode)
      . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0 "Drop jsonrpc log to improve performance")
  ;; Eglot overwrites `company-backends' to only include `company-capf'
  (eglot-stay-out-of '(flymake company eldoc eldoc-documentation-strategy))
  (eglot-ignored-server-capabilities
    '
    (:codeLensProvider
      :executeCommandProvider
      :hoverProvider ; Automatic documentation popups can be distracting
      :foldingRangeProvider
      :documentOnTypeFormattingProvider
      :documentLinkProvider
      ;; Inlay hints are distracting
      :inlayHintProvider))
  :config
  ;; Show all of the available eldoc information when we want it. This way Flymake errors
  ;; don't just get clobbered by docstrings.
  (add-hook
    'eglot-managed-mode-hook
    (lambda ()
      "Make sure Eldoc will show us all of the feedback at point."
      (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)))

  (advice-add 'jsonrpc--log-event :around (lambda (_orig-func &rest _)))

  ;; (setq-default eglot-workspace-configuration
  ;;   '
  ;;   (
  ;;     (:pylsp
  ;;       .
  ;;       (:configurationSources
  ;;         ["setup.cfg"]
  ;;         :plugins
  ;;         (:jedi_completion
  ;;           (:include_params t :fuzzy t)
  ;;           :pycodestyle (:enabled :json-false)
  ;;           :mccabe (:enabled :json-false)
  ;;           :pyflakes (:enabled :json-false)
  ;;           :flake8 (:enabled :json-false :maxLineLength 100)
  ;;           :black (:enabled :json-false :line_length 100 :cache_config t)
  ;;           :yapf (:enabled t)
  ;;           :pydocstyle (:enabled t :convention "numpy")
  ;;           :autopep8 (:enabled :json-false)
  ;;           :pylint (:enabled t)
  ;;           :ruff (:enabled :json-false :lineLength 100)
  ;;           :pylsp_isort (:enabled t)
  ;;           :pylsp_mypy (:enabled t :report_progress t :live_mode :json-false))))
  ;;     (:pyright . ((:useLibraryCodeForTypes t)))))

  (add-to-list
    'eglot-server-programs
    '
    ((c++-mode c++-ts-mode c-mode c-ts-mode)
      .
      ("clangd"
        "-j=4"
        "--all-scopes-completion"
        "--background-index"
        "--clang-tidy"
        "--completion-style=detailed"
        "--fallback-style=LLVM"
        "--header-insertion=never"
        "--header-insertion-decorators=0"
        "--log=error"
        ;; Unsupported option with Clangd 10: malloc-trim and enable-config
        ;; "--malloc-trim" ; Release memory periodically
        ;; "--enable-config"
        ;; "--pch-storage=memory" ; Increases memory usage but can improve performance
        "--pretty")))

  (add-to-list 'eglot-server-programs '(awk-mode . ("awk-language-server")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman"))))

(use-package eglot-hierarchy
  :straight (:host github :repo "dolmens/eglot-hierarchy"))

;; FIXME: Disable documentSymbol because otherwise imenu does not work
(use-package eglot-grammarly
  :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
  :when (eq sb/lsp-provider 'eglot)
  :hook
  ((text-mode LaTeX-mode org-mode markdown-mode)
    .
    (lambda ()
      (require 'eglot-grammarly)
      (eglot-ensure)))
  :custom (eglot-grammarly-active-modes '(text-mode LaTeX-mode org-mode markdown-mode))
  ;; :config
  ;; (setq eglot-server-programs (delete (car eglot-server-programs) eglot-server-programs))
  ;; (add-to-list
  ;;   'eglot-server-programs
  ;;   `(,eglot-grammarly-active-modes . ,(eglot-grammarly--server-command))
  ;;   'append)
  ;; (add-to-list 'eglot-server-programs (pop eglot-server-programs) 'append)
  ;; (add-to-list eglot-workspace-configuration
  ;;              ((@emacs-grammarly/grammarly-languageserver
  ;;                ((audience "knowledgeable")))))
  )

;; FIXME: Fix issue with LTEX 16.0.0
(use-package eglot-ltex
  :straight (:host github :repo "emacs-languagetool/eglot-ltex")
  :when (eq sb/lsp-provider 'eglot)
  :init
  (setq eglot-languagetool-server-path
    (expand-file-name "software/ltex-ls-16.0.0" sb/user-home-directory))
  :hook
  ((text-mode LaTeX-mode org-mode markdown-mode)
    .
    (lambda ()
      (require 'eglot-ltex)
      (eglot-ensure)))
  :custom (eglot-languagetool-active-modes '(text-mode LaTex-mode org-mode markdown-mode))
  ;; :config
  ;; (setq eglot-server-programs (delete (car eglot-server-programs) eglot-server-programs))
  ;; (add-to-list
  ;;   'eglot-server-programs
  ;;   `(,eglot-languagetool-active-modes . ,(eglot-languagetool--server-command))
  ;;   'append)
  ;; (add-to-list 'eglot-server-programs (pop eglot-server-programs) 'append)
  ;;   `((:ltex ((:language "en-US") (:disabledRules (:en-US ["MORFOLOGIK_RULE_EN_US"]))))))
  )

(use-package eglot-java
  :when (eq sb/lsp-provider 'eglot)
  :hook
  (java-mode
    .
    (lambda ()
      (eglot-ensure)
      (eglot-java-mode))))

(use-package which-func
  :custom
  (which-func-modes
    '
    (c-mode
      c-ts-mode
      c++-mode
      c++-ts-mode
      python-mode
      python-ts-mode
      sh-mode
      bash-ts-mode
      java-mode
      java-ts-mode)))

(use-package subword
  :straight (:type built-in)
  :hook (prog-mode . subword-mode)
  :diminish)

;; Hide top-level code blocks. Enable code folding, which is useful for browsing large files. This
;; module is part of Emacs, and is better maintained than other alternatives like `origami'.

;; (use-package hideshow
;;   :straight (:type built-in)
;;   :commands (hs-hide-all hs-hide-initial-comment-block hs-show-all hs-show-block)
;;   :hook
;;   ;; Hideshow is not defined for `ini-mode'.
;;   ((python-mode c-mode c++-mode emacs-lisp-mode java-mode sh-mode)
;;     .
;;     hs-minor-mode)
;;   :custom (hs-isearch-open t "Open all folds while searching")
;;   :diminish hs-minor-mode)

;; Highlight symbol under point
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M-p" . symbol-overlay-jump-prev) ("M-n" . symbol-overlay-jump-next))
  :custom (symbol-overlay-idle-time 2 "Delay highlighting to allow for transient cursor placements")
  :diminish)

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

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

;; Tree-sitter provides advanced syntax highlighting features. Run
;; `tree-sitter-langs-install-grammars' to install the grammar files for languages for tree-sitter.
;; Run `tree-sitter-langs-install-grammars' periodically to install new grammars.

;; https://www.reddit.com/r/emacs/comments/10iuim1/getting_emacs_29_to_automatically_use_treesitter/
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter

(use-package treesit-auto
  :when (executable-find "tree-sitter")
  :demand t
  :bind (("C-M-a" . treesit-beginning-of-defun) ("C-M-e" . treesit-end-of-defun))
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4 "Increase default font locking")
  (treesit-language-source-alist
    '
    ((bash "https://github.com/tree-sitter/tree-sitter-bash")
      (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
      (c "https://github.com/tree-sitter/tree-sitter-c")
      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
      (cmake "https://github.com/uyha/tree-sitter-cmake")
      (css "https://github.com/tree-sitter/tree-sitter-css")
      ;; (docker "https://github.com/camdencheek/tree-sitter-dockerfile")
      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
      (html "https://github.com/tree-sitter/tree-sitter-html")
      (java "https://github.com/tree-sitter/tree-sitter-java")
      ;; (js "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
      (json "https://github.com/tree-sitter/tree-sitter-json")
      (latex "https://github.com/latex-lsp/tree-sitter-latex")
      (make "https://github.com/alemuller/tree-sitter-make")
      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
      (org "https://github.com/milisims/tree-sitter-org")
      (perl "https://github.com/tree-sitter-perl/tree-sitter-perl")
      (python "https://github.com/tree-sitter/tree-sitter-python")
      (toml "https://github.com/tree-sitter/tree-sitter-toml")
      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :config
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; Install grammars
  (when
    (unless
      (and (treesit-language-available-p 'bash)
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
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))))

;; (use-package treesit
;;   :straight (:type built-in)
;;   :when (executable-find "tree-sitter")
;;   :demand t
;;   :bind (("C-M-a" . treesit-beginning-of-defun) ("C-M-e" . treesit-end-of-defun))
;;   :custom (treesit-font-lock-level 4 "Increase default font locking")
;;   :config
;;   (setq treesit-language-source-alist
;;     '
;;     ((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;       (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
;;       (c "https://github.com/tree-sitter/tree-sitter-c")
;;       (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;       (cmake "https://github.com/uyha/tree-sitter-cmake")
;;       (css "https://github.com/tree-sitter/tree-sitter-css")
;;       (docker "https://github.com/camdencheek/tree-sitter-dockerfile")
;;       (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;       (html "https://github.com/tree-sitter/tree-sitter-html")
;;       (java "https://github.com/tree-sitter/tree-sitter-java")
;;       (js "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;       (json "https://github.com/tree-sitter/tree-sitter-json")
;;       (latex "https://github.com/latex-lsp/tree-sitter-latex")
;;       (make "https://github.com/alemuller/tree-sitter-make")
;;       (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;       (org "https://github.com/milisims/tree-sitter-org")
;;       (perl "https://github.com/tree-sitter-perl/tree-sitter-perl")
;;       (python "https://github.com/tree-sitter/tree-sitter-python")
;;       (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;   ;; Old language servers do not support tree-sitter yet.

;;   (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(bibtex-mode . bibtex-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
;;   ;; ;; (add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(latex-mode . latex-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(makefile-mode . make-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(makefile-gmake-mode . make-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(org-mode . org-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(perl-mode . perl-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
;;   ;; (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

;;   ;;   (setq
;;   ;;     bash-ts-mode-hook bash-mode-hook
;;   ;;     c-ts-mode-hook c-mode-hook
;;   ;;     c++-ts-mode-hook c++-mode-hook
;;   ;;     cmake-ts-mode-hook cmake-mode-hook
;;   ;;     css-ts-mode-hook css-mode-hook
;;   ;;     html-ts-mode-hook html-mode-hook
;;   ;;     java-ts-mode-hook java-mode-hook
;;   ;;     json-ts-mode-hook json-mode-hook
;;   ;;     make-ts-mode-hook make-mode-hook
;;   ;;     markdown-ts-mode-hook markdown-mode-hook
;;   ;;     org-ts-mode-hook org-mode-hook
;;   ;;     python-ts-mode-hook python-mode-hook
;;   ;;     yaml-ts-mode-hook yaml-ts-mode-hook)
;;   )

;; (use-package tree-sitter
;;   :when (executable-find "tree-sitter")
;;   :hook
;;   ((tree-sitter-after-on . tree-sitter-hl-mode)
;;     ((c-mode c++-mode) . tree-sitter-mode))
;;   :init (advice-add 'tsc-dyn-get--log :around #'sb/inhibit-message-call-orig-fun)
;;   :config
;;   (use-package tree-sitter-langs
;;     :demand t
;;     :init (advice-add 'tree-sitter-langs-install-grammars :around #'sb/inhibit-message-call-orig-fun))
;;   :diminish tree-sitter-mode)

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
      'company-complete-selection
      'company-complete-common
      'company-capf
      'company-abort))
  :diminish)

;; Available C styles: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Built_002din-Styles
;;   "gnu": The default style for GNU projects
;;   "k&r": What Kernighan and Ritchie, the authors of C used in their book
;;   "bsd": What BSD developers use, aka "Allman style" after Eric Allman.
;;   "whitesmith": Popularized by the examples that came with Whitesmiths C, an early commercial C
;;   compiler.
;;   "stroustrup": What Stroustrup, the author of C++ used in his book
;;   "ellemtel": Popular C++ coding standards as defined by "Programming in C++, Rules and
;;   Recommendations," Erik Nyquist and Mats Henricson, Ellemtel
;;   "linux": What the Linux developers use for kernel development
;;   "python": What Python developers use for extension modules
;;   "java": The default style for java-mode (see below)
;;   "user": When you want to define your own style

(use-package cc-mode
  :straight (:type built-in)
  :mode (("\\.h\\'" . c++-mode) ("\\.c\\'" . c++-mode))
  :hook
  ((c-mode c-ts-mode c++-mode c++-ts-mode)
    .
    (lambda ()
      (setq-local
        c-set-style "cc-mode"
        c-basic-offset 2
        ;; Disable electric indentation and on-type formatting
        c-auto-newline nil
        ;; c-electric-brace nil
        c-electric-flag nil
        ;; c-electric-indent nil
        c-enable-auto-newline nil
        c-syntactic-indentation nil)
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :bind (:map c-mode-base-map ("C-c C-d"))
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection "clangd")
  ;;       :major-modes '(c-mode c++-mode)
  ;;       :remote? t
  ;;       :server-id 'clangd-r)))
  )

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
        ;; Disable electric indentation and on-type formatting
        c-auto-newline nil
        ;; c-electric-brace nil
        c-electric-flag nil
        ;; c-electric-indent nil
        c-enable-auto-newline nil
        c-syntactic-indentation nil)
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred))))))

;; (use-package modern-cpp-font-lock ; Better highlight for modern C++
;;   :hook (c++-mode . modern-c++-font-lock-mode)
;;   :diminish modern-c++-font-lock-mode)

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
      ;; (when (fboundp 'spell-fu-mode)
      ;;   (spell-fu-mode -1))
      (when (fboundp 'flyspell-mode)
        (flyspell-mode -1))
      (when (fboundp 'jinx-mode)
        (jinx-mode -1))
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (progn
            ;; Disable text checkers
            ;; (make-local-variable 'lsp-disabled-clients)
            ;; (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
            (lsp-deferred))))))
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection "cmake-language-server")
  ;;       :major-modes '(cmake-mode)
  ;;       :remote? t
  ;;       :server-id 'cmakels-r)))
  )

;; (use-package cmake-font-lock ; Advanced syntax coloring support for CMake scripts
;;   :hook (cmake-mode . cmake-font-lock-activate))

;; (use-package rmsbolt
;;   :commands rmsbolt-mode)

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
  (python-fill-docstring-style 'django)
  (python-indent-guess-indent-offset-verbose nil "Remove guess indent python message")
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
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-post-activate-hooks
    (list (lambda () (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks (list (lambda () (setq python-shell-interpreter "python3")))))

;; (use-package python-isort
;;   :straight (:host github :repo "wyuenho/emacs-python-isort")
;;   :when (and (executable-find "isort") (eq sb/python-langserver 'pyright))
;;   :hook ((python-mode python-ts-mode) . python-isort-on-save-mode)
;;   :custom
;;   (python-isort-arguments
;;     '
;;     ("--stdout" "--atomic" "-l 100"
;;       "--up" ; Use parentheses
;;       "--tc" ; Use a trailing comma on multiline imports
;;       "-")))

;; We cannot use `lsp-format-buffer' or `eglot-format-buffer' with `pyright' since it does not
;; support document formatting. So, we have to use yapf with pyright. Yapfify works on the original
;; file, so that any project settings supported by YAPF itself are used.

;; (use-package yapfify
;;   :when (and (executable-find "yapf") (eq sb/python-langserver 'pyright))
;;   :hook ((python-mode python-ts-mode) . yapf-mode)
;;   :diminish yapf-mode)

(use-package cperl-mode
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
  :config
  ;; Prefer CPerl mode to Perl mode
  (fset 'perl-mode 'cperl-mode)

  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection
  ;;       (lsp-tramp-connection
  ;;         (lambda ()
  ;;           (list
  ;;             lsp-perl-language-server-path
  ;;             "-MPerl::LanguageServer"
  ;;             "-e"
  ;;             "Perl::LanguageServer::run"
  ;;             "--"
  ;;             (format "--port %d --version %s"
  ;;               lsp-perl-language-server-port
  ;;               lsp-perl-language-server-client-version))))
  ;;       :major-modes '(perl-mode cperl-mode)
  ;;       :remote? t
  ;;       :initialized-fn
  ;;       (lambda (workspace)
  ;;         (with-lsp-workspace
  ;;           workspace
  ;;           (lsp--set-configuration (lsp-configuration-section "perl"))))
  ;;       :priority -1
  ;;       :server-id 'perlls-r)))
  )

;; (use-package ant
;;   :after java-mode
;;   :commands (ant ant-clean ant-compile ant-test))

;; (use-package autodisass-java-bytecode ; Can disassemble ".class" files from within jars
;;   :mode "\\.class\\'")

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
  (sh-indentation 2)
  (sh-indent-after-continuation 'always)
  (sh-indent-comment t "Indent comments as a regular line")
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("bash-language-server" "start"))
  ;;       :major-modes '(sh-mode)
  ;;       :remote? t
  ;;       :server-id 'bashls-r)))
  )

(use-package fish-mode
  :mode "\\.fish\\'"
  :interpreter "fish"
  :hook (fish-mode . (lambda () (add-hook 'before-save-hook #'fish_indent-before-save))))

;; Files are given `+x' permissions when they are saved, if they contain a valid shebang line.
;; (use-package executable
;;   :hook (after-save . executable-make-buffer-file-executable-if-script-p))

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
      ;; (when (fboundp 'spell-fu-mode)
      ;;   (spell-fu-mode -1))
      (when (fboundp 'flyspell-mode)
        (flyspell-mode -1))
      (when (fboundp 'jinx-mode)
        (jinx-mode -1))
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (progn
            (make-local-variable 'lsp-disabled-clients)
            (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
            (lsp-deferred))))))
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("yaml-language-server" "--stdio"))
  ;;       :major-modes '(yaml-mode)
  ;;       :remote? t
  ;;       :server-id 'yamlls-r)))
  )

(use-package yaml-imenu
  :hook ((yaml-mode yaml-ts-mode) . yaml-imenu-enable))

(use-package css-mode
  :hook
  ((css-mode css-ts-mode)
    .
    (lambda ()
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :custom (css-indent-offset 2)
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("css-languageserver" "--stdio"))
  ;;       :major-modes '(css-mode)
  ;;       :remote? t
  ;;       :server-id 'cssls-r)))
  )

(use-package make-mode
  :straight (:type built-in)
  :mode
  (("\\Makefile\\'" . makefile-mode)
    ("\\Makefile.common\\'" . makefile-mode)
    ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
    ("makefile\\.rules\\'" . makefile-mode))
  :hook ((makefile-mode make-ts-mode) . (lambda () (setq-local indent-tabs-mode t))))

;; (use-package makefile-executor
;;   :hook ((makefile-mode make-ts-mode) . makefile-executor-mode))

;; Align fields with "C-c C-a"
(use-package csv-mode
  :hook
  (csv-mode
    .
    (lambda ()
      (make-local-variable 'lsp-disabled-clients)
      (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
      ;; (when (fboundp 'spell-fu-mode)
      ;;   (spell-fu-mode -1))
      (when (fboundp 'flyspell-mode)
        (flyspell-mode -1))
      (when (fboundp 'jinx-mode)
        (jinx-mode -1))))
  :custom (csv-separators '("," ";" "|" " ")))

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

;; (use-package llvm-mode
;;   ;; :straight (llvm-mode :type git :host github
;;   ;;                      :repo "llvm/llvm-project"
;;   ;;                      :files "llvm/utils/emacs/llvm-mode.el")
;;   :straight nil
;;   :load-path "extras"
;;   :commands (llvm-mode)
;;   :mode "\\.ll\\'")

;; (use-package autodisass-llvm-bitcode
;;   :commands (autodisass-llvm-bitcode)
;;   :mode "\\.bc\\'")

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

;; (use-package markdown-toc
;;   :commands (markdown-toc-refresh-toc markdown-toc-generate-toc markdown-toc-generate-or-refresh-toc))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf. Convert `markdown' to `org': "pandoc
;; -f markdown -t org -o output-file.org input-file.md"
(use-package pandoc-mode
  :hook (markdown-mode . pandoc-mode)
  :config (pandoc-load-default-settings)
  :diminish)

;; (use-package markdown-preview-mode ; Open preview of markdown file in a browser
;;   :commands markdown-preview-mode)

;; (use-package bat-mode
;;   :straight (:type built-in)
;;   :mode
;;   (("\\.bat\\'" . bat-mode)
;;    ("\\.cmd\\'" . bat-mode)))

(use-package web-mode
  :mode "\\.html?\\'"
  :hook
  (web-mode
    .
    (lambda ()
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :bind ("C-c C-d")
  :custom
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-quoting t)
  (web-mode-enable-block-face t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t "Highlight the element under the cursor")
  (web-mode-enable-current-column-highlight t)
  (web-mode-markup-indent-offset 2) ; HTML
  (web-mode-css-indent-offset 2) ; CSS
  (web-mode-code-indent-offset 2) ; Script
  (web-mode-style-padding 2) ; For <style> tag
  (web-mode-script-padding 2) ; For <script> tag
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("html-languageserver" "--stdio"))
  ;;       :major-modes '(html-mode web-mode mhtml-mode)
  ;;       :remote? t
  ;;       :server-id 'htmlls-r)))
  )

(use-package emmet-mode
  :hook ((web-mode css-mode css-ts-mode html-mode html-ts-mode) . emmet-mode)
  :custom (emmet-move-cursor-between-quote t))

(use-package nxml-mode
  :straight (:type built-in)
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$" "\\.drawio$")
  :hook
  (nxml-mode
    .
    (lambda ()
      ;; `xml-mode' is derived from `text-mode', so disable grammar and spell checking.
      ;; (when (fboundp 'spell-fu-mode)
      ;;   (spell-fu-mode -1))
      (when (fboundp 'flyspell-mode)
        (flyspell-mode -1))
      (when (fboundp 'jinx-mode)
        (jinx-mode -1))
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (progn
            (make-local-variable 'lsp-disabled-clients)
            (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
            (lsp-deferred))))))
  :custom
  (nxml-auto-insert-xml-declaration-flag t)
  (nxml-slash-auto-complete-flag t)
  (nxml-sexp-element-flag t)
  :config (fset 'xml-mode 'nxml-mode)

  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("java" "-jar" lsp-xml-jar-file))
  ;;       :major-modes '(xml-mode nxml-mode)
  ;;       :remote? t
  ;;       :server-id 'xmlls-r)))
  )

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
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("vscode-json-languageserver" "--stdio"))
  ;;       :major-modes '(json-mode jsonc-mode)
  ;;       :remote? t
  ;;       :server-id 'jsonls-r)))
  )

(use-package json-reformat
  :after (:any json-mode jsonc-mode json-ts-mode)
  :demand t
  :custom
  (json-reformat:indent-width 2)
  (js-indent-level 2))

;; (use-package bazel
;;   :when (executable-find "bazel")
;;   :commands (bazel-mode bazelrc-mode bazel-buildifier)
;;   :hook
;;   ((bazel-mode . (lambda ()
;;                         (add-hook 'before-save-hook #'bazel-buildifier nil t)))
;;    (bazel-mode . flycheck-mode)))

;; (use-package protobuf-mode
;;   :commands (protobuf-mode)
;;   :mode "\\.proto$"
;;   :hook (protobuf-mode . flycheck-mode))

;; (use-package mlir-mode
;;   :straight nil
;;   :load-path "extras"
;;   :mode "\\.mlir\\'")

;; (use-package dotenv-mode
;;   :mode "\\.env\\'")

(use-package apt-sources-list
  :commands apt-sources-list-mode)

(use-package ssh-config-mode
  :mode ("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
  :mode ("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys\\'" . ssh-authorized-keys-mode)
  :hook (ssh-config-mode . turn-on-font-lock))

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
    '
    ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
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

;; Disable the package to get consistent styles across themes.

;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode))

;; (use-package org-superstar
;;   :hook (org-mode . org-superstar-mode))

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
    org-pandoc-export-as-markdown
    org-pandoc-export-to-markdown-and-open))

;; (use-package org-modern
;;   :hook (org-mode . org-modern-mode))

(use-package org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :hook (org-mode . org-modern-indent-mode))

;; (use-package org-block-capf
;;   :straight (:host github :repo "xenodium/org-block-capf")
;;   :hook (org-mode . org-block-capf-add-to-completion-at-point-functions)
;;   :custom (org-block-capf-edit-style 'inline))

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
    (TeX-update-style . rainbow-delimiters-mode)
    ;; Jump between editor and pdf viewer
    (LaTeX-mode . TeX-source-correlate-mode) (LaTeX-mode . turn-on-auto-fill)
    (LaTeX-mode
      .
      (lambda ()
        (cond
          ((eq sb/lsp-provider 'eglot)
            (eglot-ensure))
          ((eq sb/lsp-provider 'lsp-mode)
            (lsp-deferred))))))
  :bind
  (:map
    TeX-mode-map
    ("C-c ;")
    ("C-c C-d")
    ("C-c C-c" . TeX-command-master)
    ("$" . self-insert-command)
    ("C-c x q" . TeX-insert-quote))
  :custom
  (TeX-auto-save t "Enable parse on save, stores parsed information in an `auto' directory")
  (TeX-auto-untabify t "Remove all tabs before saving")
  (TeX-clean-confirm nil)
  ;; Automatically insert braces after typing ^ and _ in math mode
  (TeX-electric-sub-and-superscript t)
  (TeX-electric-math t "Inserting $ completes the math mode and positions the cursor")
  (TeX-parse-self t "Parse documents")
  (TeX-quote-after-quote nil "Allow original LaTeX quotes")
  (TeX-save-query nil "Save buffers automatically when compiling")
  (TeX-source-correlate-method 'synctex)
  ;; Do not start the Emacs server when correlating sources
  (TeX-source-correlate-start-server t)
  (TeX-syntactic-comment t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (LaTeX-item-indent 0 "Indent lists by two spaces")
  (LaTeX-syntactic-comments t)
  (LaTeX-fill-break-at-separators nil "Do not insert line-break at inline math")
  (tex-fontify-script nil "Avoid raising of superscripts and lowering of subscripts")
  ;; Avoid superscripts and subscripts from being displayed in a different font size
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 1.0 "Avoid emphasizing section headers")
  :config
  (when (executable-find "okular")
    (setq
      TeX-view-program-list
      '(("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
      TeX-view-program-selection '((output-pdf "Okular"))))

  ;; Always query for the master file
  (setq-default TeX-master nil)
  (with-eval-after-load "auctex"
    (bind-key "C-c C-e" LaTeX-environment LaTeX-mode-map)
    (bind-key "C-c C-s" LaTeX-section LaTeX-mode-map)
    (bind-key "C-c C-m" TeX-insert-macro LaTeX-mode-map)))

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

;; Reftex is useful to view ToC even with LSP support
(use-package reftex
  ;;   :preface
  ;;   (defun sb/get-bibtex-keys (file)
  ;;     (with-current-buffer (find-file-noselect file)
  ;;       (mapcar 'car (bibtex-parse-keys))))

  ;;   (defun sb/reftex-add-all-bibitems-from-bibtex ()
  ;;     (interactive)
  ;;     (mapc
  ;;       'LaTeX-add-bibitems
  ;;       (apply 'append (mapcar 'sb/get-bibtex-keys (reftex-get-bibfile-list)))))

  ;;   (defun sb/find-bibliography-file ()
  ;;     "Try to find a bibliography file using RefTeX.
  ;;       Returns a string with text properties (as expected by read-file-name) or
  ;; empty string if no file can be found"
  ;;     (interactive)
  ;;     (let ((bibfile-list nil))
  ;;       (condition-case nil
  ;;         (setq bibfile-list (reftex-get-bibfile-list))
  ;;         (error
  ;;           (ignore-errors
  ;;             (setq bibfile-list (reftex-default-bibliography)))))
  ;;       (if bibfile-list
  ;;         (car bibfile-list)
  ;;         "")))

  ;;   (defun sb/reftex-try-add-all-bibitems-from-bibtex ()
  ;;     "Try to find a bibliography file using RefTex and parse the bib keys.
  ;; Ignore if no file is found."
  ;;     (interactive)
  ;;     (let ((bibfile-list nil))
  ;;       (condition-case nil
  ;;         (setq bibfile-list (reftex-get-bibfile-list))
  ;;         (error
  ;;           (ignore-errors
  ;;             (setq bibfile-list (reftex-default-bibliography)))))
  ;;       ;; (message "%s" bibfile-list)
  ;;       (mapc 'LaTeX-add-bibitems (apply 'append (mapcar 'sb/get-bibtex-keys bibfile-list)))))
  :straight (:type built-in)
  :hook (LaTeX-mode . turn-on-reftex)
  :bind
  (("C-c [" . reftex-citation)
    ("C-c )" . reftex-reference)
    ("C-c (" . reftex-label)
    ("C-c =" . reftex-toc)
    ("C-c -" . reftex-toc-recenter)
    ("C-c &" . reftex-view-crossref))
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-enable-partial-scans t)
  (reftex-highlight-selection 'both)
  (reftex-save-parse-info t "Save parse info to avoid reparsing every time a file is visited")
  (reftex-revisit-to-follow t)
  (reftex-auto-recenter-toc t "Center on the section currently being edited")
  (reftex-toc-follow-mode t "Other buffer follows the point in TOC buffer")
  (reftex-toc-split-windows-fraction 0.6 "Give TOC buffer more room")
  (reftex-toc-split-windows-horizontally t) ; Show reftex TOC on the left
  (reftex-ref-macro-prompt nil) ; No unnecessary prompts
  ;; (reftex-guess-label-type t "Try to guess the label type before prompting")
  (reftex-use-fonts t "Use nice fonts for TOC")
  ;; (reftex-revisit-to-follow t "Revisit files if necessary when browsing toc")
  (reftex-use-multiple-selection-buffers t "Cache selection buffers for faster access")
  ;; Throw away buffers created for parsing, but keep the ones created for lookup
  (reftex-keep-temporary-buffers 1)
  (reftex-trust-label-prefix '("fn:" "eq:" "sec:" "fig:" "tab:"))
  (reftex-allow-automatic-rescan nil)
  (reftex-enable-partial-scans t)
  :config
  ;; (sb/reftex-try-add-all-bibitems-from-bibtex)
  ;; (add-hook 'reftex-load-hook #'sb/reftex-add-all-bibitems-from-bibtex)

  (with-eval-after-load "reftex-toc"
    (bind-keys
      :package reftex-toc
      :map
      reftex-toc-mode-map
      ("n" . reftex-toc-next)
      ("p" . reftex-toc-previous)
      ("r" . reftex-toc-rescan)
      ("R" . reftex-toc-Rescan)
      ("g" . revert-buffer)
      ("q" . reftex-toc-quit)
      ("z" . reftex-toc-jump)
      (">" . reftex-toc-demote)
      ("<" . reftex-toc-promote))

    ;; Rescan the entire document, not only the current file (`reftex-toc-rescan'), to be consistent
    ;; but this is expensive.
    (add-hook 'reftex-toc-mode-hook #'reftex-toc-rescan))
  :diminish)

;; Read document like a hypertext document, supports mouse highlighting
(use-package bib-cite
  :straight (:type built-in)
  :hook (LaTeX-mode . (lambda () (bib-cite-minor-mode 1)))
  ;;   ;; :bind
  ;;   ;; (:map bib-cite-minor-mode-map
  ;;   ;;       ("C-c b") ; We use `C-c b' for `comment-box'
  ;;   ;;       ("C-c l a" . bib-apropos)
  ;;   ;;       ("C-c l b" . bib-make-bibliography)
  ;;   ;;       ("C-c l d" . bib-display)
  ;;   ;;       ("C-c l t" . bib-etags)
  ;;   ;;       ("C-c l f" . bib-find)
  ;;   ;;       ("C-c l n" . bib-find-next))
  :custom (bib-cite-use-reftex-view-crossref t "Use RefTeX functions for finding bibliography files")
  :diminish bib-cite-minor-mode)

(use-package auctex-latexmk
  :after tex
  :when (executable-find "latexmk")
  :demand t
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t "Pass the '-pdf' flag when `TeX-PDF-mode' is active")
  :config
  (setq-default TeX-command-default "LatexMk")
  (auctex-latexmk-setup))

(with-eval-after-load "latex"
  (unbind-key "C-j" LaTeX-mode-map)
  ;; Disable `LaTeX-insert-item' in favor of `imenu'
  (unbind-key "C-c C-j" LaTeX-mode-map)

  (bind-key "C-c x q" #'TeX-insert-quote LaTeX-mode-map))

;; `math-preview' requires external nodejs program "math-preview". Make sure that "math-preview" is
;; in "$PATH".

;; (use-package math-preview
;;   :straight (:host gitlab :repo "matsievskiysv/math-preview")
;;   :commands (math-preview-all math-preview-at-point math-preview-region)
;;   :custom (math-preview-command (expand-file-name "node_modules/.bin/math-preview" sb/user-tmp-directory)))

;; TODO: Try `citar' https://github.com/emacs-citar/citar

;; Set `bibtex-capf-bibliography' in `.dir-locals.el'.
(use-package bibtex-capf
  :straight (:host github :repo "mclear-tools/bibtex-capf")
  :when (eq sb/capf 'corfu)
  :hook ((LaTeX-mode reftex-mode) . bibtex-capf-mode))

(use-package latex-extra
  :straight (:host github :repo "Malabarba/latex-extra")
  :after tex
  :hook (LaTeX-mode . latex-extra-mode)
  :bind
  (:map
    TeX-mode-map
    ("C-c C-a" . latex/compile-commands-until-done)
    ("C-c C-n" . latex/next-section)
    ("C-c C-u" . latex/up-section)
    ("C-c C-f" . latex/next-section-same-level)
    ("C-M-f" . latex/forward-environment)
    ("C-M-e" . latex/end-of-environment)
    ("C-M-b" . latex/backward-environment)
    ("C-M-a" . latex/beginning-of-environment)
    ("C-c C-p" . latex/previous-section)
    ("C-c C-b" . latex/previous-section-same-level)
    ("C-c C-q" . latex/clean-fill-indent-environment))
  :diminish)

;; (use-package math-delimiters
;;   :straight (:host github :repo "oantolin/math-delimiters")
;;   :after tex
;;   :bind (:map TeX-mode-map ("$" . math-delimiters-insert)))

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
  :custom
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package dumb-jump
  :after xref
  :demand t
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg))

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
    (let
      (
        (lsp-result
          (cond
            ((bound-and-true-p lsp-mode)
              (and (fboundp #'lsp-completion-at-point) (lsp-completion-at-point)))
            ((bound-and-true-p eglot--managed-mode)
              (and (fboundp #'eglot-completion-at-point) (eglot-completion-at-point))))))
      (if
        (and lsp-result
          (try-completion
            (buffer-substring (nth 0 lsp-result) (nth 1 lsp-result))
            (nth 2 lsp-result)))
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
  (citre-enable-capf-integration nil)
  ;; Enabling this breaks imenu for Elisp files, it cannot identify `use-package' definitions
  (citre-enable-imenu-integration nil)
  (citre-enable-xref-integration t)
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
  :config
  ;; (add-hook 'citre-mode-hook #'sb/enable-lsp-citre-capf-backend)

  (dolist
    (func
      '
      (find-function ;counsel-imenu counsel-rg lsp-ivy-workspace-symbol
        projectile-grep
        citre-jump))
    (advice-add func :before 'sb/push-point-to-xref-marker-stack))

  ;; Try lsp first, then use Citre
  (with-no-warnings
    (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
      (let
        (
          (fetcher (apply -fn -args))
          (citre-fetcher
            (let ((xref-backend-functions '(citre-xref-backend t)))
              (apply -fn -args))))
        (lambda ()
          (or
            (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher))))))

  (with-eval-after-load "company"
    (defmacro citre-backend-to-company-backend (backend)
      "Create a company backend from Citre completion backend BACKEND.
The result is a company backend called
`company-citre-<backend>' (like `company-citre-tags') and can be
used in `company-backends'."
      (let
        (
          (backend-name (intern (concat "company-citre-" (symbol-name backend))))
          (docstring
            (concat
              "`company-mode' backend from the `"
              (symbol-name backend)
              "' Citre backend.\n"
              "`citre-mode' needs to be enabled to use this.")))
        `
        (defun ,backend-name (command &optional arg &rest ignored)
          ,docstring
          (pcase command
            ('interactive (company-begin-backend ',backend-name))
            ('prefix
              (and (bound-and-true-p citre-mode)
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

;; (use-package window-stool
;;   :straight (:host github :repo "jaszhe/window-stool")
;;   :hook (prog-mode . window-stool-mode))

;; (use-package treesitter-context
;;   :straight (:host github :repo "zbelial/treesitter-context.el")
;;   :after treesit
;;   :init
;;   (use-package posframe-plus
;;     :straight (:host github :repo "zbelial/posframe-plus"))
;;   :hook
;;   (
;;     (c-ts-mode
;;       c++-ts-mode
;;       python-ts-mode
;;       java-ts-mode
;;       json-ts-mode
;;       yaml-ts-mode)
;;     . treesitter-context-mode)
;;   :diminish)

;; (use-package symbols-outline
;;   :bind ("C-c i" . symbols-outline-show)
;;   :custom (symbols-outline-window-position 'right)
;;   :config
;;   ;; By default the ctags backend is selected
;;   (unless (executable-find "ctags")
;;     ;; Use lsp-mode or eglot as backend
;;     (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch))
;;   (symbols-outline-follow-mode 1))

;; This is independent of LSP support and is more flexible.
(use-package breadcrumb
  :straight (:host github :repo "joaotavora/breadcrumb")
  :hook (emacs-startup . breadcrumb-mode))

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

(defun sb/toggle-line-spacing ()
  "Toggle line spacing.  Increase the line spacing to help readability.
Increase line spacing by two line height."
  (interactive)
  (if (eq line-spacing nil)
    (setq line-spacing 2)
    (setq line-spacing nil))
  (redraw-frame (selected-frame)))

(defun sb/byte-compile-current-file ()
  "Byte compile the current file."
  (interactive)
  (byte-compile-file buffer-file-name))

(defun sb/byte-compile-init-dir ()
  "Byte-compile all elisp files in the user init directory."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun sb/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
    (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(defun sb/switch-to-scratch ()
  "Switch to the *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun sb/insert-date (arg)
  "Insert today's date.  With prefix argument ARG, use a different format."
  (interactive "P")
  (insert
    (if arg
      (format-time-string "%d.%m.%Y")
      (format-time-string "%\"Mmmm\" %d, %Y"))))

(defun sb/move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive
    (list
      (if buffer-file-name
        (read-file-name "Move file to: ")
        (read-file-name "Move file to: "
          default-directory
          (expand-file-name (file-name-nondirectory (buffer-name)) default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when
      (and old-location (file-exists-p new-location) (not (string-equal old-location new-location)))
      (delete-file old-location))))

(defcustom sb/skippable-buffers
  '
  ("TAGS"
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
  '
  (dired-mode
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
    flycheck-verify-mode
    ibuffer-mode
    bs-mode)
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
        (while
          (or (member (buffer-name) sb/skippable-buffers)
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
  (let
    (
      (modes ())
      (parent nil))
    (while (setq parent (get mode 'derived-mode-parent))
      (push parent modes)
      (setq mode parent))
    (setq modes (nreverse modes))))

(defun sb/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
    (progn
      (linum-mode 1)
      (forward-line (read-number "Goto line: ")))
    (linum-mode -1)))

;; Configure appearance-related settings at the end

;; Install fonts with "M-x all-the-icons-install-fonts"
;; (use-package all-the-icons
;;   :preface
;;   ;; FIXME: This seems to work only with GUI Emacs.
;;   (defun sb/font-installed-p (font-name)
;;     "Check if font with FONT-NAME is available."
;;     (if (find-font (font-spec :name font-name))
;;       t
;;       nil))
;;   :when (or (eq sb/icons-provider 'all-the-icons) (eq sb/tab-bar-handler 'centaur-tabs))
;;   :commands all-the-icons-install-fonts
;;   :init
;;   (if (and (display-graphic-p) (not (sb/font-installed-p "all-the-icons")))
;;     (all-the-icons-install-fonts t))
;;   :custom
;;   ;; Small icons look nicer
;;   (all-the-icons-scale-factor 0.9)
;;   (all-the-icons-faicon-scale-factor 0.9)
;;   (all-the-icons-wicon-scale-factor 0.9)
;;   (all-the-icons-octicon-scale-factor 0.9)
;;   (all-the-icons-fileicon-scale-factor 0.9)
;;   (all-the-icons-material-scale-factor 0.9)
;;   (all-the-icons-alltheicon-scale-factor 0.9)
;;   (all-the-icons-color-icons t))

;; (use-package all-the-icons-dired
;;   :when (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p) (not (featurep 'dirvish)))
;;   :commands (all-the-icons-dired--refresh-advice)
;;   :hook
;;   (dired-mode
;;     .
;;     (lambda ()
;;       (unless (file-remote-p default-directory)
;;         (all-the-icons-dired-mode 1))))
;;   :custom (all-the-icons-dired-monochrome nil)
;;   :diminish)

;; ;; Display icons for all buffers in ibuffer
;; (use-package all-the-icons-ibuffer
;;   :when (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p))
;;   :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
;;   :custom (all-the-icons-ibuffer-icon-size 0.8))

;; ;; Icons for minibuffer completion (e.g., `find-file-at-point')
;; (use-package all-the-icons-completion
;;   :when (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p))
;;   :commands all-the-icons-completion-mode
;;   :init (all-the-icons-completion-mode 1)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

;; (use-package nerd-icons
;;   :straight (:host github :repo "rainstormstudio/nerd-icons.el")
;;   ;; `nerd-icons-ivy-rich' depends on this package
;;   :when (or (eq sb/icons-provider 'nerd-icons) (eq sb/minibuffer-completion 'ivy))
;;   :custom
;;   (nerd-icons-color-icons nil)
;;   (nerd-icons-scale-factor 0.9))

;; (use-package nerd-icons-completion
;;   :straight (:host github :repo "rainstormstudio/nerd-icons-completion")
;;   :when (eq sb/icons-provider 'nerd-icons)
;;   :init (nerd-icons-completion-mode 1)
;;   :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; (use-package nerd-icons-dired
;;   :straight (:host github :repo "rainstormstudio/nerd-icons-dired")
;;   :when (eq sb/icons-provider 'nerd-icons)
;;   :hook (dired-mode . nerd-icons-dired-mode)
;;   :diminish)

;; (use-package nerd-icons-ibuffer
;;   :when (eq sb/icons-provider 'nerd-icons)
;;   :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
;;   :custom (nerd-icons-ibuffer-icon-size 1.0))

;; Decrease minibuffer font size
(progn
  (defun sb/decrease-minibuffer-font ()
    "Customize minibuffer font."
    (set (make-local-variable 'face-remapping-alist) '((default :height 0.90))))

  (add-hook 'minibuffer-setup-hook #'sb/decrease-minibuffer-font))

;; (use-package beacon ; Highlight the cursor position after the window scrolls
;;   :hook (emacs-startup . beacon-mode)
;;   :diminish)

;; The color sometimes makes it difficult to distinguish text on terminals.
;; (use-package hl-line
;;   :hook (emacs-startup . global-hl-line-mode))

;; Copying text from the TUI includes the line numbers, which is a nuisance. So, enable line
;; numbers only for GUI and daemon.
;; (when (or (display-graphic-p) (daemonp))
;;   (global-display-line-numbers-mode 1))

;; (use-package centaur-tabs
;;   :when (eq sb/tab-bar-handler 'centaur-tabs)
;;   :hook (emacs-startup . centaur-tabs-mode)
;;   :bind*
;;   (("M-<right>" . centaur-tabs-forward-tab)
;;     ("M-<left>" . centaur-tabs-backward-tab)
;;     ("M-\"" . centaur-tabs-ace-jump))
;;   :custom
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker "•") ; Unicode Bullet (0x2022)
;;   (centaur-tabs-set-close-button nil "I do not use the mouse")
;;   (centaur-tabs-show-new-tab-button nil "I do not use the mouse")
;;   (centaur-tabs-enable-ido-completion nil)
;;   ;; Other styles like "wave" are not rendered on the terminal, and also does not work well with
;;   ;; many themes
;;   (centaur-tabs-style "bar")
;;   (centaur-tabs-set-bar 'under)
;;   (centaur-tabs-height 18)
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-icon-type 'nerd-icons)
;;   (centaur-tabs-gray-out-icons t "Gray out icons for inactive tabs")
;;   (centaur-tabs-show-count t "Helpful to identify tab overflows")
;;   :config
;;   ;; Unlike `awesome-tab', the icons do not blend well with all themes.

;;   ;; (let ((themes '("doom-one"
;;   ;;                 "doom-nord"
;;   ;;                 "doom-molokai")))
;;   ;;   (progn
;;   ;;     (if (-contains? themes (symbol-name sb/theme))
;;   ;;         (setq centaur-tabs-set-icons t)
;;   ;;       (setq centaur-tabs-set-icons nil))))

;;   ;; (centaur-tabs-headline-match)

;;   ;; Group tabs according to projectile's definition of projects
;;   (with-eval-after-load "projectile"
;;     (centaur-tabs-group-by-projectile-project)))

;; (use-package awesome-tab
;;   :preface
;;   (defun sb/awesome-tab-buffer-groups ()
;;     "`awesome-tab-buffer-groups' control buffers' group rules.
;;   Group awesome-tab with mode if buffer is derived from
;;   `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode'
;;   `magit-mode'. All buffer name start with * will group to
;;   \"Emacs\". Other buffer group by `awesome-tab-get-group-name'
;;   with project name."
;;     (list
;;       (cond
;;         (
;;           (or (string-equal "*" (substring (buffer-name) 0 1))
;;             (memq
;;               major-mode
;;               '
;;               (magit-process-mode
;;                 magit-status-mode
;;                 magit-diff-mode
;;                 magit-log-mode
;;                 magit-file-mode
;;                 magit-blob-mode
;;                 magit-blame-mode)))
;;           "Emacs")
;;         (t
;;           (awesome-tab-get-group-name (current-buffer))))))
;;   :straight (:host github :repo "manateelazycat/awesome-tab")
;;   :when (eq sb/tab-bar-handler 'awesome-tab)
;;   :hook (emacs-startup . awesome-tab-mode)
;;   :bind
;;   (("M-<right>" . awesome-tab-forward-tab)
;;     ("M-<left>" . awesome-tab-backward-tab)
;;     ("M-]" . awesome-tab-ace-jump))
;;   :custom-face
;;   (awesome-tab-selected-face ((t (:inherit default :height 1.0))))
;;   (awesome-tab-unselected-face ((t (:inherit default :height 0.8))))
;;   :custom
;;   (awesome-tab-label-fixed-length 14)
;;   (awesome-tab-cycle-scope 'tabs)
;;   :config
;;   ;; The variable is declared with a `defvar', so modifying it with `:custom' will not work.
;;   (setq awesome-tab-buffer-groups-function #'sb/awesome-tab-buffer-groups))

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
  :init (load-theme 'nano-dark t))

(use-package nordic-night-theme
  :when (eq sb/theme 'nordic-night)
  :init (load-theme 'nordic-night t))

(use-package leuven-theme
  :when (or (eq sb/theme 'leuven) (eq sb/theme 'leuven-dark))
  :init
  (cond
    ((eq sb/theme 'leuven)
      (load-theme 'leuven t))
    ((eq sb/theme 'leuven-dark)
      (load-theme 'leuven-dark t))))

(when (and (eq sb/theme 'sb/customized) (display-graphic-p))
  (progn
    (set-foreground-color "#333333")
    (with-eval-after-load "hl-line"
      (set-face-attribute 'hl-line nil :background "light yellow"))
    (set-face-attribute 'region nil :background "gainsboro")))

;; Set `sb/theme' to `none' if you use this package
;; (use-package circadian
;;   :hook (emacs-startup . circadian-setup)
;;   :custom (circadian-themes '((:sunrise . modus-vivendi) (:sunset . modus-vivendi))))

;; (use-package ef-themes
;;   :straight (:host github :repo "protesilaos/ef-themes")
;;   :when (or (eq sb/theme 'ef-trio-dark) (eq sb/theme 'ef-bio))
;;   :init
;;   (cond
;;     ((eq sb/theme 'ef-trio-dark)
;;       (load-theme 'ef-trio-dark t))
;;     ((eq sb/theme 'ef-bio)
;;       (load-theme 'ef-bio t))))

;; (use-package standard-themes
;;   :straight (:host github :repo "protesilaos/standard-themes")
;;   :if (or (eq sb/theme 'standard-light) (eq sb/theme 'standard-dark))
;;   :init
;;   (cond
;;     ((eq sb/theme 'standard-light)
;;       (load-theme 'standard-light t))
;;     ((eq sb/theme 'standard-dark)
;;       (load-theme 'standard-dark t))))

;; Python virtualenv information is not shown on the modeline. The package is not being actively
;; maintained.
(use-package powerline
  :preface
  (defun sb/powerline-raw (str &optional face pad)
    "Render STR as mode-line data using FACE and optionally PAD import.
PAD can be left (`l') or right (`r')."
    (when str
      (let*
        (
          (rendered-str (format-mode-line str))
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
      '
      ("%e"
        (:eval
          (let*
            (
              (active (powerline-selected-window-active))
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
  (doom-modeline-height 28)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-checker-simple-format nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'file-name "Reduce space on the modeline")
  (doom-modeline-unicode-fallback t))

;; (use-package awesome-tray ; Minimal modeline information
;;   :straight (:host github :repo "manateelazycat/awesome-tray")
;;   :when (eq sb/modeline-theme 'awesome-tray)
;;   :hook (emacs-startup . awesome-tray-mode)
;;   :custom
;;   (awesome-tray-active-modules
;;     '("file-path" "buffer-name" "mode-name" "location" "belong" "flymake" "git" "hostname"))
;;   (awesome-tray-essential-modules '("file-path" "buffer-name" "location"))
;;   (awesome-tray-file-path-full-dirname-levels 2)
;;   (awesome-tray-evil-show-mode nil)
;;   (awesome-tray-meow-show-mode nil)
;;   (awesome-tray-mode-line-active-color "lavender")
;;   :custom-face
;;   (awesome-tray-default-face ((t (:inherit default :height 0.8))))
;;   (awesome-tray-module-awesome-tab-face ((t (:foreground "#b83059" :weight bold :height 0.8))))
;;   (awesome-tray-module-buffer-name-face ((t (:foreground "#cc7700" :weight bold :height 0.8))))
;;   (awesome-tray-module-date-face ((t (:foreground "#717175" :weight bold :height 0.8))))
;;   (awesome-tray-module-file-path-face ((t (:foreground "#5e8e2e" :weight normal :height 0.8))))
;;   (awesome-tray-module-git-face ((t (:foreground "#cc2444" :weight normal :height 0.8))))
;;   (awesome-tray-module-last-command-face ((t (:foreground "#0061cc" :weight bold :height 0.8))))
;;   (awesome-tray-module-location-face ((t (:foreground "#cc7700" :weight normal :height 0.8))))
;;   (awesome-tray-module-mode-name-face ((t (:foreground "#00a400" :weight bold :height 0.8))))
;;   (awesome-tray-module-parent-dir-face ((t (:foreground "#5e8e2e" :weight bold :height 0.8)))))

(use-package nano-modeline
  :when (eq sb/modeline-theme 'nano-modeline)
  :hook
  ((prog-mode . nano-modeline-prog-mode)
    (text-mode . nano-modeline-text-mode)
    (org-mode . nano-modeline-org-mode)
    (pdf-view-mode . nano-modeline-pdf-mode)
    (messages-buffer-mode . nano-modeline-message-mode))
  :custom (nano-modeline-position #'nano-modeline-footer))

;; The value of font height is in 1/10pt, so 100 implies 10pt. Font preferences will be ignored when
;; we use TUI Emacs. Then, the terminal font setting will be used.

(defun sb/init-fonts-graphic ()
  (cond
    ((string= (system-name) "swarnendu-Inspiron-7572")
      (progn
        (set-face-attribute 'default nil :font "JetBrainsMono NF" :height 200)
        ;; (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 180)
        (set-face-attribute 'mode-line nil :height 130)
        (set-face-attribute 'mode-line-inactive nil :height 130)))

    ((string= (system-name) "DESKTOP-4T8O69V") ; Inspiron 7572 Windows
      (progn
        (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 150)
        (set-face-attribute 'mode-line nil :height 110)
        (set-face-attribute 'mode-line-inactive nil :height 110)))

    ((string= (system-name) "dell-7506")
      (progn
        (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 150)
        (set-face-attribute 'mode-line nil :height 120)
        (set-face-attribute 'mode-line-inactive nil :height 120)))

    ((string= (system-name) "swarnendu-Dell-XPS-L502X")
      (progn
        (set-face-attribute 'default nil :font "MesloLGS NF" :height 150)
        (set-face-attribute 'mode-line nil :height 110)
        (set-face-attribute 'mode-line-inactive nil :height 110)))

    ((string= (system-name) "DESKTOP-LDLQMCO") ; CSE Desktop Windows
      (progn
        (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 150)
        (set-face-attribute 'mode-line nil :height 110)
        (set-face-attribute 'mode-line-inactive nil :height 110)))

    ((string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
      (progn
        (set-face-attribute 'default nil :font "Hack Nerd Font" :height 180)
        (set-face-attribute 'mode-line nil :height 130)
        (set-face-attribute 'mode-line-inactive nil :height 130)))))

(add-hook 'emacs-startup-hook #'sb/init-fonts-graphic)

;; (defun sb/init-fonts-daemon (frame)
;;   (message "getting called")
;;   (cond
;;     ((string= (system-name) "inspiron-7572")
;;       (progn
;;         ;; (add-to-list 'default-frame-alist '(font . "JetBrainsMonoNF-18"))
;;         (add-to-list 'default-frame-alist '(font . "MesloLGSNF-20"))))))
;; FIXME: The hook is not working.
;; (add-hook 'server-after-make-frame-functions #'sb/init-fonts-daemon 'append)

(when (daemonp)
  (cond
    ((string= (system-name) "swarnendu-Inspiron-7572")
      (progn
        ;; (add-to-list 'default-frame-alist '(font . "JetBrainsMonoNF-18"))
        (add-to-list 'default-frame-alist '(font . "MesloLGSNF-18"))))

    ((string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
      (progn
        (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 180)))))

;; (use-package disable-mouse
;;   :hook (emacs-startup . disable-mouse-global-mode)
;;   :diminish disable-mouse-global-mode)

(use-package olivetti
  :hook
  ((text-mode prog-mode) . olivetti-mode) ; `emacs-startup' does not work
  :custom (olivetti-body-width 108)
  :diminish)

;; (use-package selected-window-accent-mode
;;   :hook (emacs-startup . selected-window-accent-mode)
;;   :custom
;;   (selected-window-accent-fringe-thickness 10)
;;   (selected-window-accent-custom-color nil)
;;   (selected-window-accent-mode-style 'subtle))

;; Inside strings, special keys like tab or F1-Fn have to be written inside angle brackets, e.g.
;; "C-<up>". Standalone special keys (and some combinations) can be written in square brackets, e.g.
;; [tab] instead of "<tab>".

(bind-keys
  ;; ("RET" . newline-and-indent)
  ("C-l" . goto-line)

  ("C-c z" . repeat)
  ("C-z" . undo)

  ("C-<f11>" . delete-other-windows)
  ("C-c d f" . auto-fill-mode)

  ("<f7>" . previous-error) ; "M-g p" is the default keybinding
  ("<f8>" . next-error) ; "M-g n" is the default keybinding

  ("C-x C-v" . find-alternate-file)
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
(unbind-key "C-j")

(unbind-key "C-x s") ; Bound to `save-some-buffers'
(bind-key "C-x s" #'scratch-buffer)

(unless sb/tab-bar-handler
  (global-set-key [remap next-buffer] #'sb/next-buffer)
  (global-set-key [remap previous-buffer] #'sb/previous-buffer)

  (bind-keys
    ("M-<left>" . sb/previous-buffer)
    ("C-S-<iso-lefttab>" . sb/previous-buffer)
    ("M-<right>" . sb/next-buffer)
    ("C-<tab>" . sb/next-buffer)))

;; (use-package default-text-scale
;;   :when (display-graphic-p)
;;   :bind (("C-M-+" . default-text-scale-increase) ("C-M--" . default-text-scale-decrease)))

(use-package free-keys
  :commands free-keys)

;; Show help popups for prefix keys
(use-package which-key
  :hook (emacs-startup . which-key-mode)
  :custom (which-key-sort-order 'which-key-key-order-alpha)
  :config (which-key-setup-side-window-right-bottom)
  :diminish)

;; Hydras, https://github.com/abo-abo/hydra

;; ":exit nil" means the hydra state will continue, ":exit t" will quit the hydra. ":color red"
;; means continue the hydra on a valid key but stop when a foreign key has been pressed. ":color
;; blue" means exit.

;; (use-package hydra)

;; (use-package hydra-posframe
;;   :straight (hydra-posframe :type git :host github :repo "Ladicle/hydra-posframe")
;;   :when (display-graphic-p)
;;   :after hydra
;;   :init (hydra-posframe-mode 1))

;; (use-package ivy-hydra ; Additional keybindings for `ivy'
;;   :after (ivy hydra)
;;   :demand t)

;; (use-package pretty-hydra
;;   :after hydra
;;   :demand t)

;; ;; https://github.com/WalkerGriggs/dot-emacs/blob/master/configs/hydra.el
;; (defun sb/with-alltheicon (icon str &optional height v-adjust)
;;   "Displays ICON from all-the-icon in STR."
;;   (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

;; (defun sb/with-faicon (icon str &optional height v-adjust)
;;   "Displays icon from Font Awesome icon in STR."
;;   (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

;; (defun sb/with-fileicon (icon str &optional height v-adjust)
;;   "Displays an icon from the Atom File Icons package."
;;   (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

;; (defun sb/with-octicon (icon str &optional height v-adjust)
;;   "Displays an icon from the GitHub Octicons."
;;   (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

;; (pretty-hydra-define
;;   sb/hydra-spelling
;;   (:color
;;     amaranth
;;     :quit-key "q"
;;     :title (sb/with-faicon "magic" "Spell check" 1 -0.05)
;;     :foreign-keys warn)
;;   ("Action" (("c" ispell "ispell") ("f" flyspell-buffer "flyspell buffer")) "Correct"
;;     (("<" flyspell-correct-previous "correct previous error")
;;       (">" flyspell-correct-next "correct next error"))
;;     "Spell fu"
;;     (("n" spell-fu-goto-next-error "next error")
;;       ("p" spell-fu-goto-previous-error "previous error")
;;       ("a" spell-fu-word-add "add word"))))

;; (pretty-hydra-define sb/hydra-text-scale-zoom
;;   (:color amaranth :quit-key "q" :title "Zoom action" :foreign-keys warn)
;;   (""
;;    (("i" default-text-scale-increase "zoom in")
;;     ("o" default-text-scale-decrease "zoom out")
;;     ("r" default-text-scale-reset "reset"))))

;; (pretty-hydra-define
;;   sb/hydra-error
;;   (:color amaranth :quit-key "q" :title "Navigate errors" :foreign-keys warn)
;;   (""
;;     (("h" first-error "first error")
;;       ("j" next-error "next error")
;;       ("k" previous-error "previous error")
;;       ("v" recenter-top-bottom "recenter"))))

;; https://github.com/abo-abo/hydra/wiki/avy
;; (pretty-hydra-define
;;   sb/hydra-avy (:color red :title "Actions" :quit-key "q" :foreign-keys warn)
;;   ("Line"
;;     (("y" avy-copy-line "yank line")
;;       ("m" avy-move-line "move line")
;;       ("l" avy-goto-line "go to line")
;;       ("L" avy-goto-end-of-line "go to end of line")
;;       ("k" avy-kill-whole-line "kill whole line"))
;;     "Region"
;;     (("Y" avy-copy-region "copy region ")
;;       ("M" avy-move-region "move region")
;;       ("K" avy-kill-region "kill region"))
;;     "Word"
;;     (("w" avy-goto-word-1 "go to word") ("W" avy-goto-word-0 "go to word"))
;;     "Character"
;;     (("c" avy-goto-char-timer "go to char with timer") ("C" avy-goto-char "go to char"))))

;; (pretty-hydra-define sb/hydra-projectile
;;   (:color teal :hint nil global-map "C-c p" :quit-key "q" :foreign-keys warn)
;;   ("PROJECTILE: %(projectile-project-root)"
;;    (("p"   projectile-switch-project "switch project")
;;     ("s"   projectile-switch-project "switch project")
;;     ("a"   projectile-add-known-project "add known project")
;;     ("x"   projectile-remove-known-project "remove known project")
;;     ("X"   projectile-cleanup-known-projects "clean up known projects"))
;;    "Files"
;;    (("f"   projectile-find-file "find file")
;;     ("F"   projectile-find-file-dwim "find file dwim")
;;     ("d"   projectile-find-dir "find directory")
;;     ("D"   projectile-find-file-in-directory "file file in directory"))
;;    "Buffers"
;;    (("i"   projectile-ibuffer "ibuffer")
;;     ("b"   projectile-switch-to-buffer "switch to buffer")
;;     ("r"   projectile-recentf "recent files")
;;     ("k"   projectile-kill-buffers "kill all buffers"))
;;    "Misc"
;;    (("c"   projectile-invalidate-cache "invalidate cache")
;;     ("z"   projectile-cache-current-file "cache current file")
;;     ("g"   projectile-find-tag "find tag")
;;     ("o"   projectile-multi-occur "multi occur")
;;     ("m"   projectile-compile "compile"))))

;; (pretty-hydra-define
;;   sb/hydra-move-text
;;   (:quit-key "q" :title "Move text" :foreign-keys warn)
;;   ("" (("u" move-text-up "up") ("d" move-text-down "down"))))

;; (pretty-hydra-define
;;   sb/hydra-flycheck
;;   (:color
;;     blue
;;     :quit-key "q"
;;     :title (sb/with-faicon "plane" "Flycheck actions" 1 -0.05)
;;     :foreign-keys warn)
;;   ("Setup"
;;     (("M" flycheck-manual "Manual")
;;       ("m" flycheck-mode "enable mode")
;;       ("v" flycheck-verify-setup "Verify setup"))
;;     "Errors"
;;     (("<" flycheck-previous-error :color pink "previous error")
;;       (">" flycheck-next-error :color pink "next error")
;;       ("f" flycheck-buffer "check buffer")
;;       ("l" flycheck-list-errors "list"))
;;     "Checker"
;;     (("d" flycheck-disable-checker "disable")
;;       ("s" flycheck-select-checker "select checker")
;;       ("?" flycheck-describe-checker "describe checker"))))

;; (pretty-hydra-define sb/hydra-python-indent
;;   (:quit-key "q" :title "Adjust Python indentation." :foreign-keys warn)
;;   (""
;;    ((">" python-indent-shift-right "right")
;;     ("<" python-indent-shift-left "left"))))

;; (with-eval-after-load "python-mode"
;;   (defvar python-mode-map)

;;   (bind-key "C-c" #'sb/hydra-python-indent/body python-mode-map))

;; (pretty-hydra-define
;;   sb/hydra-smerge
;;   (:color
;;     pink
;;     :hint nil
;;     :post (smerge-auto-leave)
;;     :quit-key "q"
;;     :title (with-alltheicon "git" "Merge actions" 1 -0.05)
;;     :foreign-keys warn)
;;   ("Conflict actions"
;;     (("n" smerge-next "Next conflict") ("p" smerge-prev "Previous conflict"))
;;     "Keep actions"
;;     (("b" smerge-keep-base "Keep base")
;;       ("u" smerge-keep-upper "Keep upper")
;;       ("l" smerge-keep-lower "Keep lower")
;;       ("a" smerge-keep-all "Keep all")
;;       ("RET" smerge-keep-current "Keep current")
;;       ("\C-m" smerge-keep-current "Keep current"))
;;     "Diff actions"
;;     (("<" smerge-diff-base-upper "Diff base upper")
;;       ("=" smerge-diff-upper-lower "Diff base lower")
;;       (">" smerge-diff-base-lower "Diff base lower")
;;       ("R" smerge-refine "Refine")
;;       ("E" smerge-ediff "Ediff"))
;;     "Others"
;;     (("C" smerge-combine-with-next "Combine")
;;       ("r" smerge-resolve "Resolve")
;;       ("k" smerge-kill-current "Kill current"))))

;; (pretty-hydra-define sb/hydra-multiple-cursors
;;   (:hint nil :quit-key "q" :title "Multiple cursors" :foreign-keys warn)
;;   ("Up"
;;    (("p" mc/mark-previous-like-this "Up next")
;;     ("P" mc/skip-to-previous-like-this "Up skip")
;;     ("M-p" mc/unmark-previous-like-this "Unmark"))
;;    "Down"
;;    (("n" mc/mark-next-like-this "Down next")
;;     ("N" mc/skip-to-next-like-this "Down skip")
;;     ("M-n" mc/unmark-next-like-this "Down unmark"))
;;    "Others"
;;    (("l" mc/edit-lines :exit t "Edit lines")
;;     ("a" mc/mark-all-like-this :exit t "Mark all")
;;     ("r" mc/mark-all-in-region-regexp :exit t "Mark by regexp"))))

;; (pretty-hydra-define sb/hydra-smartparens
;;   (:hint nil :quit-key "q" :title "Smartparens" :foreign-keys warn)
;;   ("Moving"
;;    (("a" sp-beginning-of-sexp "Beginning")
;;     ("e" sp-end-of-sexp "End")
;;     ("f" sp-forward-sexp "Forward")
;;     ("b" sp-backward-sexp "Backward")
;;     ("n" sp-down-sexp "Down")
;;     ("N" sp-backward-down-sexp "Backward down")
;;     ("p" sp-up-sexp "Up")
;;     ("P" sp-backward-up-sexp "Backward up"))
;;    "Slurping & barfing"
;;    (("h" sp-backward-slurp-sexp "Backward slurp")
;;     ("H" sp-backward-barf-sexp "Backward barf")
;;     ("l" sp-forward-slurp-sexp "Slurp")
;;     ("L" sp-forward-barf-sexp "Forward barf"))
;;    "Wrapping"
;;    (("R" sp-rewrap-sexp "Rewrap")
;;     ("u" sp-unwrap-sexp "Unwrap")
;;     ("U" sp-backward-unwrap-sexp "Backward unwrap")
;;     ("(" sp-wrap-round "Wrap parenthesis")
;;     ("{" sp-wrap-curly "Wrap curly")
;;     ("[" sp-wrap-square "Wrap square"))
;;    "Sexp juggling"
;;    (("S" sp-split-sexp "Split")
;;     ("s" sp-splice-sexp "Splice")
;;     ("r" sp-raise-sexp "Raise")
;;     ("j" sp-join-sexp "Join")
;;     ("t" sp-transpose-sexp "Transpose")
;;     ("A" sp-absorb-sexp "Absorb")
;;     ("E" sp-emit-sexp "Emit")
;;     ("o" sp-convolute-sexp "Convolute"))
;;    "Destructive editing"
;;    (("c" sp-change-inner :exit t "Change inner")
;;     ("C" sp-change-enclosing :exit t "Change outer")
;;     ("k" sp-kill-sexp "Kill sexp")
;;     ("K" sp-backward-kill-sexp "Backward kill sexp")
;;     ("w" sp-copy-sexp "Copy sexp"))))

;; (pretty-hydra-define sb/hydra-lsp
;;   (:exit t :hint nil :quit-key "q" :title "LSP Mode" :foreign-keys warn)
;;   ("Buffer"
;;    (("f" lsp-format-buffer "Format buffer")
;;     ("m" lsp-ui-imenu "Imenu")
;;     ("x" lsp-execute-code-action "Execute code action"))
;;    "Server"
;;    (("M-s" lsp-describe-session "Describe session")
;;     ("M-r" lsp-workspace-restart "Restart language server")
;;     ("S" lsp-workspace-shutdown "Shutdown"))
;;    "Symbol"
;;    (("d" lsp-find-declaration "Find declaration")
;;     ("D" lsp-ui-peek-find-definitions "Find definitions")
;;     ("R" lsp-ui-peek-find-references "Find references")
;;     ("i" lsp-ui-peek-find-implementation "Find implementation")
;;     ("t" lsp-find-type-definition "Type definition")
;;     ("s" lsp-signature-help "Signature")
;;     ("o" lsp-describe-thing-at-point "Describe thing at point")
;;     ("r" lsp-rename "Rename"))))

;; (pretty-hydra-define sb/hydra-markdown-mode
;;   (:hint nil :title "Markdown mode" :quit-key "q" :foreign-keys warn)
;;   ("Formatting"
;;    (("s" markdown-insert-bold "Bold")
;;     ("e" markdown-insert-italic "Italic")
;;     ("b" markdown-insert-blockquote :color blue "Blockquote")
;;     ("p" markdown-insert-pre :color blue "Pre-formatted")
;;     ("c" markdown-insert-code "Code"))
;;    "Headings"
;;    (("h" markdown-insert-header-dwim "DWIM")
;;     ("1" markdown-insert-header-atx-1 "H1")
;;     ("2" markdown-insert-header-atx-2 "H2")
;;     ("3" markdown-insert-header-atx-3 "H3")
;;     ("4" markdown-insert-header-atx-4 "H4"))
;;    "Lists"
;;    (("m" markdown-insert-list-item "Insert list item"))
;;    "Promote/Demote"
;;    (("l" markdown-promote "Promote")
;;     ("r" markdown-demote "Demote")
;;     ("d" markdown-move-down "Move down")
;;     ("u" markdown-move-up "Move up"))
;;    "Links"
;;    (("L" markdown-insert-link :color blue "Insert link")
;;     ("U" markdown-insert-uri :color blue "Insert uri")
;;     ("F" markdown-insert-footnote :color blue "Insert footnote")
;;     ("W" markdown-insert-wiki-link :color blue "Insert wiki link")
;;     ("R" markdown-insert-reference-link-dwim :color blue "Insert reference link"))))

;; (pretty-hydra-define
;;   sb/hydra-straight
;;   (:hint nil :quit-key "q" :title "Straight actions" :foreign-keys warn)
;;   (""
;;     (("c" straight-check-all "check all")
;;       ("C" straight-check-package "check package")
;;       ("r" straight-rebuild-all "rebuild all")
;;       ("R" straight-rebuild-package "rebuild package")
;;       ("f" straight-fetch-all "fetch all")
;;       ("F" straight-fetch-package "fetch package")
;;       ("p" straight-pull-all "pull all")
;;       ("P" straight-pull-package "pull package")
;;       ("m" straight-merge-all "merge all")
;;       ("M" straight-merge-package "merge package")
;;       ("n" straight-normalize-all "normalize all")
;;       ("N" straight-normalize-package "normalize package")
;;       ("v" straight-freeze-versions "freeze versions")
;;       ("V" straight-thaw-versions "thaw versions")
;;       ("g" straight-get-recipe "get recipe"))))

;; (pretty-hydra-define sb/hydra-comments
;;   (:hint nil :color teal :exit t :title "Commentary Actions" :foreign-keys warn)
;;   (""
;;    (("b" comment-box)
;;     ("c" comment-dwim)
;;     ("l" comment-line)
;;     ("r" comment-region))))

;; (pretty-hydra-define sb/hydra-magit
;;   (:hint nil :color teal :quit-key "q" :title (sb/with-faicon "code-fork" "Git" 1 -0.05)
;;          :foreign-keys warn)
;;   ("Action"
;;    (("b" magit-blame "blame")
;;     ("c" magit-commit "commit")
;;     ("i" magit-init "init")
;;     ("l" magit-log-buffer-file "commit log (current file)")
;;     ("L" magit-log-current "commit log (project)")
;;     ("s" magit-status "status"))))

;; (pretty-hydra-define sb/hydra-dumb-jump
;;   (:color blue :foreign-keys warn)
;;   ("Dumb Jump"
;;    (("j" dumb-jump-go "Go")
;;     ("o" dumb-jump-go-other-window "Other window")
;;     ("e" dumb-jump-go-prefer-external "Go external")
;;     ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
;;     ("i" dumb-jump-go-prompt "Prompt")
;;     ("l" dumb-jump-quick-look "Quick look")
;;     ("b" dumb-jump-back "Back"))))

;; (bind-key "C-c h a" #'sb/hydra-avy/body)
;; (bind-key "C-c h k" #'sb/hydra-markdown-mode/body)
;; (bind-key "C-c h e" #'sb/hydra-error/body)
;; (bind-key "C-c h f" #'sb/hydra-flycheck/body)
;; (bind-key "C-c h g" #'sb/hydra-smerge/body)
;; (bind-key "C-c h j" #'sb/hydra-projectile/body)
;; (bind-key "C-c h l" #'sb/hydra-lsp/body)
;; (bind-key "C-c h m" #'sb/hydra-multiple-cursors/body)
;; (bind-key "C-c h p" #'sb/hydra-smartparens/body)
;; (bind-key "C-c h s" #'sb/hydra-spelling/body)
;; (bind-key "C-c h t" #'sb/hydra-move-text/body)
;; (bind-key "C-c h z" #'sb/hydra-text-scale-zoom/body)
;; (bind-key "C-c h i" #'sb/hydra-straight/body)
;; (bind-key "C-c h v" #'sb/hydra-magit/body)
;; (bind-key "C-c h y" #'sb/hydra-python-indent/body)
;; (bind-key "C-c h d" #'sb/hydra-dumb-jump)

;; (pretty-hydra-define
;;   sb/hydra-help (:color teal :title "Hydra Overview" :foreign-keys warn)
;;   ("Groups"
;;     (("a" sb/hydra-avy/body "+ avy")
;;       ("d" sb/hydra-markdown-mode/body "+ markdown")
;;       ("e" sb/hydra-error/body "+ error")
;;       ("f" sb/hydra-flycheck/body "+ flycheck")
;;       ("g" sb/hydra-smerge/body "+ smerge")
;;       ("j" sb/hydra-projectile/body "+ projectile")
;;       ("l" sb/hydra-lsp/body "+ lsp")
;;       ;; ("m" sb/hydra-multiple-cursors/body "+ multiple cursors")
;;       ("p" sb/hydra-smartparens/body "+ smartparens")
;;       ("s" sb/hydra-spelling/body "+ spelling")
;;       ("t" sb/hydra-move-text/body "+ move text")
;;       ("z" sb/hydra-text-scale-zoom/body "+ text scale")
;;       ("i" sb/hydra-straight/body "+ straight")
;;       ;; ("v" sb/hydra-magit/body "+ magit")
;;       ("y" sb/hydra-python-indent/body "+ python indent"))))

;; (bind-key "C-c h h" #'sb/hydra-help/body)

;; Alacritty and Konsole are my preferred terminals for using Emacs.
(use-package term-keys
  :straight (:host github :repo "CyberShadow/term-keys")
  :unless (display-graphic-p)
  :hook (emacs-startup . term-keys-mode)
  :config (require 'term-keys-alacritty))

(use-package pixel-scroll
  :straight (:type built-in)
  :bind
  ([remap scroll-up-command] . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  :custom (pixel-scroll-precision-interpolate-page t)
  :init
  (when (fboundp 'pixel-scroll-mode)
    (pixel-scroll-mode 1))
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1)))

;; (use-package server
;;   :straight (:type built-in)
;;   :unless (string-equal "root" (getenv "USER")) ; Only start server if not root
;;   :hook
;;   (emacs-startup . (lambda ()
;;                          (unless (and (fboundp 'server-running-p) (server-running-p))
;;                            (server-start)))))
;;   :config
;;   ;; Hide "When done with a buffer, type C-x 5" message
;;   (when (boundp 'server-client-instructions)
;;     (setq server-client-instructions nil)))

;; (when (eq sb/op-mode 'server)
;;   ;; Start server if not root user
;;   (unless (string-equal "root" (getenv "USER"))
;;     (when (and (fboundp 'server-running-p) (not (server-running-p)))
;;       (server-mode))))

;; (when (or (eq sb/op-mode 'server) (eq sb/op-mode 'daemon))
;;   (setq server-client-instructions nil))

(setq custom-file sb/custom-file)

(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))
(when (file-exists-p sb/private-file)
  (load sb/private-file 'noerror 'nomessage))

;; Mark safe variables

(put 'compilation-read-command 'safe-local-variable #'stringp)

;; (put 'bibtex-completion-bibliography          'safe-local-variable #'listp)
;; (put 'company-bibtex-bibliography             'safe-local-variable #'listp)
;; (put 'company-clang-arguments                 'safe-local-variable #'listp)
;; (put 'counsel-find-file-ignore-regexp         'safe-local-variable #'stringp)
;; (put 'flycheck-checker                        'safe-local-variable #'listp)
;; (put 'flycheck-clang-include-path             'safe-local-variable #'listp)
;; (put 'flycheck-gcc-include-path               'safe-local-variable #'listp)
;; (put 'flycheck-python-pylint-executable       'safe-local-variable #'stringp)
;; (put 'lsp-clients-clangd-args                 'safe-local-variable #'listp)
;; (put 'lsp-latex-root-directory                'safe-local-variable #'stringp)
;; (put 'lsp-pyright-extra-paths                 'safe-local-variable #'listp)
;; (put 'projectile-enable-caching               'safe-local-variable #'stringp)
;; (put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
;; (put 'projectile-project-root                 'safe-local-variable #'stringp)
;; (put 'pyvenv-activate                         'safe-local-variable #'stringp)
;; (put 'reftex-default-bibliography             'safe-local-variable #'listp)
;; (put 'tags-table-list                         'safe-local-variable #'listp)

;; https://blog.d46.us/advanced-emacs-startup/
(add-hook
  'emacs-startup-hook
  (lambda ()
    (if (bound-and-true-p sb/disable-package.el)
      (let ((gc-time (float-time gc-elapsed)))
        (message "Emacs ready (init time = %s, gc time = %.2fs, gc count = %d)."
          (emacs-init-time)
          gc-time
          gcs-done))
      (let
        (
          (packages (length package-activated-list))
          (gc-time (float-time gc-elapsed)))
        (message "Emacs ready (init time = %s, packages = %d, gc time = %.2fs, gc count = %d)."
          (emacs-init-time)
          packages
          gc-time
          gcs-done)))))

;;; init.el ends here

;; Local variables:
;; elisp-autofmt-load-packages-local: ("use-package")
;; End:
