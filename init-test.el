;;; init-emacs28.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defgroup sb/emacs
  nil
  "Personal configuration for dotemacs."
  :group 'local)

(defcustom sb/extras-directory
  (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/gui-theme
  'none
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "leuven"          leuven)
           (const :tag "zenburn"         zenburn)
           (const :tag "doom-one-light"  doom-one-light)
           (const :tag "doom-one"        doom-one)
           (const :tag "doom-nord"       doom-nord)
           (const :tag "doom-molokai"    doom-molokai)
           (const :tag "doom-gruvbox"    doom-gruvbox)
           (const :tag "monokai"         monokai)
           (const :tag "modus-operandi"  modus-operandi)
           (const :tag "modus-vivendi"   modus-vivendi)
           (const :tag "customized"      sb/customized) ; Customizations over the default theme
           (const :tag "none"            none))
  :group 'sb/emacs)

;; A dark theme looks good on the TUI
(defcustom sb/tui-theme
  'modus-vivendi
  "Specify which Emacs theme to use."
  :type  '(radio
           (const :tag "leuven"          leuven)
           (const :tag "zenburn"         zenburn)
           (const :tag "doom-one"        doom-one)
           (const :tag "doom-molokai"    doom-molokai)
           (const :tag "doom-gruvbox"    doom-gruvbox)
           (const :tag "doom-nord"       doom-nord)
           (const :tag "monokai"         monokai)
           (const :tag "modus-operandi"  modus-operandi)
           (const :tag "modus-vivendi"   modus-vivendi)
           (const :tag "customized"      sb/customized) ; Customizations over the default theme
           (const :tag "none"            none))
  :group 'sb/emacs)

(defcustom sb/modeline-theme
  'doom-modeline
  "Specify the mode-line theme to use."
  :type  '(radio
           (const :tag "powerline"       powerline)
           (const :tag "doom-modeline"   doom-modeline)
           (const :tag "awesome-tray"    awesome-tray)
           (const :tag "spaceline"       spaceline)
           (const :tag "moody"           moody)
           (const :tag "mini-modeline"   mini)
           (const :tag "none"            none))
  :group 'sb/emacs)

(defcustom sb/window-split
  'vertical
  "Specify the direction in which the windows should be split.
This depends on the orientation of the display."
  :type  '(radio
           ;; Split into two windows one above the other (`split-window-below')
           (const :tag "vertical"   vertical)
           ;; Split into two side-by-side windows (`split-window-right')
           (const :tag "horizontal" horizontal))
  :group 'sb/emacs)

;; Large values make reading difficult when the window is split side-by-side
(defcustom sb/fill-column
  100
  "Column beyond which lines should not extend."
  :type  'number
  :group 'sb/emacs)

(defcustom sb/delete-trailing-whitespace-p
  nil
  "Delete trailing whitespace.
Control whether the trailing whitespace should be deleted or not.
Sometimes we do not want to unnecessarily add differences due to
  whitespaces."
  :type  'boolean
  :group 'sb/emacs)

;; Use the snap installation of universal-ctags
(defcustom sb/ctags-path
  "/snap/bin/ctags"
  "Absolute path to Universal Ctags executable."
  :type  'string
  :group 'sb/emacs)

(defcustom sb/debug-init-file
  nil
  "Enable features to debug errors and performance bottlenecks."
  :type  'boolean
  :group 'sb/emacs)

(defconst sb/user-home
  (getenv "HOME")
  "User HOME directory.")

(defconst sb/user-tmp
  (expand-file-name "tmp" sb/user-home)
  "User temp directory.
This location is used for temporary installations and files.")

(defcustom sb/textlint-home
  (expand-file-name "textlint-workspace" sb/user-tmp)
  "Absolute path to textlint workspace."
  :type  'string
  :group 'sb/emacs)

;; `pyls' and `mspyls' are not actively maintained, and improvements to `py-lsp' is slow
(defcustom sb/python-langserver
  'pylsp
  "Choose the Python Language Server implementation."
  :type  '(radio
           (const :tag "pylsp"   pylsp)
           (const :tag "pyright" pyright)
           (const :tag "none"    none))
  :group 'sb/emacs)

(defcustom sb/minibuffer-completion
  'vertico
  "Choose the framework to use for narrowing and selection."
  :type '(radio
          (const :tag "vertico" vertico)
          (const :tag "ivy" ivy))
  :group 'dotemacs)

(defcustom sb/capf
  'corfu
  "Choose the framework to use for completion at point.
Corfu does not support TUI, so we have to fallback on company."
  :type '(radio
          (const :tag "corfu" corfu)
          (const :tag "company" company))
  :group 'dotemacs)

(defconst sb/EMACS27    (= emacs-major-version 27))
(defconst sb/EMACS27+   (> emacs-major-version 26))
(defconst sb/EMACS28+   (> emacs-major-version 27))
(defconst sb/IS-LINUX   (eq system-type 'gnu/linux))
(defconst sb/IS-WINDOWS (eq system-type 'windows-nt))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")        t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-defer       nil
      use-package-expand-minimally   nil
      use-package-always-ensure        t
      use-package-hook-name-suffix     nil)

;; "C-h b" lists all the bindings available in a buffer, "C-h m" shows the keybindings for the major
;; and the minor modes.
(use-package bind-key
  :functions bind-key--remove
  :bind ("C-c d k" . describe-personal-keybindings))

(use-package diminish)

(use-package f
  :commands (f-exists? f-join f-dirname))

(use-package s
  :commands s-starts-with? s-ends-with?)

(use-package dash
  :commands (-contains? -tree-map))

(use-package hydra
  :commands (hydra-default-pre hydra-keyboard-quit defhydra
                               hydra-show-hint hydra-set-transient-map
                               hydra--call-interactively-remap-maybe))

(use-package no-littering :demand t)

(use-package warnings
  :init
  ;; This is not a great idea, but I expect most warnings will arise from third-party packages.
  (setq warning-minimum-level :emergency))

(use-package gcmh ; Allow GC to happen after a period of idle time
  :diminish
  :commands (gcmh-mode gcmh-idle-garbage-collect)
  :hook (after-init-hook . gcmh-mode)
  :config
  (when (bound-and-true-p sb/debug-init-file)
    (setq gcmh-verbose t)))

;; We can do `package-list-packages', then press `U' and `x'. The only thing missing from paradox
;; is `paradox-upgrade-packages' as a single command.
(use-package package
  :if sb/EMACS27+
  :bind
  (("C-c d p" . package-quickstart-refresh)
   ("C-c d l" . package-list-packages)))

(use-package paradox
  :disabled t ; This package is no longer maintained.
  :commands (paradox-enable)
  :bind
  (("C-c d l" . paradox-list-packages)
   ("C-c d u" . paradox-upgrade-packages))
  :config
  (setq paradox-display-star-count nil
        paradox-execute-asynchronously t
        paradox-github-token t)
  (paradox-enable))

;; Get PATH with "(getenv "PATH")". Set PATH with
;; "(setenv "PATH" (concat (getenv "PATH") ":/home/swarnendu/bin"))".
(use-package exec-path-from-shell
  :defines exec-path-from-shell-check-startup-files
  :commands exec-path-from-shell-initialize
  :if (or (daemonp) (memq window-system '(x ns)))
  :init
  ;; "-i" is expensive but Tramp may be unable to find executables without the option
  (setq exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "NODE_PATH" "JAVA_HOME" "PYTHONPATH"
                                         "LANG" "LC_CTYPE" "LC_ALL" "TERM"))
  (exec-path-from-shell-initialize))

;; These are alternative ways.

;; (setq exec-path (append exec-path (expand-file-name "node_modules/.bin" sb/user-tmp)))
;; (add-to-list 'exec-path (expand-file-name "node_modules/.bin" sb/user-tmp))

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

;; Revert all (e.g., PDF) files without asking
(setq revert-without-query '("\\.*"))

;; We may open a file immediately after starting Emacs, hence we are using a hook instead of a
;; timer.
(use-package saveplace ; Remember cursor position in files
  :ensure nil
  :hook (after-init-hook . save-place-mode))

(use-package savehist ; Save minibuffer history across sessions
  :ensure nil
  :commands savehist-mode
  ;; :init (run-with-idle-timer 2 nil #'savehist-mode)
  :hook (after-init-hook . savehist-mode)
  :custom
  (savehist-additional-variables '(extended-command-history
                                   kill-ring
                                   regexp-search-ring
                                   search-ring)))

(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-after-kill-buffer-p t
        uniquify-buffer-name-style   'forward
        uniquify-ignore-buffers-re   "^\\*"
        uniquify-separator           "/"
        uniquify-strip-common-suffix t))
(use-package doom-themes
  :init (load-theme 'doom-one t)
  :config
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure all-the-icons
  :ensure t
  :init
  (all-the-icons-install-fonts t)
  (doom-modeline-mode 1))

(use-package beacon
  :diminish
  :hook (after-init-hook . beacon-mode))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode)
  :config (setq all-the-icons-ibuffer-icon-size 0.8))

(use-package all-the-icons-dired
  :diminish
  :hook
  (dired-mode-hook . (lambda ()
                       (unless (file-remote-p default-directory)
                         (all-the-icons-dired-mode 1)))))

(use-package anzu
  :diminish anzu-mode
  :init
  (setq anzu-search-threshold     10000
        anzu-minimum-input-length 2)
  (global-anzu-mode 1)
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)))

(use-package recentf
  :ensure nil
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

  ;; `recentf-save-list' is called on Emacs exit. In addition, save the recent list periodically
  ;; after idling for 30 seconds.
  (run-with-idle-timer 30 t #'recentf-save-list)

  ;; Adding many functions to `kill-emacs-hook' slows down Emacs exit, hence we are only using idle
  ;; timers.
  (run-with-idle-timer 60 t #'recentf-cleanup)
  :hook (after-init-hook . recentf-mode))

(use-package init-open-recentf
  :after recentf
  :demand t
  :disabled t
  :config (init-open-recentf))

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

(use-package amx
  :hook (after-init-hook . amx-mode)
  :bind
  (("M-x"  . execute-extended-command)
   ("<f1>" . execute-extended-command-for-buffer)))

(use-package orderless
  :after (vertico)
  :demand t
  :config
  (setq completion-styles '(orderless partial-completion) ; initials, basic, emacs22
        orderless-matching-styles '(orderless-regexp)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic-remote orderless partial-completion))
                                        ;; (minibuffer (initials))))
                                        )))

(use-package ispell
  :ensure nil
  :if (symbol-value 'sb/IS-LINUX)
  :config
  (setq ispell-dictionary "en_US"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--size=90")
        ispell-local-dictionary "en_US"
        ispell-personal-dictionary (expand-file-name "spell" sb/extras-directory)
        ;; Save a new word to personal dictionary without asking
        ispell-silently-savep t)

  ;; Skip regions in Org-mode
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC"     . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_src"     . "#\\+end_src"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (add-to-list 'ispell-skip-region-alist '("#\\+begin_example" . "#\\+end_example"))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  ;; Verbatim regions in org mode should not be ispelled
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  ;; Properties block in org mode do not need to be ispelled
  (add-to-list 'ispell-skip-region-alist '("\:PROPERTIES\:$" . "\:END\:$"))
  ;; Footnotes in org that have http links that are line breaked should not be ispelled
  (add-to-list 'ispell-skip-region-alist '("^http" . "\\]"))

  (add-to-list 'ispell-skip-region-alist '("`" "`"))

  ;; Skip some math environments
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{multline}" . "\\\\end{multline}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{equation}" . "\\\\end{equation}"))
  (add-to-list 'ispell-skip-region-alist '("\\\\begin{align}"    . "\\\\end{align}")))

(use-package flyspell
  :ensure nil
  :if (symbol-value 'sb/IS-LINUX)
  :commands (flyspell-overlay-p flyspell-correct-previous flyspell-correct-next flyspell-buffer)
  :diminish
  :preface
  ;; Move point to previous error
  ;; http://emacs.stackexchange.com/a/14912/2017
  ;; http://pragmaticemacs.com/emacs/jump-back-to-previous-typo/
  (defun sb/flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; Goto beginning of buffer
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; Seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; Save the current location for the next invocation
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (if (= pos min)
            (progn
              (message "No more misspelled words!")
              (setq arg 0))
          (forward-word)))))
  :config
  (setq flyspell-abbrev-p           t ; Add corrections to abbreviation table
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook
  (;; (before-save-hook . flyspell-buffer) ; Saving files will be slow
   ;; Enabling `flyspell-prog-mode' does not seem to be very useful and highlights links and
   ;; language-specific words
   (prog-mode-hook . flyspell-prog-mode)
   ;; `find-file-hook' will not work for buffers with no associated files
   (after-init-hook . (lambda ()
                        (when (string= (buffer-name) "*scratch*")
                          (flyspell-mode 1))))
   (text-mode-hook . flyspell-mode))
  :bind
  (("C-c f f" . flyspell-mode)
   ("C-c f b" . flyspell-buffer)
   :map flyspell-mode-map
   ("C-;"     . nil)
   ("C-,"     . sb/flyspell-goto-previous-error)))

;; Flyspell popup is more efficient. Ivy-completion does not show the "Save" option in a few cases.
(use-package flyspell-popup
  :after flyspell
  :disabled t
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-popup-correct))
  :config (setq flyspell-popup-correct-delay 0.1))

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . flyspell-correct-wrapper)))

;; As of Emacs 28, `flyspell' does not provide a way to automatically check only the on-screen text.
;; Running `flyspell-buffer' on an entire buffer can be slow.
(use-package spell-fu
  :defines spell-fu-directory
  :commands spell-fu-mode
  :config (setq spell-fu-directory (expand-file-name "spell-fu" no-littering-var-directory))
  :init
  (add-hook 'text-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(hl-line
                                             ;; `nxml-mode' is derived from `text-mode'
                                             nxml-attribute-local-name))
              (spell-fu-mode)))

  (add-hook 'org-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(org-block
                                             org-block-begin-line
                                             org-block-end-line
                                             org-code
                                             org-date
                                             org-formula
                                             org-latex-and-related
                                             org-link
                                             org-meta-line
                                             org-property-value
                                             org-ref-cite-face
                                             org-special-keyword
                                             org-tag
                                             org-todo
                                             org-todo-keyword-done
                                             org-todo-keyword-habt
                                             org-todo-keyword-kill
                                             org-todo-keyword-outd
                                             org-todo-keyword-todo
                                             org-todo-keyword-wait
                                             org-verbatim
                                             hl-line))
              (spell-fu-mode)))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq spell-fu-faces-exclude '(markdown-blockquote-face
                                             markdown-code-face
                                             markdown-html-attr-name-face
                                             markdown-html-attr-value-face
                                             markdown-html-tag-name-face
                                             markdown-inline-code-face
                                             markdown-link-face
                                             markdown-markup-face
                                             markdown-plain-url-face
                                             markdown-reference-face
                                             markdown-url-face
                                             hl-line
                                             pandoc-citation-key-face))
              (spell-fu-mode)))

  (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
    (add-hook hook (lambda ()
                     (setq spell-fu-faces-exclude '(font-latex-math-face
                                                    font-latex-sedate-face
                                                    hl-line))
                     (spell-fu-mode))))
  :bind
  (("C-c f n" . spell-fu-goto-next-error)
   ("C-c f p" . spell-fu-goto-previous-error)
   ("C-c f a" . spell-fu-word-add)))

(use-package highlight-indentation
  :commands highlight-indentation-mode
  :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
  :hook ((yaml-mode-hook python-mode-hook) . highlight-indentation-mode))

;; Claims to be better than `electric-indent-mode'
(use-package aggressive-indent
  :commands aggressive-indent-mode
  :hook (emacs-lisp-mode-hook . aggressive-indent-mode)
  :diminish
  :config
  (setq aggressive-indent-comments-too t
        ;; Never use `electric-indent-mode'
        aggressive-indent-dont-electric-modes t))

(use-package paren
  :ensure nil
  ;; :init (run-with-idle-timer 2 nil #'show-paren-mode)
  :hook (after-init-hook . show-paren-mode)
  :config
  (setq show-paren-style 'parenthesis ; `mixed' may lead to performance problems
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Enable autopairing
(use-package elec-pair
  :ensure nil
  :commands (electric-pair-mode)
  :disabled t
  ;; :init (run-with-idle-timer 2 nil #'electric-pair-mode)
  :hook (after-init-hook . electric-pair-mode)
  :config
  ;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
  (defvar sb/markdown-pairs '((?` . ?`)) "Electric pairs for `markdown-mode'.")
  (defvar electric-pair-pairs)
  (defvar electric-pair-text-pairs)
  (defvar electric-pair-preserve-balance)

  (declare-function sb/add-markdown-pairs "init-emacs28")

  (defun sb/add-markdown-pairs ()
    "Add custom pairs to `markdown-mode'."
    (setq-local electric-pair-pairs (append electric-pair-pairs sb/markdown-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  (add-hook 'markdown-mode-hook #'sb/add-markdown-pairs)

  ;; Avoid balancing parentheses since they can be both irritating and slow
  (setq electric-pair-preserve-balance nil)

  ;; Disable pairs when entering minibuffer
  (add-hook 'minibuffer-setup-hook (lambda ()
                                     (electric-pair-mode -1)))
  ;; Re-enable pairs when existing minibuffer
  (add-hook 'minibuffer-exit-hook (lambda ()
                                    (electric-pair-mode 1))))

;; `sp-cheat-sheet' will show you all the commands available, with examples. Seems to have
;; performance issue with `latex-mode', `markdown-mode', and large JSON files.
;; https://web.archive.org/web/20201109035847/http://ebzzry.io/en/emacs-pairs/
(use-package smartparens
  :commands (sp-pair sp-local-pair sp-raise-sexp sp-join-sexp sp-absorb-sexp
                     sp-transpose-sexp sp-absort-sexp sp-copy-sexp
                     sp-backward-kill-sexp sp-kill-sexp sp-change-inner
                     sp-change-enclosing sp-convolute-sexp sp-emit-sexp
                     sp-backward-down-sexp sp-backward-up-sexp
                     sp-backward-slurp-sexp sp-backward-barf-sexp
                     sp-forward-barf-sexp sp-forward-slurp-sexp sp-rewrap-sexp
                     sp-unwrap-sexp sp-backward-unwrap-sexp sp-wrap-round
                     sp-wrap-curly sp-wrap-square sp-split-sexp)
  :diminish
  :preface
  ;; https://web-mode.org/
  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  :hook
  ((after-init-hook . (lambda ()
                        (require 'smartparens-config)
                        (smartparens-global-mode 1)
                        (show-smartparens-global-mode 1))))
  :config
  (setq sp-show-pair-from-inside t
        sp-autoskip-closing-pair 'always)

  (smartparens-strict-mode -1)

  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

  (sp-local-pair 'markdown-mode "<" ">")

  ;; Do not insert a parenthesis pair when the point is at the beginning of a word
  (sp-pair "("  nil :unless '(sp-point-before-word-p))
  (sp-pair "["  nil :unless '(sp-point-before-word-p))
  (sp-pair "{"  nil :unless '(sp-point-before-word-p))
  (sp-pair "\"" nil :unless '(sp-point-before-word-p sp-point-after-word-p))

  (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p))
  :bind
  (("C-M-a" . sp-beginning-of-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-e" . sp-end-of-sexp) ; "f_oo bar" -> "foo bar_"
   ("C-M-u" . sp-up-sexp) ; "f_oo bar" -> "foo bar"_
   ("C-M-w" . sp-down-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-f" . sp-forward-sexp) ; "foo ba_r" -> "foo bar"_
   ("C-M-b" . sp-backward-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-n" . sp-next-sexp) ; ))" -> ((foo) (bar))"
   ("C-M-p" . sp-previous-sexp) ; "(foo (b|ar baz))" -> "(foo| (bar baz))"
   ;; TODO: The following two keybindings are not properly supported yet in terminal Emacs.
   ("C-S-b" . sp-backward-symbol) ; "foo bar| baz" -> "foo |bar baz"
   ("C-S-f" . sp-forward-symbol) ; "|foo bar baz" -> "foo| bar baz"
   ;; "(foo bar)" -> "foo bar"
   ("C-M-k" . sp-splice-sexp)))

;; ;; v8.1: This seems a reasonable alternative to `projectile', but does not remember remote projects
;; ;; yet.
;; (use-package project
;;   :ensure nil
;;   :commands (project-switch-project project-current
;;                                     project-find-file project-execute-extended-command
;;                                     project-known-project-roots
;;                                     project-remove-known-project
;;                                     project-remember-project
;;                                     project-kill-buffers
;;                                     project-switch-to-buffer
;;                                     project-search
;;                                     project-compile)
;;   :bind
;;   (:map project-prefix-map
;;         ("f" . project-find-file)
;;         ("F" . project-or-external-find-file)
;;         ("b" . project-switch-to-buffer)
;;         ("d" . project-dired)
;;         ("v" . project-vc-dir)
;;         ("c" . project-compile)
;;         ("k" . project-kill-buffers)
;;         ("p" . project-switch-project)
;;         ("g" . project-find-regexp)
;;         ("r" . project-query-replace-regexp)))

(use-package projectile
  :commands (projectile-project-p projectile-project-name
                                  projectile-expand-root
                                  projectile-project-root
                                  projectile-mode
                                  projectile-compile
                                  projectile-compile-project)
  :preface
  (defun sb/close-treemacs-with-projectile (orig-fun &rest args)
    (let ((res (apply orig-fun args)))
      (treemacs)
      res))
  :config
  (setq projectile-enable-caching nil ; Caching will not watch for file system changes
        projectile-file-exists-remote-cache-expire nil
        projectile-mode-line-prefix "" ; Save modeline space
        ;; Use only in desired directories, too much noise otherwise
        projectile-require-project-root t
        ;; The contents of ".projectile" are ignored when using the `alien' project indexing method
        projectile-indexing-method 'alien
        ;; No sorting should be faster, note that files are not sorted if
        ;; `projectile-indexing-method' is set to `alien'.
        projectile-sort-order 'recently-active
        projectile-verbose nil)

  ;; https://github.com/MatthewZMD/.emacs.d
  (when (and (symbol-value 'sb/IS-WINDOWS)
             (executable-find "tr"))
    (setq projectile-indexing-method 'alien))

  ;; Disable computing the project type that is shown on the modeline
  (defun projectile-default-mode-line ()
    "Report project name and type in the modeline."
    (let ((project-name (projectile-project-name)))
      ;; (format " [%s: %s]"
      ;;         projectile-mode-line-prefix
      ;;         (or project-name "-"))
      (format " [%s]" (or project-name "-"))))

  (setq projectile-project-root-files '("build.gradle"
                                        "setup.py"
                                        "requirements.txt"
                                        "package.json"
                                        "composer.json"
                                        "CMakeLists.txt"
                                        "Makefile"
                                        "WORKSPACE"
                                        "meson.build"
                                        "SConstruct"
                                        "configure.ac"
                                        "configure.in"))

  ;; Set search path for finding projects when `projectile-mode' is enabled, however auto-search for
  ;; projects is disabled for faster startup.
  (setq projectile-auto-discover nil
        projectile-project-search-path (list
                                        (concat `,(getenv "HOME") "/bitbucket")
                                        (expand-file-name "github"            sb/user-home)
                                        (expand-file-name "iitk-workspace"    sb/user-home)
                                        (expand-file-name "iitkgp-workspace"  sb/user-home)
                                        (expand-file-name "iss-workspace"     sb/user-home)
                                        (expand-file-name "plass-workspace"   sb/user-home)
                                        (expand-file-name "prospar-workspace" sb/user-home)
                                        ))

  (dolist (prjs (list
                 (expand-file-name sb/user-home) ; Do not consider $HOME as a project
                 "~/" ; Do not consider $HOME as a project
                 (expand-file-name "/tmp")
                 ))
    (add-to-list 'projectile-ignored-projects prjs))

  ;; Filtering works with `alien' indexing
  (dolist (dirs
           '(".cache" ".clangd" ".dropbox" ".git" ".hg" ".metadata" ".nx" ".recommenders" ".svn"
             ".vscode" "__pycache__" "auto" "elpa" "node_modules"))
    (add-to-list 'projectile-globally-ignored-directories dirs))

  (dolist (items
           '("GPATH" "GRTAGS" "GTAGS" "GSYMS" "TAGS" "tags" ".tags" "__init__.py"))
    (add-to-list 'projectile-globally-ignored-files items))

  (dolist (exts
           '(".a" ".aux" ".bak" ".blg" ".class" ".deb" ".doc" ".docx" ".elc" ".o" ".odt" ".ppt"
             ".pptx" ".pt" ".pyc" ".rel" ".rip" ".rpm" ".so" ".xls" ".xlsx" "~$"))
    (add-to-list 'projectile-globally-ignored-file-suffixes exts))

  (projectile-mode 1)

  ;; https://github.com/Alexander-Miller/treemacs/issues/660
  ;; TODO: These do not achieve what I want.

  ;; (add-hook 'projectile-after-switch-project-hook
  ;;           (lambda ()
  ;;             (treemacs-add-and-display-current-project)
  ;;             (treemacs-display-current-project-exclusively)
  ;;             (other-window 1)))

  ;; (add-hook 'projectile-after-switch-project-hook
  ;;           (lambda ()
  ;;             (treemacs)
  ;;             (treemacs-display-current-project-exclusively)
  ;;             (other-window 1)))

  ;; (advice-add 'projectile-kill-buffers :around #'sb/close-treemacs-with-projectile)

  ;; Set these in case `counsel-projectile' is disabled. For `vertico', we use `consult-projectile'.
  (when (eq sb/minibuffer-completion 'ivy)
    (bind-key "<f5>" #'projectile-switch-project)
    (bind-key "<f6>" #'projectile-find-file))

  :bind-keymap ("C-c p" . projectile-command-map)
  ;; :init (run-with-idle-timer 2 nil #'projectile-mode)
  ;; We can open a project file without enabling projectile via bind-keys
  :hook (after-init-hook . projectile-mode)
  :bind
  (:map projectile-command-map
        ("A"    . projectile-add-known-project)))

(use-package flycheck
  :commands (flycheck-add-next-checker flycheck-next-checker
                                       flycheck-mode
                                       global-flycheck-mode
                                       flycheck-previous-error
                                       flycheck-describe-checker
                                       flycheck-buffer
                                       flycheck-list-errors
                                       flycheck-select-checker
                                       flycheck-verify-setup
                                       flycheck-next-error
                                       flycheck-disable-checker
                                       flycheck-add-mode
                                       flycheck-manual
                                       flycheck-display-error-messages-unless-error-list
                                       flycheck-sexp-to-string)
  :hook (after-init-hook . global-flycheck-mode)
  :config
  ;; Remove newline checks, since they would trigger an immediate check when we want the
  ;; `flycheck-idle-change-delay' to be in effect while editing.
  (setq flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change)
        flycheck-checker-error-threshold 1500
        flycheck-idle-buffer-switch-delay 10 ; Increase the time (s) to allow for quick transitions
        flycheck-idle-change-delay 15 ; Increase the time (s) to allow for edits
        flycheck-emacs-lisp-load-path 'inherit
        ;; Show error messages only if the error list is not already visible
        ;; flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        ;; There are no checkers for `csv-mode', and many program modes use lsp. `yaml-mode' is
        ;; derived from `text-mode'.
        flycheck-global-modes '(not csv-mode))

  ;; We prefer not to use `textlint' and `proselint'. `chktex' errors are often not very helpful.
  (dolist (checkers '(proselint textlint tex-chktex))
    (delq checkers flycheck-checkers))

  (when (eq sb/modeline-theme 'doom-modeline)
    (setq flycheck-mode-line nil))

  (setq-default flycheck-markdown-markdownlint-cli-config (expand-file-name ".markdownlint.json"
                                                                            sb/user-home)
                flycheck-chktexrc "chktexrc"
                flycheck-pylintrc '("setup.cfg" "pylintrc")
                flycheck-python-pylint-executable "python3"
                flycheck-shellcheck-follow-sources nil
                flycheck-textlint-config (expand-file-name "textlintrc.json" sb/textlint-home)
                flycheck-textlint-executable (expand-file-name "node_modules/.bin/textlint"
                                                               sb/textlint-home))

  (add-to-list 'flycheck-textlint-plugin-alist '(tex-mode . "latex"))
  (add-to-list 'flycheck-textlint-plugin-alist '(rst-mode . "rst"))

  ;; Add support for `org-lint' as a checker
  (defconst flycheck-org-lint-form
    (flycheck-prepare-emacs-lisp-form
      (require 'org)
      (require 'org-attach)
      (let ((source (car command-line-args-left))
            (process-default-directory default-directory))
        (with-temp-buffer
          (insert-file-contents source 'visit)
          (setq buffer-file-name source)
          (setq default-directory process-default-directory)
          (delay-mode-hooks (org-mode))
          (setq delayed-mode-hooks nil)
          (dolist (err (org-lint))
            (let ((inf (cl-second err)))
              (princ (elt inf 0))
              (princ ": ")
              (princ (elt inf 2))
              (terpri)))))))

  (defconst flycheck-org-lint-variables
    '(org-directory
      org-id-locations
      org-id-locations-file
      org-attach-id-dir
      org-attach-use-inheritance
      org-attach-id-to-path-function-list)
    "Variables inherited by the `org-lint' subprocess.")

  (defun flycheck-org-lint-variables-form ()
    (require 'org-attach)  ; Needed to make variables available
    `(progn
       ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
                  (seq-filter #'boundp flycheck-org-lint-variables))))

  (flycheck-define-checker org-lint
    "Org buffer checker using `org-lint'."
    :command ("emacs" (eval flycheck-emacs-args)
              "--eval" (eval (concat "(add-to-list 'load-path \""
                                     (file-name-directory (locate-library "org"))
                                     "\")"))
              "--eval" (eval (flycheck-sexp-to-string
                              (flycheck-org-lint-variables-form)))
              "--eval" (eval flycheck-org-lint-form)
              "--" source)
    :error-patterns
    ((error line-start line ": " (message) line-end))
    :modes (org-mode))

  (add-to-list 'flycheck-checkers 'org-lint t)

  ;; https://github.com/flycheck/flycheck/issues/1833
  (add-to-list 'flycheck-hooks-alist '(after-revert-hook . flycheck-buffer))

  ;; Exclude directories and files from being checked
  ;; https://github.com/flycheck/flycheck/issues/1745

  (declare-function sb/flycheck-may-check-automatically "init-emacs28.el")

  (defvar sb/excluded-directory-regexps
    '(".git/" "elpa/" ".cache" ".clangd"))

  (defun sb/flycheck-may-check-automatically (&rest _conditions)
    (or (null buffer-file-name)
        (let ((bufname (file-truename buffer-file-name)))
          (not (seq-some (lambda (re) (string-match-p re bufname))
                         sb/excluded-directory-regexps)))))

  (advice-add 'flycheck-may-check-automatically
              :after-while #'sb/flycheck-may-check-automatically)

  ;; Chain flycheck checkers with lsp, we can also use per-project directory local variables
  ;; https://github.com/flycheck/flycheck/issues/1762

  (defvar-local sb/flycheck-local-checkers nil)

  (defun sb/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker sb/flycheck-local-checkers))
        (funcall fn checker property)))

  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get)

  ;; (add-hook 'lsp-managed-mode-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'python-mode)
  ;;               (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint)))))))
  ;;             ))
  )

;; Showing error messages in the echo area is less intrusive.
(use-package flycheck-popup-tip ; Show error messages in popups
  :unless (display-graphic-p)
  :disabled t
  :hook (flycheck-mode-hook . flycheck-popup-tip-mode))

;; Does not display popup under TTY, check possible workarounds at
;; https://github.com/flycheck/flycheck-popup-tip
(use-package flycheck-pos-tip
  :disabled t
  :commands flycheck-pos-tip-mode
  :if (display-graphic-p)
  :hook (flycheck-mode-hook . flycheck-pos-tip-mode))

;; Showing errors/warnings in a posframe seems more intrusive than showing errors in the minibuffer
(use-package flycheck-posframe
  :disabled t
  :if (display-graphic-p)
  :commands (flycheck-posframe-mode flycheck-posframe-configure-pretty-defaults)
  :hook (flycheck-mode-hook . flycheck-posframe-mode)
  :custom
  (flycheck-posframe-position 'point-bottom-left-corner)
  (flycheck-posframe-border-width 1)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(use-package whitespace
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

;; This is different from `whitespace-cleanup-mode' since this is unconditional
(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (setq delete-trailing-lines t) ; "M-x delete-trailing-whitespace" deletes trailing lines
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

;; Call `whitespace-cleanup' only if the initial buffer was clean. This mode works on the entire
;; file unlike `ws-butler'. To enable the mode for an entire project, set `whitespace-cleanup-mode'
;; to `t' in the `.dir-locals.el' file.
(use-package whitespace-cleanup-mode
  :disabled t
  :diminish
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  :config
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)
  :custom
  (whitespace-cleanup-mode-preserve-point t))

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :commands ws-butler-mode
  :diminish
  :hook (prog-mode-hook . ws-butler-mode))

;; Highlight symbol under point
(use-package symbol-overlay
  :diminish
  :commands (symbol-overlay-mode)
  :hook (prog-mode-hook . symbol-overlay-mode)
  :bind
  (("M-p" . symbol-overlay-jump-prev)
   ("M-n" . symbol-overlay-jump-next))
  :custom
  ;; Delay highlighting to allow for transient cursor placements
  (symbol-overlay-idle-time 2))

(use-package hl-todo
  :commands global-hl-todo-mode
  ;; :init (run-with-idle-timer 3 nil #'global-hl-todo-mode)
  :hook (after-init-hook . global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces (append '(("LATER"    . "#d0bf8f")
                                        ("ISSUE"    . "#ff8c00")
                                        ("DEBUG"    . "#ff8c00")
                                        ("TEST"     . "tomato")
                                        ("WARNING"  . "#cc0000")
                                        ("BEWARE"   . "#aa0000")
                                        ("REFACTOR" . "#cc9393"))
                                      hl-todo-keyword-faces)))

(use-package highlight-numbers
  :commands highlight-numbers-mode
  :hook ((prog-mode-hook yaml-mode-hook conf-mode-hook
                         css-mode-hook html-mode-hook) . highlight-numbers-mode))

(use-package page-break-lines ; Display ugly "^L" page breaks as tidy horizontal lines
  :diminish
  :commands (global-page-break-lines-mode page-break-lines-mode)
  ;; :init (run-with-idle-timer 3 nil #'global-page-break-lines-mode)
  :hook (after-init-hook . global-page-break-lines-mode))

(use-package number-separator
  :ensure nil
  :load-path "extras"
  :commands number-separator-mode
  :disabled t
  :diminish
  :custom
  (number-separator ",")
  (number-separator-interval 3)
  (number-separator-ignore-threshold 4)
  (number-separator-decimal-char "."))

(use-package highlight-escape-sequences
  :commands hes-mode
  :hook (prog-mode-hook . hes-mode))

;; First mark the word, then add more cursors. Use `mc/edit-lines' to add a cursor to each line in
;; an active region that spans multiple lines.
(use-package multiple-cursors
  :bind
  (("C-<"     . mc/mark-previous-like-this)
   ("C->"     . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

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
  :ensure nil
  :after (:any markdown-mode yaml-mode prog-mode)
  :custom
  (imenu-auto-rescan t)
  (imenu-max-items 1000)
  ;; `t' will use a popup menu rather than a minibuffer prompt, `on-mouse' might be useful with
  ;; mouse support enabled
  (imenu-use-popup-menu nil)
  ;; `nil' implies no sorting and will list by position in the buffer
  (imenu-sort-function nil))

(defvar tags-revert-without-query)

(setq large-file-warning-threshold (* 500 1024 1024) ; MB
      tags-add-tables nil
      tags-case-fold-search nil ; t=case-insensitive, nil=case-sensitive
      ;; Do not ask before rereading the `TAGS' files if they have changed
      tags-revert-without-query t)

(use-package xref
  :commands xref-etags-mode
  :bind
  (("M-'"   . xref-find-definitions)
   ("M-?"   . xref-find-references)
   ("C-M-." . xref-find-apropos)
   ("M-,"   . xref-pop-marker-stack)
   :map xref--xref-buffer-mode-map
   ("C-o"   . xref-show-location-at-point)
   ("<tab>" . xref-quit-and-goto-xref)
   ("r"     . xref-query-replace-in-results)))

(use-package dumb-jump
  :after xref
  :demand t
  :commands dumb-jump-xref-activate
  :config
  (setq dumb-jump-quiet t)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; The built-in `describe-function' includes both functions and macros. `helpful-function' is
;; functions only, so we use `helpful-callable' as a drop-in replacement.
(use-package helpful
  :bind
  (("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h f" . helpful-callable)
   ("C-h c" . helpful-command)
   ("C-h p" . helpful-at-point)
   ("C-h o" . helpful-symbol)
   :map helpful-mode-map
   ("q"     . helpful-kill-buffers)))

(use-package vlf ; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
  :commands vlf
  :defines vlf-application
  :init
  (setq vlf-application 'dont-ask)
  (require 'vlf-setup))

;; Erase all consecutive white space characters in a given direction
(use-package hungry-delete
  :commands (hungry-delete-mode global-hungry-delete-mode)
  :diminish
  :hook
  ((minibuffer-setup-hook . (lambda ()
                              (hungry-delete-mode -1)))
   (after-init-hook . global-hungry-delete-mode)))

(use-package move-text ; Move lines with "M-<up>" and "M-<down>"
  :commands (move-text-up move-text-down move-text-default-bindings)
  :init (move-text-default-bindings))

(use-package duplicate-thing
  :bind* ("C-c C-d" . duplicate-thing))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind
  (("C-h C-m" . discover-my-major)
   ("C-h M-m" . discover-my-mode)))

;; Manage minor-mode on the dedicated interface buffer
(use-package manage-minor-mode
  :commands manage-minor-mode)

(use-package expand-region ; Expand region by semantic units
  :bind
  (("C-="   . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package expand-line
  :diminish
  :bind ("M-i" . turn-on-expand-line-mode))

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  ;; :init (run-with-idle-timer 3 nil #'smart-mark-mode)
  :hook (after-init-hook . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :commands (whole-line-or-region-local-mode whole-line-or-region-global-mode)
  :diminish (whole-line-or-region-local-mode)
  ;; :init (run-with-idle-timer 3 nil #'whole-line-or-region-global-mode)
  :hook (after-init-hook . whole-line-or-region-global-mode))

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

;; The real beginning and end of buffers (i.e., `point-min' and `point-max') are accessible by
;; pressing the keys "M-<" and "M->" keys again.
(use-package beginend
  ;; :init (run-with-idle-timer 3 nil #'beginend-global-mode)
  :hook (after-init-hook . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))

(use-package undo-tree
  :defines undo-tree-map
  :commands (global-undo-tree-mode undo-tree-redo)
  :diminish
  :config
  (setq undo-tree-auto-save-history              t
        undo-tree-visualizer-diff                t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-visualizer-timestamps          t)
  (unbind-key "C-/" undo-tree-map)
  :hook (find-file-hook . undo-tree-mode)
  :bind
  (([remap undo] . undo-tree-undo)
   ([remap redo] . undo-tree-redo)
   ("C-z"   . undo-tree-undo)
   ("C-x u" . undo-tree-visualize)))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :bind* ("C-." . iedit-mode))

;; Avoid the "Overwrite old session file (not loaded)?" warning by loading the `session' package
(use-package session
  :disabled t
  :commands (session-initialize)
  :hook (after-init-hook . session-initialize))

(use-package immortal-scratch
  :commands immortal-scratch-mode
  ;; :init (run-with-idle-timer 2 nil #'immortal-scratch-mode)
  :hook (after-init-hook . immortal-scratch-mode))

;; I use the "*scratch*" buffer for taking notes, this package helps to make the data persist
(use-package persistent-scratch
  :commands persistent-scratch-setup-default
  :hook (after-init-hook . persistent-scratch-setup-default)
  :config
  (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

(use-package crux
  :bind
  (("C-c d i" . crux-ispell-word-then-abbrev)
   ("<f12>"   . crux-kill-other-buffers)
   ("C-c d s" . crux-sudo-edit)
   ("C-a"     . crux-move-beginning-of-line)))

;; This package disables the mouse completely which is an extreme.
(use-package disable-mouse
  :if (display-mouse-p)
  :commands global-disable-mouse-mode
  :diminish disable-mouse-global-mode
  :hook (after-init-hook . global-disable-mouse-mode))

;; Move the cursor from the line of view
(use-package avoid
  :ensure nil
  :commands mouse-avoidance-mode
  :if (display-mouse-p)
  :init (mouse-avoidance-mode 'banish))

(use-package apt-sources-list
  :commands apt-sources-list-mode)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook ((prog-mode-hook latex-mode-hook LaTeX-mode-hook org-src-mode-hook) . rainbow-delimiters-mode))

(use-package ssh-config-mode
  :commands (ssh-config-mode ssh-known-hosts-mode ssh-authorized-keys-mode turn-on-font-lock)
  :hook (ssh-config-mode-hook . turn-on-font-lock))

(use-package ace-window
  :bind ([remap other-window] . ace-window))

(use-package windmove ; "Shift + direction" arrows
  :ensure nil
  :commands windmove-default-keybindings
  :init (windmove-default-keybindings)
  :config
  ;; Wrap around at edges
  (setq windmove-wrap-around t))

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `after-save-hook' and leads to auto-formatters being invoked more frequently. We do not need this
;; given that we have `auto-save-visited-mode' enabled.
(use-package super-save
  :defines (super-save-remote-files super-save-triggers)
  :commands super-save-mode
  :disabled t
  :diminish
  ;; :init (run-with-idle-timer 3 nil #'super-save-mode)
  :hook (after-init-hook . super-save-mode)
  :config
  (setq super-save-remote-files nil) ; Ignore remote files, can cause Emacs to hang
  (add-to-list 'super-save-triggers 'ace-window))

;; `avy-setup-default' will bind `avy-isearch' to `C-'' in `isearch-mode-map', so that you can
;; select one of the currently visible `isearch' candidates using `avy'.
(use-package avy
  :commands avy-setup-default
  :bind
  (("M-b"   . avy-goto-word-1)
   ("C-'"   . avy-goto-char-timer) ; Does not work with TUI, but works with Alacritty
   ("M-g c" . avy-goto-char-timer) ; TODO: Reuse the keybinding
   ("C-/"   . avy-goto-line) ; Does not work with TUI, but works with Alacritty
   ;; TODO: Reuse the keybinding
   ("M-g l" . avy-goto-line)))

(use-package ace-jump-buffer
  :bind ("C-b" . ace-jump-buffer)
  :config
  (setq ajb-max-window-height 30
        ajb-sort-function 'bs--sort-by-name))

(use-package bookmark
  :ensure nil)

(use-package bm
  :commands (bm-buffer-save-all bm-repository-save bm-toggle bm-next bm-previous
                                bm-repository-load bm-buffer-save bm-buffer-restore)
  :preface
  (defun sb/bm-setup ()
    "Wrapper function to help call with a timer."
    ;; `kill-buffer-hook' is not called when Emacs is killed
    (add-hook 'kill-emacs-hook (lambda ()
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
    (add-hook 'after-save-hook        #'bm-buffer-save)
    (add-hook 'kill-buffer-hook       #'bm-buffer-save)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
    (add-hook 'after-revert-hook      #'bm-buffer-restore)
    (add-hook 'find-file-hook         #'bm-buffer-restore)
    (add-hook 'after-init-hook        #'bm-repository-load))
  :init
  ;; Must be set before `bm' is loaded
  (setq bm-restore-repository-on-load t)
  ;; We need to use a reasonable delay so that reading the saved bookmarks file does not affect
  ;; usability
  ;; (run-with-idle-timer 2 nil #'sb/bm-setup)
  :hook (after-init-hook . sb/bm-setup)
  :config (setq-default bm-buffer-persistence t)
  :bind
  (("C-<f1>" . bm-toggle)
   ("C-<f2>" . bm-next)
   ("C-<f3>" . bm-previous)))

(use-package esup
  :commands esup
  :if (bound-and-true-p sb/debug-init-file))

(use-package bug-hunter
  :disabled t
  :if (bound-and-true-p sb/debug-init-file)
  :commands (bug-hunter-init-file bug-hunter-file))

(use-package explain-pause-mode
  :ensure nil
  :if (bound-and-true-p sb/debug-init-file)
  :load-path "extras"
  :disabled t
  :commands (explain-pause-mode explain-pause-top)
  :diminish)

;; `text-mode' is the parent mode for `LaTeX-mode' and `org-mode', and so any hooks defined will
;; also get run for all modes derived from a basic mode such as `text-mode'.

;; Enabling `autofill-mode' makes it difficult to include long instructions verbatim, since they get
;; wrapped around automatically.
;; (add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Identify weasel words, passive voice, and duplicate words, `textlint' includes writegood. I
;; prefer `grammarly' and `lsp-ltex'. The module does not check grammar but checks the writing
;; style.
(use-package writegood-mode
  :disabled t
  :commands (writegood-mode writegood-passive-voice-turn-off)
  :diminish
  :hook (text-mode-hook . writegood-mode))

(use-package wc-mode
  :commands wc-mode)

;; Gets the definition of word or phrase at point from https://wordnik.com/
(use-package define-word
  :commands (define-word define-word-at-point))

;; https://languagetool.org/download/LanguageTool-stable.zip
(use-package langtool
  :defines (languagetool-java-arguments languagetool-console-command languagetool-server-command)
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :init (setq langtool-default-language "en-US")
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command (no-littering-expand-etc-file-name
                                      "languagetool-commandline.jar")
        languagetool-server-command (no-littering-expand-etc-file-name
                                     "languagetool-server.jar")
        langtool-language-tool-jar (no-littering-expand-etc-file-name
                                    "languagetool-commandline.jar")))

;; https://emacs.stackexchange.com/questions/19686/how-to-use-pdf-tools-pdf-view-mode-in-emacs
;; Use `isearch', `swiper' will not work
(use-package pdf-tools
  :if (display-graphic-p)
  :defines pdf-annot-activate-created-annotations
  :commands (pdf-tools-install pdf-loader-install pdf-view-mode
                               pdf-annot-delete pdf-annot-add-highlight-markup-annotation
                               pdf-annot-add-text-annotation)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  ;; :init (run-with-idle-timer 3 nil #'require 'pdf-tools nil t) ; Expensive to load
  :hook (after-init-hook . (lambda ()
                             (require 'pdf-tools nil t)))
  :config
  (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install :no-query)'

  (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable

  (setq pdf-annot-activate-created-annotations t  ; Automatically annotate highlights
        pdf-view-resize-factor 1.1) ; Fine-grained zoom factor of 10%

  ;; We do not enable `pdf-view-themed-minor-mode' since it can change plot colors
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  :bind
  (:map pdf-view-mode-map
        ("C-s" . isearch-forward)
        ("d"   . pdf-annot-delete)
        ("h"   . pdf-annot-add-highlight-markup-annotation)
        ("t"   . pdf-annot-add-text-annotation)
        ("M"   . pdf-view-midnight-minor-mode)))

;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.
(use-package saveplace-pdf-view
  :after (pdf-tools saveplace)
  :demand t)

(use-package logview
  :commands logview-mode)

(use-package antlr-mode
  :ensure nil
  :mode "\\.g4\\'")

(use-package bison-mode
  :mode ("\\.bison\\'"))

(use-package llvm-mode
  :ensure nil
  :load-path "extras"
  :commands llvm-mode
  :mode "\\.ll\\'")

(use-package tablegen-mode
  :ensure nil
  :load-path "extras"
  :commands tablegen-mode
  :disabled t
  :mode "\\.td\\'")

(use-package autodisass-llvm-bitcode
  :commands autodisass-llvm-bitcode
  :mode "\\.bc\\'")

;; Enable live preview with "C-c C-c l" (`markdown-live-preview-mode'). The following page lists
;; more shortcuts.
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :commands (markdown-mode gfm-mode markdown-insert-bold
                           markdown-insert-italic
                           markdown-insert-blockquote
                           markdown-insert-pre
                           markdown-insert-code markdown-move-up
                           markdown-insert-link
                           markdown-insert-wiki-link
                           markdown-demote
                           markdown-move-down
                           markdown-insert-header-dwim
                           markdown-insert-reference-link-dwim
                           markdown-insert-header-atx-1
                           markdown-insert-header-atx-2
                           markdown-insert-header-atx-3
                           markdown-insert-header-atx-4
                           markdown-promote
                           markdown-insert-list-item
                           markdown-insert-uri
                           markdown-insert-footnote)
  :mode
  ;; The order is important to associate "README.md" with `gfm-mode'
  (("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  ;; :init
  ;; Looks good, but hiding markup makes it difficult to be consistent while editing
  ;; (setq-default markdown-hide-markup t)
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
  (markdown-hide-urls t)
  :bind
  (:map markdown-mode-map
        ("C-c C-j" . nil)))

;; Generate TOC with `markdown-toc-generate-toc'
(use-package markdown-toc
  :after markdown-mode
  :commands (markdown-toc-refresh-toc markdown-toc-generate-toc
                                      markdown-toc-generate-or-refresh-toc))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf
;; Convert `markdown' to `org': "pandoc -f markdown -t org -o output-file.org input-file.md"
(use-package pandoc-mode
  :commands (pandoc-load-default-settings pandoc-mode)
  :diminish
  :hook (markdown-mode-hook . pandoc-mode)
  :config (pandoc-load-default-settings))

;; Open preview of markdown file in a browser
(use-package markdown-preview-mode
  :disabled t
  :commands markdown-preview-mode)

;; LATER: Prettier times out setting up the process on a remote machine. I am using `format-all'
;; for now.
;; https://github.com/jscheid/prettier.el/issues/84
(use-package prettier
  :if (executable-find "prettier")
  :disabled t
  :commands prettier-mode
  :hook
  ;; Should work with `gfm-mode', `css-mode', and `html-mode' as they are derived modes
  ((markdown-mode-hook web-mode-hook json-mode-hook jsonc-mode-hook js2-mode-hook)
   . (lambda ()
       (when (and buffer-file-name ; Returns `nil' if not visiting a file
                  (not (file-remote-p buffer-file-name)))
         (prettier-mode 1))))
  :config (setq prettier-lighter nil))

;; Align fields with "C-c C-a"
(use-package csv-mode
  :defines lsp-disabled-clients
  :commands csv-mode
  :hook
  (csv-mode . (lambda ()
                (make-local-variable 'lsp-disabled-clients)
                (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                (spell-fu-mode -1)
                (flyspell-mode -1)))
  :custom
  (csv-separators '("," ";" "|" " ")))

(use-package highlight-doxygen
  :commands highlight-doxygen-global-mode
  :init (highlight-doxygen-global-mode))

(use-package make-mode
  :ensure nil
  :mode
  (("\\Makefile\\'"       . makefile-mode)
   ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
   ("makefile\\.rules\\'" . makefile-gmake-mode))
  :config
  (add-hook 'makefile-mode-hook (lambda()
                                  (setq-local indent-tabs-mode t)))
  (use-package makefile-executor))

(use-package eldoc
  :ensure nil
  :if (symbol-value 'sb/IS-LINUX)
  :commands turn-on-eldoc-mode
  :diminish
  :hook (prog-mode-hook . turn-on-eldoc-mode)
  ;; :config
  ;; The variable-height minibuffer and extra eldoc buffers are distracting. This variable limits
  ;; ElDoc messages to one line. This prevents the echo area from resizing itself unexpectedly when
  ;; point is on a variable with a multiline docstring, which is distracting, but then it cuts of
  ;; useful information.
  ;; (setq eldoc-echo-area-use-multiline-p nil)
  )

(use-package css-mode
  :commands css-mode
  :defines sb/flycheck-local-checkers
  :hook (css-mode-hook . lsp-deferred)
  :custom
  (css-indent-offset 2)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("css-languageserver" "--stdio"))
    :major-modes '(css-mode)
    :remote? t
    :server-id 'cssls-r)))

;; `eldoc-box-hover-at-point-mode' blocks the view because it shows up at point.
(use-package eldoc-box
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :hook (eldoc-mode-hook . eldoc-box-hover-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-fringe-use-same-bg nil)
  :diminish eldoc-box-hover-mode eldoc-box-hover-at-point-mode)

(use-package ini-mode
  :commands ini-mode)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlight and allow to open http links in strings and comments in programming
            ;; buffers.
            (goto-address-prog-mode 1)
            ;; Native from Emacs 27+, disable in TUI since the line characters also get copied.
            (when (display-graphic-p)
              (display-fill-column-indicator-mode 1))))

(use-package elisp-mode
  :ensure nil
  :mode
  (("\\.el\\'"  . emacs-lisp-mode)
   ("\\.elc\\'" . elisp-byte-code-mode))
  :hook
  ((lisp-mode emacs-lisp-mode) .
   (lambda ()
     (when buffer-file-name
       (add-hook 'after-save-hook #'check-parens nil t)
       (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-checkdoc 'append)))))

(use-package yaml-mode
  :defines lsp-ltex-enabled lsp-disabled-clients
  :commands yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'" ".clang-format" ".clang-tidy")
  :hook
  (yaml-mode-hook .
                  (lambda ()
                    ;; `yaml-mode' is derived from `text-mode', so disable grammar and spell
                    ;; checking.
                    (make-local-variable 'lsp-disabled-clients)
                    (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                    (spell-fu-mode -1)
                    (flyspell-mode -1)
                    (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("yaml-language-server" "--stdio"))
    :major-modes '(yaml-mode)
    :remote? t
    :server-id 'yamlls-r)))

(use-package yaml-imenu
  :after yaml-mode
  :demand t
  :config (yaml-imenu-enable))

(declare-function ht-merge "ht")

;; Registering `lsp-format-buffer' makes sense only if the server is active. We may not always want
;; to format unrelated files and buffers (e.g., commented YAML files in out-of-project locations).
(use-package lsp-mode
  :ensure spinner
  :diminish
  :defines (lsp-perl-language-server-path
            lsp-perl-language-server-port
            lsp-perl-language-server-client-version
            lsp-completion--regex-fuz
            lsp-clients-clangd-args
            lsp-clients-clangd-executable
            lsp-completion-enable-additional-text-edit
            lsp-completion-show-detail
            lsp-completion-provider
            lsp-completion-show-kind
            lsp-enable-semantic-tokens
            lsp-enable-which-key-integration
            lsp-headerline-breadcrumb-mode
            lsp-html-format-wrap-line-length
            lsp-html-format-end-with-newline
            lsp-html-format-indent-inner-html
            lsp-html-format-max-preserve-new-lines
            lsp-xml-logs-client
            lsp-xml-jar-file
            lsp-xml-jar-version
            lsp-yaml-print-width
            lsp-headerline-breadcrumb-enable-diagnostics
            lsp-modeline-diagnostics-scope)
  :commands (lsp--set-configuration lsp-completion--regex-fuz
                                    lsp-register-client
                                    lsp-tramp-connection
                                    make-lsp-client
                                    lsp-format-buffer
                                    lsp-configuration-section lsp
                                    lsp-deferred
                                    lsp--set-configuration
                                    lsp-package-ensure
                                    lsp-signature-help
                                    lsp-enable-which-key-integration
                                    lsp-modeline-diagnostics-mode
                                    lsp-modeline-code-actions-mode
                                    lsp-symbol-highlight ht-merge
                                    lsp-completion--regex-fuz
                                    lsp-describe-thing-at-point
                                    lsp-find-type-definition)
  :preface
  ;; https://github.com/minad/corfu/wiki
  (defun sb/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex)))
  :hook
  ((lsp-completion-mode-hook . sb/lsp-mode-setup-completion)
   (lsp-mode-hook . lsp-enable-which-key-integration)
   (lsp-mode-hook . lsp-lens-mode))
  :custom-face
  ;; Reduce the height
  (lsp-headerline-breadcrumb-symbols-face ((t (:inherit
                                               font-lock-doc-face :weight bold :height 0.9))))
  (lsp-headerline-breadcrumb-prefix-face ((t (:inherit font-lock-string-face :height 0.9))))
  (lsp-headerline-breadcrumb-project-prefix-face ((t (:inherit font-lock-string-face
                                                               :weight bold :height 0.9))))
  :config
  ;; We can add "--compile-commands-dir=<build-dir>" option to indicate the directory where
  ;; "compile_commands.json" reside. If path is invalid, clangd will look in the current directory
  ;; and parent paths of each source file.
  (setq lsp-clients-clangd-args '("-j=4"
                                  "--all-scopes-completion"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--cross-file-rename"
                                  "--fallback-style=LLVM"
                                  "--header-insertion=never"
                                  "--log=error"
                                  "--malloc-trim" ;; Release memory periodically
                                  ;; Increases memory usage but can improve performance
                                  "--pch-storage=memory"
                                  "--pretty")
        ;; Enable integration of custom backends other than `company-capf'
        lsp-completion-provider :none
        lsp-completion-show-detail nil ; Disable completion metadata since they can be very long
        ;; lsp-completion-show-kind nil
        lsp-eldoc-enable-hover nil
        lsp-enable-dap-auto-configure nil
        lsp-enable-on-type-formatting nil ; Reduce unexpected modifications to code
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        ;; lsp-semantic-tokens-enable t
        lsp-headerline-breadcrumb-enable nil ; Breadcrumb is not useful for all modes
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-html-format-wrap-line-length sb/fill-column
        lsp-html-format-end-with-newline t
        lsp-html-format-indent-inner-html t
        lsp-imenu-sort-methods '(position)
        ;; lsp-keep-workspace-alive nil
        lsp-log-io nil ; Increases memory usage because of JSON parsing if enabled
        ;; We have `flycheck' error summary listed on the modeline, but the `lsp' server may report
        ;; additional errors. The problem is that the modeline can get too congested.
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-diagnostics-scope :file ; Focus on the errors at hand
        lsp-modeline-workspace-status-enable nil
        ;; Sudden changes in the height of the echo area causes the cursor to lose position,
        ;; manually request via `lsp-signature-activate'
        ;; lsp-signature-auto-activate nil
        ;; Disable showing function documentation with `eldoc'
        ;; lsp-signature-render-documentation nil
        ;; lsp-signature-function 'lsp-signature-posframe
        ;; Avoid annoying questions. We expect a server restart to succeed more often than not.
        lsp-restart 'auto-restart
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
        lsp-use-plists nil
        lsp-xml-logs-client nil
        lsp-yaml-print-width sb/fill-column)

  (if (display-graphic-p)
      (setq lsp-modeline-code-actions-enable t)
    (setq lsp-modeline-code-actions-enable nil))

  ;; Autocomplete parentheses
  (when (featurep 'yasnippet)
    (setq lsp-enable-snippet t))

  (defvar lsp-pylsp-configuration-sources)
  (defvar lsp-pylsp-plugins-autopep8-enable)
  (defvar lsp-pylsp-plugins-mccabe-enabled)
  (defvar lsp-pylsp-plugins-pycodestyle-enabled)
  (defvar lsp-pylsp-plugins-pycodestyle-max-line-length)
  (defvar lsp-pylsp-plugins-pydocstyle-convention)
  (defvar lsp-pylsp-plugins-pydocstyle-enabled)
  (defvar lsp-pylsp-plugins-pydocstyle-ignore)
  (defvar lsp-pylsp-plugins-pyflakes-enabled)
  (defvar lsp-pylsp-plugins-pylint-args)
  (defvar lsp-pylsp-plugins-pylint-enabled)
  (defvar lsp-pylsp-plugins-yapf-enabled)
  (defvar lsp-pyright-langserver-command-args)
  (defvar lsp-pylsp-plugins-preload-modules)

  (when (eq sb/python-langserver 'pylsp)
    (setq lsp-pylsp-configuration-sources []
          lsp-pylsp-plugins-autopep8-enable nil
          ;; Do not turn on fuzzy completion with jedi, `lsp-mode' is fuzzy on the client side
          ;; lsp-pylsp-plugins-jedi-completion-fuzzy nil
          lsp-pylsp-plugins-mccabe-enabled nil
          ;; We can also set this per-project
          lsp-pylsp-plugins-preload-modules ["numpy", "csv", "pandas", "statistics", "json"]
          lsp-pylsp-plugins-pycodestyle-enabled nil
          lsp-pylsp-plugins-pycodestyle-max-line-length sb/fill-column
          lsp-pylsp-plugins-pydocstyle-convention "pep257"
          lsp-pylsp-plugins-pydocstyle-enabled nil
          lsp-pylsp-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213"))
          lsp-pylsp-plugins-pyflakes-enabled nil
          lsp-pylsp-plugins-pylint-args (vconcat
                                         (list "-j 2"
                                               (concat "--rcfile="
                                                       (expand-file-name ".config/pylintrc"
                                                                         sb/user-home))))
          lsp-pylsp-plugins-pylint-enabled t ; Pylint can be expensive
          lsp-pylsp-plugins-yapf-enabled t))

  (dolist (ignore-dirs '("/build\\'"
                         "/\\.metadata\\'"
                         "/\\.recommenders\\'"
                         "/\\.clangd\\'"
                         "/\\.cache\\'"
                         "/__pycache__\\'"
                         "/\\.log\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories ignore-dirs))

  (with-eval-after-load "lsp-lens"
    (diminish 'lsp-lens-mode))
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind
  ;; `lsp-imenu-create-categorised-index' - sorts the items by kind.
  ;; `lsp-imenu-create-uncategorized-index' - will have the items sorted by position.
  (("M-." . lsp-find-definition)
   :map lsp-command-map
   ("=" . nil)
   ("w" . nil)
   ("g" . nil)
   ("G" . nil)
   ("a" . nil)
   ("F" . nil)
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
   ("c" . lsp-imenu-create-categorised-index)
   ("u" . lsp-imenu-create-uncategorised-index)
   ("a" . lsp-workspace-folders-add)
   ("v" . lsp-workspace-folders-remove)
   ("b" . lsp-workspace-blacklist-remove)))

(use-package lsp-ui
  :defines lsp-ui-modeline-code-actions-enable
  :commands (lsp-ui-doc-mode lsp-ui-mode lsp-ui-doc--hide-frame
                             lsp-ui-peek-find-implementation lsp-ui-imenu)
  :after lsp-mode
  :demand t
  :custom
  (lsp-ui-doc-enable t "Enable/disable on-hover dialogs")
  (lsp-ui-doc-include-signature t)
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-imenu-window-width 16)
  (lsp-ui-sideline-enable t "Enable/disable whole sideline")
  ;; Showing code actions in the sideline enables understanding when to invoke them
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover nil)
  ;; Show/hide diagnostics when typing because they can be intrusive
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-max-height 8)
  (lsp-ui-doc-max-width 72 "150 (default) is too wide")
  (lsp-ui-doc-delay 0.75 "0.2 (default) is too naggy")
  :config
  (when (not (display-graphic-p))
    (setq lsp-ui-doc-enable nil
          lsp-ui-peek-enable nil))

  (lsp-ui-mode 1)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references]  . lsp-ui-peek-find-references)
        :map lsp-command-map
        ("D" . lsp-ui-doc-mode)))

;; Sync workspace folders and treemacs projects
(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list lsp-treemacs-sync-mode)
  :config (lsp-treemacs-sync-mode 1)
  :bind
  (:map lsp-command-map
        ("S" . lsp-treemacs-symbols)
        ("F" . lsp-treemacs-references)
        ("Y" . lsp-treemacs-sync-mode)
        ("C" . lsp-treemacs-call-hierarchy)
        ("T" . lsp-treemacs-type-hierarchy)
        ("E" . lsp-treemacs-errors-list)))

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :demand t
  :bind
  (:map lsp-command-map
        ("G" . lsp-ivy-global-workspace-symbol)
        ("W" . lsp-ivy-workspace-symbol)))

(use-package dap-mode
  :commands (dap-debug dap-hydra dap-mode dap-ui-mode)
  :hook
  ((lsp-mode-hook . dap-mode)
   (lsp-mode-hook . dap-ui-mode)))

(use-package docstr
  :diminish
  :hook ((c++-mode-hook python-mode-hook java-mode-hook) . docstr-mode))

(use-package cc-mode
  :ensure nil
  :defines (c-electric-brace c-enable-auto-newline c-set-style)
  :commands (c-fill-paragraph c-end-of-defun c-beginning-of-defun c++-mode)
  :mode
  (("\\.h\\'" . c++-mode)
   ("\\.c\\'" . c++-mode))
  :hook (c++-mode-hook . lsp-deferred)
  :custom
  (c-set-style "cc-mode")
  (c-basic-offset 2)
  :config
  (defvar c-electric-indent)

  ;; Disable electric indentation and on-type formatting
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq-local c-auto-newline nil
                          c-electric-brace nil
                          c-electric-flag nil
                          c-electric-indent nil
                          c-enable-auto-newline nil
                          c-syntactic-indentation nil)))

  (unbind-key "C-M-a" c-mode-map)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "clangd")
    :major-modes '(c-mode c++-mode)
    :remote? t
    :server-id 'clangd-r))
  :bind
  (:map c-mode-base-map
        ("C-c c a" . c-beginning-of-defun)
        ("C-c c e" . c-end-of-defun)
        ("M-q"     . c-fill-paragraph)))

(use-package modern-cpp-font-lock
  :commands modern-c++-font-lock-mode
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode-hook . modern-c++-font-lock-mode))

(use-package cuda-mode
  :commands cuda-mode
  :mode
  (("\\.cu\\'"  . c++-mode)
   ("\\.cuh\\'" . c++-mode)))

(use-package opencl-mode
  :commands opencl-mode
  :mode "\\.cl\\'")

(use-package pyvenv
  :commands (pyvenv-mode pyvenv-tracking-mode)
  :hook (python-mode-hook . pyvenv-mode)
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:"
                                                         pyvenv-virtual-env-name "] ")))
  (pyvenv-post-activate-hooks (list
                               (lambda ()
                                 (setq python-shell-interpreter
                                       (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks (list
                                 (lambda ()
                                   (setq python-shell-interpreter "python3")))))

(use-package py-isort
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :commands py-isort-before-save
  :hook
  (python-mode-hook . (lambda ()
                        (add-hook 'before-save-hook #'py-isort-before-save)))
  :custom
  (py-isort-options '("-l 100")))

;; "pyright --createstub pandas"
(use-package lsp-pyright
  :if (and (eq sb/python-langserver 'pyright) (executable-find "pyright"))
  :commands (lsp-pyright-locate-python lsp-pyright-locate-venv)
  :hook
  (python-mode-hook . (lambda ()
                        (require 'lsp-pyright)))
  :custom
  (lsp-pyright-python-executable-cmd "python3")
  (lsp-pyright-typechecking-mode "basic")
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     (lambda ()
                       (cons "pyright-langserver"
                             lsp-pyright-langserver-command-args)))
    :major-modes '(python-mode)
    :remote? t
    :server-id 'pyright-r
    :multi-root lsp-pyright-multi-root
    :priority 3
    :initialization-options (lambda ()
                              (ht-merge (lsp-configuration-section "pyright")
                                        (lsp-configuration-section "python")))
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
					                      (lsp--set-configuration
					                       (ht-merge (lsp-configuration-section "pyright")
						                             (lsp-configuration-section "python")))))
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure 'pyright callback error-callback))
    :notification-handlers
    (lsp-ht
     ("pyright/beginProgress"  'lsp-pyright--begin-progress-callback)
     ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
     ("pyright/endProgress"    'lsp-pyright--end-progress-callback)))))

;; Yapfify works on the original file, so that any project settings supported by YAPF itself are
;; used.
(use-package yapfify
  :diminish yapf-mode
  :if (and (eq sb/python-langserver 'pyright) (executable-find "yapf"))
  :commands yapf-mode
  :hook (python-mode-hook . yapf-mode))

(use-package bat-mode
  :ensure nil
  :commands bat-mode
  :mode
  (("\\.bat\\'" . bat-mode)
   ("\\.cmd\\'" . bat-mode)))

(use-package web-mode
  :commands web-mode
  :mode "\\.html?\\'"
  :hook (web-mode-hook . lsp-deferred)
  :custom
  (web-mode-enable-auto-closing              t)
  (web-mode-enable-auto-pairing              nil "Prefer `smartparens'")
  (web-mode-enable-auto-quoting              t)
  (web-mode-enable-block-face                t)
  (web-mode-enable-css-colorization          t)
  (web-mode-enable-current-element-highlight t "Highlight the element under the cursor")
  (web-mode-enable-current-column-highlight  t)
  (web-mode-markup-indent-offset             2) ; HTML
  (web-mode-css-indent-offset                2) ; CSS
  (web-mode-code-indent-offset               2) ; Script
  (web-mode-style-padding                    2) ; For `<style>' tag
  (web-mode-script-padding                   2) ; For `<script>' tag
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("html-languageserver" "--stdio"))
    :major-modes '(html-mode web-mode mhtml-mode)
    :remote? t
    :server-id 'htmlls-r)))

(use-package emmet-mode
  :defines emmet-move-cursor-between-quote
  :commands emmet-mode
  :hook ((web-mode-hook css-mode-hook html-mode-hook) . emmet-mode)
  :custom (emmet-move-cursor-between-quote t))

(use-package rainbow-mode
  :commands rainbow-mode
  :hook ((css-mode-hook html-mode-hook web-mode-hook) . rainbow-mode))

(use-package format-all
  :commands (format-all-ensure-formatter format-all-buffer)
  :diminish
  :preface
  (defun sb/enable-format-all ()
    "Delay enabling format-all to avoid slowing down Emacs startup."
    (dolist (hook '(bazel-mode-hook LaTeX-mode-hook web-mode-hook markdown-mode-hook))
      (add-hook hook #'format-all-mode))
    (add-hook 'format-all-mode-hook #'format-all-ensure-formatter))
  ;; :init (run-with-idle-timer 2 nil #'sb/enable-format-all)
  :diminish
  :hook
  ((format-all-mode-hook . format-all-ensure-formatter)
   ((bazel-mode-hook LaTeX-mode-hook web-mode-hook markdown-mode-hook) . format-all-mode)))

(use-package vertico
  :if (eq sb/minibuffer-completion 'vertico)
  :defines read-extended-command-predicate
  :commands command-completion-default-include-p
  :hook (after-init-hook . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil)
  (vertico-count 12)
  (vertico-scroll-margin 4)
  :config
  ;; Hide commands in "M-x" in Emacs 28 which do not work in the current mode. Vertico commands are
  ;; hidden in normal buffers.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  :bind
  (("<f2>" .  find-file)
   :map vertico-map
   ("<escape>" . minibuffer-keyboard-quit)
   ("?" . minibuffer-completion-help)
   ("M-RET" . minibuffer-force-complete-and-exit)
   ("M-TAB" . minibuffer-complete)))

;; More convenient directory navigation commands
(use-package vertico-directory
  :after vertico
  :ensure nil
  :load-path "extras"
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

(use-package vertico-repeat
  :after vertico
  :ensure nil
  :load-path "extras"
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :bind
  (("C-c r" . vertico-repeat-last)
   ("M-r" . vertico-repeat-select)))

(use-package vertico-indexed
  :after vertico
  :ensure nil
  :load-path "extras"
  :commands vertico-indexed-mode
  :init (vertico-indexed-mode 1))

(use-package vertico-quick
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
        ("C-c q" . vertico-quick-insert)
        ("C-'" . vertico-quick-exit)))

(use-package consult
  :commands consult--customize-put
  :custom
  (consult-line-start-from-top t "Start search from the beginning")
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-project-function #'projectile-project-root)
  (consult-line-numbers-widen t)
  :bind
  (("C-x M-:" . consult-complex-command)
   ([remap repeat-complex-command] . consult-complex-command)
   ("C-x b" . consult-buffer)
   ("<f3>" . consult-buffer)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap bookmark-jump] . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ("M-y" . consult-yank-pop)
   ([remap yank-pop] . consult-yank-pop)
   ([remap apropos] . consult-apropos)
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ([remap goto-line] . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("C-c C-j" . consult-imenu)
   ([remap imenu] . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ([remap load-theme] . consult-theme)
   ;; M-s bindings (search-map)
   ("M-s f" . consult-find)
   ([remap locate] . consult-locate)
   ("M-s l" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("<f4>" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ("<f9>" . consult-recent-file)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap multi-occur] . consult-multi-occur)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   )
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode-hook . consult-preview-at-point-mode)
  :config
  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; TODO: Is this what is causing issues with latex?
  (unless (display-graphic-p)
    (setq completion-in-region-function #'consult-completion-in-region))

  ;; Disable live preview
  (consult-customize
   consult-recent-file consult-buffer
   :preview-key nil))

(use-package consult-projectile
  :if (eq sb/minibuffer-completion 'vertico)
  :commands consult-projectile-recentf
  :bind
  (("<f5>" . consult-projectile-switch-project)
   ("<f6>" . consult-projectile)))

(use-package consult-lsp
  :after (consult lsp)
  :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols)
  :config (consult-lsp-marginalia-mode 1))

(use-package consult-flycheck
  :after (consult flycheck)
  :bind
  (:map flycheck-command-map
        ("!" . consult-flycheck)))

(use-package consult-flyspell
  :after (consult flyspell)
  :commands consult-flyspell)

(use-package consult-dir
  :bind
  (([remap list-directory] . consult-dir)
   ("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-project-extra)

(use-package consult-yasnippet
  :bind ("C-M-y" . consult-yasnippet))

;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
(use-package corfu
  :if (and (display-graphic-p) (eq sb/capf 'corfu))
  :preface
  (defun sb/corfu-move-to-minibuffer ()
    (interactive)
    (let ((completion-extra-properties corfu--extra)
          completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))
  :hook (after-init-hook . corfu-global-mode)
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-min-width 60)
  (corfu-max-width corfu-min-width)
  (corfu-count 15)
  (corfu-preselect-first t)
  :bind
  (:map corfu-map
        ([tab] . corfu-next)
        ([backtab] . corfu-previous)
        ("M-m" . sb/corfu-move-to-minibuffer)))

(use-package corfu-doc
  :if (and (display-graphic-p) (eq sb/capf 'corfu))
  :hook (corfu-mode-hook . corfu-doc-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :demand t
  :commands kind-icon-margin-formatter
  :custom
  (kind-icon-face 'corfu-default)
  (kind-icon-default-face 'corfu-default) ; To compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; https://kristofferbalintona.me/posts/cape/
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; Complete programming language keyword
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; Complete unicode char from TeX command, e.g. \hbar.
  (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; Complete abbreviation at point.
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; Complete word from dictionary at point.
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; Complete current line from other lines in buffer.
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol) ; Elisp symbol
  ;; Complete word at point with Ispell.
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; Complete with Dabbrev at point.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :custom
  (cape-dict-file "/home/swarnendu/.config/Code/User/spellright.dict"))

(use-package marginalia
  :after vertico
  :init (marginalia-mode 1))

;; We prefer to use "kind-icon" package for icons since it has more active commits but I do not know
;; which is better.
(use-package all-the-icons-completion
  :ensure all-the-icons
  :ensure t
  :disabled t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :after vertico
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command
        which-key-use-C-h-commands nil)
  :bind
  (([remap describe-bindings] . embark-bindings)
   :map vertico-map
   ("C-l" . embark-act)
   ("C-c C-l" . embark-export)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(use-package centaur-tabs
  :commands centaur-tabs-group-by-projectile-project
  :hook (emacs-startup-hook . centaur-tabs-mode)
  :init
  (setq centaur-tabs-set-icons nil ; The icons often do not blend well with the theme
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "*"
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-close-button nil
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-enable-ido-completion nil)
  :config
  (centaur-tabs-group-by-projectile-project)
  :bind
  (("M-<right>" . centaur-tabs-forward-tab)
   ("M-<left>" . centaur-tabs-backward-tab)))

;;; init-emacs28.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(embark-consult embark kind-icon consult-yasnippet consult-project-extra consult-dir consult-flyspell consult-flycheck consult-lsp vertico use-package spinner orderless no-littering markdown-mode marginalia lv ht format-all doom-themes doom-modeline diminish corfu-doc consult-projectile centaur-tabs cape beacon anzu amx all-the-icons-ibuffer all-the-icons-dired))
 '(safe-local-variable-values
   '((eval progn
           (add-to-list 'lsp-file-watch-ignored-directories "build"))
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; cmake --build .;")
     (projectile-project-compilation-dir . ".")
     (counsel-find-file-ignore-regexp . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|.o$\\|\\.clangd\\|^build")
     (dired-omit-files . "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.git\\'|\\.cache\\'")
     (eval add-hook 'lsp-managed-mode-hook
           (lambda nil
             (when
                 (derived-mode-p 'markdown-mode)
               (setq sb/flycheck-local-checkers
                     '((lsp
                        (next-checkers markdown-markdownlint-cli)))))
             (when
                 (derived-mode-p 'gfm-mode)
               (setq sb/flycheck-local-checkers
                     '((lsp
                        (next-checkers markdown-markdownlint-cli)))))
             (when
                 (derived-mode-p 'sh-mode)
               (setq sb/flycheck-local-checkers
                     '((lsp
                        (next-checkers sh-shellcheck)))))
             (when
                 (derived-mode-p 'yaml-mode)
               (setq sb/flycheck-local-checkers
                     '((lsp
                        (next-checkers yaml-yamllint)))))
             (when
                 (derived-mode-p 'json-mode)
               (setq sb/flycheck-local-checkers
                     '((lsp
                        (next-checkers json-jsonlint)))))
             (when
                 (derived-mode-p 'python-mode)
               (setq sb/flycheck-local-checkers
                     '((lsp
                        (next-checkers python-pylint)))))
             (when
                 (derived-mode-p 'c++-mode)
               (setq sb/flycheck-local-checkers
                     '((lsp
                        (next-checkers c/c++-cppcheck)))))
             (when
                 (derived-mode-p 'html-mode)
               (setq sb/flycheck-local-checkers
                     '((lsp
                        (next-checkers html-tidy)))))
             (when
                 (derived-mode-p 'xml-mode)
               (setq sb/flycheck-local-checkers
                     '((lsp
                        (next-checkers xml-xmllint))))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-headerline-breadcrumb-prefix-face ((t (:inherit font-lock-string-face :height 0.9))))
 '(lsp-headerline-breadcrumb-project-prefix-face ((t (:inherit font-lock-string-face :weight bold :height 0.9))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit font-lock-doc-face :weight bold :height 0.9)))))
