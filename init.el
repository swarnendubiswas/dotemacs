;;; init.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8; fill-column: 80; -*-

;; Swarnendu Biswas

;;; Commentary: My configuration is mostly targeted toward GNU Linux. I
;;; primarily use Company compared to Corfu for completion because it provides
;;; for fine-grained control. I use Eglot compared to lsp-mode because it feels
;;; lightweight and I rarely use multiple servers simultaneously.

;;; Code:

(defgroup sb/emacs nil
  "Personal configuration for GNU Emacs."
  :group 'local)

(defcustom sb/extras-directory (expand-file-name "extras" user-emacs-directory)
  "Path for third-party packages and files."
  :type 'string
  :group 'sb/emacs)

;; `Modus-vivendi' is the most complete, has good contrast, and integrates well
;; with all terminals. `Catppuccin' is more colorful.
(defcustom sb/theme
  (if (display-graphic-p)
      'kanagawa
    'kanagawa)
  "Specify which Emacs theme to use."
  :type
  '(radio
    (const :tag "doom-nord" doom-nord)
    (const :tag "modus-vivendi" modus-vivendi)
    (const :tag "catppuccin" catppuccin)
    (const :tag "rose-pine" rose-pine)
    (const :tag "kanagawa" kanagawa)
    (const :tag "leuven-dark" leuven-dark)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Powerline looks clean and nerdy, but doom-modeline is more informative and
;; maintained. A plain modeline also suffices and avoids startup overhead in
;; `standalone' mode.
(defcustom sb/modeline-theme 'doom-modeline
  "Specify the mode-line theme to use."
  :type
  '(radio
    (const :tag "powerline" powerline)
    (const :tag "doom-modeline" doom-modeline)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Company works better with both Windows and TUI Emacs, and has more extensive
;; LaTeX support than Corfu. It allows finer-grained control by allowing
;; grouping of backends. We can set up separate completion files with
;; `company-ispell' and `company-dict'. However, `company-ispell' does not keep
;; prefix case when used as a grouped backend. Company works well with TUI
;; because Emacs versions till 30.x does not support child frames on the
;; terminal which is required by Corfu.

;; Corfu integrates nicely with `orderless' and provides better completion for
;; Elisp symbols with `cape-elisp-symbol'. But `corfu-terminal-mode' with Emacs
;; < 31 has a rendering problem for completion popups appearing near the right
;; edges with terminal Emacs. The completion entries wrap around sometimes, and
;; messes up the completion.

(defcustom sb/in-buffer-completion
  (if (display-graphic-p)
      'corfu
    'corfu)
  "Choose the framework to use for completion at point."
  :type
  '(radio
    (const :tag "corfu" corfu)
    (const :tag "company" company)
    (const :tag "none" none))
  :group 'sb/emacs)

;; Keeping icons disabled should lower overhead and help with the alignment
;; issue with Company and Corfu popups in the terminal.
(defcustom sb/enable-icons t
  "Should icons be enabled?
The provider is `nerd-icons'."
  :type 'boolean
  :group 'sb/emacs)

;; Nerd-icons are more compact and look nicer than kind-icons.
(defcustom sb/corfu-icons 'kind-icons
  "Choose the provider for Corfu icons."
  :type
  '(radio
    (const :tag "kind-icon" kind-icon)
    (const :tag "nerd-icons" nerd-icons)
    (const :tag "none" none))
  :group 'sb/emacs)

;; It is tempting to use Eglot because it is built in to Emacs and is
;; lightweight. Eglot does not allow multiple servers to connect to a major
;; mode, does not support semantic tokens.

;; Lsp-mode offers several advantages. It allows connecting to multiple servers
;; simultaneously and provides helpers to install and uninstall servers.

;; Using a single server suffices for most programming language major modes, but
;; it is beneficial to use more than one LS for languages like plain text,
;; markdown, and LaTeX. Texlab is inefficient and so Eglot suffices for me.
(defcustom sb/lsp-provider 'eglot
  "Choose between Lsp-mode and Eglot."
  :type
  '(radio
    (const :tag "lsp-mode" lsp-mode)
    (const :tag "eglot" eglot)
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/python-langserver 'basedpyright
  "Choose the Python Language Server implementation."
  :type
  '(radio
    (const :tag "pylsp" pylsp)
    (const :tag "basedpyright" basedpyright)
    (const :tag "none" none))
  :group 'sb/emacs)

(defcustom sb/op-mode 'daemon
  "Specify the way you expect Emacs to be used."
  :type
  '(radio
    (const :tag "server" server)
    (const :tag "daemon" daemon)
    (const :tag "standalone" standalone))
  :group 'sb/emacs)

(defconst sb/user-home-directory (getenv "HOME")
  "User HOME directory.")

;; Install `use-package' support for Elpaca.
(elpaca
 elpaca-use-package
 ;; Enable support for the `:ensure' keyword in `use-package' for Elpaca.
 (elpaca-use-package-mode))

(eval-and-compile
  (setopt
   use-package-always-ensure t
   use-package-enable-imenu-support t))

;; Delay loading packages only in standalone mode.
(when (eq sb/op-mode 'standalone)
  (eval-and-compile
    (setopt
     use-package-expand-minimally t
     use-package-always-defer t)))

(when (eq sb/op-mode 'daemon)
  (eval-and-compile
    (setopt
     use-package-verbose t
     use-package-minimum-reported-time 0 ; Show everything
     ;; Use "M-x use-package-report" to see results
     use-package-compute-statistics t)))

(elpaca-wait) ; Wait for Elpaca to finish activating packages

;; "C-h b" lists all the bindings available in a buffer, "C-h m" shows the
;; keybindings for the major and the minor modes.

;; Check "use-package-keywords.org" for a suggested order of `use-package'
;; keywords.

;; The following webpage provides a few recommendations on how to use
;; `use-package'. Where possible, it is better to avoid :preface, :config and
;; :init. Instead, prefer autoloading keywords such as :bind, :hook, and :mode,
;; as they will take care of setting up autoloads.
;; https://batsov.com/articles/2025/04/17/using-use-package-the-right-way/

(use-package diminish
  :ensure (:wait t))

(use-package no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (custom-file (no-littering-expand-var-file-name "custom.el"))
  :config (no-littering-theme-backups))

(elpaca-wait)

;; Emacs launched in the terminal gets to see $PATH but the GUI app may not. Use
;; "~/.profile" for defining exports that modify $PATH, while use "~/.bashrc"
;; for defining aliases. Then, we can avoid passing shell arguments to be more
;; efficient.
(use-package exec-path-from-shell
  :when (and (eq system-type 'gnu/linux) (or (display-graphic-p) (daemonp)))
  :demand t
  :init
  (setopt
   exec-path-from-shell-check-startup-files nil
   exec-path-from-shell-variables '("PATH" "JAVA_HOME" "LSP_USE_PLISTS")
   exec-path-from-shell-arguments nil) ; Reduce the start up time
  (exec-path-from-shell-initialize))

(use-package emacs
  :ensure nil
  :hook
  (elpaca-after-init
   .
   (lambda ()
     (save-place-mode 1)
     ;; (size-indication-mode 1) ; No benefit in seeing the file size
     (column-number-mode 1)
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
     ;; Add a prefix to continuation lines to prevent them from being indented
     ;; too far or wrapping awkwardly
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
       (global-so-long-mode 1))))
  :bind
  (("<f1>" . execute-extended-command)
   ("<f2>" . find-file)
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
   ("C-s" . save-buffer)
   ("C-x k" . kill-current-buffer)
   ("C-<left>" . backward-word)
   ("C-<right>" . forward-word)
   ("M-\\" . delete-horizontal-space)
   ("M-#" . cycle-spacing)
   ("C-M-b" . backward-sexp)
   ("C-M-f" . forward-sexp)
   ("C-M-k" . kill-sexp))
  :bind* ("C-x s" . scratch-buffer) ; Bound to `save-some-buffers'
  :custom
  ;; (ad-redefinition-action 'accept "Turn off warnings due to redefinitions")
  (auto-save-no-message t "Do not print frequent auto-save messages")
  ;; Disable auto-saving based on number of characters typed
  (auto-save-interval 0)

  ;; ;; Save buffer to file after idling for 10s. The default of 5s may be too
  ;; ;; frequent since it runs all the save-related hooks.
  ;; (auto-save-visited-interval 10)
  ;; (apropos-do-all t "Make `apropos' search more extensively")

  ;; Save bookmark after every bookmark edit and also when Emacs is killed
  (bookmark-save-flag 1)

  ;; Autofill comments in modes that define them
  ;; (comment-auto-fill-only-comments t)

  ;; Show the actual symbol name in the *customize* buffer
  (custom-unlispify-menu-entries nil)
  (create-lockfiles nil)
  (backup-inhibited t "Disable backup for a per-file basis")
  (make-backup-files nil "Stop making backup `~' files")
  (custom-safe-themes t)
  (delete-by-moving-to-trash t) ; Safe
  (help-window-select t "Makes it easy to close the window")
  (read-process-output-max (* 4 1024 1024)) ; 4 MB as recommended by `lsp-mode'
  (remote-file-name-inhibit-locks t)
  ;; Do not auto-save remote files using `auto-save-visited-mode'
  (remote-file-name-inhibit-auto-save-visited t)
  (ring-bell-function 'ignore "Disable beeping sound")
  ;; (visible-bell nil)
  (save-interprogram-paste-before-kill t)
  (select-enable-clipboard t)

  ;; (history-delete-duplicates t)
  ;; (kill-do-not-save-duplicates t "Do not save duplicates to kill ring")

  (sentence-end-double-space nil)
  (shift-select-mode nil)

  ;; (sort-fold-case nil "Do not ignore case when sorting")
  ;; (tags-case-fold-search nil "case-sensitive")

  (standard-indent 2)
  (switch-to-buffer-preserve-window-point t)
  (view-read-only t "Use view mode for read-only buffers")

  ;; (window-combination-resize t "Resize windows proportionally")
  ;; (max-mini-window-height 0.3)

  (x-gtk-use-system-tooltips nil "Do not use system tooltips")
  ;; Disable the warning "X and Y are the same file" in case of symlinks
  (find-file-suppress-same-file-warnings t)

  ;; (auto-mode-case-fold nil "Avoid a second pass through `auto-mode-alist'")
  ;; (confirm-kill-emacs nil)

  ;; Prevent 'Active processes exist' when you quit Emacs
  (confirm-kill-processes nil)
  (require-final-newline t "Always end a file with a newline")

  ;; Reverting without confirmation is confusing, and hence it is better to be
  ;; explicit
  ;; (revert-without-query '("\\.*") "Revert all files without asking")

  (vc-handled-backends '(Git))
  ;; Disable version control for remote files to improve performance
  (vc-ignore-dir-regexp
   (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))

  (scroll-preserve-screen-position t)
  ;; Number of lines of margin at the top and bottom of a window when automatic scrolling is triggered
  (scroll-margin 2)

  ;; (scroll-step 1)
  ;; (scroll-conservatively 10)
  ;; (scroll-error-top-bottom t)

  ;; Accelerate scrolling operations when non-nil. Only those portions of the
  ;; buffer which are actually going to be displayed get fontified.
  (fast-but-imprecise-scrolling t)
  (auto-window-vscroll nil)
  (hscroll-margin 2)
  (hscroll-step 1)

  ;; (fringes-outside-margins t)

  ;; Improve Emacs' responsiveness by delaying syntax highlighting during input
  (redisplay-skip-fontification-on-input t)
  (bidi-inhibit-bpa nil) ; Disabling BPA makes redisplay faster

  ;; Show contextual lines around a match
  ;; (list-matching-lines-default-context-lines 1)

  (imenu-auto-rescan t)
  (imenu-use-popup-menu nil)
  (save-silently t)
  (show-paren-when-point-inside-paren t)
  (whitespace-line-column fill-column)
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

   ;; ;; TAB first tries to indent the current line, and if the line was already
   ;; ;; indented, then try to complete the thing at point.
   ;; tab-always-indent 'complete

   bidi-display-reordering 'left-to-right
   bidi-paragraph-direction 'left-to-right)

  ;; Not a library/file, so `eval-after-load' does not work
  ;; (diminish 'auto-fill-function)
  (diminish 'auto-fill-mode)

  ;; (when (file-in-directory-p buffer-file-name user-emacs-directory)
  ;; (advice-add 'risky-local-variable-p :override #'ignore))

  (advice-add 'risky-local-variable-p :override #'ignore)

  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))

  ;; Mark safe variables
  (put 'compilation-read-command 'safe-local-variable #'stringp)
  (put 'reftex-default-bibliography 'safe-local-variable #'stringp)

  (put 'overwrite-mode 'disabled t)

  ;; ;; Keep the cursor out of the read-only portions of the minibuffer
  ;; (setopt minibuffer-prompt-properties
  ;;         '(read-only
  ;;           t intangible t cursor-intangible t face minibuffer-prompt))

  ;; (add-hook
  ;;  'minibuffer-setup-hook
  ;;  (lambda ()
  ;;    (cursor-intangible-mode 1)
  ;;    ;; Allow Flycheck to show long error messages in the minibuffer
  ;;    (setq truncate-lines nil)
  ;;    (visual-line-mode 1)))

  ;; Originally bound to `abort-recursive-edit'. I use it as the prefix key for
  ;; Zellij.
  (unbind-key "C-]")
  (unbind-key "C-x f") ; Bound to `set-fill-column'
  (unbind-key "M-'") ; Bound to `abbrev-prefix-mark'

  ;; (unbind-key "C-j") ; Bound to `electric-newline-and-maybe-indent'

  (setopt display-buffer-alist
          '(
            ;; Allow *Help* buffers to use the full frame
            ("*Help*" (display-buffer-same-window))
            ("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
             (display-buffer-no-window)
             (allow-no-window . t))
            ("\\*compilation\\*" ; Compilation
             (display-buffer-in-side-window)
             (side . bottom)
             (slot . 1)
             (window-height . 0.5))))

  :diminish visual-line-mode)

(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-remote-files nil) ; Avoid overhead
  ;; Revert `dired' buffers if the current directory contents change. Dired
  ;; buffers do not auto-revert as a result of changes in subdirectories, or in
  ;; the contents, size, modes, etc., of files.
  (global-auto-revert-non-file-buffers t)
  (auto-revert-check-vc-info t)
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

(use-package recentf
  :ensure nil
  :hook
  (elpaca-after-init
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
  ;; list periodically after idling.
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

;; Allows viewing PDFs remotely through Tramp.
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

;; Binds "C-x C-f" to `find-file-at-point' which will continue to work like
;; `find-file' unless a prefix argument is given. Then it will find file at
;; point.
(use-package ffap
  :ensure nil
  :bind (("<f2>" . ffap) ("C-x p o" . ff-find-other-file))
  :custom
  ;; Do not ping things that look like domain names
  (ffap-machine-p-known 'reject))

;; Highlight and open http links in strings and comments in buffers.
(use-package goto-addr
  :ensure nil
  :hook ((prog-mode . goto-address-prog-mode) (text-mode . goto-address-mode))
  :bind ("C-c C-o" . goto-address-at-point))

(use-package subword
  :ensure nil
  :hook ((LaTeX-mode prog-mode conf-unix-mode) . subword-mode)
  :diminish)

;; ;; Use "Shift + direction" arrows for moving around windows.
;; (use-package windmove
;;   :ensure nil
;;   :when (display-graphic-p)
;;   :init (windmove-default-keybindings))

;; (use-package winner
;;   :hook (elpaca-after-init . winner-mode)
;;   :bind (("C-c <left>" . winner-undo) ("C-c <right>" . winner-redo)))

;; Use `ediff-regions-wordwise' for small regions and `ediff-regions-linewise'
;; for larger regions.
(use-package ediff
  :ensure nil
  :hook
  ( ;; Offer to clean up files from ediff sessions.
   (ediff-cleanup . (lambda () (ediff-janitor t nil)))
   (ediff-startup . ediff-next-difference))
  :custom
  ;; Put the control panel in the same frame as the diff windows
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split diffs side by side
  (ediff-split-window-function #'split-window-horizontally)
  ;; Prompt and kill file variants on quitting an Ediff session
  (ediff-keep-variants nil)
  :config
  (ediff-set-diff-options 'ediff-diff-options "-w")
  (with-eval-after-load 'winner
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
    (ignore-errors
      (tramp-cleanup-all-buffers)
      (tramp-cleanup-all-connections)))
  :ensure nil
  :custom
  ;; Remote files are not updated outside of Tramp
  (remote-file-name-inhibit-cache nil)
  (tramp-verbose 1 "Only errors and warnings")
  (tramp-default-method "ssh")
  (tramp-copy-size-limit (* 1024 1024)) ; 1MB
  :config
  (when (boundp 'tramp-use-connection-share)
    (setopt tramp-use-connection-share nil))
  ;; Disable backup
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil))
  ;; Include "$HOME/.local/bin" directory in $PATH on remote
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path t)
  (setopt debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))

  ;; (setenv "SHELL" shell-file-name) ; Recommended to connect with Bash

  ;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  ;; Newer versions of TRAMP will use SSH connection sharing for much faster
  ;; connections. These don’t require you to reenter your password each time you
  ;; connect. The compile command disables this feature, so we want to turn it
  ;; back on.
  (with-eval-after-load 'compile
    (remove-hook
     'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;; (use-package whitespace
;;   :ensure nil
;;   :custom (whitespace-line-column fill-column)
;;   :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode))

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
     "\\*texlab.*"))
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

;; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
(use-package vlf
  :commands vlf
  :init (setopt vlf-application 'dont-ask)
  :config (require 'vlf-setup))

(use-package immortal-scratch
  :hook (elpaca-after-init . immortal-scratch-mode))

(use-package persistent-scratch
  :hook
  (elpaca-after-init
   .
   (lambda ()
     (ignore-errors
       (persistent-scratch-setup-default))))
  :custom (persistent-scratch-what-to-save '(point narrowing))
  :config
  (advice-add
   'persistent-scratch-setup-default
   :around #'sb/inhibit-message-call-orig-fun))

;; `pixel-scroll-mode' uses line-by-line scrolling.
(use-package pixel-scroll
  :ensure nil
  :custom
  (pixel-scroll-precision-use-momentum nil)
  (pixel-scroll-precision-interpolate-page t)
  :config
  (cond
   ((fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
   ((fboundp 'pixel-scroll-mode)
    (pixel-scroll-mode 1))))

;; ;; Show temporary buffers as a popup window, and close them with "C-g"
;; (use-package popwin
;;   :hook (elpaca-after-init . popwin-mode)
;;   :config
;;   (push '(helpful-mode :noselect t :position bottom :height 0.5)
;;         popwin:special-display-config)
;;   ;; (push '(deadgrep-mode :noselect nil :position bottom :height 0.75)
;;   ;;         popwin:special-display-config)
;;   (push '(compilation-mode :noselect t :position bottom :height 0.5)
;;         popwin:special-display-config)
;;   (push '("\\*EGLOT workspace configuration\\*"
;;           :noselect nil
;;           :position bottom
;;           :height 0.5)
;;         popwin:special-display-config))

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

;; ;; Jump around buffers in few keystrokes
;; (use-package frog-jump-buffer
;;   :ensure (:host github :repo "waymondo/frog-jump-buffer")
;;   :bind ("C-b" . frog-jump-buffer)
;;   :config
;;   (dolist
;;       (regexp
;;        '("^TAGS$"
;;          "^:" ; temp buffers
;;          "errors\\*$" ; errors* buffer
;;          "stderr\\*$" ; stderr* buffer
;;          "-ls\\*$" ; -ls* buffer
;;          "^\\*\\(?:Compile-log\\|Backtrace\\|Flymake\\|vc\\|eldoc\\|Async-native-compile-log\\)\\*?$"))
;;     (add-to-list 'frog-jump-buffer-ignore-buffers regexp)))

(use-package dired
  :preface
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
    (dired-next-line -1))
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
  ;; (dired-omit-files
  ;;  (rx
  ;;   (or (seq bol "." (not (any "."))) ; dotfiles
  ;;       ".DS_Store"
  ;;       ".project"
  ;;       (optional "ile")
  ;;       ".svn"
  ;;       ".git"
  ;;       ".cache"
  ;;       ".ccls-cache"
  ;;       "__pycache__"
  ;;       "eln-cache"
  ;;       ".ctags.d"
  ;;       ".git"
  ;;       (seq (optional ".js") ".meta")
  ;;       (seq "." (or "elc" "o" "pyo" "swp" "class")))))
  :config
  (when (boundp 'dired-kill-when-opening-new-dired-buffer)
    (setopt dired-kill-when-opening-new-dired-buffer t))
  ;; Obsolete from Emacs 28+
  (unless (> emacs-major-version 27)
    (setopt dired-bind-jump t))

  ;; ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  ;; ;; Remove 'Omit' from the modeline.
  ;; (advice-add
  ;;  'dired-omit-startup
  ;;  :after (lambda () (diminish 'dired-omit-mode)))
  )

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;; (use-package dired-hist
;;   :ensure (:host github :repo "karthink/dired-hist")
;;   :hook (dired-mode . dired-hist-mode)
;;   :bind
;;   (:map
;;    dired-mode-map ("l" . dired-hist-go-back) ("r" . dired-hist-go-forward)))

;; In Emacs Lisp mode, `xref-find-definitions' will by default find only
;; functions and variables from Lisp packages which are loaded into the current
;; Emacs session or are auto-loaded.
(use-package xref
  :after consult
  :bind
  (("M-." . xref-find-definitions)
   ("M-," . xref-pop-marker-stack) ("M-?" . xref-find-references)
   ;; Find all identifiers whose name matches pattern
   ("C-M-." . xref-find-apropos))
  :custom
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;; Exclude project roots with `project-list-exclude'.
(use-package project
  :bind
  (("<f5>" . project-switch-project)
   ("<f6>" . project-find-file)
   ;; :map
   ;; project-prefix-map ; "C-x p"
   ;; ("f" . project-or-external-find-file)
   ;; ("b" . project-switch-to-buffer)
   ;; ("c" . project-compile)
   ;; ("k" . project-kill-buffers)
   ;; ("s" . project-find-regexp)
   ;; ("r" . project-query-replace-regexp)
   )
  :custom
  ;; Start `project-find-file' by default
  (project-switch-commands 'project-find-file)
  (project-vc-extra-root-markers '(".project" "pyproject.toml")))

;; ;; Allows identifying custom projects with a ".project" file (e.g., ~/Dropbox).
;; (use-package project-x
;;   :ensure (:host github :repo "karthink/project-x")
;;   :after project
;;   :demand t ; Required so that transient keybindings are available
;;   :config (add-hook 'project-find-functions #'project-x-try-local 90))

(use-package vertico
  :ensure
  (vertico
   :files (:defaults "extensions/*")
   :includes (vertico-directory vertico-repeat vertico-quick vertico-indexed))
  :hook
  ((elpaca-after-init . vertico-mode)
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
  :ensure (:host github :repo "ventruvian/vertico-timer")
  :after vertico
  :hook (vertico-mode . vertico-timer-mode)
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
  :preface
  (defun sb/consult-line-symbol-at-point ()
    (interactive)
    (consult-line (or (thing-at-point 'symbol) "")))
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
   ;; Prefix argument "C-u" allows to specify the directory. You can pass
   ;; additional grep flags to `consult-grep' with the "--" separator. E.g.:
   ;; "foo bar -- -A3" to get matches with 3 lines of 'after' context.
   ([remap rgrep] . consult-grep)
   ([remap vc-git-grep] . consult-git-grep)
   ("<f4>" . consult-line)
   ("M-g l" . sb/consult-line-symbol-at-point)
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
  (consult-buffer-filter sb/consult-buffer-filter)
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
  )

;; Easily add file and directory paths into the minibuffer.
(use-package consult-dir
  :commands consult-dir-jump-file
  :bind ("C-x C-d" . consult-dir)
  :config (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t))

;; (use-package consult-project-extra
;;   :after (project consult)
;;   :bind (:map project-prefix-map ("z" . consult-project-extra-find))
;;   :custom (consult-project-function #'consult-project-extra-project-fn)
;;   :config
;;   ;; (add-to-list 'project-switch-commands '(consult-project-extra-find "Find file" ?f))
;;   ;; (add-to-list 'project-switch-commands '(consult-project-buffer "Buffer"))
;;   (setq project-switch-commands 'consult-project-extra-find))

;; (use-package consult-jump-project
;;   :straight (:host github :repo "jdtsmith/consult-jump-project")
;;   :when (and (eq sb/minibuffer-completion 'vertico) (eq sb/project-handler 'project))
;;   :custom (consult-jump-direct-jump-modes '(dired-mode))
;;   :bind ("C-x p j" . consult-jump-project))

;; Use `consult' to select Tramp targets. Supported completion sources are ssh
;; config, known hosts, and docker containers.
(use-package consult-tramp
  :ensure (:host github :repo "Ladicle/consult-tramp")
  :commands consult-tramp)

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
   ("C-c C-e" . embark-export))
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
  ;; Reduce noise by removing annotations for files otherwise columns become
  ;; unaligned and look ugly.
  (setopt marginalia-annotators
          (cl-remove-if
           (lambda (pair) (memq (car pair) '(file project-file)))
           marginalia-annotators))

  (defun sb/marginalia-annotate-variable (cand)
    "Annotate function CAND with its documentation string."
    (when-let (sym
               (intern-soft cand))
      (marginalia--fields
       ((marginalia--variable-value sym) :truncate 0.5)
       ((documentation-property sym 'variable-documentation)
        :truncate 1.0
        :face 'marginalia-documentation))))

  ;; Override the annotators for the variable category.
  (add-to-list
   'marginalia-annotators
   '(variable sb/marginalia-annotate-variable builtin none)))

(use-package ispell
  :ensure nil
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
      ;; (setenv "LANG" "en_US")
      (setenv "DICTIONARY" "en_US")
      (setenv "DICPATH" (concat user-emacs-directory "hunspell"))
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
       ispell-hunspell-dict-paths-alist `(("en_US" ,(concat user-emacs-directory "hunspell/en_US.dic"))))))
   ((executable-find "aspell")
    (progn
      (setopt
       ispell-program-name "aspell"
       ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))))

  ;; Skip regions in `org-mode'
  (dolist
      (skip-pair
       '(("^#\\+BEGIN_SRC" . "^#\\+END_SRC")
         ("^#\\+BEGIN_EXAMPLE" . "^#\\+END_EXAMPLE")
         ("~" . "~")
         ("=" . "=")
         ("\\:PROPERTIES\\:$" . "\\:END\\:$")
         ;; Footnotes in org that have http links that are line breaked should not be ispelled
         ("^http" . "\\]")
         ("`" . "`")
         ("cite:" . "[[:space:]]")
         ("label:" . "[[:space:]]")
         ("ref:" . "[[:space:]]")
         ("\\\\begin{multline}" . "\\\\end{multline}")
         ("\\\\begin{equation}" . "\\\\end{equation}")
         ("\\\\begin{align}" . "\\\\end{align}")))
    (add-to-list 'ispell-skip-region-alist skip-pair))

  ;; Hide the "Starting new Ispell process" message
  (advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)
  (advice-add 'ispell-lookup-words :around #'sb/inhibit-message-call-orig-fun))

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; "M-$" triggers correction for the misspelled word before point, "C-u M-$"
;; triggers correction for the entire buffer, "C-u C-u M-$" forces correction of
;; the word at point, even if it is not misspelled.
(use-package jinx
  ;; Presence of the "enchant-2" executable does not imply that the header files
  ;; are present for compiling jinx.
  :preface
  (defun sb/libenchant-installed-p ()
    "Return t if libenchant-2-dev is installed, nil otherwise."
    (string=
     "INSTALLED"
     (string-trim
      (shell-command-to-string
       "dpkg -s libenchant-2-dev 2>/dev/null | grep -q '^Status: install' && echo INSTALLED || echo NOT-INSTALLED"))))
  :when
  (and (eq system-type 'gnu/linux)
       (executable-find "enchant-2")
       (sb/libenchant-installed-p))
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
  ((elpaca-after-init . global-hungry-delete-mode)
   (minibuffer-setup . (lambda () (hungry-delete-mode -1))))
  :diminish)

;; Move lines with "M-<up>" and "M-<down>"
(use-package move-text
  :bind (("M-<down>" . move-text-down) ("M-<up>" . move-text-up)))

;; Expand region increases the selected region by semantic units
(use-package expand-region
  :bind (("C-=" . er/expand-region) ("C-M-=" . er/contract-region)))

;; Change the contents inside pairs like parentheses, quotes, brackets, or
;; custom delimiters. `change-inner "' allows to kill the string contents,
;; `change-outer "' will kill the entire string including quotes.
(use-package change-inner
  :commands (change-inner change-outer))

;; Mark current line.
(use-package expand-line
  :bind ("M-i" . expand-line-mark-line)
  :diminish)

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  :hook (elpaca-after-init . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :hook (elpaca-after-init . whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode)

;; TODO: Choose one among dogears, better-jumper, and gumshoe.

(use-package dogears
  :ensure (:host github :repo "alphapapa/dogears.el")
  :hook ((prog-mode text-mode) . dogears-mode)
  :bind
  (("M-g d" . dogears-go)
   ("M-g r" . dogears-remember)
   ("M-g b" . dogears-back)
   ("M-g f" . dogears-forward)
   ("M-g t" . dogears-list))
  :custom
  (dogears-message nil)
  (dogears-idle 0.5)
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

;; Keeps track of the point position over time and allows us to navigate back
;; and forward in history.
(use-package gumshoe
  :ensure (:host github :repo "Overdr0ne/gumshoe")
  :hook
  (elpaca-after-init
   .
   (lambda ()
     (global-gumshoe-mode 1)
     (diminish 'global-gumshoe-mode)))
  :custom
  (gumshoe-show-footprints-p nil)
  (gumshoe-prefer-same-window t)
  (gumshoe-idle-time 2))

(use-package better-jumper
  :hook (elpaca-after-init . better-jumper-mode)
  :diminish better-jumper-local-mode)

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
  :ensure (:host github :repo "victorhge/iedit")
  :bind* ("C-." . iedit-mode))

;; Save a bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark,
;; use `bookmark-jump' ("C-x r b") or `bookmark-bmenu-list' ("C-x r l"). Rename
;; the bookmarked location in `bookmark-bmenu-mode' with `R'.
(use-package bm
  :init (setq bm-restore-repository-on-load t)
  :hook
  ((elpaca-after-init . bm-repository-load)
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
  (("C-<f9>" . crux-recentf-find-directory)
   ("C-<f11>" . crux-kill-other-buffers)
   ([remap keyboard-quit] . crux-keyboard-quit-dwim)
   ("C-c d i" . crux-ispell-word-then-abbrev))
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

;; Emacs theme files also have hex codes.
(use-package colorful-mode
  :hook
  ((LaTeX-mode
    css-mode
    css-ts-mode
    html-mode
    html-ts-mode
    web-mode
    help-mode
    helpful-mode
    emacs-lisp-mode)
   . colorful-mode)
  :diminish)

;; Parsing parentheses for `LaTeX-mode' is difficult.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Allow GC to happen after a period of idle time
(use-package gcmh
  :hook (elpaca-after-init . gcmh-mode)
  :diminish)

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
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
  ( ;; Starts an incremental search using the symbol under point as the initial
   ;; search string and searches forward by default unless `isearch-backward' was active.
   isearch-symbol-at-point
   ;; Will not match substrings, so foo will not match foobar.
   isearch-forward-symbol
   isearch-forward-symbol-at-point
   isearch-backward-symbol-at-point))

(with-eval-after-load 'grep
  (setopt
   grep-command "grep --color -irHn "
   grep-highlight-matches t
   grep-scroll-output t)

  (when (executable-find "rg")
    (setopt grep-program "rg")
    (grep-apply-setting 'grep-find-command '("rg -n -H --no-heading -e" . 27)))

  (dolist (dirs '(".cache" "node_modules" "vendor" ".clangd"))
    (add-to-list 'grep-find-ignored-directories dirs)))

;; `consult-rg' provides live search, while `deadgrep' provides a buffer with
;; the search results. Visit the result in another buffer with "o", move between
;; search hits with "n" and "p", and move between files with "M-n" and "M-p".
;; Change the search term with "S" and enable incremental search with "I".
(use-package deadgrep
  :when (executable-find "rg")
  :commands deadgrep-edit-mode
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

;; ;; Package `visual-regexp' provides an alternate version of `query-replace'
;; ;; which highlights matches and replacements as you type.
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
  :commands transient-define-prefix
  :demand t ; Required so that transient keybindings are available
  :custom (transient-semantic-coloring t)
  :config (transient-bind-q-to-quit))

(use-package with-editor :diminish)

(use-package cond-let
  :ensure (:host github :repo "tarsius/cond-let"))

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
  (when (bound-and-true-p sb/enable-icons)
    (setopt magit-format-file-function #'magit-format-file-nerd-icons))

  (with-eval-after-load 'magit-diff
    ;; Show fine differences for the current diff hunk only
    (setopt magit-diff-refine-hunk t)))

(use-package git-modes
  :mode ("dotgitconfig" . gitconfig-mode)
  :mode ("/\\.gitignore\\'" . gitignore-mode)
  :mode ("/\\.gitattributes\\'" . gitattributes-mode))

;; ;; Diff-hl looks nicer than git-gutter, but is based on `vc' and can be
;; ;; expensive.
;; (use-package diff-hl
;;   :preface
;;   (defun sb/diff-hl-maybe-margin ()
;;     "Fringe is unavailable in TTY. Enable margin mode for when in TTY."
;;     (unless (display-graphic-p)
;;       (diff-hl-margin-local-mode)))
;;   :hook
;;   ((elpaca-after-init . global-diff-hl-mode)
;;    (dired-mode . diff-hl-dired-mode-unless-remote)
;;    (diff-hl-mode . sb/diff-hl-maybe-margin)
;;    (after-save-hook . diff-hl-update))
;;   :bind (("C-x v [" . diff-hl-previous-hunk) ("C-x v ]" . diff-hl-next-hunk))
;;   :custom
;;   (diff-hl-draw-borders nil "Highlight without a border looks nicer")
;;   (diff-hl-update-async t)
;;   :config
;;   (diff-hl-flydiff-mode 1) ; For unsaved buffers
;;   (with-eval-after-load 'magit
;;     ;; (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
;;     (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; (use-package smerge-mode
;;   :ensure nil)

;; (use-package elec-pair
;;   :preface
;;   (defun sb/add-pairs (pairs)
;;     (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
;;     (setq-local electric-pair-text-pairs electric-pair-pairs))
;;   :ensure nil
;;   :hook (elpaca-after-init . electric-pair-mode)
;;   :custom
;;   ;; Avoid balancing parentheses since they can be both irritating and slow
;;   (electric-pair-preserve-balance nil)
;;   (electric-pair-skip-self nil)
;;   :config
;;   (setopt
;;    electric-pair-inhibit-predicate
;;    (lambda (char)
;;      (or
;;       ;; Inhibit for $ or " if point is after or before a word/symbol constituent
;;       (and (memq char '(?$ ?\"))
;;            (or (looking-back "\\(?:\\sw\\|\\s_\\)+" (line-beginning-position))
;;                (looking-at "\\(?:\\sw\\|\\s_\\)+")))
;;       ;; Fallback to default behavior
;;       (electric-pair-default-inhibit char)))))

;; "C-h m" or `describe-mode' shows all the active minor modes (and major mode)
;; and a brief description of each.

;; Discover key bindings for the current Emacs major mode
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

;; (use-package mode-minder
;;   :ensure (:host github :repo "jdtsmith/mode-minder")
;;   :commands mode-minder)

(use-package flycheck
  ;; Flycheck is disabled with `emacs-lisp-mode'
  :hook ((prog-mode text-mode) . flycheck-mode)
  :custom
  ;; Remove newline checks, since they would trigger an immediate check when we
  ;; want the `flycheck-idle-change-delay' to be in effect while editing.
  (flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change))
  ;; Eager popping of the *Flycheck error messages* buffer is irritating
  (flycheck-auto-display-errors-after-checking nil)
  (flycheck-checker-error-threshold nil)
  ;; Increase the time (s) to allow for quick transitions
  (flycheck-idle-buffer-switch-delay 2)
  ;; Increase the time (s) to allow for transient edits
  (flycheck-idle-change-delay 2)
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-global-modes '(not csv-mode conf-mode))
  ;; Left fringe does not work in the terminal mode. Disable indication because it is noisy.
  (flycheck-indication-mode nil)
  :config
  ;; Shellcheck is invoked by bash lsp
  (setopt flycheck-checkers
          (seq-difference
           flycheck-checkers
           '(proselint textlint tex-chktex emacs-lisp-checkdoc sh-shellcheck)))
  ;; Prefer linters packaged with pylsp
  (pcase sb/python-langserver
    ('pylsp
     (setopt flycheck-checkers
             (seq-difference
              flycheck-checkers
              '(python-flake8
                python-pylint
                python-mypy
                python-ruff
                python-pycompile
                python-pyright))))
    ('basedpyright
     (setopt flycheck-checkers
             (seq-difference
              flycheck-checkers
              '(python-flake8
                python-pylint python-mypy python-pycompile python-pyright)))))

  ;; These themes have their own styles for displaying flycheck info.
  (when (eq sb/modeline-theme 'doom-modeline)
    (setopt flycheck-mode-line nil))

  (setq-default
   flycheck-python-pylint-executable "python3"
   flycheck-shellcheck-follow-sources nil
   flycheck-shellcheck-excluded-warnings '("SC1091"))

  ;; https://github.com/flycheck/flycheck/issues/1833
  (add-to-list 'flycheck-hooks-alist '(after-revert-hook . flycheck-buffer))

  ;; Chain checkers with `Lsp-mode', using per-project directory local
  ;; variables.
  ;; https://github.com/flycheck/flycheck/issues/1762
  (defvar-local sb/flycheck-local-checkers nil)
  (defun sb/flycheck-checker-get (fn checker property)
    (or (alist-get property (alist-get checker sb/flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around 'sb/flycheck-checker-get)

  ;; Customize face for Flycheck lighter in the mode line.
  (when (or (eq sb/theme 'none) (eq sb/theme 'leuven-dark))
    (defface sb/flycheck-mode-line
      '((((class color) (background light))
         ;; Active window
         :inherit mode-line
         :foreground "#0474B6"
         :weight normal
         :height 1)
        (((class color) (background light))
         ;; Inactive window
         :inherit mode-line-inactive
         :foreground "#555555"
         :weight normal
         :height 1))
      "Custom face for Flycheck lighter in the mode line.")

    (setopt flycheck-mode-line
            '(:eval
              (propertize (flycheck-mode-line-status-text)
                          'face
                          'sb/flycheck-mode-line)))))

(use-package consult-flycheck
  :after flycheck
  :bind (:map flycheck-command-map ("!" . consult-flycheck)))

(use-package hl-todo
  :hook (elpaca-after-init . global-hl-todo-mode)

  ;; I use Flycheck integration (`previous-error' and `next-error') to navigate
  ;; among the highlighted lines.
  ;; :bind (("C-c p" . hl-todo-previous) ("C-c n" . hl-todo-next))

  :custom (hl-todo-highlight-punctuation ":")
  :config
  (setopt hl-todo-keyword-faces
          (append
           '(("LATER" . "#d0bf8f")
             ("IMP" . "#7cb8bb")
             ("TEST" . "tomato")
             ("WARNING" . "#cc0000"))
           hl-todo-keyword-faces)))

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
;;                   ("Shell" (shfmt "-i" "2" "-ci"))
;;                   ("XML" tidy)
;;                   ("YAML" prettier "--print-width" "80")))
;;   (with-eval-after-load 'markdown-mode
;;     (bind-key "C-x f" #'format-all-buffer markdown-mode-map))
;;   (with-eval-after-load 'tex
;;     (bind-key "C-x f" #'format-all-buffer LaTeX-mode-map))
;;   :diminish)

;; ;; We now use Bash LSP for formatting with `shfmt'.
;; (use-package shfmt
;;   :hook ((sh-mode bash-ts-mode) . shfmt-on-save-mode)
;;   :custom (shfmt-arguments '("-i" "2" "-ci"))
;;   :diminish shfmt-on-save-mode)

;; ;; Yapfify works on the original file, so that any project settings supported
;; ;; by YAPF itself are used. We now use `apheleia-mode' for Python.
;; (use-package yapfify
;;   :when (and (executable-find "yapf") (eq sb/python-langserver 'basedpyright))
;;   :hook ((python-mode python-ts-mode) . yapf-mode)
;;   :diminish yapf-mode)

;; ;; We use `apheleia-mode' for Python.
;; (use-package ruff-format
;;   :hook ((python-mode python-ts-mode) . ruff-format-on-save-mode)
;;   :diminish ruff-format-on-save-mode)

;; Basedpyright does not provide formatting feature. So, we cannot use
;; `lsp-format-buffer' or `eglot-format-buffer' with `basedpyright'.
(use-package apheleia
  :hook
  ((markdown-mode
    markdown-ts-mode python-mode python-ts-mode kdl-mode kdl-ts-mode)
   . apheleia-mode)
  :bind ("C-x f" . apheleia-format-buffer)
  :custom (apheleia-formatters-respect-fill-column t)
  :config
  (setf (alist-get 'prettier apheleia-formatters) '("prettier"))
  (setf (alist-get 'shfmt apheleia-formatters) '("shfmt" "-i" "2" "-ci"))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (when (executable-find "kdlfmt")
    (setf (alist-get 'kdlfmt apheleia-formatters)
          '("kdlfmt" "format" "--stdin"))
    (setf (alist-get 'kdl-mode apheleia-mode-alist) 'kdlfmt)
    (setf (alist-get 'kdl-ts-mode apheleia-mode-alist) 'kdlfmt))
  :diminish apheleia-mode)

;; Auto-format Elisp code
(use-package elisp-autofmt
  :hook ((emacs-lisp-mode lisp-data-mode) . elisp-autofmt-mode)
  :custom
  (elisp-autofmt-python-bin "python3")
  (elisp-autofmt-on-save-p 'always))

;; Provides indentation guide bars with optional `tree-sitter' support
(use-package indent-bars
  :hook
  ((bash-ts-mode
    c-mode
    c-ts-mode
    c++-mode
    c++-ts-mode
    java-mode
    java-ts-mode
    python-mode
    python-ts-mode
    yaml-mode
    yaml-ts-mode
    sh-mode)
   . indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
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

;; Use "M-SPC" for space-separated completion lookups.
(use-package orderless
  :demand t
  ;; TODO: What is the utility of this? Is this to highlight the different parts with orderless?
  ;; :config
  ;; (with-eval-after-load 'company
  ;;   (defun sb/just-one-face (fn &rest args)
  ;;     (let ((orderless-match-faces [completions-common-part]))
  ;;       (apply fn args)))
  ;;   (advice-add 'company-capf--candidates :around #'sb/just-one-face))
  )

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
  :custom
  (enable-recursive-minibuffers t "Tracking the depth can be confusing")
  (completion-ignore-case t)
  ;; Ignore case when reading a file name
  (read-file-name-completion-ignore-case t)
  ;; Ignore case when reading a buffer name
  (read-buffer-completion-ignore-case t)
  (completion-styles '(basic orderless))
  (completion-category-defaults nil)
  ;; The "basic" completion style needs to be tried first for TRAMP hostname
  ;; completion to work. I also want substring matching for file names.
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  ;; Show docstring description for completion candidates in commands like
  ;; `describe-function'.
  (when (boundp 'completions-detailed)
    (setopt completions-detailed t))
  (when (fboundp 'dabbrev-capf)
    (add-to-list 'completion-at-point-functions 'dabbrev-capf t))

  (defun sb/decrease-minibuffer-font ()
    "Decrease minibuffer font size."
    (setq-local face-remapping-alist '((default :height 0.95))))
  (add-hook 'minibuffer-setup-hook #'sb/decrease-minibuffer-font)

  ;; Do not open the *Messages* buffer when clicking in the Echo area.
  (unbind-key [mouse-1] minibuffer-inactive-mode-map))

;; ;; `hotfuzz' allows fuzzy matching and is faster than `flex' (built-in) for large candidate sets.
;; (use-package hotfuzz
;;   :demand t
;;   :custom completion-styles '(hotfuzz orderless))

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

(use-package nerd-icons
  :when (bound-and-true-p sb/enable-icons)
  :demand t
  :custom (nerd-icons-scale-factor 0.8))

;; `kind-icon' can be used for both Corfu and Company. I use `kind-icon' to
;; provide Nerd icons for Company while I use `nerd-icons-corfu' to set up nerd
;; icons for Corfu.
(use-package kind-icon
  :when
  (and (bound-and-true-p sb/enable-icons)
       (or (eq sb/in-buffer-completion 'company)
           (eq sb/corfu-icons 'kind-icon)))
  :after nerd-icons
  :demand t ; Required to load the library because there are no other triggers
  :config
  (add-to-list
   'svg-lib-icon-collections
   '("vscode-codicons"
     .
     "https://github.com/microsoft/vscode-codicons/raw/HEAD/src/icons/%s.svg"))
  (add-to-list
   'svg-lib-icon-collections
   '("nerd-fonts-codicons"
     .
     "https://github.com/microsoft/vscode-codicons/raw/HEAD/src/icons/%s.svg"))

  (with-eval-after-load 'corfu
    (setopt
     kind-icon-default-face 'corfu-default
     ;; Corfu icons are too big, prefer smaller icons and a more compact popup
     kind-icon-default-style
     '(:padding
       0
       :stroke 0
       :margin 0
       :radius 0
       :height 0.5
       :scale 0.8
       :background nil))
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; Company may be loaded even with Corfu for use with LaTeX. That is why we do
  ;; not use `with-eval-after-load'.
  (when (eq sb/in-buffer-completion 'company)
    ;; Prefer smaller icons and a more compact popup
    (setopt
     kind-icon-default-style
     '(:padding
       0
       :stroke 0
       :margin 0
       :radius 0
       :height 0.8
       :scale 0.6
       :background nil)
     kind-icon-mapping
     `((array
        ,(nerd-icons-codicon "nf-cod-symbol_array")
        :face font-lock-type-face)
       (boolean
        ,(nerd-icons-codicon "nf-cod-symbol_boolean")
        :face font-lock-builtin-face)
       (class
        ,(nerd-icons-codicon "nf-cod-symbol_class")
        :face font-lock-type-face)
       (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
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
       (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
       (interface
        ,(nerd-icons-codicon "nf-cod-symbol_interface")
        :face font-lock-type-face)
       (keyword
        ,(nerd-icons-codicon "nf-cod-symbol_keyword")
        :face font-lock-keyword-face)
       (macro
        ,(nerd-icons-codicon "nf-cod-symbol_misc")
        :face font-lock-keyword-face)
       (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
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
       (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
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
       (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
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
        ,(nerd-icons-codicon "nf-cod-text_size")
        :face font-lock-builtin-face)))

    (let* ((kind-func (lambda (cand) (company-call-backend 'kind cand)))
           (formatter
            (kind-icon-margin-formatter `((company-kind . ,kind-func)))))
      (defun sb/company-kind-icon-margin (cand _selected)
        (funcall formatter cand))
      (setopt company-format-margin-function #'sb/company-kind-icon-margin))))

;; Use "M-x company-diag" or the modeline status without diminish to see the
;; backend used for the last completion.
(use-package company
  :when (eq sb/in-buffer-completion 'company)
  :hook (elpaca-after-init . global-company-mode)
  :bind
  (:map
   company-active-map
   ("C-;" . company-other-backend) ; Invoke the next backend
   ("C-s" . company-search-candidates)
   ("C-f" . company-filter-candidates)

   ;; When using graphical Emacs, you need to bind both (kbd "<tab>") and (kbd
   ;; "TAB"). First TAB key press will complete the common part of all
   ;; completions, and the next will switch to the next completion in a cyclic
   ;; fashion, meaning that if you reach the end you continue from the top.
   ;; S-TAB will go in reverse direction.
   ;; ("<tab>" . company-complete-common-or-cycle)
   ;; ("TAB" . company-complete-common-or-cycle)
   ;; ("<backtab>" .
   ;;  (lambda ()
   ;;    (interactive)
   ;;    (company-complete-common-or-cycle -1)))
   ;; ("S-TAB" .
   ;;  (lambda ()
   ;;    (interactive)
   ;;    (company-complete-common-or-cycle -1)))

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

  (company-dabbrev-downcase nil "Do not downcase returned candidates")

  ;; ;; Avoid slowdown in case there are lot of buffers open
  ;; (company-dabbrev-code-other-buffers nil)

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

  ;; (company-require-match nil)
  ;; (company-insertion-triggers '())

  (company-tooltip-align-annotations t)
  (company-tooltip-width-grow-only t) ; Avoid shrinking the company popup

  ;; Allow orderless-like behavior with Company, i.e., search candidates with space-separated regexp
  ;; https://github.com/company-mode/company-mode/discussions/1211
  (company-search-regexp-function 'company-search-words-in-any-order-regexp)
  :config
  (unless (bound-and-true-p sb/enable-icons)
    (setopt company-format-margin-function nil))

  (setopt
   company-transformers
   '(delete-dups
     ;; Ignore matches from `company-dabbrev' that consist solely of numbers
     ;; https://github.com/company-mode/company-mode/issues/358
     (lambda (candidates)
       (cl-remove-if
        (lambda (c) (string-match-p "\\`[0-9]+\\'" c)) candidates))))

  ;; Disable code candidates in comments, otherwise text completions are not offered with Eglot.
  ;; https://github.com/company-mode/company-mode/discussions/1498
  (when (eq sb/lsp-provider 'eglot)
    (defun sb/company-capf-around (orig-fun &rest args)
      "Custom advice for `company-capf--prefix' to restrict completions in comments."
      (let ((syntax-info (syntax-ppss)))
        (if (nth 4 syntax-info)
            nil
          (apply orig-fun args))))
    (advice-add 'company-capf--prefix :around #'sb/company-capf-around))

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

  ;; (custom-set-faces
  ;;  ;; '(company-tooltip ((t (:inherit default))))
  ;;  ;; '(company-tooltip-selection ((t (:inherit default))))
  ;;  ;; '(company-tooltip-common ((t (:inherit default))))
  ;;  ;; '(company-tooltip-common-selection ((t (:inherit default))))
  ;;  ;; '(company-preview ((t (:inherit default))))
  ;;  '(company-preview-common ((t (:inherit default)))))

  :diminish)

;; Posframes do not have unaligned rendering issues with variable `:height'
;; unlike an overlay. However, posframes do not work with TUI. The width of the
;; popup frame is may not be sufficient sometimes, and the right side may get
;; cut off.
;; https://github.com/company-mode/company-mode/issues/1010
(use-package company-posframe
  :when (display-graphic-p)
  :hook (company-mode . company-posframe-mode)
  :custom
  ;; Difficult to distinguish the help text from completions
  (company-posframe-show-metadata nil)
  ;; The backend display in the posframe modeline gets cut for complicated backend setups and looks ugly.
  (company-posframe-show-indicator nil)
  (company-posframe-quickhelp-delay nil "Disable showing the help frame")
  :config
  (when (eq sb/theme 'leuven-dark)
    (set-face-attribute 'company-posframe-active-backend-name nil
                        :height 0.8
                        :foreground "#4FC3F7" ;; light sky blue
                        :background "#2E3440" ;; dark gray-blue
                        :weight 'normal
                        :box nil)
    (set-face-attribute 'company-posframe-inactive-backend-name nil
                        :height 0.7
                        :foreground "#B0BEC5" ;; soft gray
                        :background "#1C1C1C" ;; near black
                        :box nil))
  :diminish)

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

;; ;; I am currently using Prescient for ordering Company, Vertico, and Corfu completions.
;; (use-package company-statistics
;;   :when (eq sb/in-buffer-completion 'company)
;;   :after company
;;   :init (company-statistics-mode 1))

;; By default, the Unicode symbols backend `company-math-symbols-unicode' is not
;; active in latex math environments and latex math symbols
;; `company-math-symbols-latex' is not available outside of math latex
;; environments.
(use-package company-math
  :after company
  :demand t)

(use-package company-dict
  :after company
  :demand t
  :custom
  (company-dict-dir (expand-file-name "company-dict" user-emacs-directory))
  (company-dict-enable-yasnippet nil))

;; Use "<" to trigger company completion of org blocks.
(use-package company-org-block
  :after company
  :hook
  (org-mode
   .
   (lambda ()
     (require 'company-org-block)
     (setq-local company-backends
                 '(company-files
                   (company-org-block :separate company-dabbrev-code)
                   (company-dict company-ispell)
                   company-dabbrev)))))

;; Enables completion of C/C++ header file names
(use-package company-c-headers
  :after (company cc-mode)
  :config
  ;; Prefer GCC 13 if it is present
  (cond
   ((sb/directory-exists-p "/usr/include/c++/13")
    (add-to-list 'company-c-headers-path-system "/usr/include/c++/13"))
   ((sb/directory-exists-p "/usr/include/c++/11")
    (add-to-list 'company-c-headers-path-system "/usr/include/c++/11"))))

;; ;; Incompatible with the tags file generated by Citre, works if the tags file is generated independently with "ctags -R .".
;; (use-package company-ctags
;;   :after company)

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

;; (use-package bibtex-completion
;;   :after tex
;;   :commands bibtex-completion-insert-citation)

;; ;; I cannot get this to work correctly.
;; (use-package company-bibtex
;;   :after tex
;;   :commands company-bibtex)

(use-package company-try-hard
  :bind
  (("C-j" . company-try-hard)
   :map
   company-active-map
   ("C-j" . company-try-hard)))

;; We should enable `company-fuzzy-mode' at the very end of configuring
;; `company'. Nice feature but slows completions.

;; (use-package company-fuzzy
;;   :after company
;;   :custom
;;   ;; Using "flx" slows down completion significantly
;;   (company-fuzzy-sorting-backend 'alphabetic)
;;   ;; LATER: The right-hand side may get cut off if the annotations are
;;   ;; right-aligned. Disable this after the mode is set up properly.
;;   (company-fuzzy-show-annotation t)
;;   ;; We should not need this with "flx" sorting because the "flx" sorting
;;   ;; accounts for the prefix. Disabling the requirement may help with
;;   ;; performance.
;;   (company-fuzzy-prefix-on-top t)
;;   (company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@" "::" ":"))
;;   (company-fuzzy-reset-selection t)
;;   ;; Ignore backends that are already fuzzy
;;   (company-fuzzy-passthrough-backends
;;    '(company-capf
;;      company-yasnippet
;;      company-dabbrev
;;      company-dabbrev-code
;;      company-files
;;      company-ispell))
;;   :diminish)

;; Notes on configuring `company-backends'.

;; Most backends (e.g., `company-yasnippet') will not pass control to subsequent
;; backends. `company-yasnippet' is blocking. Only a few backends are
;; specialized on certain major modes or certain contexts (e.g., outside of
;; strings and comments), and pass control to later backends when outside of
;; that major mode or context.

;; A few mode-agnostic backends are applicable to all modes:
;; `company-yasnippet', `company-ispell', `company-dabbrev-code', and
;; `company-dabbrev'.

;; The ‘prefix’ bool command always returns non-nil for following backends even
;; when their candidates list command is empty: `company-abbrev',
;; `company-dabbrev', `company-dabbrev-code'.

;; `company-dabbrev' returns a non-nil prefix in almost any context (major mode,
;; inside strings, or comments). That is why it is better to put
;; `company-dabbrev' at the end.

;; The keyword ":with" helps to make sure the results from major/minor mode
;; agnostic backends (such as `company-yasnippet', `company-dabbrev-code') are
;; returned without preventing results from context-aware backends (such as
;; `company-capf' or `company-clang'). For this feature to work, put backends
;; dependent on a mode at the beginning of the grouped backends list, then put a
;; keyword ":with", and then put context-agnostic backend(s).

;; Company does not support grouping of entirely arbitrary backends, they need
;; to be compatible in what `prefix' returns. If the group contains keyword
;; `:with', the backends listed after the keyword are ignored for the purpose of
;; the `prefix' command. If the group contains keyword `:separate', the
;; candidates that come from different backends are sorted separately in the
;; combined list. That is, with `:separate', the multi-backend-adapter will stop
;; sorting and keep the order of completions just like the backends returned
;; them.

;; Try completion backends in order until there is a non-empty completion list:

;; (setopt company-backends '(company-xxx company-yyy company-zzz))

;; Merge completions of all the backends:

;; (setopt company-backends '((company-xxx company-yyy company-zzz)))

;; Both the backends will generate completions at the same time, and their
;; results will be merged. `company-yasnippet' will be queried even if
;; `company-capf' returns nil. Company treats the result as coming from a single
;; backend.

;; (setopt company-backends '((company-capf :with company-yasnippet)))

;; Merge completions of all the backends but keep the candidates organized in
;; accordance with the grouped backends order.

;; (setopt company-backends '((company-xxx company-yyy company-zzz :separate)))

(with-eval-after-load 'company
  ;; Override `company-backends' for unhandled major modes.
  ;; (setopt company-backends
  ;;         '(company-files
  ;;           (company-capf
  ;;            ;; `company-keywords' should not be required with LS support.
  ;;            company-keywords company-dabbrev-code
  ;;            :with company-yasnippet)
  ;;           (company-dict company-ispell :with company-yasnippet)
  ;;           ;; If we have `company-dabbrev' first, then other matches from later
  ;;           ;; backends `company-ispell' or `company-dict' will be ignored.
  ;;           (company-dabbrev :with company-yasnippet)))

  (setopt
   company-backends
   '(company-files
     ;; `company-capf' may not return all variable or type definitions, so we also use  `company-dabbrev-code'. `company-yasnippet' is blocking.
     (company-capf
      :separate
      company-dabbrev-code
      company-keywords
      :with company-yasnippet)
     (company-dict company-ispell) company-dabbrev))

  ;; `company-capf' with Texlab does not pass to later backends even if it does
  ;; not return any result. So it makes it difficult to complete non-LaTeX
  ;; commands (e.g., words) which is the majority. By combining it in a single
  ;; group with ":separate", the following code forces all listed backends to be
  ;; queried regardless of what `company-capf' returns.

  (defun sb/company-latex-backends-no-separate ()
    (setq-local
     company-backends
     '(company-files ; Have files first to allow completing paths
       (company-capf :with company-yasnippet)
       company-reftex-citations ; will trigger inside \cite{}
       ;; Will trigger inside forms like \ref{}, \eqref{}, \auroref{}, etc.
       (company-reftex-labels
        ;; LaTeX structure
        company-auctex-labels)
       company-auctex-macros
       company-auctex-environments
       ;; company-latex-commands ; company-auctex-macros seem to be better
       company-auctex-symbols
       company-math-symbols-latex ; Math latex tags
       ;; Math Unicode symbols and sub (super) scripts
       company-math-symbols-unicode
       company-ispell
       company-dict
       company-dabbrev)))

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

  (defun sb/company-latex-backends-fuzzy ()
    "Company backends for use with `company-fuzzy-mode'."
    (setq-local company-backends
                '(company-files
                  ;; References & labels
                  company-reftex-citations
                  company-reftex-labels
                  company-auctex-labels
                  ;; LaTeX macros/envs/snippets
                  company-auctex-macros
                  company-auctex-environments
                  company-auctex-symbols
                  ;; Math symbols
                  company-math-symbols-latex
                  company-math-symbols-unicode
                  company-ispell
                  company-dict
                  company-dabbrev
                  company-yasnippet)))

  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     ;; Allow showing yasnippets auto-complete which often use two letters
     (setq-local company-minimum-prefix-length 2)
     (sb/company-latex-backends-separate)
     ;; (company-fuzzy-mode 1)
     ))

  (defun sb/company-text-mode ()
    "Add backends for `text-mode' completion in company mode."
    ;; Another way to make `company-backends' local.
    (set
     (make-local-variable 'company-backends)
     '(company-files (company-dict company-ispell) company-dabbrev)))

  ;; Extends to derived modes like `markdown-mode'. We use separate hooks for `org-mode' and `LaTeX-mode'.
  (add-hook
   'text-mode-hook
   (lambda ()
     (unless (or (derived-mode-p 'LaTeX-mode) (derived-mode-p 'org-mode))
       (sb/company-text-mode)
       ;; (company-fuzzy-mode 1)
       )))

  (defun sb/company-c-mode ()
    (setq-local
     company-backends
     '(company-files

       ;; `company-capf' may not return all variable or type definitions, so we merge `company-dabbrev-code'.
       ;; (:separate company-capf company-dabbrev-code :with company-yasnippet)

       (company-capf :with company-yasnippet)
       (company-dabbrev-code :with company-yasnippet)
       company-c-headers
       (company-dict company-ispell :with company-yasnippet)
       (company-dabbrev :with company-yasnippet))))

  (defun sb/company-c-mode-new ()
    (setq-local
     company-backends
     '(company-files
       ;; `company-capf' may not return all variable or type definitions, so we merge `company-dabbrev-code'. `company-yasnippet' is blocking.
       (company-capf
        :separate
        company-dabbrev-code
        company-c-headers
        company-keywords
        :with company-yasnippet)
       (company-dict company-ispell) company-dabbrev)))

  (dolist (hook '(c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook))
    (add-hook
     hook
     (lambda ()
       (setq-local company-minimum-prefix-length 2)
       (sb/company-c-mode-new))))

  ;; TODO: It may be possible to merge `sb/company-non-lsp-prog-mode' and `sb/company-elisp-mode'.

  ;; We override `company-backends' for LSP-managed major modes.
  (defun sb/company-non-lsp-prog-mode ()
    (setq-local
     company-backends
     '(company-files
       ;; `company-capf' may not return all variable or type definitions, so we also use  `company-dabbrev-code'.
       (company-capf
        :separate
        company-dabbrev-code
        company-keywords
        :with company-yasnippet)
       (company-ispell company-dict :with company-yasnippet)
       (company-dabbrev :with company-yasnippet))))

  (defun sb/company-non-lsp-prog-mode-new ()
    (setq-local
     company-backends
     '(company-files
       ;; `company-capf' may not return all variable or type definitions, so we also use  `company-dabbrev-code'. `company-yasnippet' is blocking.
       (company-capf
        :separate
        company-dabbrev-code
        company-keywords
        :with company-yasnippet)
       (company-ispell company-dict) company-dabbrev)))

  (add-hook
   'prog-mode-hook
   (lambda ()
     (unless (or (derived-mode-p 'emacs-lisp-mode)
                 (derived-mode-p 'lisp-data-mode)
                 (derived-mode-p 'flex-mode)
                 (derived-mode-p 'bison-mode)
                 (derived-mode-p 'cmake-ts-mode))
       (setq-local company-minimum-prefix-length 2)
       (sb/company-non-lsp-prog-mode-new))))

  ;; `company-capf' is not complete for Elisp. For example, it will not suggest `doom-modeline' but suggests `doom-modeline-mode'. So I need to merge `company-capf' with `company-dabbrev-code'.
  (defun sb/company-elisp-mode ()
    "Add backends for `emacs-lisp-mode' completion in company mode."
    (setq-local
     company-backends
     '(company-files
       (:separate
        company-capf
        company-dabbrev-code ; Useful for local (e.g., variable) names
        :with company-yasnippet)
       (company-ispell company-dict :with company-yasnippet)
       (company-dabbrev :with company-yasnippet))))

  (defun sb/company-elisp-mode-new ()
    "Add backends for `emacs-lisp-mode' completion in company mode."
    (setq-local
     company-backends
     '(company-files
       ;; `company-capf' may not return all variable or type definitions, so we also use `company-dabbrev-code'. `company-yasnippet' is blocking.
       (company-capf
        :separate
        company-dabbrev-code
        company-keywords
        :with company-yasnippet)
       (company-ispell company-dict) company-dabbrev)))

  (dolist (hook '(emacs-lisp-mode-hook lisp-data-mode-hook))
    (add-hook
     hook
     (lambda ()
       (setq-local company-minimum-prefix-length 2)
       (sb/company-elisp-mode)))))

;; Corfu is not a completion framework, it is a front-end for
;; `completion-at-point'.
(use-package corfu
  :preface
  (defun sb/corfu-prog-setup ()
    "Use shorter prefix for Corfu in `prog-mode'."
    (setq-local corfu-auto-prefix 2))
  :ensure
  (corfu
   :files (:defaults "extensions/*")
   :includes
   (corfu-info
    corfu-history corfu-echo corfu-popupinfo corfu-indexed corfu-quick))
  :when (eq sb/in-buffer-completion 'corfu)
  :hook
  ((elpaca-after-init . global-corfu-mode)
   ;; We want 2-character prefixes in LaTeX-mode for yasnippets
   ((prog-mode LaTeX-mode) . sb/corfu-prog-setup))
  :bind
  (:map
   corfu-map
   ("TAB" . corfu-quick-insert)
   ("<tab>" . corfu-quick-insert)
   ("M-d" . corfu-info-documentation)
   ("M-l" . corfu-info-location)
   ("M-n" . corfu-popupinfo-scroll-up)
   ("M-p" . corfu-popupinfo-scroll-down)
   ([remap corfu-show-documentation] . corfu-popupinfo-toggle)
   ("[escape]" . corfu-quit))
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  ;; (corfu-on-exact-match 'show)

  ;; ;; Do not close popup when adjacent to other characters
  ;; (corfu-quit-at-boundary nil)

  :config
  ;; I prefer `corfu-quick' compared to `corfu-indexed-mode' because pressing TAB is easier.
  (corfu-history-mode 1)
  (corfu-echo-mode 1)
  (corfu-popupinfo-mode 1)

  ;; Add space at the right edge so characters do not get cut off in the terminal interface.
  (unless (display-graphic-p)
    (setopt corfu-right-margin-width 1.5)))

;; Emacs 31+ has in-built support for child frames in the terminal
(use-package corfu-terminal
  :ensure (:host codeberg :repo "akib/emacs-corfu-terminal")
  :when
  (and (eq sb/in-buffer-completion 'corfu)
       (not (display-graphic-p))
       (< emacs-major-version 31))
  :hook (corfu-mode . corfu-terminal-mode)
  :custom
  ;; Prevent wraparound at the right edge, although this breaks sometimes.
  (corfu-terminal-position-right-margin 2))

(use-package yasnippet-capf
  :ensure (:host github :repo "elken/yasnippet-capf")
  :when (eq sb/in-buffer-completion 'corfu)
  :after yasnippet
  :demand t)

(use-package cape
  :after corfu
  :demand t
  :custom
  (cape-dict-file
   `(,(expand-file-name "wordlist.5" sb/extras-directory)
     ,(expand-file-name "company-dict/text-mode" user-emacs-directory)))
  :config
  ;; `cape-capf-super' works for static completion functions like
  ;; `cape-dabbrev', `cape-keyword', and `cape-dict', but not for multi-step
  ;; completions like `cape-file'. File completion with `cape-file' is available
  ;; in comments and string literals, but not in normal code.

  ;; There is no mechanism to force deduplication if candidates from `cape-dict'
  ;; and `cape-dabbrev' are the same (equal string and equal text
  ;; properties) because of different metadata.
  ;; https://github.com/minad/cape/discussions/130

  ;; We do not merge `cape-dict' and `cape-dabbrev' because there will be
  ;; duplicates and we expect `cape-dict' to mostly suffice.
  (add-hook
   'text-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  (cape-capf-inside-string #'cape-file)
                  #'cape-dict
                  (cape-capf-buster #'cape-dabbrev)))))

  (dolist (hook '(markdown-mode-hook bibtex-mode-hook))
    (add-hook
     hook
     (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    (cape-capf-inside-string #'cape-file)
                    #'yasnippet-capf
                    #'cape-dict
                    (cape-capf-buster #'cape-dabbrev))))))

  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  (cape-capf-inside-string #'cape-file)
                  #'yasnippet-capf
                  #'cape-elisp-block
                  #'cape-dict
                  (cape-capf-buster #'cape-dabbrev)))))

  ;; For generic non-LSP-managed programming modes
  (add-hook
   'prog-mode-hook
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  #'yasnippet-capf
                  (cape-capf-inside-code
                   (cape-capf-super #'cape-keyword #'cape-dabbrev))
                  (cape-capf-inside-comment #'cape-dict)
                  (cape-capf-inside-string #'cape-file)
                  (cape-capf-buster #'cape-dabbrev)))))

  (add-hook
   'LaTeX-mode-hook
   (lambda ()
     (setq-local
      completion-at-point-functions
      (list
       (cape-capf-inside-string #'cape-file)
       #'yasnippet-capf
       #'citar-capf
       (cape-capf-super
        (cape-company-to-capf #'company-reftex-labels)
        (cape-company-to-capf #'company-auctex-labels))
       (cape-capf-super
        (cape-company-to-capf #'company-latex-commands)
        (cape-company-to-capf #'company-auctex-environments)
        (cape-company-to-capf #'company-auctex-macros)
        ;; Math Unicode symbols and sub(super)scripts
        (cape-company-to-capf #'company-auctex-symbols)
        ;; `cape-tex' is used for Unicode symbols and not for the corresponding LaTeX names.
        #'cape-tex)
       ;; Math latex tags
       (cape-company-to-capf #'company-math-symbols-latex)
       (cape-company-to-capf #'company-math-symbols-unicode)
       #'cape-dict
       (cape-capf-buster #'cape-dabbrev)))))

  (dolist (mode '(emacs-lisp-mode-hook lisp-data-mode-hook))
    (add-hook
     mode
     (lambda ()
       (setq-local completion-at-point-functions
                   (list
                    (cape-capf-inside-string #'cape-file)
                    #'yasnippet-capf
                    (cape-capf-inside-code
                     (cape-capf-nonexclusive #'elisp-completion-at-point))
                    (cape-capf-inside-code #'cape-elisp-symbol)
                    (cape-capf-inside-comment #'cape-dict)
                    (cape-capf-buster #'cape-dabbrev)))))))

;; Vertico does its own sorting based on recency, Corfu has `corfu-history', and
;; Company has `company-statistics'. Prescient uses frecency (frequency +
;; recency) for sorting. Recently used commands should be sorted first. Only
;; commands that have never been used before will be sorted by length.
(use-package prescient
  :ensure (:host github :repo "radian-software/prescient.el" :files (:defaults "/*.el"))
  :hook (elpaca-after-init . prescient-persist-mode)
  :custom (prescient-sort-full-matches-first t)
  :config
  (with-eval-after-load 'corfu
    (corfu-prescient-mode 1))
  (with-eval-after-load 'vertico
    (vertico-prescient-mode 1))
  (with-eval-after-load 'company
    (company-prescient-mode 1)))

(defun sb/setup-lsp-provider ()
  "Set up LSP based on `sb/lsp-provider`.
Uses `eglot` or `lsp-mode` depending on configuration."
  (cond
   ((eq sb/lsp-provider 'eglot)
    (eglot-ensure))
   ((eq sb/lsp-provider 'lsp-mode)
    (lsp-deferred))))

(use-package lsp-mode
  :when (eq sb/lsp-provider 'lsp-mode)
  :bind (:map lsp-command-map ("g"))
  :custom (lsp-use-plists t)
  ;; I mostly SSH into the remote machine and launch Emacs, rather than using
  ;; Tramp which is slower and more error-prone.
  (lsp-auto-register-remote-clients nil)
  (lsp-keep-workspace-alive nil)
  (lsp-progress-via-spinner nil)
  (lsp-progress-function 'ignore)
  ;; Increase the delay to allow transient changes and avoid getting flooded with LSP errors.
  (lsp-idle-delay 1.5)
  ;; Avoid annoying questions, we expect a server restart to succeed
  (lsp-restart 'auto-restart)
  ;; Avoid warning messages for unsupported modes like `csv-mode'
  (lsp-warn-no-matched-clients nil)
  ;; (lsp-enable-suggest-server-download nil)
  (lsp-enable-dap-auto-configure nil "I do not use dap-mode")
  (lsp-format-buffer-on-save nil "Enable per-project")
  (lsp-format-buffer-on-save-list
   '(bash-ts-mode
     c-mode
     c-ts-mode
     c++-mode
     c++-ts-mode
     cmake-mode
     cmake-ts-mode
     java-mode
     java-ts-mode
     json-mode
     json-ts-mode
     jsonc-mode
     python-mode
     python-ts-mode
     sh-mode
     yaml-mode
     yaml-ts-mode))
  (lsp-enable-on-type-formatting nil "Reduce unexpected modifications to code")
  (lsp-enable-folding nil "I do not find the feature useful")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-file-watchers nil "Avoid watcher warnings and slowdown")
  (lsp-lens-enable nil "Lenses are distracting")
  ;; Disable highlighting references of the symbol at point. I use
  ;; `symbol-overlay' to include languages that do not have a language server.
  (lsp-enable-symbol-highlighting nil)
  (lsp-semantic-tokens-enable nil)
  ;; I do not use mouse with TUI, so showing code actions is not useful.
  (lsp-modeline-code-actions-enable nil)
  ;; We already show Flycheck status on the modeline
  (lsp-modeline-diagnostics-enable nil)
  ;; Simpler to focus on the errors at hand
  (lsp-modeline-diagnostics-scope :file)
  ;; The workspace status icon on the terminal interface persists across
  ;; projects and hence is misleading.
  (lsp-modeline-workspace-status-enable nil)
  (lsp-inlay-hint-enable nil)
  (lsp-enable-snippet nil)
  ;; Enable integration of custom backends other than `capf'
  (lsp-completion-provider :none)
  ;; Show/hide completion metadata, e.g., "java.util.ArrayList". This increases
  ;; the width of the completion popup.
  (lsp-completion-show-detail nil)
  ;; Show/hide completion kind, e.g., interface/class.
  (lsp-completion-show-kind t)
  (lsp-completion-default-behaviour :insert)
  (lsp-imenu-sort-methods '(position) "More natural way of listing symbols")
  (lsp-eldoc-enable-hover nil "Do not show noisy hover info with mouse")
  ;; Sudden changes in the height of the echo area causes the cursor to lose
  ;; position, manually request via `lsp-signature-activate'.
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation t)
  ;; Client-specific configuration
  (lsp-clangd-version "19.1.2")
  (lsp-clients-clangd-args
   '("-j=4"
     "--all-scopes-completion"
     "--background-index"
     ;; Unsupported option with Clangd 14
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
  (lsp-pylsp-configuration-sources ["pyproject.toml" "setup.cfg"])
  (lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-pylsp-plugins-preload-enabled nil)
  (lsp-pylsp-plugins-preload-modules [])
  (lsp-pylsp-plugins-pydocstyle-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-convention "pep257")
  (lsp-pylsp-plugins-pylint-enabled t)
  (lsp-pylsp-plugins-yapf-enabled nil "Using Apheleia")
  (lsp-pylsp-plugins-flake8-enabled nil)
  (lsp-pylsp-plugins-isort-enabled t)
  ;; Delay checking for types till the changes are saved
  (lsp-pylsp-plugins-mypy-live-mode nil)
  (lsp-pylsp-plugins-ruff-enabled t)
  (lsp-pylsp-plugins-ruff-config "pyproject.toml")
  (lsp-pylsp-plugins-ruff-format nil "Using Apheleia")
  (lsp-pylsp-plugins-ruff-line-length 80)
  (lsp-pylsp-plugins-ruff-target-version "py310")
  (lsp-semgrep-metrics-enabled nil)
  :config
  (cond
   ((eq sb/python-langserver 'pylsp)
    (setopt lsp-pylsp-plugins-mypy-enabled t))
   ((eq sb/python-langserver 'basedpyright)
    (setopt lsp-pylsp-plugins-mypy-enabled nil)))
  (when (display-graphic-p)
    (setopt lsp-modeline-code-actions-segments '(count icon name)))

  ;; Enable `lsp-booster'
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

  (defun sb/lsp-mode-capf ()
    ;; Prescient does not work well with certain dynamic completion tables that use a prefix string to produce candidates before filtering.
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(prescient basic)))
  (add-hook 'lsp-completion-mode-hook #'sb/lsp-mode-capf)

  ;; Clean completion metadata with `cape-capf-buster'. Make the capf composable
  ;; allowing falling back to other backends with `cape-capf-nonexclusive'.
  ;; Ensure that completion does not get interrupted by the user pressing keys
  ;; or other operations with `cape-wrap-noninterruptible'.
  (with-eval-after-load 'lsp-mode
    (advice-add
     #'lsp-completion-at-point
     :around
     (lambda (orig-fun &rest args)
       (apply (cape-capf-buster (cape-capf-nonexclusive orig-fun)) args))))

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
  (lsp-ui-sideline-show-hover nil)
  ;; Hide diagnostics when typing because they can be intrusive, Flycheck and
  ;; Flymake already highlight errors
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-peek-enable nil))

(use-package consult-lsp
  :after (consult lsp-mode)
  :when (eq sb/lsp-provider 'lsp-mode)
  :commands (consult-lsp-symbols consult-lsp-file-symbols consult-lsp-diagnostics))

(use-package lsp-java
  :when (eq sb/lsp-provider 'lsp-mode)
  :hook
  ((java-mode java-ts-mode)
   .
   (lambda ()
     (setq-local c-basic-offset 4)
     (c-set-style "java")
     (lsp-deferred)))
  :custom
  (lsp-java-progress-reports-enabled nil)
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-format-settings-url
   (concat
    "file://"
    (file-truename (locate-user-emacs-file "servers/eclipse-formatter.xml"))))
  ;; Provide my own installation which Eglot can also use
  (lsp-java-server-install-dir
   (file-truename
    (locate-user-emacs-file
     "servers/eclipse.jdt.ls-1.50.0/org.eclipse.jdt.ls.product/target/repository/"))))

(use-package lsp-ltex-plus
  :ensure (:host github :repo "emacs-languagetool/lsp-ltex-plus")
  :when (eq sb/lsp-provider 'lsp-mode)
  :init (setopt lsp-ltex-plus-version "18.5.1")
  :hook
  ((text-mode markdown-mode org-mode LaTeX-mode)
   .
   (lambda ()
     ;; Disable LSP for git commit message buffers which are usually ephemeral
     (unless (string-equal
              (file-name-nondirectory (or buffer-file-name ""))
              "COMMIT_EDITMSG")
       (require 'lsp-ltex-plus)
       (lsp-deferred))))
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
      "EN_DIACRITICS_REPLACE"])))

;; Use a per-project "pyrightconfig.json" file for configuring the language
;; server.
(use-package lsp-pyright
  :when
  (and (eq sb/lsp-provider 'lsp-mode)
       (eq sb/python-langserver 'basedpyright)
       (executable-find "basedpyright"))
  :commands (lsp-pyright-locate-python lsp-pyright-locate-venv)
  :hook
  ((python-mode python-ts-mode)
   .
   (lambda ()
     (require 'lsp-pyright)
     (lsp-deferred)))
  :custom
  (lsp-pyright-langserver-command "basedpyright")
  (lsp-pyright-python-executable-cmd "python3"))

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
  (compilation-scroll-output t)
  (compilation-auto-jump-to-first-error t)
  (compilation-max-output-line-length nil)
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
  ;; Frequent eldoc messages are irritating.
  ;; :hook ((emacs-lisp-mode lisp-data-mode) . global-eldoc-mode)
  :custom
  (eldoc-area-prefer-doc-buffer t "Disable popups")
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :config
  ;; Allow Eldoc to trigger after completions
  (with-eval-after-load 'company
    (eldoc-add-command
     'company-complete-selection
     'company-complete-common
     'company-capf
     'company-abort))
  :diminish)

;; (use-package eldoc-box
;;   :when (display-graphic-p)
;;   ;; :hook (eldoc-mode . eldoc-box-hover-at-point-mode)
;;   :custom (eldoc-box-clear-with-C-g t)
;;   ;; Reduce the width to cover a lesser portion of the screen
;;   (eldoc-box-max-pixel-height 400)
;;   :config (eldoc-box-mouse-mode -1)
;;   :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode))

;; Tree-sitter provides advanced syntax highlighting features. Run
;; `tree-sitter-langs-install-grammar' to install the grammar files for
;; languages for tree-sitter. Many treesitter modes are derived from their based
;; modes since Emacs 30. For example, `(derived-mode-p 'c-mode)' will return t
;; in `c-ts-mode'. That means `.dir-locals.el' settings and yasnippets for
;; `c-mode' will work for `c-ts-mode' too. However, `c-ts-mode' still does not
;; run c-mode's major mode hooks. Also, there's still no major mode fallback.

;; (use-package treesit
;;   :ensure nil
;;   :when
;;   (and (executable-find "tree-sitter")
;;        (fboundp 'treesit-available-p)
;;        (treesit-available-p))
;;   :demand t
;;   :commands (treesit-install-language-grammar)
;;   :bind (("C-M-<up>" . treesit-up-list) ("C-M-<down>" . treesit-down-list))
;;   :custom
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

(use-package treesit-auto
  :when
  (and (executable-find "tree-sitter")
       (fboundp 'treesit-available-p)
       (treesit-available-p))
  :demand t
  :bind (("C-M-<up>" . treesit-up-list) ("C-M-<down>" . treesit-down-list))
  :custom
  ;; Increased default font locking may hurt performance
  (treesit-font-lock-level 4)
  (treesit-auto-install t)
  :config
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; (with-eval-after-load 'c++-ts-mode
;;   (bind-key "C-M-a" #'treesit-beginning-of-defun c++-ts-mode-map)
;;   (bind-key "C-M-e" #'treesit-end-of-defun c++-ts-mode-map))

;; (with-eval-after-load 'treesit
;;   ;; Improves performance with large files without significantly diminishing
;;   ;; highlight quality
;;   (setq font-lock-maximum-decoration '((c-mode . 2) (c++-mode . 2) (t . t))))

;; Some systems may not have treesitter mode enabled.
(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c-or-c++-mode)
  :hook
  ((awk-mode
    .
    (lambda ()
      (setq-local c-basic-offset 4)
      (c-set-style "awk")
      (sb/setup-lsp-provider)))
   ((c-mode c++-mode)
    .
    (lambda ()
      (setq-local c-basic-offset 2)
      (c-set-style "linux")
      (sb/setup-lsp-provider)))))

(use-package c-ts-mode
  :ensure nil
  :when
  (and (executable-find "tree-sitter")
       (fboundp 'treesit-available-p)
       (treesit-available-p))
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
     (sb/setup-lsp-provider)))
  :bind
  (:map
   c++-ts-mode-map
   ("C-M-a" . treesit-beginning-of-defun)
   ("C-M-e" . treesit-end-of-defun)))

;; Some systems may not have treesitter mode enabled.
(if (and (executable-find "tree-sitter")
         (fboundp 'treesit-available-p)
         (treesit-available-p))
    (use-package cuda-mode
      :mode ("\\.cu\\'" . c++-ts-mode)
      :mode ("\\.cuh\\'" . c++-ts-mode))
  (use-package cuda-mode
    :mode ("\\.cu\\'" . c++-mode)
    :mode ("\\.cuh\\'" . c++-mode)))

(use-package opencl-c-mode
  :mode "\\.cl\\'")

;; I have disabled treesitter support for CMake because it seems unstable. For
;; example, I get "treesit-font-lock-fontify-region: Query pattern is malformed"
;; on search with `consult-line'.
;; https://github.com/regen100/cmake-language-server/pull/103/files
(use-package cmake-mode
  :when (executable-find "cmake")
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
  :hook
  ((cmake-mode cmake-ts-mode)
   .
   (lambda ()
     ;; `cmake-mode' is derived from `text-mode', so disable grammar and spell
     ;; checking.
     (jinx-mode -1)
     (when (eq sb/lsp-provider 'lsp-mode)
       (setq-local lsp-disabled-clients '(ltex-ls-plus)))
     (sb/setup-lsp-provider))))

(use-package doxymacs
  :ensure (:host github :repo "pniedzielski/doxymacs")
  :hook (c-mode-common-hook . doxymacs-mode)
  :bind
  (:map
   c-mode-base-map
   ;; Lookup documentation for the symbol at point.
   ("C-c d ?" . doxymacs-lookup)
   ;; Rescan your Doxygen tags file.
   ("C-c d r" . doxymacs-rescan-tags)
   ;; Prompt you for a Doxygen command to enter, and its
   ;; arguments.
   ("C-c d RET" . doxymacs-insert-command)
   ;; Insert a Doxygen comment for the next function.
   ("C-c d f" . doxymacs-insert-function-comment)
   ;; Insert a Doxygen comment for the current file.
   ("C-c d i" . doxymacs-insert-file-comment)
   ;; Insert a Doxygen comment for the current member.
   ("C-c d ;" . doxymacs-insert-member-comment)
   ;; Insert a blank multi-line Doxygen comment.
   ("C-c d m" . doxymacs-insert-blank-multiline-comment)
   ;; Insert a blank single-line Doxygen comment.
   ("C-c d s" . doxymacs-insert-blank-singleline-comment)
   ;; Insert a grouping comments around the current region.
   ("C-c d @" . doxymacs-insert-grouping-comments))
  ;; :custom
  ;;   ;; Configure source code <-> Doxygen tag file <-> Doxygen HTML
  ;;   ;; documentation mapping:
  ;;   ;;   - Files in /home/me/project/foo/ have their tag file at
  ;;   ;;     http://someplace.com/doc/foo/foo.xml, and HTML documentation
  ;;   ;;     at http://someplace.com/doc/foo/.
  ;;   ;;   - Files in /home/me/project/bar/ have their tag file at
  ;;   ;;     ~/project/bar/doc/bar.xml, and HTML documentation at
  ;;   ;;     file:///home/me/project/bar/doc/.
  ;;   ;; This must be configured for Doxymacs to function!
  ;;   (doxymacs-doxygen-dirs
  ;;    '(("^/home/me/project/foo/"
  ;;       "http://someplace.com/doc/foo/foo.xml"
  ;;        "http://someplace.com/doc/foo/")
  ;;      ("^/home/me/project/bar/"
  ;;       "~/project/bar/doc/bar.xml"
  ;;       "file:///home/me/project/bar/doc/")))
  )

(use-package highlight-doxygen
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode cuda-mode) . highlight-doxygen-mode))

(use-package python
  :ensure nil
  :mode
  (("SCon\\(struct\\|script\\)\\'" . python-ts-mode)
   ("[./]flake8\\'" . conf-mode)
   ("/Pipfile\\'" . conf-mode))
  :hook
  ((python-mode python-ts-mode)
   .
   (lambda ()
     (setq-local tab-width 4)
     (sb/setup-lsp-provider)))
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
     (sb/setup-lsp-provider)))
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
     ;; (when (eq sb/lsp-provider 'eglot)
     ;;   (eglot-ensure))
     ;; ;; `lsp-mode' does not support fish yet
     ;; (when (eq sb/lsp-provider 'lsp-mode)
     ;;   (with-eval-after-load 'lsp-mode
     ;;     (lsp-register-client
     ;;      (make-lsp-client
     ;;       :new-connection
     ;;       (lsp-stdio-connection '("fish-lsp" "start"))
     ;;       :activation-fn (lsp-activate-on "fish")
     ;;       :server-id 'fish-lsp))
     ;;     (lsp-deferred)))
     )))

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

;; (use-package semel
;;   :ensure (:host github :repo "eshelyaron/semel")
;;   :hook ((emacs-lisp-mode lisp-data-mode) . semel-mode))

(use-package ini-mode
  :commands ini-mode)

(use-package conf-mode
  :ensure nil
  :mode
  "\\.cfg\\'"
  "\\.conf\\'"
  :hook
  (conf-unix-mode
   .
   (lambda ()
     (setq-local completion-at-point-functions
                 (list
                  (cape-capf-inside-string #'cape-file)
                  #'cape-dict
                  (cape-capf-buster #'cape-dabbrev))))))

(use-package toml-ts-mode
  :ensure nil
  :mode ("\\.toml\\'" "Cargo\\.lock\\'")
  :hook (toml-ts-mode . sb/setup-lsp-provider))

(use-package yaml-mode
  :mode
  (("\\.ya?ml\\'" . yaml-ts-mode)
   ("\\.clang-format\\'" . yaml-ts-mode)
   ("\\.clang-tidy\\'" . yaml-ts-mode)
   ("\\.clangd\\'" . yaml-ts-mode))
  :hook
  ((yaml-mode yaml-ts-mode)
   .
   (lambda ()
     ;; `yaml-mode' is derived from `text-mode', so disable grammar and spell
     ;; checking.
     (jinx-mode -1)
     (when (eq sb/lsp-provider 'lsp-mode)
       (setq-local lsp-disabled-clients '(ltex-ls-plus)))
     (sb/setup-lsp-provider))))

(use-package yaml-imenu
  :hook ((yaml-mode yaml-ts-mode) . yaml-imenu-enable))

(use-package web-mode
  :mode "\\.html?\\'"
  :hook (web-mode . sb/setup-lsp-provider)
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
  :hook ((css-mode css-ts-mode) . sb/setup-lsp-provider)
  :custom (css-indent-offset 2))

(use-package autoconf
  :ensure nil
  :hook (autoconf-mode . sb/setup-lsp-provider))

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
     (sb/setup-lsp-provider))))

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

;; More shortcuts: https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :preface
  (defun sb/markdown-setup ()
    ;; Auto-pair backticks
    (with-eval-after-load 'elec-pair
      (sb/add-pairs '((?` . ?`))))
    ;; We prefer to use "harper-ls" or "ltex-ls-plus" for checking grammar with Eglot.
    (when (eq sb/lsp-provider 'lsp-mode)
      (require 'lsp-marksman))
    (sb/setup-lsp-provider))
  :mode ("README\\.md\\'" . gfm-mode)
  :hook (markdown-mode . sb/markdown-setup)
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

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf. Convert
;; `markdown' to `org': "pandoc -f markdown -t org -o output-file.org
;; input-file.md".
(use-package pandoc-mode
  :hook ((markdown-mode markdown-ts-mode) . pandoc-mode)
  :config (pandoc-load-default-settings)
  :diminish)

(use-package nxml-mode
  :preface
  (defun sb/nxml-setup ()
    ;; `xml-mode' is derived from `text-mode', so disable grammar and spell
    ;; checking.
    (jinx-mode -1)
    (when (eq sb/lsp-provider 'lsp-mode)
      (setq-local lsp-disabled-clients '(ltex-ls-plus)))
    (sb/setup-lsp-provider))
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
    (sb/setup-lsp-provider))
  :mode
  (("pyrightconfig\\.json\\'" . jsonc-mode)
   (".*/vscode/settings.json\\'" . jsonc-mode)
   (".*/\\.vscode/settings.json\\'" . jsonc-mode)
   ("User/settings\\.json\\'" . jsonc-mode)
   ("\\.htmlhintrc\\'" . json-mode)
   ("\\.json\\'" . json-ts-mode))
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

(use-package org
  :defer 2
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
   ("M-{" . org-backward-element)
   ("M-}" . org-forward-element)
   ("C-c C-," . org-insert-structure-template)
   ("C-c C-j" . consult-outline)
   ("C-c C-l" . org-store-link)
   ("C-c C-k" . org-insert-link))
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
  (org-latex-pdf-process
   '("latexmk -pdflatex='-shell-escape -interaction nonstopmode -output-directory %o' -pdf -bibtex -f %f"))
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

(use-package org-modern
  :hook (org-mode . global-org-modern-mode))

;; ;; Useful if `org-indent-mode' is enabled
;; (use-package org-modern-indent
;;   :ensure (:host github :repo "jdtsmith/org-modern-indent")
;;   :hook (org-mode . org-modern-indent-mode))

;; ;; Use "<" to trigger org block completion at point.
;; (use-package org-block-capf
;;   :ensure (:host github :repo "xenodium/org-block-capf")
;;   :after corfu
;;   :hook (org-mode . org-block-capf-add-to-completion-at-point-functions)
;;   :custom (org-block-capf-edit-style 'inline))

;; Without auctex
(with-eval-after-load 'tex-mode
  (setopt tex-command "pdflatex"))

;; ;; The LSP setup seems to work very poorly with large LaTeX files, leading to
;; ;; frequent hangs while communicating with Emacs. Furthermore, this package
;; ;; is not required for completions with `company-mode'.

;; (use-package lsp-latex
;;   :when (and (eq sb/lsp-provider 'lsp-mode) (executable-find "texlab"))
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
;;   (with-eval-after-load 'latex
;;     (bind-key "C-c C-c" #'lsp-latex-build LaTeX-mode-map)))

;; Auctex provides enhanced versions of `tex-mode' and `latex-mode', which
;; automatically replace the vanilla ones. Auctex provides `LaTeX-mode', which
;; is an alias to `latex-mode'. Auctex overrides the tex package. "P" in the
;; modeline highlighter "LaTeX/MPS" is due to `TeX-PDF-mode'.
(use-package latex
  :ensure
  (auctex
   :repo "https://git.savannah.gnu.org/git/auctex.git"
   :branch "main"
   :pre-build (("make" "elpa"))
   :build (:not elpaca--compile-info) ;; Make will take care of this step
   :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
   :version
   (lambda (_)
     (require 'auctex)
     AUCTeX-version))
  :init (require 'tex-site)
  :hook
  (LaTeX-mode
   .
   (lambda ()
     (LaTeX-math-mode 1)
     (TeX-PDF-mode) ; Use `pdflatex'
     (turn-on-reftex)

     ;; Revert buffer visiting PDF file (e.g., "PDF Tools") after TeX compilation has finished.
     ;; (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)

     ;; Enable rainbow mode after applying styles to the buffer
     ;; (TeX-update-style . rainbow-delimiters-mode)

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
    (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))

  ;; (with-eval-after-load 'elec-pair
  ;; (defvar sb/latex-pairs '((?\{ . ?\}) (?\[ . ?\]) (?\( . ?\))))
  ;; (add-hook
  ;;  'LaTeX-mode-hook
  ;;  (lambda () (sb/add-pairs '((?\{ . ?\}) (?\[ . ?\]) (?\( . ?\)))))))
  )

;; (use-package tex
;;   :ensure nil
;;   :config
;;   (when (executable-find "okular")
;;     (add-to-list
;;      'TeX-view-program-list
;;      '("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
;;     (add-to-list 'TeX-view-program-selection '(output-pdf "Okular"))))

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

;; Enabling LSP for large bibtex files incurs large overhead
(use-package bibtex
  :ensure nil
  :hook
  (bibtex-mode
   .
   (lambda ()
     (when (eq sb/lsp-provider 'lsp-mode)
       (setq-local lsp-disabled-clients '(ltex-ls-plus)))))
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-maintain-sorted-entries t)
  (bibtex-comma-after-last-field nil))

(use-package consult-reftex
  :ensure (:host github :repo "karthink/consult-reftex")
  :after (consult reftex)
  :commands (consult-reftex-insert-reference consult-reftex-goto-label))

;; (use-package math-delimiters
;;   :ensure (:host github :repo "oantolin/math-delimiters")
;;   :demand t
;;   :commands (math-delimiters-no-dollars math-delimiters-toggle)
;;   :bind
;;   (:map
;;    TeX-mode-map ("$" . math-delimiters-insert)
;;    :map LaTeX-mode-map ("$" . math-delimiters-insert)))

;; ;; Set `bibtex-capf-bibliography' in `.dir-locals.el'.
;; (use-package bibtex-capf
;;   :ensure (:host github :repo "mclear-tools/bibtex-capf")
;;   :when (eq sb/in-buffer-completion 'corfu)
;;   :after latex
;;   :commands bibtex-capf)

(use-package citar
  :when (eq sb/in-buffer-completion 'corfu)
  :after (tex cape)
  :custom
  ;; Remove support for `org-mode' and `markdown-mode'
  (citar-major-mode-functions
   '((LaTeX-mode
      .
      ((local-bib-files . citar-latex-local-bib-files)
       (insert-citation . citar-latex-insert-citation)
       (insert-edit . citar-latex-insert-edit)
       (key-at-point . citar-latex-key-at-point)
       (citation-at-point . citar-latex-citation-at-point)
       (list-keys . citar-latex-list-keys)))
     (t . ((insert-keys . citar--insert-keys-comma-space-separated)))))
  ;; Only show key and title in Corfu popup
  (citar-format-reference-function 'citar-citeproc-format-reference))

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

;; ;; Insert an environment with "C-c {". For a full list of environment abbreviations, use `C-c ?'.
;; (use-package cdlatex
;;   :hook ((LaTeX-mode latex-mode) . turn-on-cdlatex)
;;   :diminish)

;; (use-package citre
;;   :preface
;;   (defun sb/jump-citre-xref ()
;;     "Jump to the definition of the symbol at point using `citre-jump' first. Falls back to `xref-find-definitions' on failure."
;;     (interactive)
;;     (condition-case _
;;         (citre-jump)
;;       (error
;;        (let* ((xref-prompt-for-identifier nil))
;;          (call-interactively #'xref-find-definitions)))))

;;   (defun sb/jump-xref-citre ()
;;     "Jump to the definition of the symbol at point using `xref-find-definitions' first. Falls back to `citre-jump' on failure."
;;     (interactive)
;;     (let ((ofn
;;            (lambda ()
;;              (let* ((xref-prompt-for-identifier nil))
;;                (call-interactively #'xref-find-definitions)))))
;;       (condition-case _
;;           (citre-jump)
;;         (error
;;          (funcall ofn)))))

;;   (defun sb/jump-back-citre-xref ()
;;     "Go back to the position before last `citre-jump'.
;; Fallback to `xref-go-back'."
;;     (interactive)
;;     (condition-case _
;;         (citre-jump-back)
;;       (error
;;        (if (fboundp #'xref-go-back)
;;            (call-interactively #'xref-go-back)
;;          (call-interactively #'xref-pop-marker-stack)))))
;;   :hook ((prog-mode LaTeX-mode) . citre-mode)
;;   :bind* (("M-." . sb/jump-xref-citre) ("M-," . sb/jump-back-citre-xref))
;;   :custom (citre-default-create-tags-file-location 'in-dir)
;;   ;; Add exclude by: --exclude=target or by --exclude=@./.ctagsignore
;;   ;; Add dirs/files to scan here, one line per dir/file
;;   (citre-ctags-default-options
;;    (string-join
;;     '("-o %TAGSFILE%"
;;       "--languages=BibTeX,C,C++,CUDA,CMake,EmacsLisp,Java,Make,Python,Sh,TeX"
;;       "--kinds-all=*"
;;       "--fields=*"
;;       "--extras=*"
;;       "--recurse")
;;     " "))
;;   ;; Add Elisp to the backend lists.
;;   (citre-find-definition-backends '(elisp eglot tags global))
;;   (citre-find-reference-backends '(elisp eglot global))
;;   :config
;;   (setq-default
;;    citre-enable-imenu-integration nil ; Conflicts with Elisp imenu entries
;;    ;; Large tags file slows down completion
;;    citre-enable-capf-integration nil)

;;   ;; Use `citre' with Emacs Lisp
;;   (defvar citre-elisp-backend
;;     (citre-xref-backend-to-citre-backend
;;      ;; This is the xref backend name
;;      'elisp
;;      ;; A function to tell if the backend is usable
;;      (lambda () (derived-mode-p 'emacs-lisp-mode))))
;;   ;; Register the backend, which means to bind it with the symbol `elisp'.
;;   (citre-register-backend 'elisp citre-elisp-backend)

;;   ;; Integrate with `lsp-mode' and `eglot'
;;   (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
;;     (let ((fetcher (apply -fn -args))
;;           (citre-fetcher
;;            (let ((xref-backend-functions '(citre-xref-backend t)))
;;              (apply -fn -args))))
;;       (lambda ()
;;         (or (with-demoted-errors "%s, fallback to citre"
;;               (funcall fetcher))
;;             (funcall citre-fetcher)))))

;;   (defun sb/push-point-to-xref-marker-stack (&rest r)
;;     (xref-push-marker-stack (point-marker)))

;;   (dolist (func
;;            '(xref-find-definitions
;;              xref-find-references
;;              find-function
;;              consult-imenu
;;              project-grep
;;              deadgrep
;;              counsel-rg
;;              consult-lsp-file-symbols
;;              citre-jump))
;;     (advice-add func :before 'sb/push-point-to-xref-marker-stack))
;;   :diminish)

(use-package leuven-theme
  :when (eq sb/theme 'leuven-dark)
  :init (load-theme 'leuven-dark t))

(use-package doom-themes
  :when (eq sb/theme 'doom-nord)
  :init (load-theme 'doom-nord t)
  :config
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-themes
  :when (eq sb/theme 'modus-vivendi)
  :init (load-theme 'modus-vivendi t)
  :custom
  (modus-themes-mixed-fonts nil)
  (modus-themes-completions
   '((matches . (extrabold underline))
     (selection . (semibold accented text-also))
     (popup . (accented intense)))))

(use-package catppuccin-theme
  :when (eq sb/theme 'catppuccin)
  :init (load-theme 'catppuccin t)
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

(use-package autothemer)

(use-package rose-pine-theme
  :ensure (:host github :repo "konrad1977/pinerose-emacs")
  :after autothemer
  :when (eq sb/theme 'rose-pine)
  :init (load-theme 'rose-pine t))

;; (use-package rose-pine-emacs
;;   :ensure (:host github :repo "thongpv87/rose-pine-emacs"
;;                  :files ("*.el")
;;    :includes (rose-pine-moon-theme))
;;   :after autothemer
;;   :when (eq sb/theme 'rose-pine)
;;   :init (load-theme 'rose-pine-moon-theme t))

(use-package kanagawa-themes
  :when (eq sb/theme 'kanagawa)
  :init (load-theme 'kanagawa-wave t)
  :config
  ;; The default colors are difficult to distinguish
  (with-eval-after-load 'company
    (custom-set-faces
     ;; Normal tooltip
     '(company-tooltip ((t (:background "#1e1e1e" :foreground "#dcdcdc"))))
     ;; Selected item
     '(company-tooltip-selection
       ((t (:background "#264f78" :foreground "#ffffff"))))
     ;; Match highlight
     '(company-tooltip-common ((t (:foreground "#c586c0" :weight bold))))
     ;; Scrollbar background
     '(company-scrollbar-bg ((t (:background "#333333"))))
     ;; Scrollbar foreground
     '(company-scrollbar-fg ((t (:background "#555555")))))))

(use-package nerd-icons-corfu
  :ensure (:host github :repo "LuigiPiucco/nerd-icons-corfu")
  :when
  (and (bound-and-true-p sb/enable-icons)
       (eq sb/in-buffer-completion 'corfu)
       (eq sb/corfu-icons 'nerd-icons))
  :after corfu
  :demand t
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Icons in the minibuffer
(use-package nerd-icons-completion
  :ensure (:host github :repo "rainstormstudio/nerd-icons-completion")
  :when (bound-and-true-p sb/enable-icons)
  :after (nerd-icons marginalia)
  :init (nerd-icons-completion-mode 1)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :ensure (:host github :repo "rainstormstudio/nerd-icons-dired")
  :when (bound-and-true-p sb/enable-icons)
  :hook (dired-mode . nerd-icons-dired-mode)
  :diminish)

(use-package nerd-icons-ibuffer
  :when (bound-and-true-p sb/enable-icons)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom (nerd-icons-ibuffer-icon-size 1.0))

;; (use-package nerd-icons-grep
;;   :ensure (:host github :repo "hron/nerd-icons-grep")
;;   :when (bound-and-true-p sb/enable-icons)
;;   :after grep
;;   :init (nerd-icons-grep-mode)
;;   :custom (grep-use-headings t))

;; Powerline theme for Nano looks great, and takes less space on the modeline.
;; But the package is not being actively maintained. Inspired by
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

  ;; Flycheck segment with distinct colors for errors (red) and warnings (yellow)
  (defun sb/flycheck-status ()
    "Return Flycheck status with red for errors and yellow for warnings.
Shows both colors when errors and warnings are present."
    (when (bound-and-true-p flycheck-mode)
      (let-alist
       (flycheck-count-errors flycheck-current-errors)
       (cond
        ;; Both errors and warnings
        ((and .error .warning)
         (concat
          (propertize (format "E:%s" .error) 'face '(:foreground "red"))
          " "
          (propertize (format "W:%s" .warning) 'face '(:foreground "gold"))))
        ;; Only errors
        (.error
         (propertize (format "E:%s" .error) 'face '(:foreground "red")))
        ;; Only warnings
        (.warning
         (propertize (format "W:%s" .warning) 'face '(:foreground "gold")))
        ;; Clean: show nothing
        (t
         (propertize "✔ " 'face '(:foreground "green")))))))

  ;; https://github.com/dgellow/config/blob/master/emacs.d/modules/01-style.el
  (defun sb/powerline-nano-theme ()
    "Setup a nano-like modeline"
    (interactive)
    (setq-default
     mode-line-format
     '("%e" (:eval
        (let*
            ((active (powerline-selected-window-active))
             (face0
              (if active
                  'powerline-active0
                'powerline-inactive0))
             (emacs-icon
              (if (fboundp 'nerd-icons-devicon)
                  (nerd-icons-devicon
                   "nf-dev-emacs"
                   :height 1.1
                   :v-adjust -0.05)
                "λ"))

             ;; Left-hand side (GNU Emacs version)

             ;; (lhs
             ;;  (list
             ;;   (powerline-raw
             ;;    (concat
             ;;     "GNU Emacs "
             ;;     (number-to-string emacs-major-version)
             ;;     "."
             ;;     (number-to-string emacs-minor-version))
             ;;    nil 'l)))

             (lhs
              (list
               (powerline-raw
                (format "%s GNU Emacs %d.%d"
                        emacs-icon emacs-major-version emacs-minor-version)
                nil 'l)))

             ;; Center (buffer name)
             (center (list (powerline-raw "%b" nil 'r)))

             ;; Right-hand side (function, VCS, Flycheck, line/col, modified flag)
             (rhs
              (list
               (when which-function-mode
                 (powerline-raw which-func-format nil 'l))

               (when-let*
                   ((proj (project-current))
                    (proj-name (project-name proj))
                    (branch
                     (when vc-mode
                       (let* ((backend
                               (substring-no-properties vc-mode)))
                         (replace-regexp-in-string
                          "^ *[Gg]it[:-]" "" backend))))
                    ;; Git branch icon (fallback if Nerd Icons not available)
                    (git-icon
                     (if (fboundp 'nerd-icons-octicon)
                         (nerd-icons-octicon
                          "nf-oct-git_branch"
                          :v-adjust -0.05)
                       "")))
                 (powerline-raw
                  (format " [%s  %s %s]"
                          proj-name
                          git-icon
                          (or branch "—"))
                  nil 'l))

               ;; (when-let ((proj (project-current)))
               ;;   (powerline-raw (format "[%s" (project-name proj)) nil 'l))

               ;; Remove the "Git:" prefix to save space.
               ;; (powerline-vc nil 'l)

               ;; (when vc-mode
               ;;   (let* ((backend
               ;;           (substring-no-properties vc-mode))
               ;;          (branch
               ;;           (replace-regexp-in-string "^ *[Gg]it[:-]" "" backend)))
               ;;     (powerline-raw (format "  %s]" branch) nil 'l)))

               (powerline-raw " ")
               (powerline-raw "%4l" nil 'l)
               (powerline-raw ",")
               (powerline-raw "%3c" nil 'r)
               (if (buffer-modified-p)
                   (powerline-raw " ⠾" nil 'r)
                 (powerline-raw "  " nil 'r))
               (let ((status (sb/flycheck-status)))
                 (when status
                   (powerline-raw (format "  %s" status) nil 'r)))
               (powerline-raw " "))))
          (concat
           (powerline-render lhs)
           (powerline-fill-center nil (/ (powerline-width center) 2.0))
           (powerline-render center)
           (powerline-fill nil (powerline-width rhs))
           (powerline-render rhs)))))))
  :when (eq sb/modeline-theme 'powerline)
  :hook
  ((elpaca-after-init . sb/powerline-nano-theme)
   ((flycheck-status-changed flycheck-mode-hook) . force-mode-line-update))
  :custom
  ;; Visualization of the buffer position is not useful
  (powerline-display-hud nil)
  (powerline-display-buffer-size nil)
  (powerline-display-mule-info nil "File encoding information is not useful")
  (powerline-gui-use-vcs-glyph t)
  (powerline-height 20))

;; TODO: Merge this with `powerline' modeline theme.

;; (use-package sb-modeline
;;   :preface
;;   (defvar sb/modeline-max-project-length 20
;;     "Maximum length of displayed project name before truncation.")

;;   (defvar sb/modeline-show-percentage t
;;     "If non-nil, show percentage-of-file instead of total-lines.")

;;   (defvar sb/modeline-min-width-for-vcs 90
;;     "Minimum window width to show Git/VCS segment.")

;;   ;; ---------------------------------------------
;;   ;; Internal caches
;;   ;; ---------------------------------------------
;;   (defvar sb/modeline--project-cache (make-hash-table :test 'equal)
;;     "Caches project root → displayed project name.")

;;   (defun sb/modeline--project-name ()
;;     "Return truncated project name, cached."
;;     (let* ((proj
;;             (ignore-errors
;;               (project-current)))
;;            (root
;;             (when proj
;;               (project-root proj))))
;;       (if (not root)
;;           ""
;;         (or (gethash root sb/modeline--project-cache)
;;             (let* ((name (file-name-nondirectory (directory-file-name root)))
;;                    (trunc
;;                     (if (> (length name) sb/modeline-max-project-length)
;;                         (concat
;;                          (substring name 0 sb/modeline-max-project-length) "…")
;;                       name)))
;;               (puthash root trunc sb/modeline--project-cache)
;;               trunc)))))

;;   (defun sb/modeline--vcs-info-visible-p ()
;;     "Return non-nil if VCS information should be shown."
;;     (>= (window-total-width) sb/modeline-min-width-for-vcs))

;;   (defun sb/modeline--vcs-segment ()
;;     "Return VCS segment (branch name) or empty string if hidden."
;;     (if (sb/modeline--vcs-info-visible-p)
;;         (let ((branch (vc-working-revision buffer-file-name)))
;;           (if branch
;;               (format "  %s " branch)
;;             ""))
;;       ""))

;;   (defun sb/modeline--position ()
;;     "Return position segment: percentage or line/total-lines."
;;     (if sb/modeline-show-percentage
;;         (let* ((cur (line-number-at-pos))
;;                (max (max 1 (line-number-at-pos (point-max)))))
;;           (format " %d%%%% " (/ (* 100 cur) max)))
;;       (format " %d/%d " (line-number-at-pos) (line-number-at-pos (point-max)))))


;;   ;; GUI icon wrapper (fallback to plain text in TTY)
;;   (defun sb/modeline--icon (text)
;;     (if (display-graphic-p)
;;         (propertize text 'face 'shadow)
;;       text))

;;   (defun sb/modeline-format ()
;;     `(" "
;;       ;; Project
;;       ,(sb/modeline--icon "📁 ") ,(sb/modeline--project-name)

;;       " | "

;;       ;; Buffer
;;       ,(sb/modeline--icon "📄 ") ,mode-line-buffer-identification

;;       ;; Modified indicator
;;       ,(when (buffer-modified-p)
;;          (propertize " ●" 'face 'error))

;;       " | "

;;       ;; Position
;;       ,(sb/modeline--position)

;;       ;; Major mode
;;       " | " "(" ,mode-name ")"

;;       ;; VCS (hidden on narrow windows)
;;       ,(sb/modeline--vcs-segment)

;;       " "))

;;   (defun sb/modeline-tty-setup ()
;;     "Compact modeline for terminal use."
;;     (unless (display-graphic-p)
;;       ;; Reduce modeline visuals
;;       (set-face-attribute 'mode-line nil :height 0.8 :weight 'normal :box nil)
;;       (set-face-attribute 'mode-line-inactive nil
;;                           :height 0.8
;;                           :weight 'normal
;;                           :box nil)
;;       ;; Very minimal TTY format
;;       (setq-default mode-line-format
;;                     '(" "
;;                       mode-line-buffer-identification
;;                       " "
;;                       (:eval (sb/modeline--position))
;;                       " ("
;;                       mode-name
;;                       ") "))))

;;   ;; ---------------------------------------------
;;   ;; Enable
;;   ;; ---------------------------------------------
;;   (defun sb/enable-smart-modeline ()
;;     "Activate the smart, width-adaptive modeline."
;;     (interactive)
;;     (setq-default mode-line-format '((:eval (sb/modeline-format))))
;;     (sb/modeline-tty-setup))

;;   :ensure nil
;;   :hook (elpaca-after-init . sb/enable-smart-modeline))

(use-package doom-modeline
  :when (eq sb/modeline-theme 'doom-modeline)
  :hook (elpaca-after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-unicode-fallback t)
  ;; Wrong LSP state is shown for non-LSP-managed files
  (doom-modeline-lsp nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon sb/enable-icons)
  :config
  (unless (display-graphic-p)
    ;; All other choices can lead to the modeline text overflowing
    (setopt doom-modeline-buffer-file-name-style 'buffer-name)))

;; (use-package centaur-tabs
;;   :hook ((elpaca-after-init . centaur-tabs-mode) (dired-mode . centaur-tabs-local-mode))
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
  :hook ((text-mode prog-mode fundamental-mode conf-mode org-mode) . olivetti-mode)
  :bind (:map olivetti-mode-map ("C-c {") ("C-c }") ("C-c \\"))
  :diminish)

(use-package kdl-mode
  :ensure (:host github :repo "taquangtrung/emacs-kdl-mode")
  :mode ("\\.kdl\\'" . kdl-mode))

;; (use-package kdl-ts-mode
;;   :ensure (:host github :repo "dataphract/kdl-ts-mode")
;;   :mode ("\\.kdl\\'" . kdl-ts-mode))

;; ;; I use `apheleia-mode' to sort kdl files.
;; (use-package reformatter
;;   :after (:any kdl-ts-mode kdl-mode)
;;   :demand t
;;   :config
;;   (reformatter-define
;;    kdlformat
;;    :program "kdlfmt"
;;    :args '("format" "--config" "~/private-dotfiles/kdlfmt.kdl")
;;    :lighter " KDLFMT"
;;    :group 'reformatter)
;;   (add-hook 'kdl-ts-mode-hook #'kdlformat-on-save-mode)
;;   (add-hook 'kdl-mode-hook #'kdlformat-on-save-mode))

;; Fontify ssh files
(use-package ssh-config-mode
  :mode ("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys\\'" . ssh-authorized-keys-mode))

;; The LSP is too slow to start.
(use-package asm-mode
  :ensure nil
  ;; :hook (asm-mode . sb/setup-lsp-provider)
  )

;; Guess the indentation offset originally used in foreign source code files and
;; transparently adjust the corresponding settings in Emacs making it more
;; convenient to edit the foreign files.
(use-package dtrt-indent
  :ensure (:host github :repo "jscheid/dtrt-indent")
  :hook (find-file . dtrt-indent-mode)
  :diminish)

;; Navigate the xref stack with consult
(use-package consult-xref-stack
  :ensure (:host github :repo "brett-lempereur/consult-xref-stack")
  :commands consult-xref-stack-forward
  :bind ("C-," . consult-xref-stack-backward))

;; ;; Kill Emacs buffers automatically after a timeout
;; (use-package buffer-terminator
;;   :ensure (:host github :repo "jamescherti/buffer-terminator.el")
;;   :hook (find-file . buffer-terminator-mode)
;;   :custom (buffer-terminator-verbose nil)
;;   :diminish)

;; (use-package hl-line
;;   :ensure nil
;;   :hook (dired-mode . hl-line-mode)
;;   :custom
;;   ;; Restrict `hl-line-mode' highlighting to the current window
;;   (hl-line-sticky-flag nil))

;; ;; Combined clipboard integration for terminal & GUI. Sends every kill from a
;; ;; TTY frame to the system clipboard. Clipetty handles clipboard via OSC 52.
;; (use-package clipetty
;;   :hook (elpaca-after-init . global-clipetty-mode)
;;   :diminish)

;; Only enable xclip in TTY under X11
(use-package xclip
  :when
  (and (not (display-graphic-p)) ; only in TTY
       (not (getenv "WAYLAND_DISPLAY")) ; avoid Wayland
       (or (executable-find "xclip") (executable-find "xsel")))
  :hook (elpaca-after-init . xclip-mode))

(use-package ztree
  :commands (ztree-diff))

;; ;; Provides pixel-precise smooth scrolling which can keep up with the very high
;; ;; event rates of modern trackpads and high-precision wheel mice.
;; (use-package ultra-scroll
;;   :ensure (:host github :repo "jdtsmith/ultra-scroll")
;;   :custom
;;   (scroll-conservatively 101)
;;   (scroll-margin 0)
;;   :hook (find-file . ultra-scroll-mode))

;; ;; Fold text using indentation levels
;; (use-package outline-indent
;;   :hook
;;   ((python-mode python-ts-mode yaml-mode yaml-ts-mode)
;;    .
;;    outline-indent-minor-mode)
;;   :custom
;;   (outline-indent-ellipsis " ▼ ")
;;   (outline-blank-line t)
;;   :diminish (outline-minor-mode outline-indent-minor-mode))

;; (use-package all-the-icons
;;   :commands all-the-icons-install-fonts
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

;; ;; Highlight the cursor position after the window scrolls
;; (use-package beacon
;;   :disabled
;;   :hook (elpaca-after-init . beacon-mode)
;;   :diminish)

;; ;; Allows to easily identify the file path in a project. But does not support
;; ;; imenu.
;; (use-package project-headerline
;;   :ensure (:host github :repo "gavv/project-headerline")
;;   :hook (elpaca-after-init . global-project-headerline-mode)
;;   :custom
;;   (project-headerline-segment-separator " > ")
;;   (project-headerline-path-separator " > "))

;; (use-package breadcrumb
;;   :ensure (:host github :repo "joaotavora/breadcrumb")
;;   :hook (prog-mode . breadcrumb-mode)
;;   :config (breadcrumb-imenu-crumbs))

;; ;; Hide a block with "C-c @ C-d", hide all folds with "C-c @ C-t", hide all
;; ;; blocks below the current level with "C-c @ C-l", show a block with "C-c @
;; ;; C-s", show all folds with "C-c @ C-a", and toggle hiding of a block with "C-c
;; ;; @ C-c".
;; (use-package hideshow
;;   :preface
;;   (defun sb/toggle-fold ()
;;     (interactive)
;;     (save-excursion
;;       (end-of-line)
;;       (hs-toggle-hiding)))
;;   :ensure nil
;;   :hook
;;   ((c-mode-common
;;     c-ts-mode
;;     c++-mode
;;     c++-ts-mode
;;     cmake-mode
;;     cmake-ts-mode
;;     css-mode
;;     css-ts-mode
;;     emacs-lisp-mode
;;     fish-mode
;;     html-mode
;;     java-mode
;;     java-ts-mode
;;     makefile-mode
;;     perl-mode
;;     python-mode
;;     python-ts-mode
;;     sh-mode
;;     bash-ts-mode
;;     json-mode
;;     json-ts-mode
;;     jsonc-mode
;;     yaml-mode
;;     yaml-ts-mode)
;;    . hs-minor-mode)
;;   :custom (hs-isearch-open t "Open all folds while searching")
;;   :diminish hs-minor-mode)

(use-package string-inflection
  :bind (:map prog-mode-map ("C-c C-u" . string-inflection-all-cycle)))

(use-package kill-file-path
  :ensure (:host github :repo "chyla/kill-file-path")
  :commands
  (kill-file-path-basename
   kill-file-path-basename-without-extension
   kill-file-path-dirname
   kill-file-path))

;; Allow fetching the latest versions via Elapaca to satisfy Eglot requirements
(use-package flymake)

(use-package jsonrpc)

(use-package eglot
  ;; :preface
  ;; ;; FIXME: It seems there is a race condition between Eglot shutting down the
  ;; ;; servers and closing the project buffers.
  ;; (defun sb/project-kill-buffers-disconnect-eglot ()
  ;;   "Shutdown Eglot for the current project before killing its buffers."
  ;;   (interactive)
  ;;   (when-let* ((proj (project-current))
  ;;               (root (project-root proj)))
  ;;     (dolist (buf (project-buffers proj))
  ;;       (with-current-buffer buf
  ;;         (when (and eglot--managed-mode (eglot-current-server))
  ;;           ;; Shut down once per server, not once per buffer
  ;;           (let ((server (eglot-current-server)))
  ;;             (when (and server
  ;;                        (equal (project-root (eglot--project server)) root))
  ;;               (ignore-errors
  ;;                 (eglot-shutdown server))))))))
  ;;   (project-kill-buffers))
  :ensure (:source (gnu-elpa-mirror))
  :when (eq sb/lsp-provider 'eglot)
  :hook
  ((html-mode html-ts-mode LaTeX-mode markdown-mode org-mode text-mode)
   .
   (lambda ()
     ;; Disable LSP for git commit message buffers which are usually ephemeral
     (unless (string-equal
              (file-name-nondirectory (or buffer-file-name ""))
              "COMMIT_EDITMSG")
       (eglot-ensure))))
  :custom
  ;; Disabling this helps avoid the race condition between closing a project and shutting down LSP servers.
  ;; (eglot-autoshutdown t)

  (eglot-sync-connect nil "Do not block waiting to connect to the LSP")
  (eglot-send-changes-idle-time 3)
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
  :config
  ;; Avoid overhead from logging
  (setf (plist-get eglot-events-buffer-config :size) 0)
  (fset #'jsonrpc--log-event #'ignore)

  (setopt
   eglot-server-programs
   `(((toml-mode toml-ts-mode conf-toml-mode) . ("taplo" "lsp" "stdio"))
     ;; `harper-ls' is more efficient than `ltex-ls-plus' but does not support `LaTeX-mode' completely
     ((text-mode org-mode markdown-mode markdown-ts-mode LaTeX-mode)
      .
      ,(eglot-alternatives '(("harper-ls" "--stdio") "ltex-ls-plus")))
     ((autoconf-mode makefile-mode makefile-automake-mode makefile-gmake-mode)
      . ("autotools-language-server"))
     (fish-mode . ("fish-lsp" "start"))
     ((asm-mode fasm-mode masm-mode nasm-mode gas-mode) . ("asm-lsp"))
     ((c-mode c-ts-mode c++-mode c++-ts-mode c-or-c++-ts-mode c-or-c++-mode)
      .
      ("clangd"
       "-j=4"
       "--compile-commands-dir=./."
       "--all-scopes-completion"
       "--background-index"
       ;; Unsupported option with Clangd 14
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
     ((cmake-mode cmake-ts-mode) . ("cmake-language-server"))
     ((bash-ts-mode sh-mode) . ("bash-language-server" "start"))
     ;; Download the source from
     ;; https://github.com/eclipse-jdtls/eclipse.jdt.ls/tags. Build with "./mvnw
     ;; clean verify -U -DskipTests=true". Change the url in
     ;; "org.eclipse.jdt.ls.target/org.eclipse.jdt.ls.tp.target" if there is a
     ;; "No repository found" error.
     ((java-mode java-ts-mode)
      .
      ("jdtls" "--illegal-access=warn" "-Xms2G" "-Xmx8G"))
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
                          user-emacs-directory)))))

  (if (eq sb/python-langserver 'pylsp)
      (add-to-list
       'eglot-server-programs '((python-mode python-ts-mode) . ("pylsp")))
    (add-to-list
     'eglot-server-programs
     '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))))

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
      :disabledRules ["ELLIPSIS" "EN_QUOTES" "MORFOLOGIK_RULE_EN_US"]
      ;; Keep grammar and style checking
      :additionalRules (:enablePickyRules t :motherTongue "en-IN"))
     :yaml
     (:format
      (:enable t :singleQuote nil :bracketSpacing t)
      :validate t
      :hover t
      :completion t)
     :vscode-json-language-server (:provideFormatter t)
     ;; Harper uses four dictionaries: per-user, per-workspace, file-local, and a in-built static dictionary.
     :harper-ls
     (:userDictPath
      (expand-file-name ())
      :workspaceDictPath "${workspaceFolder}/.harper-dictionary.txt"
      :fileDictPath ""
      :linters
      (:SpellCheck
       :json-false
       :SpelledNumbers
       :json-false
       :AnA t
       :UnclosedQuotes t
       :WrongQuotes
       :json-false
       :LongSentences
       :json-false
       :RepeatedWords t
       :Spaces t
       :Matcher t
       :CorrectNumberSuffix t
       :SentenceCapitalization t)
      :codeActions (:ForceStable :json-false)
      :diagnosticSeverity "hint"
      :markdown (:IgnoreLinkTitle :json-false)
      :isolateEnglish
      :json-false
      :dialect "American")))

  ;; (setq-default completion-category-overrides
  ;;               '((eglot (styles hotfuzz basic substring orderless))
  ;;                 (eglot-capf (styles hotfuzz orderless))))

  ;; ;; Try to avoid the race condition when closing a project and shutting down the LSP server.
  ;; (with-eval-after-load 'project
  ;;   (bind-key
  ;;    "k" #'sb/project-kill-buffers-disconnect-eglot project-prefix-map))

  (add-to-list
   'display-buffer-alist
   '("\\*EGLOT workspace configuration\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . 2)
     (window-height . 0.5)))

  ;; Clean completion metadata with `cape-capf-buster'. Make the capf composable
  ;; allowing falling back to other backends with `cape-capf-nonexclusive'.
  ;; Ensure that completion does not get interrupted by the user pressing keys
  ;; or other operations with `cape-wrap-noninterruptible'.
  (with-eval-after-load 'cape
    (advice-add
     #'eglot-completion-at-point
     :around
     (lambda (orig-fun &rest args)
       (apply (cape-capf-buster (cape-capf-nonexclusive orig-fun)) args)))

    (defun sb/setup-capfs-for-eglot ()
      "Setup custom CAPFs for programming buffers."
      (when (derived-mode-p 'prog-mode)
        (setq-local
         completion-at-point-functions
         (list
          (cape-capf-inside-code
           (cape-capf-super
            #'eglot-completion-at-point
            ;; We do not need to merge `cape-keyword' and `cape-dabbrev' if for Eglot-managed major modes
            #'yasnippet-capf))
          (cape-capf-inside-comment #'cape-dict)
          (cape-capf-inside-string #'cape-file)
          (cape-capf-buster #'cape-dabbrev)))))
    (add-hook 'eglot-managed-mode-hook #'sb/setup-capfs-for-eglot)))

(use-package eglot-booster
  :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :when (executable-find "emacs-lsp-booster")
  :after eglot
  :demand t
  :config (eglot-booster-mode))

(use-package eglot-java
  :when (eq sb/lsp-provider 'eglot)
  :hook
  ((java-mode . eglot-ensure)
   (eglot-managed-mode
    .
    (lambda ()
      (when (derived-mode-p 'java-mode)
        (eglot-java-mode)))))
  :config
  (defun sb/eglot-java-init-opts (server eglot-java-eclipse-jdt)
    "Custom options that will be merged with any default settings."
    `( ;;:workspaceFolders: ["file:///home/swarnendu/mavenproject"]
      :settings
      (:java
       (:home "/usr/lib/jvm/java-21-openjdk-amd64/")
       :configuration
       (:runtimes
        [(:name "JavaSE-17" :path "/usr/lib/jvm/openjdk-17/")
         (:name "JavaSE-21" :path "/usr/lib/jvm/openjdk-21/" :default t)])
       :completion (:guessMethodArguments t)
       :format
       (:enabled
        t
        :comments (:enabled t)
        :onType (:enabled :json-false)
        :tabSize 4
        :insertSpaces t
        :settings
        (:url
         "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"))
       :extendedClientCapabilities (:classFileContentsSupport t))))

  (setopt eglot-java-user-init-opts-fn 'sb/eglot-java-init-opts))

(use-package eglot-hierarchy
  :ensure (:host github :repo "dolmens/eglot-hierarchy")
  :when (eq sb/lsp-provider 'eglot)
  :after eglot
  :commands
  (eglot-hierarchy-call-hierarchy
   eglot-hierarchy-incoming-calls
   eglot-hierarchy-outgoing-calls
   eglot-hierarchy-type-hierarchy))

(use-package consult-eglot
  :when (eq sb/lsp-provider 'eglot)
  :after (consult eglot)
  :commands consult-eglot-symbols)

(use-package flycheck-eglot
  :when (eq sb/lsp-provider 'eglot)
  :after (flycheck eglot)
  :init (global-flycheck-eglot-mode 1)
  :custom (flycheck-eglot-exclusive nil))

(use-package eglot-semantic-tokens
  :ensure (:host github :repo "eownerdead/eglot-semantic-tokens")
  :when (eq sb/lsp-provider 'eglot)
  :after eglot
  :demand t
  :custom (eglot-enable-semantic-tokens t))

(use-package eglot-inactive-regions
  :when (eq sb/lsp-provider 'eglot)
  :after eglot
  :demand t
  :hook (eglot-managed-mode . eglot-inactive-regions-mode)
  :custom
  (eglot-inactive-regions-style 'darken-foreground)
  (eglot-inactive-regions-opacity 0.4))

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
    "*Compile-Log*"
    "*lsp-log*"
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
  :type '(repeat symbol)
  :group 'sb/emacs)

(defun sb/get-buffer-major-mode (buffer-or-string)
  "Return the major mode associated with BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
    major-mode))

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
                    (sb/get-buffer-major-mode (current-buffer))
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

;; Define what counts as a popup window
(defun sb/popup-window-p (window)
  "Return non-nil if WINDOW is a popup (side-window or marked)."
  (or
   (window-parameter window 'window-side) ; side-window (display-buffer-in-side-window)
   (window-parameter window 'popup))) ; manually marked

(defun sb/close-popups ()
  "Close all popup windows."
  (interactive)
  (dolist (win (window-list))
    (when (sb/popup-window-p win)
      (delete-window win))))

(defun sb/keyboard-quit-dwim ()
  "Quit popups if any, otherwise run `keyboard-quit`."
  (interactive)
  (if (cl-some #'sb/popup-window-p (window-list))
      (sb/close-popups)
    (keyboard-quit)))

(defun sb/directory-exists-p (dir)
  "Return non-nil if DIR exists and is a directory.
DIR can be relative or absolute."
  (and (stringp dir) (file-directory-p (expand-file-name dir))))

;; Inside strings, special keys like tab or F1-Fn have to be written inside
;; angle brackets, e.g., "C-<up>". Standalone special keys (and some
;; combinations) can be written in square brackets, e.g. [tab] instead of
;; "<tab>".

;; ESC serves as a substitute for META, but there is no need to hold down ESC
;; while pressing the subsequent key. Instead "M-something" keybindings can be
;; triggered by pressing ESC and the other key sequentially.

(bind-keys
 ("C-c ;" . sb/comment-line)
 ("C-S-s" . sb/save-all-buffers)
 ("C-x |" . sb/toggle-window-split)
 ("M-<left>" . sb/previous-buffer)
 ("M-<right>" . sb/next-buffer)
 ("C-S-<iso-lefttab>" . sb/previous-buffer)
 ("C-<tab>" . sb/next-buffer)
 ("C-g" . sb/keyboard-quit-dwim))

;; (use-package default-text-scale
;;   :when (display-graphic-p)
;;   :bind
;;   (("C-M-+" . default-text-scale-increase)
;;    ("C-M--" . default-text-scale-decrease)))

;; Show free bindings in current buffer
(use-package free-keys
  :commands free-keys)

;; I prefer `embark' to show help about keybindings compared to `which-key'.

;; ;; Displays available keybindings following the currently entered incomplete
;; ;; command/prefix in a popup.
;; (when (< emacs-major-version 30)
;;   (use-package which-key))
;; (add-hook 'elpaca-after-init-hook #'which-key-mode)
;; (with-eval-after-load 'which-key
;;   (diminish 'which-key-mode))

;; ;; https://gist.github.com/mmarshall540/a12f95ab25b1941244c759b1da24296d
;; (which-key-add-key-based-replacements
;;  "<f1> 4"
;;  "help-other-win"
;;  "<f1>"
;;  "help"
;;  "<f2>"
;;  "2-column"
;;  "C-c"
;;  "mode-and-user"
;;  "C-h 4"
;;  "help-other-win"
;;  "C-h"
;;  "help"
;;  "C-x 4"
;;  "other-window"
;;  "C-x 5"
;;  "other-frame"
;;  "C-x 6"
;;  "2-column"
;;  "C-x 8"
;;  "insert-special"
;;  "C-x C-k C-q"
;;  "kmacro-counters"
;;  "C-x C-k C-r a"
;;  "kmacro-add"
;;  "C-x C-k C-r"
;;  "kmacro-register"
;;  "C-x C-k"
;;  "keyboard-macros"
;;  "C-x RET"
;;  "encoding/input"
;;  "C-x a i"
;;  "abbrevs-inverse-add"
;;  "C-x a"
;;  "abbrevs"
;;  "C-x n"
;;  "narrowing"
;;  "C-x p"
;;  "projects"
;;  "C-x r"
;;  "reg/rect/bkmks"
;;  "C-x t ^"
;;  "tab-bar-detach"
;;  "C-x t"
;;  "tab-bar"
;;  "C-x v M"
;;  "vc-mergebase"
;;  "C-x v b"
;;  "vc-branch"
;;  "C-x v"
;;  "version-control"
;;  "C-x w ^"
;;  "window-detach"
;;  "C-x w"
;;  "window-extras"
;;  "C-x x"
;;  "buffer-extras"
;;  "C-x"
;;  "extra-commands"
;;  "M-g"
;;  "goto-map"
;;  "M-s h"
;;  "search-highlight"
;;  "M-s"
;;  "search-map")

;; ;; Upon loading, the built-in `page-ext' package turns "C-x C-p" into
;; ;; a prefix-key.  If you know of other built-in packages that have
;; ;; this behavior, please let me know, so I can add them.
;; (with-eval-after-load 'page-ext
;;   (which-key-add-key-based-replacements "C-x C-p" "page-extras"))

;; Support the Kitty keyboard protocol in Emacs
(use-package kkp
  :hook (elpaca-after-init . global-kkp-mode)
  ;; :bind
  ;; ("M-<backspace>" . backward-kill-word) ; Should be remapped to "M-DEL"
  :config
  ;; These workarounds are to help with Zellij.
  (define-key key-translation-map (kbd "M-S-;") (kbd "M-:"))
  (define-key key-translation-map (kbd "M-S-4") (kbd "M-$"))
  (define-key key-translation-map (kbd "M-S-/") (kbd "M-?")))

;; (use-package keyfreq
;;   :ensure (:host github :repo "dacap/keyfreq")
;;   :hook
;;   (elpaca-after-init
;;    .
;;    (lambda ()
;;      (keyfreq-mode 1)
;;      (keyfreq-autosave-mode 1))))

;; (use-package flyover
;;   :ensure (:host github :repo "konrad1977/flyover")
;;   :when (display-graphic-p)
;;   :hook (flycheck-mode . flyover-mode)
;;   :custom
;;   (flyover-hide-checker-name nil)
;;   (flyover-background-lightness 40)
;;   :diminish)

;; (use-package disable-mouse
;;   :hook (emacs-startup . disable-mouse-global-mode)
;;   :diminish disable-mouse-global-mode)

;; ;; `inhibit-mouse' is supposed to be more efficient than `disable-mouse'
;; (use-package inhibit-mouse
;;   :hook (elpaca-after-init . inhibit-mouse-mode)
;;   :custom
;;   ;; Disable highlighting of clickable text such as URLs and hyperlinks when
;;   ;; hovered by the mouse pointer.
;;   (inhibit-mouse-adjust-mouse-highlight t)
;;   ;; Disables the use of tooltips (show-help-function) during mouse events.
;;   (inhibit-mouse-adjust-show-help-function t)
;;   :config
;;   (when (daemonp)
;;     (add-hook 'server-after-make-frame-hook #'inhibit-mouse-mode))
;;   :diminish)

(with-eval-after-load 'transient
  (transient-define-prefix
   sb/search-transient () "Search commands"
   [["Isearch"
     ("i" "Forward" isearch-forward)
     ("b" "Backward" isearch-backward)
     ("s" "Symbol at point" isearch-symbol-at-point)
     ("w" "Forward symbol at point" isearch-forward-symbol-at-point)
     ;; During an Isearch session, this command picks a search string from
     ;; history and continues the search with the newly selected string. Outside
     ;; of Isearch, the command allows you to pick a string from the history and
     ;; starts a new Isearch.
     ("h" "History" consult-isearch-history)]
    ["Ripgrep" ("d" "Deadgrep" deadgrep)
     ;; Filter by file extension with `consult-ripgrep' "... -- -g *.jsx"
     ("r" "Ripgrep" consult-ripgrep)]
    ["Wgrep"
     ("C-p" "Enable" wgrep-change-to-wgrep-mode)
     ("C-s" "Finish edit" wgrep-finish-edit)
     ("C-k" "Abort changes" wgrep-abort-changes)
     ("C-q" "Exit" wgrep-exit)]
    ["Search locations"
     ("n" "Find" consult-find)
     ("f" "Fd" consult-fd)
     ("l" "Locate" consult-locate)]
    ["Other tools"
     ("o" "Occur" occur)
     ("h" "Isearch occur" isearch-occur)
     ("g" "Grep" consult-grep)
     ("t" "Git grep" consult-git-grep)]])
  (bind-key "C-c s" #'sb/search-transient)

  (transient-define-prefix
   sb/dotemacs-transient () "Config-specific keybindings"
   [["Utilities"
     ("s" "Sudo edit" crux-sudo-edit)
     ("i" "Ispell then abbrev" crux-ispell-word-then-abbrev)
     ("w" "Toggle whitespace" whitespace-mode)
     ("m" "Describe major mode" discover-my-mode)]
    ["Buffers"
     ("c" "*scratch*" scratch-buffer)
     ("v" "Echo-area messages" view-echo-area-messages)
     ("l" "*Messages*"
      (lambda ()
        (interactive)
        (switch-to-buffer "*Messages*")))]
    ["Tramp" ("t" "Choose target" consult-tramp)
     ;; We use `q' to quit transient
     ("Q" "Cleanup connections" sb/cleanup-tramp)]
    ["Emacs config"
     ("e" "Edit init.el"
      (lambda ()
        (interactive)
        (find-file user-init-file)))
     ("r" "Reload init.el"
      (lambda ()
        (interactive)
        (load-file user-init-file)))
     ("d" "Open .emacs.d"
      (lambda ()
        (interactive)
        (dired user-emacs-directory)))
     ("m" "Restart" restart-emacs)
     ("b" "keybindings" embark-bindings)
     ("k" "Personal keybindings" describe-personal-keybindings)]])
  (bind-key "C-c d" #'sb/dotemacs-transient)

  (transient-define-prefix
   sb/smerge-transient () "Smerge menu"
   [["Navigation"
     ("n" "Next conflict" smerge-next)
     ("p" "Previous conflict" smerge-prev)]
    ["Merge"
     ("u" "Keep upper" smerge-keep-upper)
     ("l" "Keep lower" smerge-keep-lower)
     ("a" "Keep both" smerge-keep-all)]
    ["Diff" ("e" "Ediff" smerge-ediff) ("r" "Resolve" smerge-resolve)]])
  (bind-key "C-c ^" #'sb/smerge-transient)

  (with-eval-after-load 'lsp-mode
    (transient-define-prefix
     sb/lsp-transient () "Lsp menu"
     [["Lsp functionality"
       ("l" "Start Lsp" lsp)
       ("q" "Disconnect Lsp" lsp-disconnect)
       ("w" "Workspace shutdown" lsp-workspace-shutdown)
       ("R" "Workspace restart" lsp-workspace-restart)
       ("a" "Add folder to workspace" lsp-workspace-folders-add)
       ("v" "Remove folder from workspace" lsp-workspace-folders-remove)
       ("b" "Blacklist and remove workspace" lsp-workspace-blocklist-remove)]
      ["Browsing functionality"
       ("d" "Find declaration" lsp-find-declaration)
       ("e" "Find definition" lsp-find-definition)
       ("i" "Find implementation" lsp-find-implementation)
       ("c" "Find references" lsp-find-references)
       ("I" "Go to implementation" lsp-goto-implementation)
       ("t" "Go to type definition" lsp-goto-type-definition)]
      ["Code actions"
       ("r" "Rename" lsp-rename)
       ("f" "Format buffer" lsp-format-buffer)
       ("x" "Execute code action" lsp-execute-code-action)]
      ["Diagnostics" ("s" "Diagnostics" consult-lsp-diagnostics)]])

    (transient-define-prefix
     sb/lsp-imenu-transient () "Imenu commands"
     [["Imenu"
       ("i" "Imenu" consult-imenu)
       ("j" "Breadcrumb jump" breadcrumb-jump)]
      ["Lsp imenu"
       ("g" "File symbols" consult-lsp-file-symbols)
       ("h" "Workspace symbols" consult-lsp-symbols)]
      ["Dogears"
       ("d" "Go" dogears-go)
       ("r" "Remember" dogears-remember)
       ("b" "Back" dogears-back)
       ("f" "Forward" dogears-forward)
       ("t" "List" dogears-list)]]))

  (when (eq sb/lsp-provider 'lsp-mode)
    (bind-key "C-c l" #'sb/lsp-transient)
    (bind-key "C-c i" #'sb/lsp-imenu-transient))

  (with-eval-after-load 'eglot
    (transient-define-prefix
     sb/eglot-transient () "Eglot menu"
     [["Eglot" ("l" "Start" eglot) ("q" "Shutdown" eglot-shutdown)]
      ["Browsing functionality"
       ("d" "Find declaration" eglot-find-declaration)
       ("i" "Find implementation" eglot-find-implementation)
       ("t" "Find type definition" eglot-find-typeDefinition)]
      ["Code actions"
       ("r" "Rename" eglot-rename)
       ("f" "Format buffer" eglot-format)
       ("x" "Execute code action" eglot-code-actions)
       ("k" "Execution code action: quickfix" eglot-code-action-quickfix)
       ("e" "Execution code action: extract" eglot-code-action-extract)
       ("n" "Execution code action: inline" eglot-code-action-inline)
       ("w" "Execution code action: rewrite" eglot-code-action-rewrite)
       ("o"
        "Execution code action: organize imports"
        eglot-code-action-organize-imports)]
      ["Diagnostics" ("s" "Diagnostics" consult-lsp-diagnostics)]]))

  (when (eq sb/lsp-provider 'eglot)
    (bind-key "C-c l" #'sb/eglot-transient))

  (transient-define-prefix
   sb/file-buffer-transient () "File and Buffer commands"
   [["File"
     ("w" "Save" write-file)
     ("r" "Rename" rename-file)
     ("a" "Find alternate" find-alternate-file)
     ("o" "FFAP find other" ff-find-other-file)]
    ["Buffer"
     ("g" "Revert quick" revert-buffer-quick)
     ("b" "Rename" rename-buffer)]])
  (bind-key "C-c x" #'sb/file-buffer-transient)

  (transient-define-prefix
   sb/navigation-transient () "Jump commands"
   [["Parentheses"
     ("b" "Backward sexp" backward-sexp)
     ("f" "Forward sexp" forward-sexp)
     ("k" "Kill sexp" kill-sexp)]
    ["Functions"
     ("a" "Begin" treesit-beginning-of-defun)
     ("e" "End" treesit-end-of-defun)]
    ;; ["Expressions"
    ;;  ;; ("u" "Up list" treesit-up-list)
    ;;  ;; ("d" "Down list" treesit-down-list)
    ;;  ("F" "Forward sexp" treesit-forward-sexp)]
    ["Flycheck"
     ("n" "Next error" next-error)
     ("p" "Previous error" previous-error)]
    ["Imenu" ("i" "Imenu" consult-imenu)]
    ["Eglot" ("h" "Workspace symbols" consult-eglot-symbols)]
    ;; ["Dogears"
    ;;  ("d" "Go" dogears-go)
    ;;  ("r" "Remember" dogears-remember)
    ;;  ("b" "Back" dogears-back)
    ;;  ("f" "Forward" dogears-forward)
    ;;  ("t" "List" dogears-list)]
    ])
  (bind-key "C-c n" #'sb/navigation-transient)

  (transient-define-prefix
   sb/ediff-transient () "Launch Ediff in all it's variants"
   ["Ediff" ["2 Way"
     ("b" "Buffers" ediff-buffers)
     ("f" "Files" ediff-files)
     ("d" "Directories" ediff-directories)
     ("c" "Buffer vs File" ediff-current-file)
     ("~" "File vs Backup" ediff-backup)]
    ["3 Way"
     ("3b" "Buffers" ediff-buffers3)
     ("3f" "Files" ediff-files3)
     ("3d" "Directories" ediff-directories3)]
    ["Patches"
     ("pb" "Buffer" ediff-patch-buffer)
     ("pf" "File" ediff-patch-file)]
    ["Regions"
     ("rl" "Linewise" ediff-regions-linewise)
     ("rw" "Wordwise" ediff-regions-wordwise)]
    ["Windows"
     ("wl" "Linewise" ediff-windows-linewise)
     ("ww" "Wordwise" ediff-windows-wordwise)]])
  (bind-key "C-c e" #'sb/ediff-transient)

  (with-eval-after-load 'citre
    (transient-define-prefix
     sb/citre-transient () "Citre commands"
     [["Jump"
       ("j" "Jump" sb/jump-citre-xref)
       ("b" "Jump back" citre-jump-back)
       ("p" "Peek" citre-peek)
       ("a" "Ace peek" citre-ace-peek)
       ("r" "Reference" citre-jump-to-reference)]
      ["Manage"
       ("c" "Create tags" citre-create-tags-file)
       ("u" "Update tags" citre-update-tags-file)
       ("e" "Edit recipe" citre-edit-tags-file-recipe)
       ("g" "Update global db" citre-global-update-database)]])
    (bind-key "C-c c" #'sb/citre-transient))

  ;; We want Cape functions to be autoloaded if not already.
  (when (eq sb/in-buffer-completion 'corfu)
    (transient-define-prefix
     sb/corfu-transient () "Corfu commands"
     [["Capf"
       ("d" "Dict" cape-dict)
       ("v" "Dabbrev" cape-dabbrev)
       ("h" "History" cape-history)
       ("f" "File" cape-file)]
      [""
       ("t" "TeX" cape-tex)
       ("a" "Abbrev" cape-abbrev)
       ("k" "Keyword" cape-keyword)
       ("e" "Elisp symbol" cape-elisp-symbol)]
      [""
       ("j" "Emoji" cape-emoji)
       ("l" "Line" cape-line)
       ("b" "Elisp block" cape-elisp-block)]
      ["" ("r" "RFC 1345" cape-rfc1345) ("s" "Unicode from SGML" cape-sgml)]])
    (bind-key "C-c p" #'sb/corfu-transient))

  (transient-define-prefix
   sb/latex-transient () "LaTeX commands"
   [[""
     ("l" "Insert label" reftex-label)
     ("b" "Insert block" latex-insert-block)
     ("r" "Insert reference" consult-reftex-insert-reference)
     ("g" "Go to label" consult-reftex-goto-label)]])
  (bind-key "C-c j" #'sb/latex-transient)

  (transient-define-prefix
   sb/root-transient () "Top-level menu"
   [["Emacs"
     ("d" "Dotemacs" sb/dotemacs-transient)
     ("x" "File/Buffer" sb/file-buffer-transient)
     ("s" "Search" sb/search-transient)
     ("^" "Smerge" sb/smerge-transient)]
    ["Programming" ("l" "LSP/Eglot"
      (lambda ()
        (interactive)
        (cond
         ((eq sb/lsp-provider 'eglot)
          (sb/eglot-transient))
         ((eq sb/lsp-provider 'lsp-mode)
          (sb/lsp-transient)))))]
    ["Navigation"
     ("n" "Navigation" sb/navigation-transient)
     ("c" "Citre" sb/citre-transient)]
    ["Completion" ("p" "Corfu" sb/corfu-transient)]
    ["Misc" ("e" "Ediff" sb/ediff-transient) ("j" "LaTeX" sb/latex-transient)]])
  (bind-key "C-c SPC" #'sb/root-transient))

(when (eq sb/lsp-provider 'lsp-mode)
  (defun sb/jump-choose-definition ()
    "Interactive jump menu with iconified options."
    (interactive)
    (let* ((options
            `(("🔍 LSP: go to definition" . lsp-find-definition)
              ("🧠 LSP: search symbol (consult)" . consult-lsp-symbols)
              ("📚 Citre: jump" . citre-jump)
              ("📎 Xref: find definitions" . xref-find-definitions)
              ("🗂  Imenu (consult)" . consult-imenu)))
           (choice (completing-read "Jump using: " (mapcar #'car options))))
      (call-interactively (cdr (assoc choice options)))))
  (bind-key "M-'" #'sb/jump-choose-definition))

(when (eq sb/lsp-provider 'eglot)
  (defun sb/jump-choose-definition ()
    "Interactive jump menu with iconified options."
    (interactive)
    (let* ((options
            `(("🔍 Eglot: go to declaration" . eglot-find-declaration)
              ("🧠 Eglot: search symbol (consult)" . consult-eglot-symbols)
              ("📚 Citre: jump" . citre-jump)
              ("📎 Xref: find definitions" . xref-find-definitions)
              ("🗂  Imenu: " . consult-imenu)))
           (choice (completing-read "Jump using: " (mapcar #'car options))))
      (call-interactively (cdr (assoc choice options)))))
  (bind-key "M-'" #'sb/jump-choose-definition))

(add-hook
 'elpaca-after-init-hook
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
;; elisp-autofmt-load-packages-local: ("use-package-core")
;; End:
