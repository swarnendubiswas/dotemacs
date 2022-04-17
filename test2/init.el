;;; init-emacs28.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; Bootstrap `straight.el'
(defvar bootstrap-version)
(setq ;; straight-check-for-modifications '(find-when-checking)
      ;; straight-host-usernames '((github . "swarnendubiswas"))
      ;; straight-vc-git-default-clone-depth 1
      straight-build-dir (format "build/%d%s%d"
                                 emacs-major-version
                                 version-separator
                                 emacs-minor-version))

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq use-package-enable-imenu-support t
      ;; Avoid manual installations whenever I modify package installations
      use-package-always-ensure        nil
      use-package-hook-name-suffix     nil)

;; If we omit `:defer', `:hook', `:commands', or `:after', then the package is loaded immediately.
;; We do not need `:commands' with `:hook' or `:bind'. The setting `use-package-always-defer'
;; implies always load features lazily unless told otherwise. This implies we should use
;; `after-init' hook or `:init' instead of `:config', since otherwise packages may not be loaded. Be
;; careful about using `:after' and always deferring loading, because then we will need to specifiy
;; alternate ways of loading the package.
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading

;; Hooks in the `:hook' section run in reverse order. Example:
;; (use-package package-name
;;   :hook
;;   ((x-mode-hook . last)
;;    (x-mode-hook . second)
;;    (x-mode-hook . first)))

(when (bound-and-true-p sb/debug-init-file)
  (setq debug-on-error                 t
        debug-on-event                 'sigusr2
        garbage-collection-messages    t
        use-package-compute-statistics t ; Use "M-x use-package-report" to see results
        use-package-verbose            t
        use-package-expand-minimally   nil)
  (debug-on-entry 'projectile-remove-known-project))

(unless (bound-and-true-p sb/debug-init-file)
  (setq use-package-always-defer       t
        ;; Avoid printing errors and warnings since the configuration is known to work
        use-package-expand-minimally   t
        use-package-compute-statistics nil
        use-package-verbose            nil))


(use-package f
  :straight t
  :commands (f-exists? f-join f-dirname))

(use-package s
  :straight t
  :commands s-starts-with? s-ends-with?)

(use-package dash
  :straight t
  :commands (-contains? -tree-map))

(use-package no-littering
  :straight t
  :demand t)

(defcustom sb/custom-file
  (no-littering-expand-etc-file-name "custom.el")
  "File to write Emacs customizations."
  :type  'string
  :group 'sb/emacs)

(setq custom-file sb/custom-file)

;; NOTE: Make a symlink to "private.el" in "$HOME/.emacs.d/etc".
(defcustom sb/private-file
  (no-littering-expand-etc-file-name "private.el")
  "File to include private information."
  :type  'string
  :group 'sb/emacs)

(let ((gc-cons-threshold most-positive-fixnum))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))
  (when (file-exists-p sb/private-file)
    (load sb/private-file 'noerror 'nomessage)))

(use-package warnings
  :straight nil
  :init
  ;; This is not a great idea, but I expect most warnings will arise from third-party packages.
  (setq warning-minimum-level :emergency))

(use-package gcmh ; Allow GC to happen after a period of idle time
  :straight t
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

;; Get PATH with "(getenv "PATH")". Set PATH with
;; "(setenv "PATH" (concat (getenv "PATH") ":/home/swarnendu/bin"))".
(use-package exec-path-from-shell
  :straight t
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


;; Replace `dabbrev-exp' with `hippie-expand'. Use "C-M-/" for `dabbrev-completion' which finds all
;; expansions in the current buffer and presents suggestions for completion.
(use-package hippie-exp
  :straight nil
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev
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
  :bind
  (("M-/"   . hippie-expand)
   ("C-M-/" . dabbrev-completion)))

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






;; Asynchronously byte compile packages installed with `package.el'
(use-package async
  :straight t
  :functions async-bytecomp-package-mode
  :commands async-bytecomp-package-mode
  :init (async-bytecomp-package-mode 1))

(use-package dired-async
  :straight async
  :diminish
  :hook (dired-mode-hook . dired-async-mode))




(use-package treemacs-magit
  :straight t
  :straight magit
  :after (treemacs magit)
  :demand t)



(progn
  (defvar reb-re-syntax)

  (setq reb-re-syntax 'string))

(use-package visual-regexp
  :straight t
  :commands (vr/replace vr/query-replace vr/mark)
  :bind ([remap query-replace] . vr/query-replace))


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

;; Use "M-x company-diag" or the modeline status to see the backend used. Try "M-x
;; company-complete-common" when there are no completions. Use "C-M-i" for `complete-symbol' with
;; regex search.
(use-package company
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :commands (company-abort company-files company-yasnippet
                           company-ispell company-dabbrev
                           company-capf company-dabbrev-code
                           company-clang-set-prefix
                           global-company-mode)
  :defines (company-dabbrev-downcase company-dabbrev-ignore-case
                                     company-dabbrev-other-buffers
                                     company-ispell-available
                                     company-ispell-dictionary
                                     company-clang-insert-arguments)
  :hook (after-init-hook . global-company-mode)
  ;; The `company-posframe' completion kind indicator is not great, but we are now using
  ;; `company-fuzzy'.
  :diminish
  :config
  (setq company-dabbrev-downcase nil ; Do not downcase returned candidates
        company-dabbrev-ignore-case nil ; Do not ignore case when collecting completion candidates
        ;; Search in other buffers with the same major mode. This can cause
        ;; performance overhead if there are lots of open buffers.
        company-dabbrev-other-buffers nil
        company-ispell-available t
        company-ispell-dictionary (expand-file-name "wordlist.5" sb/extras-directory)
        company-minimum-prefix-length 3 ; Small words can be faster to type
        company-require-match nil ; Allow input string that do not match candidates
        company-selection-wrap-around t
        company-show-quick-access t ; Speed up completion
        ;; Align additional metadata, like type signatures, to the right-hand side
        company-tooltip-align-annotations t
        ;; Disable insertion of arguments
        company-clang-insert-arguments nil
        ;; Start a search using `company-filter-candidates' (bound to "C-s") to narrow out-of-order
        ;; strings
        ;; https://github.com/company-mode/company-mode/discussions/1211
        company-search-regexp-function 'company-search-words-in-any-order-regexp
        company-frontends '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
                            ;; show selected candidate docs in echo area
                            company-echo-metadata-frontend)
        company-backends '(company-capf))

  ;; We set `company-backends' as a local variable, so it is not important to delete backends
  ;; (dolist (backends '(company-semantic company-bbdb company-oddmuse company-cmake company-clang))
  ;;   (delq backends company-backends))

  ;; Ignore matches that consist solely of numbers from `company-dabbrev'
  ;; https://github.com/company-mode/company-mode/issues/358
  (push (apply-partially #'cl-remove-if
                         (lambda (c)
                           (string-match-p "\\`[0-9]+\\'" c)))
        company-transformers)
  :bind
  (:map company-active-map
        ("C-n"      . company-select-next)
        ("C-p"      . company-select-previous)
        ;; Insert the common part of all candidates, or select the next one
        ("<tab>"    . company-complete-common-or-cycle)
        ("C-M-/"    . company-other-backend) ; Was bound to `dabbrev-completion'
        ("<escape>" . company-abort)))

;; Silence "Starting 'look' process..." message
(advice-add 'lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; Hide the "Starting new Ispell process" message
(advice-add 'ispell-init-process :around #'sb/inhibit-message-call-orig-fun)
(advice-add 'ispell-lookup-words :around #'sb/inhibit-message-call-orig-fun)

;; Posframes do not have unaligned rendering issues with variable `:height' unlike an overlay.
;; However, the width of the frame popup is often not enough and the right side gets cut off.
;; https://github.com/company-mode/company-mode/issues/1010
(use-package company-posframe
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after company
  :demand t
  :commands company-posframe-mode
  :diminish
  :config
  (setq company-posframe-show-metadata nil ; Difficult to distinguish the help text from completions
        company-posframe-show-indicator nil ; Hide the backends, the display is not great
        ;; Disable showing the help frame
        company-posframe-quickhelp-delay nil)
  (company-posframe-mode 1))

(use-package company-quickhelp
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after company
  :commands company-quickhelp-mode
  ;; :init (run-with-idle-timer 3 nil #'company-quickhelp-mode)
  :hook (after-init-hook . company-quickhelp-mode))

(use-package company-statistics
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after company
  :demand t
  :commands company-statistics-mode
  :config (company-statistics-mode 1))

;; Nice but slows completions. We should invoke this only at the very end of configuring `company'.
(use-package company-fuzzy
  :straight t
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :straight flx
  :after company
  :diminish (company-fuzzy-mode global-company-fuzzy-mode)
  :commands (global-company-fuzzy-mode company-fuzzy-mode)
  :demand t
  :custom
  (company-fuzzy-sorting-backend 'flx)
  (company-fuzzy-show-annotation nil "The right-hand side gets cut off")
  ;; We should not need this because the `flx' sorting accounts for the prefix
  (company-fuzzy-prefix-on-top t))

(use-package yasnippet
  :straight t
  :commands (yas-global-mode snippet-mode yas-hippie-try-expand)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook ((text-mode-hook prog-mode-hook) . yas-global-mode)
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-verbosity 0)
  :config
  (with-eval-after-load "hippie-expand"
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  (unbind-key "<tab>" yas-minor-mode-map))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet
  :demand t
  :commands yasnippet-snippets-initialize
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
  :straight t
  :after ivy
  :bind ("C-M-y" . ivy-yasnippet))

;; `amx-major-mode-commands' limits to commands that are relevant to the current major mode
;; `amx-show-unbound-commands' shows frequently used commands that have no key bindings
(use-package amx
  :straight t
  :commands amx-mode
  :hook (after-init-hook . amx-mode)
  :bind
  ;; We need this if we use `vertico' and `consult'
  (("M-x"  . execute-extended-command)
   ("<f1>" . execute-extended-command-for-buffer))
  :custom
  (amx-auto-update-interval 10 "Update the command list every n minutes"))

(use-package ivy
  :straight t
  :functions ivy-format-function-line
  :commands (ivy-read ivy-mode)
  :if (eq sb/minibuffer-completion 'ivy)
  :preface
  ;; https://github.com/abo-abo/swiper/wiki/Hiding-dired-buffers
  (defun sb/ignore-dired-buffers (str)
    "Return non-nil if STR names a Dired buffer.
  This function is intended for use with `ivy-ignore-buffers'."
    (let ((buf (get-buffer str)))
      (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))
  :hook (after-init-hook . ivy-mode)
  :config
  (setq ivy-count-format "(%d/%d) " ; Helps identify wrap around
        ivy-extra-directories nil ; Hide . and ..
        ivy-fixed-height-minibuffer t ; Distracting if the height keeps changing
        ivy-height 12
        ;; Make the height of the minibuffer proportionate to the screen
        ;; ivy-height-alist '((t
        ;;                      lambda (_caller)
        ;;                      (/ (frame-height) 2)))
        ;; We update `ivy-re-builders-alist' after loading `orderless'
        ;; ivy-re-builders-alist '((counsel-M-x       . ivy--regex-fuzzy)
        ;;                         (counsel-find-file . ivy--regex-fuzzy)
        ;;                         (t                 . ivy--regex-ignore-order))
        ivy-truncate-lines nil ; `counsel-flycheck' output gets truncated
        ivy-wrap t
        ;; Do not start searches with ^
        ivy-initial-inputs-alist nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
        ;; is way too big (30,000). Turn it down so big repos affect project
        ;; navigation less.
        ivy-sort-max-size 7500)

  (dolist (buffer
           '("TAGS" "magit-process" "*emacs*" "*xref*"
             ;; "*eldoc for use-package*" "^\\*Help\\*$" "^\\*Ibuffer\\*$" "*Warnings*"
             ;; "^\\*Compile-Log\\*$" "^\\*.+Completions\\*$" "^\\*Backtrace\\*$"
             ;; "*flycheck-posframe-buffer*" "^\\*prettier" "^\\*json*" "^\\*texlab*"
             ;; "^\\*clangd*" "^\\*shfmt*" "*company-documentation*"
             ))
    (add-to-list 'ivy-ignore-buffers buffer))

  ;; Ignore `dired' buffers from `ivy-switch-buffer'
  ;; (add-to-list 'ivy-ignore-buffers #'sb/ignore-dired-buffers)
  :diminish
  :bind
  (("C-c r"    . ivy-resume)
   ("<f3>"     . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<return>" . ivy-alt-done) ; Continue completion
   ("<left>"   . ivy-previous-line)
   ("<right>"  . ivy-next-line)))

(use-package counsel
  :straight t
  :straight amx ; `counsel' makes use of `amx' if installed
  :if (eq sb/minibuffer-completion 'ivy)
  :commands counsel-mode
  :preface
  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
  (defun sb/counsel-goto-recent-directory ()
    "Open recent directories with `dired'."
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
            (append (mapcar 'file-name-directory recentf-list)
                    (if (executable-find "fasd")
                        (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "Directories:" collection :action 'dired)))
  :bind
  (;; Counsel can use the sorting from `amx' or `smex' for `counsel-M-x'.
   ([remap execute-extended-command] . counsel-M-x)
   ([remap completion-at-point]      . counsel-company)
   ("C-M-i"                          . counsel-company)
   ([remap find-file]                . counsel-find-file)
   ;; `counsel-flycheck' shows less information than `flycheck-list-errors'
   ;; ([remap flycheck-list-errors]  . counsel-flycheck)
   ("<f1>"                           . counsel-M-x)
   ("<f2>"                           . counsel-find-file)
   ("C-c s g"                        . counsel-git-grep)
   ("C-<f9>"                         . sb/counsel-goto-recent-directory)
   ("C-c d m"                        . counsel-minor)
   ("<f9>"                           . counsel-recentf)
   ("C-c s r"                        . counsel-rg)
   ("C-c C-m"                        . counsel-mark-ring)
   ;; Enabling preview can make switching over remote buffers slow
   ("S-<f3>"                         . counsel-switch-buffer)
   ("<f4>"                           . counsel-grep-or-swiper))
  :bind* ("C-c C-j"                  . counsel-imenu)
  :diminish
  :hook (ivy-mode-hook . counsel-mode)
  :config
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable
        counsel-find-file-at-point t
        counsel-find-file-ignore-regexp (concat
                                         "\\(?:\\`[#.]\\)"
                                         "\\|\\(?:\\`.+?[#~]\\'\\)"
                                         "\\|.cb$"
                                         "\\|.cb2$"
                                         "\\|.class$"
                                         "\\|.djvu$"
                                         "\\|.doc$"
                                         "\\|.docx$"
                                         "\\|.elc$"
                                         "\\|.fdb_latexmk$"
                                         "\\|.fls$"
                                         "\\|.lof$"
                                         "\\|.lot$"
                                         "\\|.o$"
                                         "\\|.ppt$"
                                         "\\|.pptx$"
                                         "\\|.pyc$"
                                         "\\|.rel$"
                                         "\\|.rip$"
                                         "\\|.so$"
                                         "\\|.synctex$"
                                         "\\|.synctex.gz$"
                                         "\\|.toc$"
                                         "\\|.xls$"
                                         "\\|.xlsx$"
                                         "\\|tags"
                                         "\\|TAGS"
                                         "\\|GPATH"
                                         "\\|GRTAGS"
                                         "\\|GTAGS"
                                         "\\|tramp"
                                         "\\|.clangd"
                                         "\\|.cache"
                                         "\\|.metadata"
                                         "\\|.recommenders"
                                         "\\|typings"
                                         "\\|__pycache__")
        counsel-mode-override-describe-bindings t
        counsel-preselect-current-file t
        counsel-switch-buffer-preview-virtual-buffers nil ; Removes recent files and bookmarks
        counsel-yank-pop-preselect-last t
        counsel-yank-pop-separator "\n------------------------------------------\n"))

(use-package hydra
  :straight t
  :commands (hydra-default-pre hydra-keyboard-quit defhydra
                               hydra-show-hint hydra-set-transient-map
                               hydra--call-interactively-remap-maybe))

(use-package ivy-hydra ; Additional keybindings for `ivy'
  :straight t
  :after (ivy hydra)
  :demand t
  :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body))

;; Ivy is not well supported, and we are using `company-fuzzy' for sorting completion frameworks
(use-package prescient
  :straight t
  :commands prescient-persist-mode
  :hook (after-init-hook . prescient-persist-mode)
  :custom (prescient-sort-full-matches-first t))

;; We are using `company-fuzzy' for sorting completion candidates
(use-package company-prescient
  :straight t
  :after company
  :demand t
  :commands company-prescient-mode
  :disabled t
  :config
  ;; We want `capf' sort for programming modes, not with recency. This breaks support for the
  ;; `:separate' keyword in `company'.
  ;; (setq company-prescient-sort-length-enable nil)
  (company-prescient-mode 1))

(use-package all-the-icons-ivy
  :straight t
  :after ivy
  :demand t
  :commands all-the-icons-ivy-setup
  :config (all-the-icons-ivy-setup))

(use-package orderless
  :straight t
  :after (:any ivy vertico)
  :demand t
  :defines orderless-component-separator
  :functions sb/just-one-face
  :config
  (with-eval-after-load "ivy"
    ;; (defvar ivy-re-builders-alist)
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

  (setq completion-styles '(orderless partial-completion) ; initials, basic, emacs22
        orderless-matching-styles '(orderless-regexp)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic-remote orderless partial-completion))
                                        ;; (minibuffer (initials))))
                                        )
        ))




;; Enable before `ivy-rich-mode' for better performance. The new transformers (file permissions)
;; seem an overkill, and it hides long file names.
(use-package all-the-icons-ivy-rich
  :straight t
  :straight ivy-rich
  :commands all-the-icons-ivy-rich-mode
  :if (display-graphic-p)
  :hook (ivy-mode-hook . all-the-icons-ivy-rich-mode)
  :custom (all-the-icons-ivy-rich-icon-size 0.9))

(use-package ivy-rich
  :straight t
  :commands (ivy-rich-modify-column ivy-rich-set-columns ivy-rich-modify-columns)
  :after (ivy counsel) ; We do not enable `all-the-icons-ivy-rich' in TUI mode
  :preface
  ;; Adapted from
  ;; https://github.com/tshu-w/.emacs.d/blob/master/lisp/editor-completion.el
  (defun sb/ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))

  (defun sb/ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))
  :init (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (if (display-graphic-p)
      (ivy-rich-set-columns 'counsel-find-file
                            '((all-the-icons-ivy-rich-file-icon)
                              (ivy-rich-candidate    (:width 0.70))
                              (sb/ivy-rich-file-user (:width 15 :face font-lock-doc-face))
                              (sb/ivy-rich-file-size (:width 10 :align right
                                                             :face font-lock-doc-face))))
    (ivy-rich-set-columns 'counsel-find-file
                          '((ivy-rich-candidate    (:width 0.70))
                            (sb/ivy-rich-file-user (:width 15 :face font-lock-doc-face))
                            (sb/ivy-rich-file-size (:width 10 :align right
                                                           :face font-lock-doc-face)))))

  ;; Increase the width to see the major mode clearly
  (ivy-rich-modify-columns 'ivy-switch-buffer
                           '((ivy-rich-switch-buffer-size (:align right))
                             (ivy-rich-switch-buffer-major-mode (:width 16 :face error))
                             (ivy-rich-switch-buffer-project (:width 0.24 :face success))))

  (ivy-rich-set-columns 'counsel-recentf
                        '((file-name-nondirectory (:width 0.24))
                          (ivy-rich-candidate (:width 0.75)))))




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

;; This is different from `whitespace-cleanup-mode' since this is unconditional
(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (setq delete-trailing-lines t) ; "M-x delete-trailing-whitespace" deletes trailing lines
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

;; Call `whitespace-cleanup' only if the initial buffer was clean. This mode works on the entire
;; file unlike `ws-butler'. To enable the mode for an entire project, set `whitespace-cleanup-mode'
;; to `t' in the `.dir-locals.el' file.
(use-package whitespace-cleanup-mode
  :straight t
  :disabled t
  :diminish
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  :config
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)
  :custom
  (whitespace-cleanup-mode-preserve-point t))






(use-package number-separator
  :straight nil
  :load-path "extras"
  :commands number-separator-mode
  :disabled t
  :diminish
  :custom
  (number-separator ",")
  (number-separator-interval 3)
  (number-separator-ignore-threshold 4)
  (number-separator-decimal-char "."))

;; (declare-function sb/sshlist "private")

;; (progn
;;   (defun sb/ivy-tramp ()
;;     "Invoke remote hosts with ivy and tramp."
;;     (interactive)
;;     (counsel-find-file (ivy-read "Remote Tramp targets: " (sb/sshlist))))

;;   (bind-key "C-c d t" #'sb/ivy-tramp))

(use-package counsel-tramp
  :straight t
  :if (eq sb/minibuffer-completion 'ivy)
  :bind ("C-c d t" . counsel-tramp))

;; TODO: SSH into Gcloud
;; https://gist.github.com/jackrusher/36c80a2fd6a8fe8ddf46bc7e408ae1f9
;; Make sure you have set your default project with:
;; "gcloud config set project <project-name>"
;; "C-x C-f /gcssh:compute-instance:/path/to/filename.clj"

;; LATER: Can we shorten long Tramp file names? This does not work with Tramp.
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/data/swarnendu/" . "/vindhya/data/swarnendu/"))
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/home/swarnendu/" . "/vindhya/home/swarnendu/"))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(use-package popwin
  :straight t
  :commands popwin-mode
  :hook (after-init-hook . popwin-mode)
  :config
  (defvar popwin:special-display-config-backup popwin:special-display-config)

  (push '("*Help*"              :noselect t)   popwin:special-display-config)
  (push '(compilation-mode      :noselect t)   popwin:special-display-config)
  (push '("*Compile-Log*"       :noselect t)   popwin:special-display-config)
  (push '("*manage-minor-mode*" :noselect t)   popwin:special-display-config)
  (push '("*Paradox Report*"    :noselect t)   popwin:special-display-config)
  (push '("*Selection Ring:")                  popwin:special-display-config)
  (push '("*Flycheck checkers*" :noselect nil) popwin:special-display-config)
  (push '(flycheck-error-list-mode :noselect nil) popwin:special-display-config)
  (push '("*ripgrep-search*"    :noselect nil) popwin:special-display-config)
  (push '("^\*magit:.+\*$"      :noselect nil) popwin:special-display-config)
  (push '("*xref*"              :noselect nil) popwin:special-display-config)
  (push '(helpful-mode          :noselect t)   popwin:special-display-config)
  (push "*Shell Command Output*"               popwin:special-display-config)
  (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
  (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
  (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  (add-to-list 'popwin:special-display-config '("*Apropos*"))
  (add-to-list 'popwin:special-display-config '("*Warnings*"))
  (add-to-list 'popwin:special-display-config '("*prettier errors*"))
  (add-to-list 'popwin:special-display-config '("*explain-pause-top*"))
  (add-to-list 'popwin:special-display-config '(ivy-occur-grep-mode))
  (add-to-list 'popwin:special-display-config '(deadgrep-mode))
  (add-to-list 'popwin:special-display-config '("*lsp session*")))

;; Learn about display actions, see [[info:elisp#Display Action Functions]]
;; https://emacs.stackexchange.com/questions/22499/how-can-i-tell-emacs-to-always-open-help-buffers-in-the-current-window
(add-to-list 'display-buffer-alist '("*Faces*"                  display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Flycheck checkers*"      display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Flycheck errors*"        display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Help*"                   display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Bufler*"                 display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*manage-minor-mode*"      display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*use-package statistics*" display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*deadgrep*"            display-buffer-same-window))
;; Open shell in same window.
(add-to-list 'display-buffer-alist `(,(regexp-quote "*shell")   display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Compile-Log\\*"       display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Warnings\\*"          display-buffer-same-window))
(add-to-list 'display-buffer-alist '("^\\*Backtrace\\*"         display-buffer-same-window))
(add-to-list 'display-buffer-alist '("*Async Shell Command*"    display-buffer-no-window))

;; ;; Do not popup the *Async Shell Command* buffer
;; (add-to-list 'display-buffer-alist
;;              (cons "\\*Async Shell Command\\*.*"
;;                    (cons #'display-buffer-no-window nil)))





(use-package crux
  :straight t
  :bind
  (("C-c d i" . crux-ispell-word-then-abbrev)
   ("<f12>"   . crux-kill-other-buffers)
   ("C-c d s" . crux-sudo-edit)
   ("C-a"     . crux-move-beginning-of-line)))

;; This package disables the mouse completely which is an extreme.
(use-package disable-mouse
  :straight t
  :if (display-mouse-p)
  :commands global-disable-mouse-mode
  :diminish disable-mouse-global-mode
  :hook (after-init-hook . global-disable-mouse-mode))

;; Move the cursor from the line of view
(use-package avoid
  :straight nil
  :commands mouse-avoidance-mode
  :if (display-mouse-p)
  :init (mouse-avoidance-mode 'banish))

(use-package apt-sources-list
  :straight t
  :commands apt-sources-list-mode)

(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode
  :hook ((prog-mode-hook latex-mode-hook LaTeX-mode-hook org-src-mode-hook) . rainbow-delimiters-mode))

(use-package ssh-config-mode
  :straight t
  :commands (ssh-config-mode ssh-known-hosts-mode ssh-authorized-keys-mode turn-on-font-lock)
  :hook (ssh-config-mode-hook . turn-on-font-lock))

(use-package ace-window
  :straight t
  :bind ([remap other-window] . ace-window))

(use-package windmove ; "Shift + direction" arrows
  :straight nil
  :commands windmove-default-keybindings
  :init (windmove-default-keybindings)
  :config
  ;; Wrap around at edges
  (setq windmove-wrap-around t))

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `after-save-hook' and leads to auto-formatters being invoked more frequently. We do not need this
;; given that we have `auto-save-visited-mode' enabled.
(use-package super-save
  :straight t
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
  :straight t
  :commands avy-setup-default
  :bind
  (("M-b"   . avy-goto-word-1)
   ("C-'"   . avy-goto-char-timer) ; Does not work with TUI, but works with Alacritty
   ("M-g c" . avy-goto-char-timer) ; TODO: Reuse the keybinding
   ("C-/"   . avy-goto-line) ; Does not work with TUI, but works with Alacritty
   ;; TODO: Reuse the keybinding
   ("M-g l" . avy-goto-line)))

(use-package ace-jump-buffer
  :straight t
  :bind ("C-b" . ace-jump-buffer)
  :config
  (setq ajb-max-window-height 30
        ajb-sort-function 'bs--sort-by-name))

;; This package adds a "C-'" binding to the Ivy minibuffer that uses Avy
(use-package ivy-avy
  :straight t
  :after ivy
  :bind
  (:map ivy-minibuffer-map
        ("C-'"   . ivy-avy) ; Does not work with TUI, but works with Alacritty
        ;; TODO: Reuse the keybinding
        ("M-g l" . ivy-avy)))

(use-package bookmark
  :straight nil)

(use-package bm
  :straight t
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
  :straight t
  :commands esup
  :if (bound-and-true-p sb/debug-init-file))

(use-package bug-hunter
  :straight t
  :disabled t
  :if (bound-and-true-p sb/debug-init-file)
  :commands (bug-hunter-init-file bug-hunter-file))

(use-package explain-pause-mode
  :straight nil
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



;; LATER: Prettier times out setting up the process on a remote machine. I am using `format-all'
;; for now.
;; https://github.com/jscheid/prettier.el/issues/84
(use-package prettier
  :if (executable-find "prettier")
  :straight t
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



(use-package highlight-doxygen
  :straight t
  :commands highlight-doxygen-global-mode
  :init (highlight-doxygen-global-mode))



(use-package eldoc
  :straight nil
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



;; `eldoc-box-hover-at-point-mode' blocks the view because it shows up at point.
(use-package eldoc-box
  :straight t
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :hook (eldoc-mode-hook . eldoc-box-hover-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-fringe-use-same-bg nil)
  :diminish eldoc-box-hover-mode eldoc-box-hover-at-point-mode)


(declare-function ht-merge "ht")

;; Registering `lsp-format-buffer' makes sense only if the server is active. We may not always want
;; to format unrelated files and buffers (e.g., commented YAML files in out-of-project locations).
(use-package lsp-mode
  :straight t
  :straight spinner
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
  :straight t
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
  :straight t
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
  :straight t
  :after (lsp-mode ivy)
  :demand t
  :bind
  (:map lsp-command-map
        ("G" . lsp-ivy-global-workspace-symbol)
        ("W" . lsp-ivy-workspace-symbol)))

(use-package dap-mode
  :straight t
  :commands (dap-debug dap-hydra dap-mode dap-ui-mode)
  :hook
  ((lsp-mode-hook . dap-mode)
   (lsp-mode-hook . dap-ui-mode)))

(use-package docstr
  :straight t
  :diminish
  :hook ((c++-mode-hook python-mode-hook java-mode-hook) . docstr-mode))

(use-package cc-mode
  :straight nil
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
  :straight t
  :commands modern-c++-font-lock-mode
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode-hook . modern-c++-font-lock-mode))

(use-package cuda-mode
  :straight t
  :commands cuda-mode
  :mode
  (("\\.cu\\'"  . c++-mode)
   ("\\.cuh\\'" . c++-mode)))

(use-package opencl-mode
  :straight t
  :commands opencl-mode
  :mode "\\.cl\\'")

(use-package cmake-mode
  :straight t
  :if (executable-find "cmake")
  :commands cmake-mode
  :mode "\(CMakeLists\.txt|\.cmake\)$"
  :hook
  (cmake-mode-hook . (lambda ()
                       (make-local-variable 'lsp-disabled-clients)
                       (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                       (spell-fu-mode -1)
                       (flyspell-mode -1)
                       (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "cmake-language-server")
    :major-modes '(cmake-mode)
    :remote? t
    :server-id 'cmakels-r)))

(use-package cmake-font-lock
  :straight t
  :commands cmake-font-lock-activate
  :hook (cmake-mode-hook . cmake-font-lock-activate))

(use-package python
  :straight nil
  :hook (python-mode-hook . lsp-deferred)
  :mode ("SCon\(struct\|script\)$" . python-mode)
  :bind
  (:map python-mode-map
        ;; Assigning a keybinding such as "C-[" is involved, `[' is treated as `meta'
        ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c
        ("M-{"   . python-nav-backward-block)
        ("M-}"   . python-nav-forward-block)
        ("C-c <" . python-indent-shift-left)
        ("C-c >" . python-indent-shift-right))
  :custom
  (python-shell-completion-native-enable nil "Disable readline based native completion")
  (python-fill-docstring-style 'django)
  (python-indent-guess-indent-offset-verbose nil "Remove guess indent python message")
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4)
  (python-shell-exec-path "python3")
  (python-shell-interpreter "python3")
  :config
  (setenv "PYTHONPATH" "python3")

  ;; (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint))))))

  ;; (setq auto-mode-alist (append '(("SConstruct\\'" . python-mode)
  ;;                                 ("SConscript\\'" . python-mode))
  ;;                               auto-mode-alist))
  )

(use-package python-docstring
  :straight t
  :after python-mode
  :demand t
  :commands (python-docstring-mode python-docstring-install)
  :diminish
  :config (python-docstring-install))

(use-package pip-requirements
  :straight t
  :commands pip-requirements-mode)

(use-package pyvenv
  :straight t
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
  :straight t
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :commands py-isort-before-save
  :hook
  (python-mode-hook . (lambda ()
                        (add-hook 'before-save-hook #'py-isort-before-save)))
  :custom
  (py-isort-options '("-l 100")))

;; "pyright --createstub pandas"
(use-package lsp-pyright
  :straight t
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
  :straight t
  :diminish yapf-mode
  :if (and (eq sb/python-langserver 'pyright) (executable-find "yapf"))
  :commands yapf-mode
  :hook (python-mode-hook . yapf-mode))

(use-package cperl-mode
  :straight nil
  :mode ("latexmkrc\\'")
  :hook (cperl-mode-hook . lsp-deferred)
  :config
  ;; Prefer CPerl mode to Perl mode
  (fset 'perl-mode 'cperl-mode)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     (lambda ()
                       (list lsp-perl-language-server-path
                             "-MPerl::LanguageServer" "-e"
                             "Perl::LanguageServer::run" "--"
                             (format "--port %d --version %s"
                                     lsp-perl-language-server-port
                                     lsp-perl-language-server-client-version))))
    :major-modes '(perl-mode cperl-mode)
    :remote? t
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "perl"))))
    :priority -1
    :server-id 'perlls-r)))

;; Try to delete `lsp-java-workspace-dir' if the JDTLS fails
(use-package lsp-java
  :straight t
  :commands (lsp-java-organize-imports lsp-java-build-project
                                       lsp-java-update-project-configuration
                                       lsp-java-actionable-notifications
                                       lsp-java-update-user-settings
                                       lsp-java-update-server
                                       lsp-java-generate-to-string
                                       lsp-java-generate-equals-and-hash-code
                                       lsp-java-generate-overrides
                                       lsp-java-generate-getters-and-setters
                                       lsp-java-type-hierarchy
                                       lsp-java-dependency-list
                                       lsp-java-extract-to-constant
                                       lsp-java-add-unimplemented-methods
                                       lsp-java-create-parameter
                                       lsp-java-create-field
                                       lsp-java-create-local
                                       lsp-java-extract-method
                                       lsp-java-add-import)
  :hook
  (java-mode-hook . (lambda ()
                      (setq-default c-basic-offset 4
                                    c-set-style "java")
                      (lsp-deferred)))
  :custom
  (lsp-java-inhibit-message t)
  ;; Requires Java 11+, Java 11 is the LTS
  (lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java")
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-format-settings-profile "Swarnendu")
  (lsp-java-format-settings-url (expand-file-name
                                 "github/dotfiles/java/eclipse-format-swarnendu.xml"
                                 sb/user-home)))

(use-package ant
  :straight t
  :commands (ant ant-clean ant-compile ant-test))

(use-package autodisass-java-bytecode ; Can disassemble ".class" files from within jars
  :straight t
  :commands autodisass-java-bytecode
  :mode "\\.class\\'")

(use-package groovy-mode ; Syntax highlighting for Gradle files
  :straight t
  :commands groovy-mode
  :mode "\\.gradle\\'")

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

(use-package sh-script ; Shell script mode
  :straight nil
  :mode
  (("\\.zsh\\'"   . sh-mode)
   ("\\bashrc\\'" . sh-mode))
  :hook (sh-mode-hook . lsp-deferred)
  :custom
  (sh-basic-offset 2)
  (sh-indent-after-continuation 'always)
  (sh-indent-comment t "Indent comments as a regular line")
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("bash-language-server" "start"))
    :major-modes '(sh-mode)
    :remote? t
    :server-id 'bashls-r)))

(use-package fish-mode
  :straight t
  :mode "\\.fish\\'"
  :interpreter "fish"
  :commands (fish-mode fish_indent-before-save)
  :hook
  (fish-mode-hook . (lambda ()
                      (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package company-shell
  :straight t
:if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after (:any sh-mode fish-mode)
  :demand t
  :defines company-shell-delete-duplictes
  :commands (company-shell company-shell-env company-fish-shell)
  :custom (company-shell-delete-duplictes t))

(use-package shfmt
  :straight t
  :hook (sh-mode-hook . shfmt-on-save-mode)
  :custom
  ;; p: Posix, ci: indent case labels, i: indent with spaces
  (shfmt-arguments '("-i" "4" "-p" "-ci")))

;; The following section helper ensures that files are given `+x' permissions when they are saved,
;; if they contain a valid shebang line
(use-package executable
  :straight nil
  :commands (executable-make-buffer-file-executable-if-script-p)
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

;; Remove `vc-refresh-state' if we are not using `vc', i.e., `vc-handled-backends' is nil
(use-package vc
  :straight nil
  :init
  (if (boundp 'vc-handled-backends)
      (add-hook 'find-file-hook #'vc-refresh-state)
    (remove-hook 'find-file-hook #'vc-refresh-state)))

(use-package magit
  :straight t
  :commands magit-display-buffer-fullframe-status-v1
  :bind
  (("C-x g"   . magit-status)
   ("C-c M-g" . magit-file-dispatch)
   ("C-x M-g" . magit-dispatch))
  :custom
  ;; Open the status buffer in a full frame
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; Suppress the message we get about "Turning on magit-auto-revert-mode" when loading Magit
  (magit-no-message '("Turning on magit-auto-revert-mode..."))
  ;; https://irreal.org/blog/?p=8877
  (magit-section-initial-visibility-alist '((stashes   . show)
                                            (untracked . show)
                                            (unpushed  . show)
                                            (unpulled  . show)))
  ;; :config
  ;; These give a performance boost to Magit
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

  (use-package magit-diff
    :straight nil
    :demand t
    :custom
    (magit-diff-refine-hunk  t)
    (magit-diff-highlight-trailing nil)
    (magit-diff-paint-whitespace   nil)))

(use-package git-modes
  :straight t
  :commands gitignore-mode gitattributes-mode gitconfig-mode)

(use-package git-gutter
  :straight t
  :if (unless (boundp 'vc-handled-backends))
  :disabled t
  :commands global-git-gutter-mode
  :diminish
  :bind
  (("C-x p" . git-gutter:previous-hunk)
   ("C-x n" . git-gutter:next-hunk))
  :hook (after-init-hook . global-git-gutter-mode)
  :custom
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  (git-gutter:modified-sign " ")
  (git-gutter:update-interval 1)
  :config
  ;; https://github.com/syl20bnr/spacemacs/issues/10555
  ;; https://github.com/syohex/emacs-git-gutter/issues/24
  (git-gutter:disabled-modes '(fundamental-mode org-mode image-mode doc-view-mode pdf-view-mode)))

;; Diff-hl looks nicer than git-gutter, based on `vc'
(use-package diff-hl
  :straight t
  :if (boundp 'vc-handled-backends)
  :commands (diff-hl-magit-pre-refresh diff-hl-magit-post-refresh
                                       diff-hl-dired-mode-unless-remote global-diff-hl-mode)
  :custom
  (diff-hl-draw-borders nil "Highlight without a border looks nicer")
  :config
  ;; Display margin since the fringe is unavailable in TTY
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  :hook
  ((magit-post-refresh-hook . diff-hl-magit-post-refresh)
   (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
   (dired-mode-hook         . diff-hl-dired-mode-unless-remote)
   (diff-hl-mode-hook       . diff-hl-flydiff-mode)
   (after-init-hook         . global-diff-hl-mode)))

(use-package git-commit
  :straight t
  :commands git-commit-turn-on-flyspell
  :hook (git-commit-setup-hook . git-commit-turn-on-flyspell)
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line)))

;; Use the minor mode `smerge-mode' to move between conflicts and resolve them
(use-package smerge-mode
  :straight nil
  :after hydra
  :commands (smerge-next smerge-prev smerge-auto-leave
                         smerge-keep-base smerge-keep-upper
                         smerge-keep-lower smerge-keep-all
                         smerge-diff-base-lower
                         smerge-diff-base-upper
                         smerge-diff-upper-lower smerge-refine
                         smerge-combine-with-next smerge-resolve)
  :preface
  (defun sb/enable-smerge-maybe ()
    "Enable smerge automatically based on conflict markers."
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1)))))

  (defun sb/enable-smerge-maybe2 ()
    "Enable `smerge-mode' automatically."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))
  :init
  (add-hook 'find-file-hook #'sb/enable-smerge-maybe2 :append)
  (add-hook 'magit-diff-visit-file-hook (lambda nil
                                          (when smerge-mode
                                            (sb/smerge-hydra/body))))
  :bind-keymap ("C-c v" . smerge-command-prefix)
  :bind
  (:map smerge-mode-map
        ("M-g n"   . smerge-next)
        ("M-g p"   . smerge-prev)
        ("M-g k c" . smerge-keep-current)
        ("M-g k u" . smerge-keep-upper)
        ("M-g k l" . smerge-keep-lower)
        ("M-g k b" . smerge-keep-base)
        ("M-g k a" . smerge-keep-all)
        ("M-g e"   . smerge-ediff)
        ("M-g K"   . smerge-kill-current)
        ("M-g m"   . smerge-context-menu)
        ("M-g M"   . smerge-popup-context-menu)))

(use-package ediff
  :straight nil
  :after magit
  :demand t
  :defines ediff-window-setup-function
  :commands (ediff-setup-windows-plain ediff-set-diff-options)
  :custom
  ;; Change default ediff style: do not start another frame with `ediff-setup-windows-default'
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split windows horizontally in ediff (instead of vertically)
  (ediff-split-window-function #'split-window-horizontally)
  :config
  (ediff-set-diff-options 'ediff-diff-options "-w"))

(use-package bat-mode
  :straight nil
  :commands bat-mode
  :mode
  (("\\.bat\\'" . bat-mode)
   ("\\.cmd\\'" . bat-mode)))

(use-package web-mode
  :straight nil
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
  :straight t
  :defines emmet-move-cursor-between-quote
  :commands emmet-mode
  :hook ((web-mode-hook css-mode-hook html-mode-hook) . emmet-mode)
  :custom (emmet-move-cursor-between-quote t))

(use-package rainbow-mode
  :straight t
  :commands rainbow-mode
  :hook ((css-mode-hook html-mode-hook web-mode-hook) . rainbow-mode))

(use-package nxml-mode
  :straight nil
  :commands nxml-mode
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$")
  :hook
  (nxml-mode-hook . (lambda ()
                      ;; `xml-mode' is derived from `text-mode', so disable grammar and spell
                      ;; checking.
                      (make-local-variable 'lsp-disabled-clients)
                      (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                      (spell-fu-mode -1)
                      (flyspell-mode -1)
                      (lsp-deferred)))
  :custom
  (nxml-auto-insert-xml-declaration-flag t)
  (nxml-slash-auto-complete-flag t)
  :config
  (fset 'xml-mode 'nxml-mode)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("java" "-jar" lsp-xml-jar-file))
    :major-modes '(xml-mode nxml-mode)
    :remote? t
    :server-id 'xmlls-r)))

;; The advantage with `flycheck-grammarly' over `lsp-grammarly' is that you need not set up lsp
;; support, so you can use it anywhere. But `flycheck-grammarly' does not support a PRO Grammarly
;; account. We only need this package for checking text in "*scratch*" buffer.
(use-package flycheck-grammarly
  :straight t
  :after flycheck
  :defines flycheck-grammarly-check-time
  :demand t
  :config
  (setq flycheck-grammarly-check-time 3
        ;; Remove from the beginning of the list `flycheck-checkers' and append to the end
        flycheck-checkers (delete 'grammarly flycheck-checkers))

  (add-to-list 'flycheck-checkers 'grammarly t))

;; https://languagetool.org/download/LanguageTool-stable.zip
(use-package flycheck-languagetool
  :straight t
  :defines (flycheck-languagetool-commandline-jar flycheck-languagetool-check-time)
  :hook (text-mode-hook . flycheck-languagetool-setup)
  :init
  (setq flycheck-languagetool-server-jar (no-littering-expand-etc-file-name
                                          "languagetool-server.jar")
        flycheck-checkers (delete 'languagetool flycheck-checkers)
        flycheck-languagetool-check-time 3)

  (add-to-list 'flycheck-checkers 'languagetool t))

;; Most likely, `org', `markdown', and `latex' files will be in directories that can use LSP
;; support. We only need to enable `flycheck-grammarly' support for the "*scratch*" buffer which is
;; in `text-mode'.

;; org -> grammarly -> languagetool
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (flycheck-select-checker 'org-lint)
;;             (when (featurep 'flycheck-grammarly)
;;               (flycheck-add-next-checker 'org-lint 'grammarly))
;;             (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
;;               (flycheck-add-next-checker 'grammarly 'languagetool))
;;             (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
;;               (flycheck-add-next-checker 'org-lint 'languagetool))))

;; We only limit to "*scratch*" buffer since we can use `grammarly' and `ltex' for directories.
(add-hook 'text-mode-hook
          (lambda ()
            (when (and (featurep 'flycheck-grammarly) (string= (buffer-name) "*scratch*"))
              (flycheck-select-checker 'grammarly))
            ;; (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
            ;;   (flycheck-add-next-checker 'grammarly 'languagetool))
            ;; (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
            ;;   (flycheck-select-checker 'languagetool))
            ))

;; `markdown-mode' is derived from `text-mode'
;; markdown-markdownlint-cli -> grammarly -> languagetool
;; (add-hook 'markdown-mode-hook
;;           (lambda()
;;             (flycheck-select-checker 'markdown-markdownlint-cli)
;;             (when (featurep 'flycheck-grammarly)
;;               ;; (make-local-variable 'flycheck-error-list-minimum-level)
;;               ;; (setq flycheck-error-list-minimum-level 'warning
;;               ;;       flycheck-navigation-minimum-level 'warning)
;;               ;; (flycheck-add-next-checker 'markdown-markdownlint-cli '(warning . grammarly) 'append)
;;               (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly))
;;             ;; (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
;;             ;;   (flycheck-add-next-checker 'grammarly 'languagetool))
;;             ;; (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
;;             ;;   (flycheck-add-next-checker 'markdown-markdownlint-cli 'languagetool))
;;             ))

;; (dolist (hook '(LaTex-mode-hook latex-mode-hook))
;;   (add-hook hook (lambda ()
;;                    (flycheck-select-checker 'tex-chktex)
;;                    (when (featurep 'flycheck-grammarly)
;;                      (flycheck-add-next-checker 'tex-chktex 'grammarly))
;;                    (when (and (featurep 'flycheck-grammarly) (featurep 'flycheck-languagetool))
;;                      (flycheck-add-next-checker 'grammarly 'languagetool))
;;                    (when (and (not (featurep 'flycheck-grammarly)) (featurep 'flycheck-languagetool))
;;                      (flycheck-add-next-checker 'tex-chktex 'languagetool)))))

;; We need to enable lsp workspace to allow `lsp-grammarly' to work, which makes it ineffective for
;; temporary text files. However, `lsp-grammarly' supports PRO Grammarly accounts. If there are
;; failures, then try logging out of Grammarly and logging in again. Make sure to run "M-x
;; keytar-install".
(use-package lsp-grammarly
  :straight t
  :straight keytar
  :disabled t
  :defines (lsp-grammarly-active-modes lsp-grammarly-user-words)
  :commands (lsp-grammarly--server-command lsp-grammarly--init
                                           lsp-grammarly--get-credentials lsp-grammarly--get-token
                                           lsp-grammarly--store-token lsp-grammarly--show-error
                                           lsp-grammarly--update-document-state)
  :hook
  ((text-mode-hook markdown-mode-hook org-mode-hook LaTeX-mode-hook) .
   (lambda ()
     (require 'lsp-grammarly)
     (lsp-deferred)))
  :config
  ;; (setq lsp-grammarly-active-modes '(text-mode latex-mode
  ;;                                              LaTeX-mode org-mode markdown-mode gfm-mode)
  ;;       lsp-grammarly-user-words '(
  ;;                                  ))

  (defvar lsp-grammarly-active-modes)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection #'lsp-grammarly--server-command)
    :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-grammarly-active-modes))
    :priority -1
    :remote? t
    :add-on? t
    :server-id 'grammarly-r
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure 'grammarly-ls callback error-callback))
    :after-open-fn #'lsp-grammarly--init
    :async-request-handlers
    (ht ("$/getCredentials" #'lsp-grammarly--get-credentials)
        ("$/getToken" #'lsp-grammarly--get-token)
        ("$/storeToken" #'lsp-grammarly--store-token)
        ("$/showError" #'lsp-grammarly--show-error)
        ("$/updateDocumentState" #'lsp-grammarly--update-document-state)))))

(use-package lsp-ltex
  :straight t
  :defines (lsp-ltex-enabled lsp-ltex-check-frequency lsp-ltex-dictionary lsp-ltex-java-path)
  :commands (lsp-ltex--downloaded-extension-path lsp-ltex--execute)
  :hook
  ((text-mode-hook markdown-mode-hook org-mode-hook LaTeX-mode-hook) .
   (lambda ()
     (require 'lsp-ltex)
     (lsp-deferred)))
  :init
  (setq lsp-ltex-check-frequency "save"
        ;; lsp-ltex-dictionary ("microbenchmarks")
        lsp-ltex-java-path "/usr/lib/jvm/java-11-openjdk-amd64"
        lsp-ltex-version "15.2.0")
  :config
  ;; https://github.com/ggbaker/doom-emacs-config/blob/f977ee6f33ef2d19b577e38a81b32af43ced6df5/config.el
  ;; Disable spell checking since we cannot get `lsp-ltex' to work with custom dict words
  (setq lsp-ltex-disabled-rules
        #s(hash-table size 30 data
                      ("en-US" ["MORFOLOGIK_RULE_EN_US"])
                      ("en-US" ["WHITESPACE_RULE"])))

  (defvar lsp-ltex-active-modes)

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     "/home/swarnendu/.emacs.d/var/lsp/server/ltex-ls/latest/bin/ltex-ls")
    :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-ltex-active-modes))
    :priority -2
    :add-on? t
    :remote? t
    :server-id 'ltex-r
    :download-server-fn
    (lambda (_client _callback error-callback _update?)
      (lsp-package-ensure
       'ltex-ls
       (lambda ()
         (let ((dest (f-dirname (lsp-ltex--downloaded-extension-path))))
           (unless (lsp-ltex--execute "tar" "-xvzf" (lsp-ltex--downloaded-extension-path)
                                      "-C" dest)
             (error "Error during the unzip process: tar"))))
       error-callback)))))

;; `lsp-latex' provides better support for the `texlab' server compared to `lsp-tex'. On the other
;; hand, `lsp-tex' supports `digestif'. `lsp-latex' does not require `auctex'. However, the server
;; performance is very poor, so I continue to prefer `auctex'.

(use-package lsp-latex
  :straight t
  :defines (lsp-latex-bibtex-formatter lsp-latex-latex-formatter
                                       lsp-latex-bibtex-formatter-line-length
                                       lsp-latex-chktex-on-open-and-save
                                       lsp-latex-build-on-save
                                       lsp-latex-build-is-continuous
                                       lsp-latex-build-args
                                       lsp-latex-diagnostics-delay)
  :hook
  (latex-mode-hook . (lambda()
                       (require 'lsp-latex)
                       (lsp-deferred)))
  :custom
  (lsp-latex-bibtex-formatter             "latexindent")
  (lsp-latex-latex-formatter              "latexindent")
  (lsp-latex-bibtex-formatter-line-length sb/fill-column)
  (lsp-latex-chktex-on-open-and-save      t)
  (lsp-latex-build-is-continuous          t)
  ;; Delay time in milliseconds before reporting diagnostics
  (lsp-latex-diagnostics-delay            2000)
  :config
  (add-to-list 'lsp-latex-build-args "-c")
  (add-to-list 'lsp-latex-build-args "-pvc")

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "texlab")
    :major-modes '(tex-mode latex-mode LaTeX-mode bibtex-mode)
    :remote? t
    :server-id 'texlab-r)))

;; Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides the tex
;; package.
(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :defines (tex-fontify-script font-latex-fontify-script
                               font-latex-fontify-sectioning
                               TeX-syntactic-comment
                               TeX-save-query LaTeX-item-indent
                               LaTeX-syntactic-comments
                               LaTeX-fill-break-at-separators)
  :functions (TeX-active-process)
  :commands (TeX-active-process TeX-save-document tex-site
                                LaTeX-mode LaTeX-math-mode
                                TeX-PDF-mode
                                TeX-source-correlate-mode
                                TeX-active-process
                                TeX-command-menu
                                TeX-revert-document-buffer
                                TeX-master-file
                                TeX-next-error)
  :hook
  (((latex-mode-hook LaTeX-mode-hook) . LaTeX-math-mode)
   ((latex-mode-hook LaTeX-mode-hook) . TeX-PDF-mode) ; Use `pdflatex'
   ((latex-mode-hook LaTeX-mode-hook) . TeX-source-correlate-mode)
   (LaTeX-mode-hook . turn-on-auto-fill))
  :config
  (setq TeX-auto-save t ; Enable parse on save, stores parsed information in an `auto' directory
        TeX-auto-untabify t ; Remove all tabs before saving
        TeX-clean-confirm nil
        ;; Automatically insert braces after typing ^ and _ in math mode
        TeX-electric-sub-and-superscript t
        TeX-electric-math t ; Inserting $ completes the math mode and positions the cursor
        TeX-parse-self t ; Parse documents
        TeX-quote-after-quote nil ; Allow original LaTeX quotes
        TeX-save-query nil ; Save buffers automatically when compiling
        TeX-source-correlate-method 'synctex
        ;; Do not start the emacs server when correlating sources
        TeX-source-correlate-start-server t
        TeX-syntactic-comment t
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        LaTeX-item-indent 0 ; Indent lists by two spaces
        LaTeX-syntactic-comments t
        LaTeX-fill-break-at-separators nil ; Do not insert line-break at inline math
        tex-fontify-script nil ; Avoid raising of superscripts and lowering of subscripts
        ;; Avoid superscripts and subscripts from being displayed in a different font size
        font-latex-fontify-script nil
        ;; Avoid emphasizing section headers
        font-latex-fontify-sectioning 1.0)

  (setq-default TeX-master nil) ; Query for master file

  ;; Revert PDF buffer after TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  :bind
  ("C-c x q" . TeX-insert-quote))

(use-package bibtex
  :straight nil
  :hook
  ((bibtex-mode-hook . turn-on-auto-revert-mode)
   (bibtex-mode-hook . lsp-deferred))
  :custom
  (bibtex-align-at-equal-sign     t)
  (bibtex-maintain-sorted-entries t))

(use-package ivy-bibtex
  :if (eq sb/minibuffer-completion 'ivy)
  :bind ("C-c x b" . ivy-bibtex)
  :custom (ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package bibtex-completion
  :straight nil
  :after ivy-bibtex
  :demand t
  :custom
  (bibtex-completion-cite-default-as-initial-input t)
  (bibtex-completion-cite-prompt-for-optional-arguments nil)
  (bibtex-completion-display-formats '((t . "${author:24} ${title:*} ${=key=:16} ${=type=:12}"))))

;; Reftex is useful to view ToC even with LSP support
;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
(use-package reftex
  :straight nil
  :commands (reftex-get-bibfile-list bibtex-parse-keys
                                     reftex-mode
                                     reftex-toc-rescan
                                     reftex-toc-Rescan
                                     reftex-default-bibliography)
  :diminish
  :hook
  (;; TODO: Rescan the entire document, not only the current file (`reftex-toc-rescan'), to be
   ;; consistent but this is expensive. We can use an idle timer.
   (reftex-toc-mode-hook . reftex-toc-Rescan)
   ((LaTeX-mode-hook latex-mode-hook) . reftex-mode))
  :bind
  (("C-c ["   . reftex-citation)
   ("C-c )"   . reftex-reference)
   ("C-c ("   . reftex-label)
   ("C-c ="   . reftex-toc))
  :preface
  (defun sb/get-bibtex-keys (file)
    (with-current-buffer (find-file-noselect file)
      (mapcar 'car (bibtex-parse-keys))))

  (defun sb/reftex-add-all-bibitems-from-bibtex ()
    (interactive)
    (mapc 'LaTeX-add-bibitems
          (apply 'append
                 (mapcar 'sb/get-bibtex-keys (reftex-get-bibfile-list)))))

  (defun sb/find-bibliography-file ()
    "Try to find a bibliography file using RefTeX.
      Returns a string with text properties (as expected by read-file-name) or
empty string if no file can be found"
    (interactive)
    (let ((bibfile-list nil))
      (condition-case nil
          (setq bibfile-list (reftex-get-bibfile-list))
        (error (ignore-errors
                 (setq bibfile-list (reftex-default-bibliography)))))
      (if bibfile-list
          (car bibfile-list) "")))

  (defun sb/reftex-try-add-all-bibitems-from-bibtex ()
    "Try to find a bibliography file using RefTex and parse the bib keys.
Ignore if no file is found."
    (interactive)
    (let ((bibfile-list nil))
      (condition-case nil
          (setq bibfile-list (reftex-get-bibfile-list))
        (error (ignore-errors
                 (setq bibfile-list (reftex-default-bibliography)))))
      ;; (message "%s" bibfile-list)
      (mapc 'LaTeX-add-bibitems
            (apply 'append
                   (mapcar 'sb/get-bibtex-keys bibfile-list)))))
  :custom
  (reftex-enable-partial-scans t)
  (reftex-highlight-selection 'both)
  (reftex-plug-into-AUCTeX t)
  (reftex-save-parse-info t "Save parse info to avoid reparsing every time a file is visited")
  (reftex-toc-follow-mode t "Other buffer follows the point in toc buffer")
  ;; Make the toc display with a vertical split, since it is easy to read long lines
  (reftex-toc-split-windows-horizontally nil)
  (reftex-guess-label-type t "Try to guess the label type before prompting")
  (reftex-use-fonts t "Use nice fonts for toc")
  (reftex-revisit-to-follow t "Revisit files if necessary when browsing toc")
  (reftex-auto-recenter-toc t "Center on the section currently being edited")
  (reftex-use-multiple-selection-buffers t "Cache selection buffers for faster access")
  :config
  (sb/reftex-try-add-all-bibitems-from-bibtex))

(use-package bib-cite
  :straight nil
  :disabled t
  :diminish bib-cite-minor-mode
  :commands bib-cite-minor-mode
  :hook ((LaTeX-mode-hook latex-mode-hook) . bib-cite-minor-mode )
  :custom (bib-cite-use-reftex-view-crossref t)
  :bind (:map bib-cite-minor-mode-map
              ("C-c b"   . nil) ; We use `C-c b' for `comment-box'
              ("C-c l a" . bib-apropos)
              ("C-c l b" . bib-make-bibliography)
              ("C-c l d" . bib-display)
              ("C-c l t" . bib-etags)
              ("C-c l f" . bib-find)
              ("C-c l n" . bib-find-next)
              ("C-c l h" . bib-highlight-mouse)))

;; We can disable this once `lsp-latex-build' works well
(use-package auctex-latexmk
  :after tex-mode
  :straight t
  :demand t
  :commands (auctex-latexmk-setup auctex-latexmk)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t "Pass the '-pdf' flag when `TeX-PDF-mode' is active")
  (TeX-command-default "LatexMk")
  :config
  (auctex-latexmk-setup))

(use-package company-auctex
  :straight t
:if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after tex-mode
  :demand t
  :commands (company-auctex-init company-auctex-labels
                                 company-auctex-bibs company-auctex-macros
                                 company-auctex-symbols company-auctex-environments))

(use-package math-symbols
  :straight t
:if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :after tex-mode
  :demand t) ; Required by `ac-math' and `company-math'

(use-package company-math
  :straight t
  :after tex-mode
  :demand t
  :commands (company-math-symbols-latex company-math-symbols-unicode company-latex-commands))

(use-package company-reftex ; Reftex must be enabled to work
  :straight t
  :after tex-mode
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :demand t
  :commands (company-reftex-labels company-reftex-citations))

(use-package company-bibtex
  :straight t
  :after tex-mode
  :if (or (not (display-graphic-p)) (eq sb/capf 'company))
  :demand t
  :commands company-bibtex)

;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
(declare-function TeX-active-process "tex.el")

(defun sb/save-buffer-and-run-latexmk ()
  "Save the current buffer and run LaTeXMk also."
  (interactive)
  (require 'tex)
  ;; (require 'tex-buf)
  ;; Kill any active compilation process
  (let ((process (TeX-active-process)))
    (if process (delete-process process)))
  (let ((TeX-save-query nil))
    (TeX-save-document ""))
  (TeX-command-menu "LaTeXMk"))

(declare-function TeX-run-TeX "tex")

(defun sb/latex-compile-open-pdf ()
  "Save the current buffer, run LaTeXMk, and switch to the PDF after a successful compilation."
  (interactive)
  (let ((TeX-save-query nil)
        (process (TeX-active-process))
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (if process (delete-process process))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk" "latexmk -pdf" master-file)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (TeX-next-error t)
      (progn
        (minibuffer-message "LaTeXMk done")
        (when (display-graphic-p)
          (find-file (concat (file-name-directory (concat master-file ".tex"))
                             (concat master-file ".pdf"))))))))

;; (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
;;   (add-hook hook
;;             (lambda ()
;;               (add-hook 'after-save-hook
;;                         (lambda ()
;;                           (sb/save-buffer-and-run-latexmk)) nil t))))

(with-eval-after-load "tex-mode"
  (defvar latex-mode-map)
  (bind-key "C-x C-s" #'sb/latex-compile-open-pdf latex-mode-map))

(with-eval-after-load "latex"
  (defvar LaTeX-mode-map)
  (bind-key "C-x C-s" #'sb/latex-compile-open-pdf LaTeX-mode-map))

(use-package math-preview
  :straight nil
  :disabled t
  :commands (math-preview-all math-preview-at-point math-preview-region)
  :custom
  (math-preview-command (expand-file-name "node_modules/.bin/math-preview"
                                          sb/user-tmp)))

(use-package json-mode
  :straight json-reformat
  :straight json-snatcher
  :straight t
  :commands (json-mode jsonc-mode json-mode-beautify)
  :mode
  (("\\.json\\'"                  . json-mode)
   ("pyrightconfig.json"          . jsonc-mode)
   (".*/vscode/settings.json$"    . jsonc-mode)
   (".*/\\.vscode/settings.json$" . jsonc-mode)
   ("User/settings.json$"         . jsonc-mode))
  :hook
  ((json-mode-hook jsonc-mode-hook) . (lambda ()
                                        (make-local-variable 'js-indent-level)
                                        (setq js-indent-level 2)
                                        (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("vscode-json-languageserver" "--stdio"))
    :major-modes '(json-mode jsonc-mode)
    :remote? t
    :server-id 'jsonls-r)))

(use-package json-reformat
  :straight t
  :after (:any json-mode jsonc-mode)
  :demand t
  :custom
  (json-reformat:indent-width 2)
  (js-indent-level 2))

(use-package bazel
  :straight t
  :if (executable-find "bazel")
  :commands (bazel-mode bazelrc-mode bazel-buildifier)
  :hook
  ((bazel-mode-hook . (lambda ()
                        (add-hook 'before-save-hook #'bazel-buildifier nil t)))
   (bazel-mode-hook . flycheck-mode)))

(use-package protobuf-mode
  :straight t
  :commands protobuf-mode
  :mode "\\.proto$"
  :hook (protobuf-mode-hook . flycheck-mode))

(use-package mlir-mode
  :straight nil
  :ensure nil
  :commands mlir-mode
  :load-path "extras"
  :mode "\\.mlir\\'")

(use-package clang-format
  :straight t
  :if (executable-find "clang-format")
  :after (mlir-mode)
  :commands (clang-format clang-format-buffer clang-format-region)
  :custom (clang-format-style "file"))

(use-package clang-format+
  :straight t
  :straight clang-format
  :defines clang-format+-always-enable
  :hook (mlir-mode-hook . clang-format+-mode)
  :custom (clang-format+-always-enable t))

;; Use for major modes which do not provide a formatter. `aphelia' allows for formatting via a
;; background process but does not support Tramp and supports fewer formatters.
(use-package format-all
  :straight t
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

;; Tree-sitter provides advanced syntax highlighting features
(use-package tree-sitter
  :straight tree-sitter-langs
  :straight t
  :functions tree-sitter-hl-mode
  :commands (global-tree-sitter-mode tree-sitter-hl-mode)
  :diminish tree-sitter-mode
  :preface
  (defun sb/enable-tree-sitter ()
    "Delay enabling tree-sitter to avoid slowing down Emacs startup."
    (dolist (hook '(sh-mode-hook c-mode-hook c++-mode-hook
                                 css-mode-hook html-mode-hook
                                 java-mode-hook json-mode-hook
                                 jsonc-mode-hook php-mode-hook
                                 python-mode-hook))
      (add-hook hook (lambda ()
                       (require 'tree-sitter-langs)
                       (global-tree-sitter-mode 1)))))
  ;; :init (run-with-idle-timer 2 nil #'sb/enable-tree-sitter)
  :hook (after-init-hook . sb/enable-tree-sitter)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package editorconfig
  :straight t
  :if (executable-find "editorconfig")
  :commands editorconfig-mode)

;; Hooks into to `find-file-hook' to add all visited files and directories to `fasd'
(use-package fasd
  :straight t
  :defines fasd-enable-initial-prompt
  :commands (global-fasd-mode fasd-find-file)
  :if (executable-find "fasd")
  ;; :init (run-with-idle-timer 3 nil #'global-fasd-mode)
  :hook (after-init-hook . global-fasd-mode)
  :config (setq fasd-enable-initial-prompt nil)
  :bind* ("C-c /" . fasd-find-file))

(use-package dotenv-mode
  :straight t
  :mode "\\.env\\'")

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-compile.el
(use-package ansi-color
  :straight nil
  :commands ansi-color-apply-on-region
  :preface
  (defun sb/colorize-compilation-buffer ()
    "Colorize compile mode output."
    (require 'ansi-color)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :config
  ;; (add-hook 'compilation-filter-hook #'sb/colorize-compilation-buffer)
  (add-hook 'compilation-filter-hook #'sanityinc/colourise-compilation-buffer))

(use-package info-colors
  :straight nil
  :commands info-colors-fontify-node
  :hook (Info-selection-hook . info-colors-fontify-node))

;; A few backends are applicable to all modes and can be blocking: `company-yasnippet',
;; `company-ispell', and `company-dabbrev'. `company-dabbrev' returns a non-nil prefix in almost any
;; context (major mode, inside strings or comments). That is why it is better to put it at the end.

;; https://tychoish.com/post/better-company/
;; https://www.reddit.com/r/emacs/comments/l03dy1/priority_for_companymode/
;; https://emacs.stackexchange.com/questions/64038/how-to-use-multiple-backends-in-priority-for-company-mode

;; Try completion backends in order till there is a non-empty completion list
;; `(setq company-backends '(company-xxx company-yyy company-zzz))'
;; Merge completions of all the backends
;; `(setq company-backends '((company-xxx company-yyy company-zzz)))'
;; Merge completions of all the backends, give priority to `company-xxx'
;; `(setq company-backends '((company-xxx :separate company-yyy company-zzz)))'
;; Company does not support grouping of entirely arbitrary backends, they need to be compatible in
;; what `prefix' returns.

;; If the group contains keyword `:with', the backends listed after this keyword are ignored for
;; the purpose of the `prefix' command. If the group contains keyword `:separate', the candidates
;; that come from different backends are sorted separately in the combined list.

;; LATER: I do not understand the difference between the following two, and the explanation.
;; `(add-to-list 'company-backends '(company-capf company-dabbrev))'
;; `(add-to-list 'company-backends '(company-capf :with company-dabbrev))'

(progn
  (defun sb/company-xml-mode ()
    "Add backends for completion with company."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev-code
                             company-dabbrev)))

  (dolist (hook '(nxml-mode-hook))
    (add-hook hook (lambda ()
                     (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                       (sb/company-xml-mode)
                       (company-fuzzy-mode 1)
                       (diminish 'company-fuzzy-mode))))))

(progn
  (defun sb/company-latex-mode ()
    "Add backends for latex completion in company mode."

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)

    ;; `company-reftex' should be considerably more powerful than `company-auctex' backends for
    ;; labels and citations.

    (setq company-backends '(company-capf
                             company-files
                             company-reftex-citations
                             company-reftex-labels
                             company-auctex-environments
                             company-auctex-macros
                             company-latex-commands
                             company-math-symbols-latex
                             company-math-symbols-unicode
                             company-auctex-symbols
                             company-auctex-bibs
                             company-auctex-labels
                             company-bibtex
                             company-dabbrev
                             company-ispell)))

  (dolist (hook '(latex-mode-hook))
    (add-hook hook (lambda ()
                     (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                       (sb/company-latex-mode))))))

(progn
  (defun sb/company-web-mode ()
    "Add backends for web completion in company mode."

    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-files
                             company-yasnippet
                             company-dabbrev
                             company-ispell)))

  (dolist (hook '(web-mode-hook))
    (add-hook hook (lambda ()
                     (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                       (sb/company-web-mode)
                       (company-fuzzy-mode 1)
                       (diminish 'company-fuzzy-mode))))))

(progn
  (defun sb/company-text-mode ()
    "Add backends for text completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    ;; Slightly larger value to have more precise matches and so that the popup does not block
    (setq-local company-minimum-prefix-length 3
                company-transformers '(delete-dups))

    (set (make-local-variable 'company-backends)
         '(company-files
           company-dabbrev
           company-ispell
           company-abbrev)))

  (dolist (hook '(text-mode-hook)) ; Extends to derived modes like `markdown-mode' and `org-mode'
    (add-hook hook (lambda ()
                     (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                       (unless (derived-mode-p 'latex-mode)
                         (sb/company-text-mode)
                         (company-fuzzy-mode 1)
                         (diminish 'company-fuzzy-mode)))))))

;; (progn
;;   (defun sb/company-java-mode ()
;;     "Add backends for Java completion in company mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 3)
;;     (make-local-variable 'company-backends)
;;     (setq company-backends '((company-capf :with company-yasnippet)
;;                              (company-files : with company-yasnippet)
;;                              (company-dabbrev-code :with company-yasnippet)
;;                              company-dabbrev)))

;;   (add-hook 'java-mode-hook #'sb/company-java-mode))

;; https://emacs.stackexchange.com/questions/19072/company-completion-very-slow
;; `company-clang' is slow
;; (progn
;;   (defun sb/company-c-mode ()
;;     "Add backends for C/C++ completion in company mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 2)
;;     (make-local-variable 'company-backends)

;;     (setq company-backends '(company-capf
;;                              company-dabbrev-code
;;                              company-files
;;                              company-yasnippet
;;                              company-dabbrev)))

;;   (add-hook 'c-mode-common-hook (lambda ()
;;                                   (sb/company-c-mode)
;;                                   (company-fuzzy-mode 1)
;;                                   (diminish 'company-fuzzy-mode))))

(progn
  (defun sb/company-sh-mode ()
    "Add backends for shell script completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-shell
                             company-shell-env
                             company-dabbrev-code
                             company-yasnippet
                             company-files
                             company-dabbrev)))

  (add-hook 'sh-mode-hook (lambda ()
                            (unless (display-graphic-p)
                              (sb/company-sh-mode)
                              (company-fuzzy-mode 1)
                              (diminish 'company-fuzzy-mode))))

  (defun sb/company-fish-mode ()
    "Add backends for fish shell script completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-shell
                             company-shell-env
                             company-fish-shell
                             company-dabbrev-code
                             company-yasnippet
                             company-files
                             company-dabbrev)))

  (add-hook 'fish-mode-hook (lambda ()
                              (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                                (sb/company-fish-mode)
                                (company-fuzzy-mode 1)
                                (diminish 'company-fuzzy-mode)))))

(progn
  (defun sb/company-elisp-mode ()
    "Set up company for elisp mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 2)
    (make-local-variable 'company-backends)

    (setq company-backends '(company-capf
                             company-yasnippet
                             company-files
                             company-dabbrev-code
                             company-dabbrev)))

  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                                      (sb/company-elisp-mode)
                                      (company-fuzzy-mode 1)
                                      (diminish 'company-fuzzy-mode)))))

(progn
  (defun sb/company-python-mode ()
    "Add backends for Python completion in company mode."
    (defvar company-minimum-prefix-length)
    (defvar company-backends)

    (setq-local company-minimum-prefix-length 3)
    (make-local-variable 'company-backends)

    ;; `company-dabbrev-code' is useful for variable names
    (setq company-backends '(company-capf
                             company-yasnippet
                             company-dabbrev-code
                             company-files
                             company-dabbrev)))

  (add-hook 'python-mode-hook (lambda ()
                                (when (or (not (display-graphic-p)) (eq sb/capf 'company))
                                  (sb/company-python-mode)
                                  (company-fuzzy-mode 1)
                                  (diminish 'company-fuzzy-mode)))))

;; (progn
;;   (defun sb/company-prog-mode ()
;;     "Add backends for program completion in company mode."
;;     (defvar company-minimum-prefix-length)
;;     (defvar company-backends)

;;     (setq-local company-minimum-prefix-length 2)
;;     (make-local-variable 'company-backends)

;;     ;; https://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
;;     (setq company-backends '(company-capf
;;                              company-yasnippet
;;                              company-files
;;                              company-dabbrev-code
;;                              company-dabbrev)))

;;   (add-hook 'prog-mode-hook (lambda ()
;;                               (sb/company-prog-mode)
;;                               (company-fuzzy-mode 1)
;;                               (diminish 'company-fuzzy-mode))))

;; Use "emacsclient -c -nw" to start a new frame.
(use-package server
  :straight nil
  :disabled t
  :unless (string-equal "root" (getenv "USER")) ; Only start server if not root
  :commands server-running-p
  :init
  (unless (server-running-p)
    (server-start)))

;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(setenv "SHELL" shell-file-name) ; Recommended to connect with Bash

;; `vterm' provides better performance than `eshell', `shell', and `(ansi-)term'. The advantage of
;; the later modules are they are built-in to Emacs. The package requires shell-side configuration.
;; Check https://github.com/akermu/emacs-libvterm.
(use-package vterm
  :straight t
  :config
  (setq vterm-always-compile-module t
        vterm-max-scrollback 5000
        vterm-term-environment-variable "xterm-24bit")

  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
              (buffer-face-mode t))))

(use-package vterm-toggle
  :straight t
  :commands vterm-toggle
  :bind ("C-`" . vterm-toggle))



(bind-keys
 ("RET"       . newline-and-indent)
 ("C-l"       . goto-line)
 ("C-c z"     . repeat)
 ("C-z"       . undo)
 ;; Conflicts with Gnome window manager keybindings
 ;; ("<f11>"     . delete-other-windows)
 ("C-x k"     . kill-this-buffer)
 ("M-<left>"  . previous-buffer)
 ("C-S-<tab>" . previous-buffer)
 ("M-<right>" . next-buffer)
 ("C-<tab>"   . next-buffer)
 ("C-c d f"   . auto-fill-mode)
 ("M-c"       . capitalize-dwim)
 ("M-u"       . upcase-dwim)
 ("M-l"       . downcase-dwim)
 ("<f7>"      . previous-error)
 ("<f8>"      . next-error)
 ;; The default keybinding "C-S-backspace" does not work with the TUI.
 ("M-k"       . kill-whole-line))

;; In a line with comments, "C-u M-;" removes the comments altogether. That means deleting the
;; comment, NOT UNCOMMENTING but removing all commented text and the comment marker itself.
(bind-keys*
 ("C-c n" . comment-region)
 ("C-c m" . uncomment-region)
 ("C-c ;" . sb/comment-line)
 ("C-c b" . comment-box)
 ("C-s"   . save-buffer)
 ("C-S-s" . sb/save-all-buffers))

(unbind-key "C-]") ; Bound to `abort-recursive-edit'

(unbind-key "C-x s") ; Bound to `save-some-buffers'
(bind-key   "C-x s" #'sb/switch-to-scratch)
(bind-key   "C-x j" #'sb/counsel-all-files-recursively)

(unless (featurep 'centaur-tabs)
  (global-set-key [remap next-buffer]     #'sb/next-buffer)
  (global-set-key [remap previous-buffer] #'sb/previous-buffer))

(use-package default-text-scale
  :straight t
  :bind
  (("C-M-+" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(use-package free-keys
  :straight t
  :commands free-keys)

(use-package keyfreq
  :straight t
  :hook
  (after-init-hook . (lambda ()
                       (keyfreq-mode 1)
                       (keyfreq-autosave-mode 1))))

(use-package which-key ; Show help popups for prefix keys
  :straight t
  :diminish
  :commands (which-key-mode which-key-setup-side-window-right-bottom)
  ;; :init (run-with-idle-timer 3 nil #'which-key-mode)
  :hook (after-init-hook . which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  ;; Apply suggested settings for minibuffer. Do not use this if we use paging across keys.
  ;; (which-key-setup-minibuffer)

  :custom
  ;; Allow "C-h" to trigger `which-key' before it is done automatically
  (which-key-show-early-on-C-h t)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-idle-delay 0.3))

(use-package which-key-posframe
  :straight t
  :commands which-key-posframe-mode
  :hook (which-key-mode-hook . which-key-posframe-mode)
  :config
  ;; Modify the posframe background if it has a low contrast
  ;; (set-face-attribute 'which-key-posframe nil :background "floralwhite" :foreground "black")

  ;; Thicker border makes the posframe easier to distinguish
  (setq which-key-posframe-border-width 4)

  ;; Positioning the frame at the top obstructs the view to a lesser degree
  (setq which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))

;; Hydras

;; https://github.com/abo-abo/hydra
;; `:exit nil' means the hydra state will continue, `:exit t' will quit the hydra. `:color red'
;; means continue the hydra on a valid key but stop when a foreign key has been pressed. `:color
;; blue' means exit.

(setq lv-use-separator t)

;; (declare-function spell-fu-goto-next-error "spell-fu")
;; (declare-function spell-fu-goto-previous-error "spell-fu")

(defhydra sb/hydra-spelling (:color amaranth)
  "
  ^Spell Check^          ^Errors^            ^Spell fu^
  ^────────^──────────^──────^────────────^───────^─────────────^
  _c_ ispell            _<_ previous         _p_ previous error
  _f_ flyspell          _>_ next             _n_ next error
  _q_ quit              ^^                   _a_ add word
  "
  ("c" ispell)
  ("f" flyspell-buffer)

  ("<" flyspell-correct-previous)
  (">" flyspell-correct-next)

  ("n" spell-fu-goto-next-error)
  ("p" spell-fu-goto-previous-error)
  ("a" spell-fu-word-add)

  ("q" nil "quit"))

(defhydra sb/hydra-text-scale-zoom (:color amaranth)
  "Zoom the text"
  ("i" default-text-scale-increase "in")
  ("o" default-text-scale-decrease "out")

  ("q" nil "quit"))

;; (defhydra sb/hydra-error (:color amaranth)
;;   "Navigate errors"
;;   ("h" first-error "first")
;;   ("j" next-error "next")
;;   ("k" previous-error "prev")
;;   ("v" recenter-top-bottom "recenter")
;;   ("q" nil "quit"))

;; https://github.com/abo-abo/hydra/wiki/avy
(defhydra sb/hydra-avy (:color red)
  "
  Line^^       Region^^        Goto
----------------------------------q------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"

  ("y" avy-copy-line)
  ("m" avy-move-line)
  ("k" avy-kill-whole-line)

  ("Y" avy-copy-region)
  ("M" avy-move-region)
  ("K" avy-kill-region)

  ("c" avy-goto-char-timer)
  ("w" avy-goto-word-1)
  ("l" avy-goto-line)

  ("C" avy-goto-char)
  ("W" avy-goto-word-0)
  ("L" avy-goto-end-of-line))

(defhydra sb/hydra-projectile (:color teal :hint nil global-map "C-c p")
  "
     PROJECTILE: %(projectile-project-root)

  Project                    Find File             Buffers                  Misc
-----------------------------------------------------------------------------------------------------------
  _p_: switch project        _f_: find file        _i_: ibuffer           _c_: invalidate cache
  _s_: switch project        _F_: find file dwim   _b_: switch to buffer  _z_: cache current
  _a_: add known project     _d_: find directory   _r_: recent file       _g_: find tag
  _x_: remove known project  _D_: find file cwd    _k_: kill all buffers  _o_: multi-occur
  _X_: clean known project   ^^                    ^^                     _m_: compile
"
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("a"   projectile-add-known-project)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)

  ("f"   projectile-find-file)
  ("F"   projectile-find-file-dwim)
  ("d"   projectile-find-dir)
  ("D"   projectile-find-file-in-directory)

  ("i"   projectile-ibuffer)
  ("b"   projectile-switch-to-buffer)
  ("r"   projectile-recentf)
  ("k"   projectile-kill-buffers)

  ("c"   projectile-invalidate-cache)
  ("z"   projectile-cache-current-file)
  ("g"   projectile-find-tag)
  ("o"   projectile-multi-occur)
  ("m"   projectile-compile)
  ("q"   nil "cancel"))

(defhydra sb/hydra-move-text ()
  "Move text"
  ("u" move-text-up "up")
  ("d" move-text-down "down")
  ("q"   nil "cancel"))

;; (declare-function flycheck-verify-setup "flycheck")
;; (declare-function flycheck-previous-error "flycheck")
;; (declare-function flycheck-next-error "flycheck")
;; (declare-function flycheck-list-errors "flycheck")
;; (declare-function flycheck-select-checker "flycheck")
;; (declare-function flycheck-describe-checker "flycheck")
;; (declare-function flycheck-disable-checker "flycheck")
;; (declare-function flycheck-buffer "flycheck")

(defhydra sb/hydra-flycheck (:color blue)
  "
^
^Flycheck^          ^Errors^            ^Checker^
^────────^──────────^──────^────────────^───────^─────
_q_ quit            _<_ previous        _?_ describe
_M_ manual          _>_ next            _d_ disable
_v_ verify setup    _f_ check           _m_ mode
^^                  _l_ list            _s_ select
^^                  ^^                  ^^


                                              ^
                                              ^Flycheck^          ^Errors^            ^Checker^
                                              ^────────^──────────^──────^────────────^───────^─────
                                              _q_ quit            _<_ previous        _?_ describe
                                              _M_ manual          _>_ next            _d_ disable
                                              _v_ verify setup    _f_ check           _m_ mode
                                              ^^                  _l_ list            _s_ select
                                              ^^                  ^^                  ^^
                                              "
  ("q" nil)
  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("?" flycheck-describe-checker)
  ("M" flycheck-manual)
  ("d" flycheck-disable-checker)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)
  ("m" flycheck-mode)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup))

(defhydra sb/hydra-python-indent ()
  "Adjust Python indentation."
  (">" python-indent-shift-right "right")
  ("<" python-indent-shift-left "left"))

(with-eval-after-load "python-mode"
  (bind-key "C-c" #'sb/hydra-python-indent/body python-mode-map))

(defhydra sb/smerge-hydra
  (:color pink :hint nil :post (smerge-auto-leave))
  "
                                              ^Move^       ^Keep^               ^Diff^                 ^Other^
                                              ^^-----------^^-------------------^^---------------------^^-------
                                              _n_ext       _b_ase               _<_: upper/base        _C_ombine
                                              _p_rev       _u_pper              _=_: upper/lower       _r_esolve
                                              ^^           _l_ower              _>_: base/lower        _k_ill current
                                              ^^           _a_ll                _R_efine
                                              ^^           _RET_: current       _E_diff
                                              "
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("q" nil "cancel" :color blue))

(defhydra sb/hydra-multiple-cursors (:hint nil)
  "
                                              ^Up^            ^Down^        ^Other^
                                              ----------------------------------------------
                                              [_p_]   Next    [_n_]   Next    [_l_] Edit lines
                                              [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
                                              [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
                                              ^ ^             ^ ^             [_q_] Quit
                                              "
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))

(defhydra sb/hydra-smartparens (:hint nil)
  "
                                              Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
                                              ------------------------------------------------------------------------------------------------------------------------
                                              [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
                                              [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
                                              [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
                                              [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)

  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)

  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))

(defhydra sb/hydra-lsp (:exit t :hint nil)
  "
  Buffer^^               Server^^                   Symbol
  -----------------------------------------------------------------------------------------------------------
  [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
  [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
  [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"

  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown))

(defhydra sb/hydra-markdown-mode (:hint nil)
  "
  Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

  Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

  Lists             C-c C-x    _m_: insert item

  Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

  Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference

  "

  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue))

(bind-key "C-c h a" #'sb/hydra-avy/body)
(bind-key "C-c h d" #'sb/hydra-markdown-mode/body)
;; (bind-key "C-c h e" #'sb/hydra-error/body)
(bind-key "C-c h f" #'sb/hydra-flycheck/body)
(bind-key "C-c h g" #'sb/smerge-hydra/body)
(bind-key "C-c h j" #'sb/hydra-projectile/body)
(bind-key "C-c h l" #'sb/hydra-lsp/body)
(bind-key "C-c h m" #'sb/hydra-multiple-cursors/body)
(bind-key "C-c h p" #'sb/hydra-smartparens/body)
(bind-key "C-c h s" #'sb/hydra-spelling/body)
(bind-key "C-c h t" #'sb/hydra-move-text/body)
(bind-key "C-c h z" #'sb/hydra-text-scale-zoom/body)

;; Mark safe variables

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



(use-package consult-lsp
  :straight t
  :after (consult lsp)
  :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols)
  :config (consult-lsp-marginalia-mode 1))









(use-package consult-yasnippet
  :straight t
  :bind ("C-M-y" . consult-yasnippet))

;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
(use-package corfu
  :straight t
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

;; (use-package corfu-doc
;;   :straight t
;;   :if (and (display-graphic-p) (eq sb/capf 'corfu))
;;   :hook (corfu-mode-hook . corfu-doc-mode))



;; https://kristofferbalintona.me/posts/cape/
(use-package cape
  :straight t
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
  :straight t
  :after vertico
  :init (marginalia-mode 1))



;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :after vertico
  :straight t
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
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))


;; https://blog.d46.us/advanced-emacs-startup/
(add-hook 'emacs-startup-hook
          (lambda ()
            (let (;;(packages  (length package-activated-list))
                  (gc-time   (float-time gc-elapsed)))
              ;; (message "Emacs ready (init time = %s, packages = %d, gc time = %.2fs, gc count = %d)."
              ;;          (emacs-init-time) packages gc-time gcs-done)
              (message "Emacs ready (init time = %s, gc time = %.2fs, gc count = %d)."
                       (emacs-init-time) gc-time gcs-done))))

;;; init-emacs28.el ends here
