;;; init.el --- Tianshu Wang Personal Emacs Configuration. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Load path
;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.

(dolist (dir '("site-lisp" "lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

;; bootstrap `straight.el'
(defvar bootstrap-version)
(setq straight-check-for-modifications '(find-when-checking)
      straight-host-usernames '((github . "tshu-w"))
      straight-vc-git-default-clone-depth 1
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

(if init-file-debug
    (setq use-package-verbose t
          use-package-minimum-reported-time 0
          use-package-compute-statistics t
          use-package-inject-hooks t
          debug-on-error t)
  (setq use-package-expand-minimally t))
(straight-use-package 'use-package)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :straight t
  :defer t
  :init
  (setq exec-path-from-shell-arguments nil
        exec-path-from-shell-variables '("PATH" "MANPATH"
                                         "GNUPGHOME" "SSH_AUTH_SOCK"))
  (exec-path-from-shell-initialize))

(use-package no-littering :straight t :defer t)

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(setq user-full-name "Tianshu Wang"
      user-mail-address "wang@tianshu.me")

(setq initial-scratch-message nil   ;; "make scratch buffer empty"
      inhibit-startup-message t)    ;; "disable splash screen"

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; widen `fill-column'
(setq-default fill-column 80)

;; no beep and visual blinking
(setq ring-bell-function 'ignore
      visible-bell nil)

(setq frame-resize-pixelwise t)

;; highlight current line
(global-hl-line-mode 1)
;; no blink
(blink-cursor-mode 0)
;; prettify symbols
(global-prettify-symbols-mode 1)

;; Single space between sentences is more widespread than double
(setq sentence-end-double-space nil)

;; smooth scrolling
(setq scroll-conservatively 101
      scroll-margin 2)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; enable flymake-mode in prog-mode
(add-hook 'prog-mode-hook #'flymake-mode)
;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
;; enable subword-mode in prog-mode
(add-hook 'prog-mode-hook #'subword-mode)

;; scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Don't try to ping things that look like domain names
(setq ffap-machine-p-known 'reject)

;; Use system trash for file deletion.
(setq delete-by-moving-to-trash t)

;; autosave each change
(setq bookmark-save-flag 1)

;; don't set a fringe mark at bookmarked lines
(setq bookmark-set-fringe-mark nil)

;; keep focus while navigating help buffers
(setq help-window-select t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
;; (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; don't load outdated compiled files.
(setq load-prefer-newer t)

;; don't save duplicates in kill-ring
(setq kill-do-not-save-duplicates t)

(defun server-remove-kill-buffer-hook ()
  "Remove prompt if the file is opened in other clients."
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t))

(use-package desktop
  :commands restart-emacs-without-desktop
  :init (desktop-save-mode)
  :config
  ;; inhibit no-loaded prompt
  (setq desktop-file-modtime (file-attribute-modification-time
                              (file-attributes
                               (desktop-full-file-name)))
        desktop-lazy-verbose nil
        desktop-load-locked-desktop t
        desktop-restore-eager 1
        desktop-save t)

  (dolist (param '(foreground-color background-color font cursor-color mouse-color))
    (push `(,param . :never) frameset-filter-alist))

  (defun restart-emacs-without-desktop (&optional args)
    "Restart emacs without desktop."
    (interactive)
    (restart-emacs (cons "--no-desktop" args)))

  (defun desktop-read@override (desktop-read)
    "Inhibit `desktop-read' message"
    (let ((inhibit-message t))
      (funcall desktop-read)))
  (advice-add 'desktop-read :around #'desktop-read@override))

(use-package dired
  :defer t
  :config
  (setq dired-auto-revert-buffer t
        dired-create-destination-dirs 'ask
        dired-dwim-target t
        dired-listing-switches "-aBhl --group-directories-first"
        dired-vc-rename-file t))

(use-package doc-view
  :defer t
  :config
  (setq doc-view-resolution 400))

(use-package elec-pair
  :hook (after-init . electric-pair-mode))

(use-package ediff
  :hook ((ediff-quit . winner-undo)
         (ediff-prepare-buffer . outline-show-all))
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally))

(use-package files
  :hook ((before-save . delete-trailing-whitespace)
         (after-save . executable-make-buffer-file-executable-if-script-p))
  :init
  (setq make-backup-files nil        ;; don't create backup~ files
        revert-without-query '(".*") ;; disable revert query
        enable-remote-dir-locals t)
  :config
  ;; Prompt to open file literally if large file.
  (defun check-large-file ()
    "Check when opening large files - literal file open."
    (let* ((filename (buffer-file-name))
           (size (nth 7 (file-attributes filename))))
      (when (and
             (not (memq major-mode
                        '(archive-mode doc-view-mode doc-view-mode-maybe
                                       ebrowse-tree-mode emacs-lisp-mode
                                       fundamental-mode git-commit-mode
                                       image-mode jka-compr pdf-view-mode
                                       tags-table-mode tar-mode)))
             size (> size (* 1024 1024 1))
             (y-or-n-p (format (concat "%s is a large file, open literally to "
                                       "avoid performance issues?")
                               filename)))
        (setq buffer-read-only t)
        (buffer-disable-undo)
        (fundamental-mode))))
  (add-hook 'find-file-hook #'check-large-file)

  (defun system-move-file-to-trash (filename)
    (process-file-shell-command
     (format "trash %S" (file-local-name filename))))

  (defun make-directory-maybe ()
    "Create parent directory if not exists while visiting file."
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
            (make-directory dir t)
          (keyboard-quit)))))
  (add-to-list 'find-file-not-found-functions 'make-directory-maybe nil #'eq))

(use-package newcomment
  :commands comment-or-uncomment
  :config
  (defun comment-or-uncomment (n)
    (interactive "*p")
    (if (or (region-active-p)
            (save-excursion
              (beginning-of-line)
              (looking-at "\\s-*$")))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position n)))))

(use-package project
  :defer t
  :config
  (setq project-current-inhibit-prompt t
        project-vc-merge-submodules nil
        project-switch-commands '((project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory")
                                  (project-vc-dir "VC-Dir"))
        project-switch-use-entire-map t)

  (defun project-vc-dir ()
    "Run VC-Dir in the current project's root."
    (interactive)
    (let ((project-root (project-root (project-current t))))
      (if (file-exists-p (expand-file-name ".git" project-root))
          (cond ((fboundp 'magit-status-internal)
                 (magit-status-internal project-root))
                ((fboundp 'magit-status)
                 (with-no-warnings (magit-status project-root)))
                (t
                 (vc-dir project-root)))
        (vc-dir project-root)))))

(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-max-saved-items 100
        recentf-initialize-file-name-history nil)

  (add-to-list 'recentf-exclude `(recentf-expand-file-name ,(straight--emacs-dir "straight"))))

(use-package savehist
  :hook (after-init . savehist-mode)
  :config
  (setq enable-recursive-minibuffers t ; allow commands in minibuffers
        history-length 100
        savehist-autosave-interval nil
        savehist-additional-variables '(evil-jumps-history
                                        mark-ring global-mark-ring
                                        search-ring regexp-search-ring
                                        extended-command-history))

  (add-hook 'savehist-save-hook
            (defun savehist-unpropertize-variables-h ()
              "Remove text properties from `kill-ring' to reduce savehist cache size."
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring))
                    register-alist
                    (cl-loop for (reg . item) in register-alist
                             if (stringp item)
                             collect (cons reg (substring-no-properties item))
                             else collect (cons reg item)))))

  (add-hook 'savehist-save-hook
            (defun savehist-remove-unprintable-registers-h ()
              "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
              ;; Save new value in the temp buffer savehist is running
              ;; `savehist-save-hook' in. We don't want to actually remove the
              ;; unserializable registers in the current session!
              (setq-local register-alist
                          (cl-remove-if-not #'savehist-printable register-alist)))))

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package simple
  :config
  (setq column-number-mode t
        delete-trailing-lines nil
        eval-expression-print-length nil
        eval-expression-print-level nil
        ;; save clipboard contents into kill-ring before replace them
        save-interprogram-paste-before-kill t))

(use-package tab-bar
  :hook (after-init . tab-bar-mode)
  :config
  (setq tab-bar-show nil
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-select-tab-modifiers '(hyper)))

(use-package tramp
  :defer t
  :config
  (setq remote-file-name-inhibit-cache 60
        tramp-verbose 1
        vc-handled-backends '(SVN Git))

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package xref
  :defer t
  :config
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))

  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function       #'xref-show-definitions-completing-read))

(use-package whitespace
  :hook ((prog-mode . show-trailing-whitespace)
         (diff-mode . whitespace-mode))
  :config
  (defun show-trailing-whitespace ()
    (set-face-attribute 'trailing-whitespace nil :background
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (setq show-trailing-whitespace t)))

(use-package winner
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (setq winner-boring-buffers-regexp "\\*.*\\*"))


(setq-default custom-file (no-littering-expand-var-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@after (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'load-theme@after)

(add-hook 'after-load-theme-hook
          (defun bolder-faces ()
            (set-face-attribute 'font-lock-function-name-face nil :weight 'semi-bold)
            (set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold)))

(use-package doom-themes
  :straight t
  :defer t
  :custom-face
  ;; fix doom-theme diff-hl face
  (diff-hl-change ((t (:background nil))))
  (diff-hl-delete ((t (:background nil))))
  (diff-hl-insert ((t (:background nil)))))

(use-package berrys-theme
  :straight t
  :defer t
  :custom-face
  (fringe ((t nil))))

(use-package flucui-themes :straight t :defer t)

(use-package humanoid-themes
  :straight t
  :defer t
  :config
  (setq humanoid-themes-org-height nil
        humanoid-themes-org-bold nil))

(use-package kaolin-themes
  :straight t
  :defer t
  :config
  (setq kaolin-themes-org-scale-headings nil
        kaolin-themes-modeline-border nil))

(use-package lab-themes
  :straight t
  :defer t
  :custom-face
  (font-lock-keyword-face ((t (:slant normal))))
  (font-lock-constant-face ((t (:slant normal)))))

(use-package spacemacs-common
  :straight spacemacs-theme
  :defer t
  :init
  (setq spacemacs-theme-org-height nil
        spacemacs-theme-comment-bg nil))

(use-package solo-jazz-theme :straight t :defer t)

(defvar light-themes '(doom-acario-light
                       ;; doom-ayu-light
                       doom-flatwhite
                       doom-gruvbox-light
                       doom-homage-white
                       doom-nord-light
                       doom-one-light
                       doom-opera-light
                       ;; doom-plain
                       doom-solarized-light
                       doom-tomorrow-day
                       berrys
                       flucui-light
                       humanoid-light
                       kaolin-breeze
                       kaolin-light
                       kaolin-mono-light
                       kaolin-valley-light
                       lab-light
                       spacemacs-light
                       solo-jazz
                       modus-operandi)
  "Light themes to switch.")

(defvar dark-themes '(;; doom-1337
                      ;; doom-acario-dark
                      ;; doom-ayu-mirage
                      ;; doom-badger
                      doom-challenger-deep
                      doom-city-lights
                      doom-dark+
                      ;; doom-dracula
                      ;; doom-ephemeral
                      ;; doom-fairy-floss
                      doom-gruvbox
                      ;; doom-henna
                      doom-homage-black
                      doom-horizon
                      doom-Iosvkem
                      ;; doom-ir-black
                      ;; doom-laserwave
                      ;; doom-manegarm
                      doom-material
                      doom-material-dark
                      ;; doom-meltbus
                      ;; doom-miramare
                      ;; doom-molokai
                      ;; doom-monokai-classic
                      doom-monokai-pro
                      ;; doom-monokai-machine
                      ;; doom-monokai-octagon
                      ;; doom-monokai-ristretto
                      ;; doom-monokai-spectrum
                      doom-moonlight
                      doom-nord
                      ;; doom-nova
                      doom-oceanic-next
                      ;; doom-old-hope
                      doom-one
                      doom-opera
                      ;; doom-outrun-electric
                      doom-palenight
                      doom-peacock
                      ;; doom-plain-dark
                      doom-rouge
                      ;; doom-shades-of-purple
                      doom-snazzy
                      doom-solarized-dark
                      doom-solarized-dark-high-contrast
                      doom-sourcerer
                      doom-spacegrey
                      doom-tokyo-night
                      doom-tomorrow-night
                      doom-vibrant
                      doom-wilmersdorf
                      doom-xcode
                      doom-zenburn
                      ;; flucui-dark
                      humanoid-dark
                      ;; kaolin-blossom
                      kaolin-bubblegum
                      kaolin-galaxy
                      kaolin-ocean
                      kaolin-temple
                      kaolin-valley-dark
                      lab-dark
                      spacemacs-dark
                      modus-vivendi)
  "Dark themes to switch.")

(if (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions
              (defun load-theme-randomly (appearance)
                (mapc #'disable-theme custom-enabled-themes)
                (let* ((day (string-to-number
                             (format-time-string "%j" (current-time))))
                       (themes (pcase appearance
                                 ('light light-themes)
                                 ('dark dark-themes)))
                       (themes-length (length themes))
                       (theme (nth (% day themes-length) themes)))
                  (load-theme theme t)))))

(use-package doom-modeline
  :straight t
  :hook ((after-load-theme . doom-modeline-mode)
         (after-load-theme . smaller-modeline))
  :config
  (setq inhibit-compacting-font-caches t

        doom-modeline-height 0
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-project-detection 'project

        doom-modeline-gnus nil
        doom-modeline-icon nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil)

  (defun smaller-modeline ()
    "Make doom-modeline smaller."
    (set-face-attribute 'mode-line nil :height 0.85 :inherit 'default)
    (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)))

(use-package shackle
  :straight t
  :hook (after-init . shackle-mode)
  :config
  (setq shackle-default-size 0.4
        shackle-rules `((help-mode                       :select t :align right :size ,fill-column)
                        (helpful-mode                    :select t :align right :size ,fill-column)
                        ("*Messages*"                    :select t :align t)
                        ("*eldoc*"                       :align t)
                        (special-mode                    :align t)
                        (process-menu-mode               :align t)
                        (compilation-mode                :align t)
                        (flymake-diagnostics-buffer-mode :align t)
                        ("*Shell Command Output*"        :align t)
                        ("*Async Shell Command*"         :align t)
                        ("\\*EGLOT.*"                    :select t :align right :size ,fill-column :regexp t))))

(use-package tree-sitter :straight t :defer t)

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

(use-package writeroom-mode
  :straight t
  :hook (after-init . global-writeroom-mode)
  :config
  (setq split-width-threshold 120

        writeroom-width 128
        writeroom-bottom-divider-width 0
        writeroom-fringes-outside-margins nil
        writeroom-fullscreen-effect nil
        writeroom-major-modes '(text-mode prog-mode conf-mode special-mode Info-mode dired-mode)
        writeroom-major-modes-exceptions '(process-menu-mode proced-mode)
        writeroom-maximize-window nil
        writeroom-mode-line t
        writeroom-mode-line-toggle-position 'mode-line-format))

(use-package hl-todo
  :straight t
  :hook (after-init . global-hl-todo-mode))

(use-package highlight-parentheses
  :straight t
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  (setq highlight-parentheses-colors '("Springgreen3"
                                       "IndianRed1"
                                       "IndianRed3"
                                       "IndianRed4"))
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'ultra-bold))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package xterm-color
  :straight t
  :defer t
  :init
  (setq compilation-environment '("TERM=xterm-256color"))

  (defun compilation-filter@around (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'compilation-filter@around))

(use-package vertico
  :straight (:files (:defaults "extensions/*.el"))
  :hook (after-init . vertico-mode)
  :config
  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (setq vertico-cycle t)

  (use-package vertico-buffer
    :defer t
    :hook (vertico-mode . vertico-buffer-mode)
    :config
    (setq vertico-buffer-display-action `(display-buffer-in-side-window
                                          (window-height . ,(+ 3 vertico-count))
                                          (side . top))))

  (use-package vertico-directory
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  (use-package vertico-quick))

(use-package marginalia
  :straight t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :straight t
  :defer t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
  :config
  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-style-dispatchers '(flex-if-twiddle without-if-bang))

  (advice-add 'company-capf
              :around
              (lambda (capf-fn &rest args)
                (let ((completion-styles '(basic partial-completion substring)))
                  (apply capf-fn args)))))

(use-package consult
  :straight t
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  :config
  (setq consult-narrow-key "?"
        consult-preview-key (kbd "M-.")
        consult-project-root-function (lambda () (project-root (project-current t)))

        consult-ripgrep-args "rg --hidden --glob \"!.git/\" --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number .")

  (consult-customize consult-theme :preview-key '(:debounce 0.2 any)
                     consult-goto-line consult-imenu consult-line
                     :preview-key 'any))

(use-package embark
  :straight t
  :init
  (with-eval-after-load 'avy
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))
  :config
  (with-eval-after-load 'which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (caar targets) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators '(embark-which-key-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (when-let ((win (get-buffer-window which-key--buffer
                                         'visible)))
        (quit-window 'kill-buffer win)
        (let ((embark-indicators (delq #'embark-which-key-indicator embark-indicators)))
          (apply fn args))))

    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator))

  (with-eval-after-load 'vertico
    (defun embark-vertico-indicator ()
      (let ((fr face-remapping-alist))
        (lambda (&optional keymap _targets prefix)
          (when (bound-and-true-p vertico--input)
            (setq-local face-remapping-alist
                        (if keymap
                            (cons '(vertico-current . embark-target) fr)
                          fr))))))

    (add-to-list 'embark-indicators #'embark-vertico-indicator)))

(use-package embark-consult
  :straight t
  :demand t
  :after (consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep :straight t :defer t)


(use-package company
  :straight t
  :custom-face (company-tooltip-mouse ((t (:background nil))))
  :hook ((after-init . global-company-mode)
         (after-init . company-tng-mode))
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-require-match nil
        company-selection-wrap-around t
        company-show-quick-access t
        company-tooltip-align-annotations t
        company-transformers '(delete-dups company-sort-prefer-same-case-prefix)
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-dabbrev-char-regexp "[A-Za-z-_]"
        company-dabbrev-ignore-buffers "\\`[ *]\\|\\.pdf\\'"
        company-backends '(company-files
                           company-capf
                           (company-dabbrev-code company-keywords)
                           company-dabbrev
                           company-ispell)
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode))
  :config
  ;; `yasnippet' integration
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun company-enable-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends
              (mapcar #'company-backend-with-yas company-backends)))
      (company-enable-yas)

      (defun company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq command 'prefix)
            (when-let ((prefix (funcall fun 'prefix)))
              (unless (memq (char-before (- (point) (length prefix))) '(?. ?> ?\())
                prefix))
          (funcall fun command arg)))

      (advice-add #'company-yasnippet :around #'company-yasnippet-disable-inline))))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-backends-colors nil
        company-box-enable-icon nil
        company-box-scrollbar nil
        company-box-doc-frame-parameters '((internal-border-width . 3))))

(use-package prescient
  :straight t
  :hook (after-init . prescient-persist-mode)
  :init
  (use-package company-prescient
    :straight t
    :hook (company-mode . company-prescient-mode))
  :config
  (setq prescient-sort-full-matches-first t
        prescient-sort-length-enable nil))

(use-package eglot
  :straight t
  :commands expand-absolute-name
  :init
  (setq read-process-output-max (* 1024 1024))
  :config
  (setq eglot-stay-out-of '(company))

  ;; https://github.com/company-mode/company-mode/discussions/1313
  (advice-remove #'eglot--snippet-expansion-fn #'ignore)

  (defun expand-absolute-name (name)
    (if (file-name-absolute-p name)
        (tramp-file-local-name
         (expand-file-name
          (concat (file-remote-p default-directory) name)))
      name)))

(use-package yasnippet
  :straight t
  :hook (after-init . yas-global-mode)
  :init
  (setq yas-minor-mode-map nil)
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand t)

  (setq yas-triggers-in-field t
        yas-wrap-around-region t))


(use-package vc
  :defer t
  :config
  (setq vc-follow-symlinks t))

(use-package magit
  :straight t
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (setq magit-diff-refine-hunk t
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-save-repository-buffers 'dontask)

  (add-hook 'magit-process-find-password-functions 'magit-process-password-auth-source)

  (defun org-reveal-advice (&rest _args)
    "Unfold the org headings for a target line.
    This can be used to advice functions that might open .org files.

    For example: To unfold from a magit diff buffer, evaluate the following:
    (advice-add 'magit-diff-visit-file :after #'org-reveal-advice)"
    (when (derived-mode-p 'org-mode) (org-reveal)))

  (advice-add 'magit-blame-addition           :after #'org-reveal-advice)
  (advice-add 'magit-diff-visit-file          :after #'org-reveal-advice)
  (advice-add 'magit-diff-visit-worktree-file :after #'org-reveal-advice))

(use-package forge
  :straight t
  :after magit
  :init
  (setq forge-add-default-bindings nil))

(use-package transient
  :straight t
  :defer t)

(use-package diff-hl
  :straight t
  :hook (after-init . global-diff-hl-mode)
  :config
  (setq diff-hl-side 'right)

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package git-modes :straight t :defer t)

(use-package git-link
  :straight t
  :config
  (setq git-link-open-in-browser t)

  (defun git-link-copy-url-only ()
    "Only copy the generated link to the kill ring."
    (interactive)
    (let (git-link-open-in-browser)
      (call-interactively 'git-link)))

  (defun git-link-commit-copy-url-only ()
    "Only copy the generated link to the kill ring."
    (interactive)
    (let (git-link-open-in-browser)
      (call-interactively 'git-link-commit))))

(use-package github-stars :straight t :defer t)

(use-package dumb-jump
  :straight t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

(use-package editorconfig
  :disabled t
  :straight t
  :init (editorconfig-mode))

(use-package flyspell-correct
  :straight t)

(use-package gcmh
  :straight t
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-high-cons-threshold #x6400000))

(use-package helpful
  :straight t
  :config
  (defun helpful-reuse-window (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (pop-to-buffer-same-window buffer-or-name)
      (pop-to-buffer buffer-or-name)))

  (setq helpful-max-buffers 3
        helpful-switch-buffer-function #'helpful-reuse-window)

  (with-eval-after-load 'ibuffer
    (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode)))

(use-package link-hint
  :straight t
  :config
  (setq link-hint-restore nil))

(use-package nov
  :straight t
  :commands (nov-org-link-follow nov-org-link-store)
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (with-eval-after-load 'org
    (org-link-set-parameters "nov"
                             :follow 'nov-org-link-follow
                             :store 'nov-org-link-store)))

(use-package pandoc-mode
  :straight t
  :hook (pandoc-mode . pandoc-load-default-settings)
  :commands pandoc
  :config
  (defun pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body)))

(use-package pangu-spacing
  :straight t
  :hook (org-mode . pangu-spacing-mode)
  :config
  (setq pangu-spacing-real-insert-separtor t))

(use-package popper
  :straight t
  :hook ((after-init . popper-mode)
         (after-init . popper-echo-mode))
  :config
  (setq popper-display-control nil
        popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eldoc\\*"
          "^\\*EGLOT"
          help-mode
          helpful-mode
          compilation-mode
          process-menu-mode
          special-mode
          flymake-diagnostics-buffer-mode)))

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

;; https://kristofferbalintona.me/posts/vertico-marginalia-all-the-icons-completion-and-orderless/
(use-package vertico
  :straight t
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
  :straight nil
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
  :straight nil
  :load-path "extras"
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :bind
  (("C-c r" . vertico-repeat-last)
   ("M-r" . vertico-repeat-select)))

(use-package vertico-indexed
  :after vertico
  :straight nil
  :load-path "extras"
  :commands vertico-indexed-mode
  :init (vertico-indexed-mode 1))

(use-package vertico-quick
  :after vertico
  :straight nil
  :bind
  (:map vertico-map
        ("C-c q" . vertico-quick-insert)
        ("C-'" . vertico-quick-exit)))

(use-package consult
:straight t
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
:straight t
  :commands consult-projectile-recentf
  :bind
  (("<f5>" . consult-projectile-switch-project)
   ("<f6>" . consult-projectile)))

(use-package consult-lsp
:straight t
  :after (consult lsp)
  :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols)
  :config (consult-lsp-marginalia-mode 1))

(use-package consult-flycheck
:straight t
  :after (consult flycheck)
  :bind
  (:map flycheck-command-map
        ("!" . consult-flycheck)))

(use-package consult-flyspell
:straight t
  :after (consult flyspell)
  :commands consult-flyspell)

(use-package consult-dir
:straight t
  :bind
  (([remap list-directory] . consult-dir)
   ("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-project-extra
:straight t)

(use-package consult-yasnippet
:straight t
  :bind ("C-M-y" . consult-yasnippet))

;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
(use-package corfu
:straight t
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
  :hook (corfu-mode-hook . corfu-doc-mode))

(use-package kind-icon
  :straight t
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

;; We prefer to use "kind-icon" package for icons since it has more active commits but I do not know
;; which is better.
(use-package all-the-icons-completion
  :straight t
  :disabled t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

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


(use-package centaur-tabs
  :straight t
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

;;; init.el ends here
