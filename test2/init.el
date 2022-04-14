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
                                         "GNUPGHOME" "SSH_AUTH_SOCK"
                                         "WAKATIME_HOME"))
  (exec-path-from-shell-initialize))

(use-package no-littering :straight t :defer t)

(use-package restart-emacs
  :straight t
  :commands restart-emacs-debug-init
  :config
  (defun restart-emacs-debug-init (&optional args)
    "Restart emacs and enable debug-init."
    (interactive)
    (restart-emacs (cons "--debug-init" args))))


(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(setq user-full-name "Tianshu Wang"
      user-mail-address "wang@tianshu.me")

(defvar default-font "Source Code Pro")
(defvar font-size 14)
(defvar unicode-font "Noto Sans CJK SC")
(defvar unicode-scale (/ 16.0 14))
(defvar emoji-font "Noto Color Emoji")
(defvar symbol-font "Noto Sans Symbols")

(set-face-attribute 'default nil :font (font-spec :family default-font :size font-size))
(add-hook 'after-init-hook
          (defun setup-font ()
            (dolist (charset '(kana han cjk-misc bopomofo))
              (set-fontset-font t charset unicode-font))
            (add-to-list 'face-font-rescale-alist `(,unicode-font . ,unicode-scale))

            (set-fontset-font t 'emoji emoji-font nil 'prepend)
            (set-fontset-font t 'symbol symbol-font nil 'prepend)))

(when (memq window-system '(mac ns))
  (setq ns-pop-up-frames nil)

  (setq unicode-font "PingFang SC"
        emoji-font "Apple Color Emoji"
        symbol-font "Apple Symbols"))

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

(use-package reformatter :straight t :defer t)

(use-package undohist
  :straight t
  :hook (after-init . undohist-initialize)
  :config
  (setq undohist-ignored-files '("EDITMSG"))

  (defun undohist-recover-safe@around (fun)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (p) nil)))
      (funcall fun)))
  (advice-add #'undohist-recover-safe :around #'undohist-recover-safe@around))

(use-package winum
  :straight t
  :hook (after-init . winum-mode)
  :config
  (setq winum-auto-assign-0-to-minibuffer t
        winum-auto-setup-mode-line t
        winum-scope 'frame-local)

  (defun move-buffer-to-window (windownum follow-focus-p)
    "Moves a buffer to a window. follow-focus-p controls
whether focus moves to new window (with buffer), or stays on current"
    (interactive)
    (let ((b (current-buffer))
          (w1 (selected-window))
          (w2 (winum-get-window-by-number windownum)))
      (unless (eq w1 w2)
        (set-window-buffer w2 b)
        (switch-to-prev-buffer)
        (unrecord-window-buffer w1 b)))
    (when follow-focus-p (select-window (winum-get-window-by-number windownum))))

  (defun swap-buffers-to-window (windownum follow-focus-p)
    "Swaps visible buffers between active window and selected window.
follow-focus-p controls whether focus moves to new window (with buffer), or
stays on current"
    (interactive)
    (let* ((b1 (current-buffer))
           (w1 (selected-window))
           (w2 (winum-get-window-by-number windownum))
           (b2 (window-buffer w2)))
      (unless (eq w1 w2)
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (unrecord-window-buffer w1 b1)
        (unrecord-window-buffer w2 b2)))
    (when follow-focus-p (winum-select-window-by-number windownum)))

  (dotimes (i 9)
    (let ((n (+ i 1)))
      (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
               ,(format "Move buffer to the window with number %i." n)
               (interactive "P")
               (if arg
                   (move-buffer-to-window ,n t)
                 (swap-buffers-to-window ,n t)))))))

(use-package wakatime-mode
  :straight t
  :hook (prog-mode . wakatime-mode)
  :config
  (setq wakatime-cli-path "wakatime-cli")

  (defun wakatime-dashboard ()
    (interactive)
    (browse-url "https://wakatime.com/dashboard")))


(use-package tex
  :straight auctex
  :defer t
  :mode ("\\.[tT]e[xX]\\'" . TeX-tex-mode)
  :config
  (setq TeX-auto-save t
        TeX-command-default "LatexMk"
        TeX-master t
        TeX-parse-self t
        TeX-save-query nil
        TeX-source-correlate-start-server t
        TeX-view-program-list '(("Preview.app" "open -a Preview.app %o")
                                ("Skim" "open -a Skim.app %o")
                                ("displayline" "displayline -b %n %o %b")
                                ("open" "open %o"))
        TeX-view-program-selection '((output-dvi "open")
                                     (output-pdf "displayline")
                                     (output-html "open"))
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)

  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

  (defun TeX-build ()
    (interactive)
    (TeX-save-document (TeX-master-file))
    (TeX-command TeX-command-default 'TeX-master-file -1))

  ;; Rebindings for TeX-font
  (defun font-bold () (interactive) (TeX-font nil ?\C-b))
  (defun font-medium () (interactive) (TeX-font nil ?\C-m))
  (defun font-code () (interactive) (TeX-font nil ?\C-t))
  (defun font-emphasis () (interactive) (TeX-font nil ?\C-e))
  (defun font-italic () (interactive) (TeX-font nil ?\C-i))
  (defun font-clear () (interactive) (TeX-font nil ?\C-d))
  (defun font-calligraphic () (interactive) (TeX-font nil ?\C-a))
  (defun font-small-caps () (interactive) (TeX-font nil ?\C-c))
  (defun font-sans-serif () (interactive) (TeX-font nil ?\C-f))
  (defun font-normal () (interactive) (TeX-font nil ?\C-n))
  (defun font-serif () (interactive) (TeX-font nil ?\C-r))
  (defun font-oblique () (interactive) (TeX-font nil ?\C-s))
  (defun font-upright () (interactive) (TeX-font nil ?\C-u))
)

(use-package reftex
  :hook (TeX-mode . reftex-mode)
  :config
  (setq reftex-default-bibliography '("~/Documents/Bibliography/references.bib")
        reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t))

(use-package auctex-latexmk
  :straight t
  :after tex
  :config
  (auctex-latexmk-setup)
  (setq-default TeX-command-default "LatexMk")
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package aas
  :straight t
  :hook ((TeX-mode org-mode) . aas-activate-for-major-mode))

(use-package laas
  :straight t
  :hook (TeX-mode . laas-mode))

(use-package bibtex
  :defer t
  :config
  (setq bibtex-file-path "~/Documents/Bibliography/"
        bibtex-files '("references.bib")
        bibtex-notes-path "~/Documents/Org/notes/refs/"

        bibtex-align-at-equal-sign t
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-name-year-separator "-"
        bibtex-dialect 'biblatex))

(use-package python
  :hook ((python-mode . eglot-ensure)
         (python-mode . tree-sitter-hl-mode))
  :config
  (setq python-indent-def-block-scale 1
        python-indent-guess-indent-offset-verbose nil)

  (add-hook 'python-mode-hook
            (defun init-python-mode ()
              "Stuff to do when opening `python-mode' files."
              (set (make-local-variable 'comment-inline-offset) 2)
              (setq fill-column 88
                    tab-width 4))))


(use-package eval-sexp-fu
  :straight t
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))

(defun display-startup-echo-area-message ()
  "Display startup message."
  (message (concat "Startup time: " (emacs-init-time))))

(use-package dockerfile-mode :straight t :defer t)

(use-package js
  :mode ("\\.json\\'" . json-mode)
  :config
  (defvar json-mode-map
    (make-sparse-keymap)
    "Keymap for `json-mode'.")

  (define-derived-mode json-mode js-mode "JSON"
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2)))

(use-package jsonnet-mode
  :straight t
  :defer t)

(use-package web-mode :straight t :defer t)

(use-package yaml-mode :straight t :defer t)

(use-package markdown-mode
  :straight t
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t))

;;; init.el ends here
