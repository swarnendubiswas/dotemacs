;;; init-buffer.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar sb/EMACS27)
(defvar sb/EMACS28+)
(defvar sb/user-home-directory)

(declare-function s-starts-with? "s")
(declare-function s-ends-with? "s")
(declare-function sb/inhibit-message-call-orig-fun "init-core.el")

(use-package ibuffer
  :straight (:type built-in)
  :hook (ibuffer-hook . ibuffer-auto-mode)
  :bind ("C-x C-b" . ibuffer-jump)
  :custom
  (ibuffer-display-summary nil)
  (ibuffer-default-sorting-mode 'alphabetic)
  (ibuffer-use-header-line t)
  (ibuffer-show-empty-filter-groups nil "Do not show empty groups if there are no buffers")
  :config (defalias 'list-buffers 'ibuffer))

;; Provides ibuffer filtering and sorting functions to group buffers by function or regexp applied
;; to `default-directory'. By default buffers are grouped by `project-current' or by
;; `default-directory'.

;; (use-package ibuffer-project
;;   :after project
;;   :hook
;;   (ibuffer-hook
;;     .
;;     (lambda ()
;;       (unless (eq ibuffer-sorting-mode 'project-file-relative)
;;         (ibuffer-do-sort-by-project-file-relative))))
;;   :custom
;;   (ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
;;   (ibuffer-project-use-cache t "Avoid calculating project root, use cache")
;;   :config
;;   ;; Remote buffers will be grouped by protocol and host
;;   (add-to-list 'ibuffer-project-root-functions '(file-remote-p . "Remote")))

(use-package ibuffer-projectile ; Group buffers by Projectile project
  :if (eq sb/project-handler 'projectile)
  :after projectile
  :hook (ibuffer-hook . ibuffer-projectile-set-filter-groups))

;; (use-package vlf ; Speed up Emacs for large files: "M-x vlf <PATH-TO-FILE>"
;;   :demand t
;;   :defines vlf-application
;;   :commands vlf
;;   :init
;;   (setq vlf-application 'dont-ask)
;;   (require 'vlf-setup))

;; When the *scratch* buffer is killed, immediately respawn it.
(use-package immortal-scratch
  :hook (emacs-startup-hook . immortal-scratch-mode))

;; Helps to make the data in the "*scratch*" buffer persist.
(use-package persistent-scratch
  :hook
  (emacs-startup-hook
    .
    (lambda ()
      (ignore-errors
        (persistent-scratch-setup-default))))
  :config (advice-add 'persistent-scratch-setup-default :around #'sb/inhibit-message-call-orig-fun))

(use-package popwin
  :hook (emacs-startup-hook . popwin-mode)
  :config (defvar popwin:special-display-config-backup popwin:special-display-config)

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

;; `ace-window' replaces `other-window' by assigning each window a short, unique label.
(use-package ace-window
  :bind (([remap other-window] . ace-window) ("M-o" . ace-window))
  :custom (aw-minibuffer-flag t)
  :config
  (add-to-list 'aw-ignored-buffers "*toc*")
  (ace-window-display-mode 1))

;; The keybinding will be hidden if we use tmux with its default prefix key, and we will need to
;; press twice.
(use-package ace-jump-buffer
  :bind ("C-b" . ace-jump-buffer)
  :custom
  (ajb-bs-configuration "files-and-scratch")
  (ajb-max-window-height 30)
  (ajb-sort-function 'bs--sort-by-filename "Always predictable"))

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `before-save-hook' and `after-save-hook' and leads to auto-formatters being invoked more
;; frequently.
(use-package super-save
  :defines (super-save-remote-files super-save-triggers super-save-hook-triggers)
  :hook (emacs-startup-hook . super-save-mode)
  :custom (super-save-remote-files nil "Ignore remote files, can cause Emacs to hang")
  :config (add-to-list 'super-save-triggers 'ace-window)
  :diminish)

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
  :defines (dired-clean-confirm-killing-deleted-buffers)
  :hook
  ((dired-mode-hook . auto-revert-mode) ; Auto refresh dired when files change
    (dired-mode-hook . dired-hide-details-mode))
  :bind
  (:map
    dired-mode-map
    ("M-<home>" . sb/dired-go-home)
    ("M-<up>" . sb/dired-jump-to-top)
    ("M-<down>" . sb/dired-jump-to-bottom)
    ("i" . find-file))
  :custom (dired-auto-revert-buffer t "Revert each dired buffer automatically when you revisit it")
  ;; Guess a default target directory. When there are two dired buffers, Emacs will select another
  ;; buffer as the target (e.g., target for copying files).
  (dired-dwim-target t)
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
  :defines dired-cleanup-buffers-too
  :hook
  ;; Load Dired X when Dired is loaded
  (dired-mode-hook
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

  ;; We can also configure dired-omit-extensions

  ;; ":diminish dired-omit-mode" does not work
  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Remove 'Omit' from the modeline."
    (diminish 'dired-omit-mode)
    dired-mode-map))

(use-package dired-narrow ; Narrow `dired' to match filter
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
;;   (dired-mode-hook
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
;;   :hook (dired-mode-hook . dired-async-mode)
;;   :diminish)

;; (use-package dired-rsync
;;   :after dired
;;   :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

(use-package dired-rsync-transient
  :after dired
  :bind (:map dired-mode-map ("C-c C-x" . dired-rsync-transient)))

(use-package diredfl
  :hook (dired-mode-hook . diredfl-mode))

(use-package dired-hist
  :straight (:host github :repo "karthink/dired-hist")
  :hook (dired-mode-hook . dired-hist-mode)
  :bind (:map dired-mode-map ("l" . dired-hist-go-back) ("r" . dired-hist-go-forward)))

(provide 'buffer-dired)

;;; init-buffer.el ends here
