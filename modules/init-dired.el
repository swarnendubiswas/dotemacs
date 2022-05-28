;;; init-dired.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/EMACS27)
(defvar sb/EMACS28+)
(defvar sb/user-home-directory)
(defvar sb/gui-theme)
(defvar sb/minibuffer-completion)

(declare-function s-starts-with? "s")
(declare-function s-ends-with? "s")

(use-package dired
  :straight (:type built-in)
  :commands (dired-next-line dired-jump)
  :defines dired-clean-confirm-killing-deleted-buffers
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
  :bind
  (:map dired-mode-map
        ("M-<home>" . sb/dired-go-home)
        ("M-<up>"   . sb/dired-jump-to-top)
        ("M-<down>" . sb/dired-jump-to-bottom)
        ("i"        . find-file))
  :hook
  ;; Auto refresh dired when files change
  (dired-mode-hook . auto-revert-mode)
  :custom
  (dired-auto-revert-buffer t "Revert each dired buffer automatically when you revisit it")
  ;; Guess a default target directory. When there are two dired buffers, Emacs will select another
  ;; buffer as the target (e.g., target for copying files).
  (dired-dwim-target t)
  ;; Check "ls" for additional options
  (dired-listing-switches "-ABhl --si --group-directories-first")
  (dired-ls-F-marks-symlinks t "-F marks links with @")
  (dired-recursive-copies 'always "Single prompt for all n directories")
  (dired-recursive-deletes 'always "Single prompt for all n directories")
  ;; Do not ask whether to kill buffers visiting deleted files
  (dired-clean-confirm-killing-deleted-buffers nil)
  :config
  (when (boundp 'dired-kill-when-opening-new-dired-buffer)
    (setq dired-kill-when-opening-new-dired-buffer t)))

(use-package dired-x
  :straight (:type built-in)
  :defines dired-cleanup-buffers-too
  :hook
  ;; Load Dired X when Dired is loaded
  (dired-mode-hook . (lambda ()
                       (require 'dired-x)
                       (dired-omit-mode)))
  :bind ("C-x C-j"  . dired-jump)
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
        (concat dired-omit-files
                "\\|^.DS_Store\\'"
                "\\|^.project\\(?:ile\\)?\\'"
                "\\|^.\\(svn\\|git\\)\\'"
                "\\|^.cache\\'"
                "\\|^.ccls-cache\\'"
                "\\|^__pycache__\\'"
                "\\|^eln-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))

  ;; ":diminish dired-omit-mode" does not work
  ;; https://github.com/pdcawley/dotemacs/blob/master/initscripts/dired-setup.el
  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Remove 'Omit' from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map))

(use-package dired-narrow ; Narrow `dired' to match filter
  :after dired
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

;; Do not create multiple dired buffers
(use-package dired+
  :straight (dired+ :type git :host github :repo "emacsmirror/dired-plus")
  :commands diredp-toggle-find-file-reuse-dir
  :init
  ;; Set before the module is loaded
  (setq diredp-bind-problematic-terminal-keys nil)
  :custom
  (diredp-hide-details-initially-flag nil)
  (diredp-hide-details-propagate-flag nil)
  :hook
  (dired-mode-hook . (lambda ()
                       (when sb/EMACS27
                         (diredp-toggle-find-file-reuse-dir 1)))))

;; "r" is bound to `diredp-rename-this-file', but I prefer `dired-efap'. This binding only works if
;; we load `dired-efap' after `dired+' and not `dired', even with `bind-keys*'.
(use-package dired-efap
  :after dired
  :defines dired-efap-initial-filename-selection
  :custom (dired-efap-initial-filename-selection nil)
  :bind*
  (:map dired-mode-map
        ("r" . dired-efap)))

(use-package all-the-icons-dired
  :commands all-the-icons-dired--refresh-advice
  :diminish
  :hook
  (dired-mode-hook . (lambda ()
                       (unless (file-remote-p default-directory)
                         (all-the-icons-dired-mode 1)))))

(use-package treemacs
  :functions treemacs-tag-follow-mode
  :commands (treemacs-current-workspace
             treemacs--find-current-user-project
             treemacs-do-add-project-to-workspace
             treemacs-add-project-to-workspace treemacs-git-mode
             treemacs-follow-mode treemacs-fringe-indicator-mode
             treemacs-filewatch-mode treemacs-goto-file-node
             treemacs--propagate-new-icons
             treemacs-scope->current-scope
             treemacs--restore-eldoc-after-log
             treemacs-load-theme treemacs-find-file-node
             treemacs-indent-guide-mode treemacs-resize-icons
             treemacs-select-window
             treemacs-add-and-display-current-project
             treemacs-display-current-project-exclusively
             projectile-project-p treemacs--select-workspace-by-name
             adob--rescan-windows)
  :preface
  ;; The problem is there is no toggle support.
  (defun sb/setup-treemacs-quick ()
    "Setup treemacs."
    (interactive)
    (when (projectile-project-p)
      (treemacs-display-current-project-exclusively)
      (other-window 1)))

  (defun sb/setup-treemacs-detailed (args)
    "Setup treemacs."
    (let* ((root (treemacs--find-current-user-project))
           (path (treemacs-canonical-path root))
           (name (treemacs--filename path)))
      (unless (treemacs-current-workspace)
        (treemacs--find-workspace))
      (if (treemacs-workspace->is-empty?)
          (progn
            (treemacs-do-add-project-to-workspace path name)
            (treemacs-select-window)
            (treemacs-pulse-on-success)
            (other-window 1)
            (when (featurep 'auto-dim-other-buffers)
              (adob--rescan-windows)))
        (treemacs-select-window)
        (if (treemacs-is-path path :in-workspace)
            (treemacs-goto-file-node path)
          (treemacs-add-project-to-workspace path name))
        (other-window 1)
        (when (featurep 'auto-dim-other-buffers)
          (adob--rescan-windows)))))

  (defun sb/treemacs-ignore-files (filename absolute-path)
    "Ignore files in the Treemacs explorer"
    (or
     (-contains? '("__pycache__" "node_modules" "package-lock.json") filename)
	 (s-ends-with? ".pyc" filename)
	 (s-ends-with? ".elc" filename)
	 (s-ends-with? ".o" filename)
	 (s-ends-with? ".so" filename)
	 (s-ends-with? ".dll" filename)
     ))
  :config
  (setq treemacs-follow-after-init t
        treemacs-indentation 1
        ;; Prevents Treemacs from being selected with `other-window' if non-nil, but it hurts easy
        ;; navigability. Use `treemacs-select-window'.
        treemacs-is-never-other-window t
        treemacs-project-follow-cleanup t
        treemacs-missing-project-action 'remove
        treemacs-recenter-after-file-follow 'on-distance
        treemacs-recenter-after-tag-follow 'on-distance
        treemacs-silent-refresh t ; Silence all refresh messages including file watches
        treemacs-width 24
        ;; Hide the mode-line in the Treemacs buffer
        treemacs-user-mode-line-format 'none)

  ;; (if (display-graphic-p)
  ;;     (setq treemacs-indentation-string (propertize "⫶" 'face 'font-lock-comment-face))
  ;;   (setq treemacs-indentation-string (propertize "|" 'face 'font-lock-comment-face)))

  (treemacs-filewatch-mode 1)
  ;; `treemacs-tag-follow-mode' disables `treemacs-follow-mode', focuses the tag, but following tags
  ;; in noisy
  (treemacs-follow-mode 1)
  (treemacs-git-mode 'deferred)
  (treemacs-fringe-indicator-mode 'always) ; Always show the file indicator
  (treemacs-indent-guide-mode 1)
  ;; (treemacs-project-follow-mode 1) ; Ignores workspace features

  ;; https://github.com/Alexander-Miller/treemacs/issues/735
  ;; (treemacs-create-theme "Default-Tighter"
  ;;   :extends "Default"
  ;;   :config
  ;;   (let ((icons (treemacs-theme->gui-icons theme)))
  ;;     (maphash (lambda
  ;;                (ext icon)
  ;;                (puthash ext
  ;;                         (concat
  ;;                          (substring icon 0 1)
  ;;                          (propertize " " 'display
  ;;                                      '(space . (:width 0.5))))
  ;;                         icons))
  ;;              icons)))

  ;; (treemacs-create-theme "all-the-icons-tighter"
  ;;   :extends "all-the-icons"
  ;;   :config
  ;;   (let ((icons (treemacs-theme->gui-icons theme)))
  ;;     (maphash (lambda
  ;;                (ext icon)
  ;;                (puthash ext
  ;;                         (concat
  ;;                          (substring icon 0 1)
  ;;                          (propertize " " 'display
  ;;                                      '(space . (:width 0.5))))
  ;;                         icons))
  ;;              icons)))

  (set-face-attribute 'treemacs-directory-collapsed-face nil :height 0.8)
  (set-face-attribute 'treemacs-directory-face           nil :height 0.7)
  (set-face-attribute 'treemacs-file-face                nil :height 0.7)
  (set-face-attribute 'treemacs-root-face                nil :height 0.7)
  (set-face-attribute 'treemacs-tags-face                nil :height 0.7)
  (set-face-attribute 'treemacs-git-ignored-face         nil :height 0.7)
  (set-face-attribute 'treemacs-git-untracked-face       nil :height 0.7)
  (set-face-attribute 'treemacs-git-modified-face        nil :height 0.7)
  (set-face-attribute 'treemacs-git-unmodified-face      nil :height 0.7)

  (when (or (eq sb/gui-theme 'sb/customized)
            (eq sb/gui-theme 'none))
    (set-face-attribute 'treemacs-git-modified-face   nil :height 0.8)
    (set-face-attribute 'treemacs-git-unmodified-face nil :height 1.0))

  (when (display-graphic-p)
    (treemacs-resize-icons 16))

  (add-to-list 'treemacs-ignored-file-predicates #'sb/treemacs-ignore-files)
  :bind*
  (;; The keybinding interferes with `dired-jump' and imenu `C-c C-j'
   ("C-j"     . treemacs)
   ("C-c t d" . treemacs-add-and-display-current-project)
   ("C-c t e" . treemacs-display-current-project-exclusively)
   ("M-0"     . treemacs-select-window)))

(use-package treemacs-all-the-icons
  :if (display-graphic-p)
  :after treemacs
  :demand t
  :config (treemacs-load-theme "all-the-icons"))

(use-package dired-async
  :straight async
  :after (dired async)
  :diminish
  :hook (dired-mode-hook . dired-async-mode))

(use-package consult-dir
  :if (eq sb/minibuffer-completion 'vertico)
  :bind
  (("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

(provide 'init-dired)

;;; init-dired.el ends here
