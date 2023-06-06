;;; init-dired.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/EMACS27)
(defvar sb/EMACS28+)
(defvar sb/user-home-directory)
(defvar sb/minibuffer-completion)

(declare-function s-starts-with? "s")
(declare-function s-ends-with? "s")

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
  (dired-mode-hook . auto-revert-mode) ; Auto refresh dired when files change
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
(use-package dired+
  :straight (:host github :repo "emacsmirror/dired-plus")
  :commands diredp-toggle-find-file-reuse-dir
  :init
  ;; Set before the module is loaded
  (setq diredp-bind-problematic-terminal-keys nil)
  :hook
  (dired-mode-hook
    .
    (lambda ()
      (when sb/EMACS27
        (diredp-toggle-find-file-reuse-dir 1))))
  :custom
  (diredp-hide-details-initially-flag nil)
  (diredp-hide-details-propagate-flag nil))

;; ;; "r" is bound to `diredp-rename-this-file', but I prefer `dired-efap'. This binding only works if
;; ;; we load `dired-efap' after `dired+' and not `dired', even with `bind-keys*'.
;; (use-package dired-efap
;;   :disabled t ; Recent themes do not seem to support in-place rename with efap well.
;;   :after dired
;;   :defines dired-efap-initial-filename-selection
;;   :bind*
;;   (:map dired-mode-map
;;         ("r" . dired-efap))
;;   :custom
;;   (dired-efap-initial-filename-selection nil))

(use-package dired-async
  :straight async
  :after (dired async)
  :hook (dired-mode-hook . dired-async-mode)
  :diminish)

(use-package dired-rsync
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

(provide 'init-dired)

;;; init-dired.el ends here
