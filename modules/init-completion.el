;;; init-completion.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar sb/extras-directory)
(defvar sb/EMACS28+)
(defvar sb/capf)

(defvar hippie-expand-verbose)
(defvar savehist-additional-variables)
(defvar recentf-list)
(defvar dabbrev-ignored-buffer-regexps)
(defvar which-key-use-C-h-commands)
(defvar dabbrev-completion-ignored-buffer-regexps)

(declare-function sb/inhibit-message-call-orig-fun "init-core.el")

(setq
  completion-ignore-case t ; Ignore case when completing
  ;; Ignore case when reading a buffer name
  read-buffer-completion-ignore-case t)

(dolist
  (exts '(".dll" ".exe" ".fdb_latexmk" ".fls" ".lof" ".pyc" ".rel" ".rip" ".synctex.gz" "TAGS"))
  (add-to-list 'completion-ignored-extensions exts))

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
  :preface
  (defun sb/just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  :demand t
  :defines orderless-component-separator
  :commands orderless-escapable-split-on-space
  :custom
  ;; Allow escaping space with backslash
  (orderless-component-separator 'orderless-escapable-split-on-space)
  :config
  (with-eval-after-load "ivy"
    (defvar ivy-re-builders-alist)
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
    ;; `counsel-rg' fails with `orderless'
    (add-to-list
      'ivy-highlight-functions-alist
      '(orderless-ivy-re-builder . orderless-ivy-highlight)))

  (with-eval-after-load "company"
    (advice-add 'company-capf--candidates :around #'sb/just-one-face)))

;; It is recommended to load `yasnippet' before `eglot'
(use-package yasnippet
  :commands (snippet-mode yas-hippie-try-expand yas-reload-all)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook ((prog-mode-hook org-mode-hook LaTeX-mode-hook latex-mode-hook) . yas-minor-mode-on)
  :custom (yas-verbosity 0)
  :config
  (with-eval-after-load "hippie-expand"
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  (unbind-key "<tab>" yas-minor-mode-map)
  :diminish yas-minor-mode)

;; YASnippet no longer bundles snippets directly
(use-package yasnippet-snippets :after yasnippet :commands yasnippet-snippets-initialize)

;; Prescient uses frecency (frequency + recency) for sorting. recently used commands should be
;; sorted first. Only commands that have never been used before will be sorted by length. Vertico
;; does its own sorting based on recency, and Corfu has corfu-history. Company has
;; company-statistics. Ivy is not actively supported with prescient.

;; (use-package prescient
;;   :straight (:host github :repo "radian-software/prescient.el" :files (:defaults "/*.el"))
;;   :hook (emacs-startup-hook . prescient-persist-mode)
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

;; NOTE: "basic" matches only the prefix, "substring" matches the whole string. "initials" matches
;; acronyms and initialisms, e.g., can complete "M-x lch" to "list-command-history".
;; "partial-completion" style allows to use wildcards for file completion and partial paths, e.g.,
;; "/u/s/l" for "/usr/share/local". While "partial-completion" matches search terms must match in
;; order, "orderless" can match search terms in any order.

;; https://www.reddit.com/r/emacs/comments/y4sec4/how_to_get_corfu_completions_that_include/
;; https://www.reddit.com/r/emacs/comments/nichkl/how_to_use_different_completion_styles_in_the/

(use-package minibuffer
  :straight (:type built-in)
  :after orderless
  :bind
  (("M-p" . minibuffer-previous-completion)
    ("M-n" . minibuffer-next-completion)
    ("M-RET" . minibuffer-choose-completion))
  :custom
  (completions-format 'vertical)
  (read-file-name-completion-ignore-case t "Ignore case when reading a file name")
  (completion-cycle-threshold 3 "TAB cycle if there are only few candidates")
  ;; substring is needed to complete common prefix, orderless does not
  (completion-styles '(orderless substring basic))
  (completion-category-defaults nil)
  :config
  ;; Show docstring description for completion candidates in commands like `describe-function'.
  (when sb/EMACS28+
    (setq completions-detailed t))

  (setq
    ;; The "basic" completion style needs to be tried first for TRAMP hostname completion to
    ;; work. I also want substring matching for file names.
    completion-category-overrides
    '
    ((file (styles basic substring partial-completion))
      ;; (buffer (styles basic substring flex))
      ;; (project-file (styles basic substring flex))
      ;; (minibuffer (orderless flex))
      )))

(provide 'init-completion)

;;; init-completion.el ends here
