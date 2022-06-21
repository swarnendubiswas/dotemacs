;;; init-completion.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

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

;; Use "C-M-/" for `dabbrev-completion' which finds all expansions in the current buffer and
;; presents suggestions for completion.
(use-package dabbrev
  :straight (:type built-in)
  :custom
  (dabbrev-completion-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  :bind ("C-M-;" . dabbrev-completion))

;; Replace `dabbrev-exp' with `hippie-expand'.
(use-package hippie-exp
  :straight (:type built-in)
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
   ([remap dabbrev-expand] . hippie-expand)))

;; Use "M-SPC" for space-separated completion lookups
(use-package orderless
  :after (:any ivy vertico)
  :demand t
  :defines orderless-component-separator
  :commands orderless-escapable-split-on-space
  :preface
  (defun sb/just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))
  :custom
  ;; Allow escaping space with backslash
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal
                               orderless-prefixes
                               orderless-initialism
                               orderless-regexp))
  :config
  (with-eval-after-load "ivy"
    (defvar ivy-re-builders-alist)
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder))))

  (with-eval-after-load "company"
    (advice-add 'company-capf--candidates :around #'sb/just-one-face)))

;; To use YASnippet as a non-global minor mode, do not call `yas-global-mode'; instead call
;; `yas-reload-all' to load the snippet tables and then call `yas-minor-mode' from the hooks of
;; major-modes where you want YASnippet enabled.
;; https://github.com/joaotavora/yasnippet/blob/master/README.mdown
(use-package yasnippet
  :commands (snippet-mode yas-hippie-try-expand yas-reload-all)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :hook (prog-mode-hook . yas-global-mode)
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (yas-verbosity 0)
  :config
  (with-eval-after-load "hippie-expand"
    (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand))
  (unbind-key "<tab>" yas-minor-mode-map))

;; YASnippet no longer bundles snippets directly
(use-package yasnippet-snippets
  :after yasnippet
  :commands yasnippet-snippets-initialize)

(use-package ivy-yasnippet
  :if (eq sb/minibuffer-completion 'ivy)
  :after ivy
  :bind ("C-M-y" . ivy-yasnippet))

(use-package consult-yasnippet
  :if (eq sb/minibuffer-completion 'vertico)
  :after consult
  :bind ("C-M-y" . consult-yasnippet))

;; Prescient uses frequency + recency for sorting. Vertico does its own sorting based on recency,
;; and corfu has corfu-history. Company has company-statistics. Furthermore, ivy is not well
;; supported with prescient. So, I do not see a reason to use prescient.

;; (use-package prescient
;;   :commands prescient-persist-mode
;;   :hook (after-init-hook . prescient-persist-mode)
;;   :custom (prescient-sort-full-matches-first t))

;; ;; We want `capf' sort for programming modes, not with recency. This breaks support for the
;; ;; `:separate' keyword in `company'. We are using `company-fuzzy' for sorting completion candidates.
;; (use-package company-prescient
;;   :after (company prescient)
;;   :demand t
;;   :commands company-prescient-mode
;;   :config
;;   ;; (setq company-prescient-sort-length-enable nil)
;;   (company-prescient-mode 1))

(use-package consult-company
  :if (eq sb/minibuffer-completion 'vertico)
  :bind
  (:map company-mode-map
        ([remap completion-at-point] . consult-company)))

;; (use-package fussy
;;   :straight (fussy :type git :host github :repo "jojojames/fussy")
;;   :demand t
;;   :commands fussy-all-completions)

;; "basic" matches only the prefix, "substring" matches the whole string. "initials" matches
;; acronyms and initialisms, e.g., can complete "M-x lch" to "list-command-history".
;; "partial-completion" style allows to use wildcards for file completion and partial paths, e.g.,
;; "/u/s/l" for "/usr/share/local"
(use-package minibuffer
  :straight (:type built-in)
  :custom
  (completion-styles '(orderless basic))
  ;; The "basic" completion style needs to be tried first (not as a fallback) for TRAMP hostname
  ;; completion to work. I want substring matching for file names.
  ;; https://www.reddit.com/r/emacs/comments/nichkl/how_to_use_different_completion_styles_in_the/
  (completion-category-overrides '((file (styles basic substring partial-completion))
                                   (buffer (styles basic substring flex))
                                   (project-file (styles basic substring flex))
                                   (minibuffer (orderless basic initials))))
  ;; Serves as a default value for `completion-category-overrides'
  (completion-category-defaults nil))

(provide 'init-completion)

;;; init-completion.el ends here
