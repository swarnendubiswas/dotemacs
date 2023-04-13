;;; init-tags.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar tags-revert-without-query)

(setq
  large-file-warning-threshold (* 500 1024 1024) ; MB
  tags-add-tables nil
  tags-case-fold-search nil ; "t"=case-insensitive, "nil"=case-sensitive
  ;; Do not ask before rereading the "TAGS" files if they have changed
  tags-revert-without-query t)

;; In Emacs Lisp mode, `xref-find-definitions' will by default find only functions and variables
;; from Lisp packages which are loaded into the current Emacs session or are auto-loaded.
(use-package xref
  ;; :hook
  ;; ((prog-mode-hook LaTeX-mode-hook) . xref-etags-mode)
  :bind
  (("M-'" . xref-find-definitions)
    ("M-?" . xref-find-references)
    ("C-M-." . xref-find-apropos) ; Find all identifiers whose name matches pattern
    ("M-," . xref-go-back)
    :map
    xref--xref-buffer-mode-map
    ("C-o" . xref-show-location-at-point)
    ("<tab>" . xref-quit-and-goto-xref)
    ("r" . xref-query-replace-in-results))
  :custom
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package ivy-xref
  :after (ivy xref)
  :demand t
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package dumb-jump
  :after xref
  :demand t
  :commands dumb-jump-xref-activate
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom (dumb-jump-quiet t))

;; https://github.com/universal-ctags/citre/wiki/Use-Citre-together-with-lsp-mode

(use-package citre
  :preface
  (defun sb/push-point-to-xref-marker-stack (&rest r)
    (xref-push-marker-stack (point-marker)))

  (defun sb/lsp-citre-capf-function ()
    "A capf backend that tries lsp first, then Citre."
    (let ((lsp-result (lsp-completion-at-point)))
      (if
        (and lsp-result
          (try-completion
            (buffer-substring (nth 0 lsp-result) (nth 1 lsp-result))
            (nth 2 lsp-result)))
        lsp-result
        (citre-completion-at-point))))
  :commands (citre-create-tags-file citre-update-tags-file citre-completion-at-point)
  :hook
  ;; Using "(require citre-config)" will enable `citre-mode' for all files as long as it finds a
  ;; tags backend, which is not desired for plain text files.
  (prog-mode-hook . citre-mode)
  :bind
  (("C-x c j" . citre-jump)
    ("M-'" . citre-jump)
    ("C-x c b" . citre-jump-back)
    ("C-x c p" . citre-peek)
    ("C-x c c" . citre-create-tags-file)
    ("C-x c u" . citre-update-this-tags-file)
    ("C-x c e" . citre-edit-tags-file-recipe))
  :custom
  (citre-use-project-root-when-creating-tags t)
  (citre-default-create-tags-file-location 'project-cache)
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-enable-capf-integration t)
  (citre-enable-imenu-integration nil) ; Breaks imenu if enabled
  (citre-edit-cmd-buf-default-cmd
    "ctags
-o
%TAGSFILE%
;; Edit the relevant programming languages to keep the tags file size reasonable
--languages=BibTeX,C,C++,CUDA,CMake,EmacsLisp,Java,Make,Python,Sh,TeX
--kinds-all=*
--fields=*
--extras=*
-R
;; -e
--exclude=@./.ctagsignore
;; add exclude by: --exclude=target
;; add dirs/files to scan here, one line per dir/file")
  :config
  (with-eval-after-load "lsp-mode"
    ;; Enable the lsp + Citre capf backend in current buffer.
    (add-hook 'completion-at-point-functions #'sb/lsp-citre-capf-function nil t))

  ;; FIXME: Is this required?
  ;; (with-eval-after-load "eglot"
  ;;   (add-hook 'citre-mode-hook #'sb/enable-lsp-citre-capf-backend))

  (dolist
    (func
      '(find-function counsel-imenu projectile-grep counsel-rg lsp-ivy-workspace-symbol citre-jump))
    (advice-add func :before 'sb/push-point-to-xref-marker-stack))

  ;; Try lsp first, then use Citre
  (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
    (let
      (
        (fetcher (apply -fn -args))
        (citre-fetcher
          (let ((xref-backend-functions '(citre-xref-backend t)))
            (apply -fn -args))))
      (lambda ()
        (or
          (with-demoted-errors "%s, fallback to citre"
            (funcall fetcher))
          (funcall citre-fetcher)))))

  (with-eval-after-load "company"
    (defmacro citre-backend-to-company-backend (backend)
      "Create a company backend from Citre completion backend BACKEND.
The result is a company backend called
`company-citre-<backend>' (like `company-citre-tags') and can be
used in `company-backends'."
      (let
        (
          (backend-name (intern (concat "company-citre-" (symbol-name backend))))
          (docstring
            (concat
              "`company-mode' backend from the `"
              (symbol-name backend)
              "' Citre backend.\n"
              "`citre-mode' needs to be enabled to use this.")))
        `
        (defun ,backend-name (command &optional arg &rest ignored)
          ,docstring
          (pcase command
            ('interactive (company-begin-backend ',backend-name))
            ('prefix
              (and (bound-and-true-p citre-mode)
                (citre-backend-usable-p ',backend)
                ;; We shouldn't use this as it's defined for getting
                ;; definitions/references. But the Citre completion backend design is not
                ;; fully compliant with company's design so there's no simple "right"
                ;; solution, and this works for tags/global backends.
                (or (citre-get-symbol-at-point-for-backend ',backend) 'stop)))
            ('meta (citre-get-property 'signature arg))
            ('annotation (citre-get-property 'annotation arg))
            ('candidates
              (let ((citre-completion-backends '(,backend)))
                (all-completions arg (nth 2 (citre-completion-at-point)))))))))

    (citre-backend-to-company-backend tags))
  :diminish)

(provide 'init-tags)

;;; init-tags.el ends here
