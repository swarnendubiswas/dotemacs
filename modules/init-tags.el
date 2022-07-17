;;; init-tags.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar tags-revert-without-query)

(setq large-file-warning-threshold (* 500 1024 1024) ; MB
      tags-add-tables nil
      tags-case-fold-search nil ; t=case-insensitive, nil=case-sensitive
      ;; Do not ask before rereading the `TAGS' files if they have changed
      tags-revert-without-query t)

;; In Emacs Lisp mode, `xref-find-definitions' will by default find only functions and variables
;; from Lisp packages which are loaded into the current Emacs session or are auto-loaded.
(use-package xref
  :hook
  ((prog-mode-hook LaTeX-mode-hook) . xref-etags-mode)
  :bind
  (("M-'"   . xref-find-definitions)
   ("M-?"   . xref-find-references)
   ;; Find all identifiers whose name matches pattern
   ("C-M-." . xref-find-apropos)
   ("M-,"   . xref-go-back)
   :map xref--xref-buffer-mode-map
   ("C-o"   . xref-show-location-at-point)
   ("<tab>" . xref-quit-and-goto-xref)
   ("r"     . xref-query-replace-in-results))
  :custom
  (xref-search-program 'ripgrep))

(use-package dumb-jump
  :after xref
  :demand t
  :commands dumb-jump-xref-activate
  :hook
  (xref-backend-functions . dumb-jump-xref-activate)
  :custom
  (dumb-jump-quiet t))

(use-package ivy-xref
  :if (eq sb/minibuffer-completion 'ivy)
  :after (ivy xref)
  :demand t
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function       #'ivy-xref-show-xrefs))

;; By default, the output file name of `u-ctags' is `tags', and it is `TAGS' with `etags' enabled.
;; (use-package counsel-etags
;;   :defines (counsel-etags-ignore-directories counsel-etags-ignore-filenames)
;;   :commands counsel-etags-virtual-update-tags
;;   :if (and (symbol-value 'sb/IS-LINUX) (executable-find "ctags"))
;;   :bind
;;   (("M-]"     . counsel-etags-find-tag-at-point)
;;    ("C-c g s" . counsel-etags-find-symbol-at-point)
;;    ("C-c g f" . counsel-etags-find-tag)
;;    ("C-c g l" . counsel-etags-list-tag)
;;    ("C-c g c" . counsel-etags-scan-code))
;;   :config
;;   (defalias 'list-tags 'counsel-etags-list-tag-in-current-file)

;;   (dolist (ignore-dirs '("build" ".metadata" ".recommenders" ".clangd" ".cache"))
;;     (add-to-list 'counsel-etags-ignore-directories ignore-dirs))

;;   (dolist (ignore-files '(".clang-tidy" "*.json" "*.html" "*.xml"))
;;     (add-to-list 'counsel-etags-ignore-filenames ignore-files))

;;   (add-hook 'prog-mode-hook
;;             (lambda ()
;;               (add-hook 'after-save-hook #'counsel-etags-virtual-update-tags 'append 'local))))

;; https://github.com/universal-ctags/citre/wiki/Use-Citre-together-with-lsp-mode
(use-package citre
  :preface
  (defun sb/citre-jump+ ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error (let* ((xref-prompt-for-identifier nil))
               (call-interactively #'xref-find-definitions)))))

  (defun sb/push-point-to-xref-marker-stack (&rest r)
    (xref-push-marker-stack (point-marker)))
  :demand t
  :commands (citre-create-tags-file citre-update-tags-file)
  :init
  (require 'citre-config)
  :bind
  (("C-x c j" . citre-jump)
   ("M-'"     . sb/citre-jump+)
   ("C-x c J" . citre-jump-back)
   ("C-x c p" . citre-peek)
   ("C-x c c" . citre-create-tags-file)
   ("C-x c u" . citre-update-this-tags-file)
   ("C-x c e" . citre-edit-tags-file-recipe))
  :custom
  (citre-use-project-root-when-creating-tags t)
  (citre-default-create-tags-file-location 'project-cache)
  (citre-prompt-language-for-ctags-command nil)
  (citre-auto-enable-citre-mode-modes '(prog-mode latex-mode))
  (citre-edit-cmd-buf-default-cmd "ctags
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
  (dolist (func '(find-function
                  counsel-imenu
                  projectile-grep
                  counsel-rg
                  lsp-ivy-workspace-symbol
                  citre-jump))
    (advice-add func :before 'sb/push-point-to-xref-marker-stack))

  (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
    (let ((fetcher (apply -fn -args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (apply -fn -args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher)))))
  :diminish)

(provide 'init-tags)

;;; init-tags.el ends here
