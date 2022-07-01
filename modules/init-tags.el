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

(use-package xref
  :commands xref-etags-mode
  :custom (xref-search-program 'ripgrep)
  :bind
  (("M-'"   . xref-find-definitions)
   ("M-?"   . xref-find-references)
   ("C-M-." . xref-find-apropos)
   ("M-,"   . xref-go-back)
   :map xref--xref-buffer-mode-map
   ("C-o"   . xref-show-location-at-point)
   ("<tab>" . xref-quit-and-goto-xref)
   ("r"     . xref-query-replace-in-results)))

(use-package dumb-jump
  :after xref
  :demand t
  :commands dumb-jump-xref-activate
  :custom (dumb-jump-quiet t)
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package ivy-xref
  :if (eq sb/minibuffer-completion 'ivy)
  :after (ivy xref)
  :demand t
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function       #'ivy-xref-show-xrefs))

(use-package counsel-etags
  :defines (counsel-etags-ignore-directories counsel-etags-ignore-filenames)
  :commands counsel-etags-virtual-update-tags
  :if (and (symbol-value 'sb/IS-LINUX) (executable-find "ctags"))
  :bind
  (("M-]"     . counsel-etags-find-tag-at-point)
   ("C-c g s" . counsel-etags-find-symbol-at-point)
   ("C-c g f" . counsel-etags-find-tag)
   ("C-c g l" . counsel-etags-list-tag)
   ("C-c g c" . counsel-etags-scan-code))
  :config
  (defalias 'list-tags 'counsel-etags-list-tag-in-current-file)

  (dolist (ignore-dirs '(".vscode" "build" ".metadata" ".recommenders" ".clangd" ".cache"))
    (add-to-list 'counsel-etags-ignore-directories ignore-dirs))

  (dolist (ignore-files '(".clang-format" ".clang-tidy" "*.json" "*.html" "*.xml"))
    (add-to-list 'counsel-etags-ignore-filenames ignore-files))

  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'counsel-etags-virtual-update-tags 'append 'local))))

(provide 'init-tags)

;;; init-tags.el ends here
