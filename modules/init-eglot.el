;;; init-eglot.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(use-package eglot
  :after flycheck
  :commands
  (eglot eglot-ensure eglot-server-programs eglot-rename eglot-code-actions eglot-format
         eglot-find-declaration eglot-find-implementation eglot-find-typeDefinition)
  :bind
  (("M-." . eglot-find-implementation)
   ("C-c l q" . eglot-shutdown)
   ("C-c l Q" . eglot-shutdown-all)
   ("C-c l d" . eglot-find-declaration)
   ("C-c l i" . eglot-find-implementation)
   ("C-c l I" . lsp-goto-implementation)
   ("C-c l t" . eglot-find-typeDefinition)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format)
   ("C-c l x" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) .
                                        ("clangd" "-j=4" "--all-scopes-completion"
                                         "--background-index"
                                         "--clang-tidy"
                                         "--completion-style=detailed"
                                         "--fallback-style=LLVM"
                                         "--header-insertion=never"
                                         "--header-insertion-decorators=0"
                                         "--log=error"
                                         "--malloc-trim" ;; Release memory periodically
                                         ;; Increases memory usage but can improve performance
                                         "--pch-storage=memory"
                                         "--pretty")))
  ;; (add-to-list 'eglot-server-programs  '((tex-mode bibtex-mode latex-mode) "texlab"))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman"))))

(use-package consult-eglot
  :if (eq sb/minibuffer-completion 'vertico)
  :after eglot)

(use-package eglot-grammarly
  :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
  :hook
  ((text-mode-hook markdown-mode-hook org-mode-hook). (lambda ()
                                                        (require 'eglot-grammarly)
                                                        (eglot-ensure))))

;; (use-package eglot-ltex
;;   :ensure t
;;   :hook (text-mode . (lambda ()
;;                        (require 'eglot-ltex)
;;                        (call-interactively #'eglot)))
;;   :init
;;   (setq eglot-languagetool-server-path ""))

(provide 'init-eglot)

;;; init-eglot.el ends here
