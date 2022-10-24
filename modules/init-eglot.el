;;; init-eglot.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(use-package eglot
  :after flycheck
  :commands
  (eglot eglot-ensure eglot-server-programs eglot-rename eglot-code-actions eglot-format
         eglot-find-declaration eglot-find-implementation)
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

(provide 'init-eglot)

;;; init-eglot.el ends here
