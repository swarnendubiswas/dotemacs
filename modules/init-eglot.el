;;; init-eglot.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary: Eglot does not support multiple servers simultaneously per major mode.

;;; Code:

(use-package eglot
  :commands
  (eglot eglot-ensure eglot-server-programs eglot-rename eglot-code-actions eglot-format
         eglot-find-declaration eglot-find-implementation eglot-find-typeDefinition)
  :bind
  (("M-."     . xref-find-definitions)
   ("C-c l q" . eglot-shutdown)
   ("C-c l Q" . eglot-shutdown-all)
   ("C-c l d" . eglot-find-declaration)
   ("C-c l i" . eglot-find-implementation)
   ("C-c l t" . eglot-find-typeDefinition)
   ("C-c l r" . eglot-rename)
   ("C-c l f" . eglot-format-buffer)
   ("C-c l x" . eglot-code-actions))
  :custom
  (eglot-autoshutdown       t)
  (eglot-extend-to-xref     t)
  (eglot-events-buffer-size 0)
  :config
  ;; Eglot overwrites `company-backends' to only include `company-capf'
  (setq eglot-stay-out-of '(flymake company eldoc))
  (setq eglot-ignored-server-capabilities '(:codeLensProvider
                                            :executeCommandProvider
                                            :hoverProvider
                                            :foldingRangeProvider
                                            :documentOnTypeFormattingProvider))

  ;; https://github.com/joaotavora/eglot/discussions/875
  (setq-default eglot-workspace-configuration
                '((pylsp
                   (configurationSources . ["setup.cfg"])
                   (plugins
                    (jedi_completion (include_params . t) (fuzzy . t))
                    (pycodestyle (enabled . nil))
                    (mccabe (enabled . nil))
                    (pyflakes (enabled . nil))
                    (flake8 (enabled . nil))
                    (blacken (enabled . nil))
                    (yapf (enabled . t))
                    (pydocstyle (enabled . t))
                    (pylint (enabled . t))))))

  (add-to-list 'eglot-server-programs '((c++-mode c-mode) .
                                        ("clangd" "-j=4" "--all-scopes-completion"
                                         "--background-index"
                                         "--clang-tidy"
                                         "--completion-style=detailed"
                                         "--fallback-style=LLVM"
                                         "--header-insertion=never"
                                         "--header-insertion-decorators=0"
                                         "--log=error"
                                         "--malloc-trim" ; Release memory periodically
                                         ;; Increases memory usage but can improve performance
                                         "--pch-storage=memory"
                                         "--pretty")))

  ;; It is more useful to use Grammarly to check these files.

  ;; (add-to-list 'eglot-server-programs  '((tex-mode bibtex-mode latex-mode) "texlab"))
  ;; (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  )

(use-package consult-eglot
  :after (consult eglot))

(use-package eglot-grammarly
  :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
  :after eglot
  :hook
  ((text-mode-hook markdown-mode-hook org-mode-hook) . (lambda ()
                                                         (unless (derived-mode-p 'yaml-mode)
                                                           (require 'eglot-grammarly)
                                                           (eglot-ensure))))
  :config
  (add-to-list eglot-workspace-configuration
               ((@emacs-grammarly/grammarly-languageserver
                 . ((audience . "knowledgeable"))))))

;; (use-package eglot-ltex
;;   :hook (text-mode . (lambda ()
;;                        (require 'eglot-ltex)
;;                        (call-interactively #'eglot)))
;;   :init
;;   (setq eglot-languagetool-server-path ""))

(use-package eglot-java
  :after eglot
  :hook
  (java-mode-hook . eglot-java-init))

(provide 'init-eglot)

;;; init-eglot.el ends here
