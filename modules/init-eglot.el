;;; init-eglot.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary: Eglot does not support multiple servers simultaneously per major mode.

;;; Code:

(use-package eglot
  :commands (eglot)
  :bind
  (("M-." . xref-find-definitions)
    ("C-c l q" . eglot-shutdown)
    ("C-c l Q" . eglot-shutdown-all)
    ("C-c l d" . eglot-find-declaration)
    ("C-c l i" . eglot-find-implementation)
    ("C-c l t" . eglot-find-typeDefinition)
    ("C-c l r" . eglot-rename)
    ("C-c l f" . eglot-format)
    ("C-c l F" . eglot-format-buffer)
    ("C-c l x" . eglot-code-actions))
  :hook
  ( ;; (eglot-managed-mode-hook . eglot-inlay-hints-mode) ; Inlay hints are distracting
    (
      (c-mode-hook
        c++-mode-hook
        python-mode-hook
        markdown-mode-hook
        sh-mode-hook
        LaTeX-mode-hook
        bibtex-mode-hook
        html-mode-hook
        json-mode-hook
        perl-mode-hook)
      . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-events-buffer-size 0 "Drop jsonrpc log to improve performance")
  :config
  ;; Eglot overwrites `company-backends' to only include `company-capf'
  (setq
    eglot-stay-out-of '(flymake company eldoc)
    eglot-ignored-server-capabilities
    '
    (:codeLensProvider
      :executeCommandProvider
      :hoverProvider ; Automatic documentation popups can be distracting
      :foldingRangeProvider
      :documentOnTypeFormattingProvider
      :documentLinkProvider
      :documentHighlightProvider
      ;; Inlay hints are distracting
      :inlayHintProvider))

  ;; Show all of the available eldoc information when we want it. This way Flymake errors
  ;; don't just get clobbered by docstrings.
  (add-hook
    'eglot-managed-mode-hook
    (lambda ()
      "Make sure Eldoc will show us all of the feedback at point."
      (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)))

  (advice-add 'jsonrpc--log-event :around (lambda (_orig-func &rest _)))

  ;; (setq-default eglot-workspace-configuration
  ;;   '
  ;;   (
  ;;     (:pylsp
  ;;       .
  ;;       (:configurationSources
  ;;         ["setup.cfg"]
  ;;         :plugins
  ;;         (:jedi_completion
  ;;           (:include_params t :fuzzy t)
  ;;           :pycodestyle (:enabled :json-false)
  ;;           :mccabe (:enabled :json-false)
  ;;           :pyflakes (:enabled :json-false)
  ;;           :flake8 (:enabled :json-false :maxLineLength 100)
  ;;           :black (:enabled :json-false :line_length 100)
  ;;           :yapf (:enabled t)
  ;;           :pydocstyle (:enabled t :convention "numpy")
  ;;           :autopep8 (:enabled :json-false)
  ;;           :pylint (:enabled t)
  ;;           :pylsp_isort (:enabled t)
  ;;           :pylsp_mypy (:enabled t))))
  ;;     (:pyright . ((:useLibraryCodeForTypes t)))))

  ;; (add-hook
  ;;   'before-save-hook
  ;;   (lambda ()
  ;;     (when (derived-mode-p 'rust-mode 'rust-ts-mode)
  ;;       (ignore-errors
  ;;         (eglot-code-action-organize-imports))
  ;;       (eglot-format-buffer))))

  (add-to-list
    'eglot-server-programs
    '
    ((c++-mode c++-ts-mode c-mode c-ts-mode)
      .
      ("clangd"
        "-j=4"
        "--all-scopes-completion"
        "--background-index"
        "--clang-tidy"
        "--completion-style=detailed"
        "--fallback-style=LLVM"
        "--header-insertion=never"
        "--header-insertion-decorators=0"
        "--log=error"
        ;; Unsupported option with Clangd 10
        ;; "--malloc-trim" ; Release memory periodically
        "--pch-storage=memory" ; Increases memory usage but can improve performance
        "--pretty")))

  ;; (add-to-list
  ;;   'eglot-server-programs
  ;;   `
  ;;   ((python-mode python-ts-mode)
  ;;     .
  ;;     ,(eglot-alternatives '(("pylsp") ("pyright-langserver" "--stdio")))))
  ;; (add-to-list 'eglot-server-programs '((php-mode phps-mode) . ("intelephense" "--stdio")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  ;; (add-to-list 'eglot-server-programs '(web-mode . ("vscode-html-language-server" "--stdio")))
  )

;; FIXME: Disable documentSymbol because otherwise imenu does not work

;; (use-package eglot-grammarly
;;   :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
;;   :hook
;;   ((text-mode-hook LaTeX-mode-hook org-mode-hook markdown-mode-hook)
;;     .
;;     (lambda ()
;;       (require 'eglot-grammarly)
;;       (eglot-ensure)))
;;   :custom (eglot-grammarly-active-modes '(text-mode LaTeX-mode org-mode markdown-mode))
;;   :config
;;   ;; (setq eglot-server-programs (delete (car eglot-server-programs) eglot-server-programs))
;;   ;; (add-to-list
;;   ;;   'eglot-server-programs
;;   ;;   `(,eglot-grammarly-active-modes . ,(eglot-grammarly--server-command))
;;   ;;   'append)
;;   (add-to-list 'eglot-server-programs (pop eglot-server-programs) 'append)
;;   ;; (add-to-list eglot-workspace-configuration
;;   ;;              ((@emacs-grammarly/grammarly-languageserver
;;   ;;                ((audience "knowledgeable")))))
;;   )

;; FIXME: Fix issue with SLF4J with LTEX 16.0.0
(use-package eglot-ltex
  :straight (:host github :repo "emacs-languagetool/eglot-ltex")
  :init
  (setq eglot-languagetool-server-path
    (expand-file-name "software/ltex-ls-16.0.0" sb/user-home-directory))
  :hook
  ((text-mode-hook LaTeX-mode-hook org-mode-hook markdown-mode-hook)
    .
    (lambda ()
      (require 'eglot-ltex)
      (eglot-ensure)))
  :custom (eglot-languagetool-active-modes '(text-mode LaTex-mode org-mode markdown-mode))
  :config
  ;; (setq eglot-server-programs (delete (car eglot-server-programs) eglot-server-programs))
  ;; (add-to-list
  ;;   'eglot-server-programs
  ;;   `(,eglot-languagetool-active-modes . ,(eglot-languagetool--server-command))
  ;;   'append)
  ;; (add-to-list 'eglot-server-programs (pop eglot-server-programs) 'append)
  ;;   `((:ltex ((:language "en-US") (:disabledRules (:en-US ["MORFOLOGIK_RULE_EN_US"]))))))
  )

(use-package eglot-java
  :hook
  (java-mode-hook
    .
    (lambda ()
      (eglot-ensure)
      (eglot-java-mode))))

(provide 'init-eglot)

;;; init-eglot.el ends here
