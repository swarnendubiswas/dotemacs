;;; init-lsp.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/capf)

(declare-function lsp-register-client "lsp-mode")
(declare-function make-lsp-client "lsp-mode")

;; Registering `lsp-format-buffer' makes sense only if the server is active. We may not always want
;; to format unrelated files and buffers (e.g., commented YAML files in out-of-project locations).
(use-package lsp-mode
  :preface
  ;; https://github.com/minad/corfu/wiki
  (defun sb/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(flex))
    (with-eval-after-load "orderless"
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults)) '(orderless))))
  :if (eq sb/lsp-provider 'lsp-mode)
  :after tree-sitter
  :defines
  (lsp-perl-language-server-path
    lsp-perl-language-server-port
    lsp-perl-language-server-client-version
    lsp-completion--regex-fuz
    lsp-clients-clangd-args
    lsp-clients-clangd-executable
    lsp-completion-enable-additional-text-edit
    lsp-completion-show-detail
    lsp-completion-provider
    lsp-completion-show-kind
    lsp-enable-semantic-tokens
    lsp-enable-which-key-integration
    lsp-headerline-breadcrumb-mode
    lsp-html-format-wrap-line-length
    lsp-html-format-end-with-newline
    lsp-html-format-indent-inner-html
    lsp-html-format-max-preserve-new-lines
    lsp-xml-logs-client
    lsp-xml-jar-file
    lsp-xml-jar-version
    lsp-yaml-print-width
    lsp-headerline-breadcrumb-enable-diagnostics
    lsp-modeline-diagnostics-scope)
  :commands (lsp-deferred lsp-describe-thing-at-point)
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind
  (("M-." . xref-find-definitions)
    :map
    lsp-command-map
    ("=")
    ("w")
    ("g")
    ("G")
    ("a")
    ("F")
    ("L" . lsp)
    ("q" . lsp-disconnect)
    ("Q" . lsp-workspace-shutdown)
    ("H" . lsp-describe-session)
    ("R" . lsp-workspace-restart)
    ("d" . lsp-find-declaration)
    ("e" . lsp-find-definition)
    ("r" . lsp-find-references)
    ("i" . lsp-find-implementation)
    ("I" . lsp-goto-implementation)
    ("t" . lsp-goto-type-definition)
    ("r" . lsp-rename)
    ("h" . lsp-symbol-highlight)
    ("f" . lsp-format-buffer)
    ("x" . lsp-execute-code-action)
    ("c" . lsp-imenu-create-categorised-index) ; sorts the items by kind.
    ("u" . lsp-imenu-create-uncategorised-index) ; sorts the items by position
    ("a" . lsp-workspace-folders-add)
    ("v" . lsp-workspace-folders-remove)
    ("b" . lsp-workspace-blacklist-remove))
  :custom
  ;; We can add "--compile-commands-dir=<build-dir>" option to indicate the directory where
  ;; "compile_commands.json" reside. If path is invalid, clangd will look in the current directory
  ;; and parent paths of each source file.
  (lsp-clients-clangd-args
    '
    ("-j=4"
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
      "--pretty"))
  (lsp-completion-provider :none "Enable integration of custom backends other than `capf'")
  (lsp-completion-show-detail nil "Disable completion metadata, e.g., java.util.ArrayList")
  (lsp-completion-show-kind t "Show completion kind, e.g., interface/class")
  (lsp-completion-show-label-description t "Show description of completion candidates")
  (lsp-eldoc-enable-hover nil "Do not show noisy hover info with mouse")
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-on-type-formatting nil "Reduce unexpected modifications to code")
  (lsp-enable-folding nil "I do not find the feature useful")
  (lsp-headerline-breadcrumb-enable nil "Breadcrumb is not useful for all modes")
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-diagnostics-provider :auto "Prefer flycheck, otherwise use flymake")
  ;; Do not customize breadcrumb faces based on errors
  (lsp-html-format-wrap-line-length sb/fill-column)
  (lsp-html-format-end-with-newline t)
  (lsp-html-format-indent-inner-html t)
  (lsp-imenu-sort-methods '(position))
  (lsp-lens-enable nil "Lenses are intrusive")
  ;; We have `flycheck' error summary listed on the modeline, but the `lsp' server may report
  ;; additional errors. The downside is that the modeline gets congested.
  (lsp-modeline-diagnostics-enable (eq sb/modeline-theme 'doom-modeline))
  (lsp-modeline-diagnostics-scope :file "Simpler to focus on the errors at hand")
  (lsp-modeline-code-actions-enable t "Useful to show code actions on the modeline")
  (lsp-modeline-workspace-status-enable t)
  ;; Sudden changes in the height of the echo area causes the cursor to lose position, manually
  ;; request via `lsp-signature-activate'.
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation t "Show the function documentation along with the prototype")
  (lsp-restart 'auto-restart "Avoid annoying questions, we expect a server restart to succeed")
  (lsp-xml-logs-client nil)
  (lsp-yaml-print-width sb/fill-column)
  (lsp-warn-no-matched-clients nil)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-file-watchers nil)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-symbol-highlighting t)
  :config
  (dolist
    (ignore-dirs
      '
      ("/build\\'"
        "/\\.metadata\\'"
        "/\\.recommenders\\'"
        "/\\.clangd\\'"
        "/\\.cache\\'"
        "/__pycache__\\'"
        "/\\.log\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories ignore-dirs))

  (with-eval-after-load "lsp-lens"
    (diminish 'lsp-lens-mode))

  (with-eval-after-load "corfu"
    ;; (add-hook 'text-mode-hook (lambda () (setq-local lsp-completion-enable nil)))
    (add-hook 'lsp-completion-mode-hook #'sb/lsp-mode-setup-completion))
  :diminish)

(use-package lsp-ui
  :after lsp-mode
  :defines lsp-ui-modeline-code-actions-enable
  :commands (lsp-ui-doc-mode lsp-ui-mode lsp-ui-peek-find-implementation lsp-ui-imenu)
  :hook (lsp-mode-hook . lsp-ui-mode)
  :bind
  (:map
    lsp-ui-mode-map
    ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
    ([remap xref-find-references] . lsp-ui-peek-find-references)
    :map
    lsp-command-map
    ("D" . lsp-ui-doc-show))
  :custom
  (lsp-ui-doc-enable nil "Disable intrusive on-hover dialogs, invoke with `lsp-ui-doc-show'")
  (lsp-ui-doc-include-signature t)
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-sideline-show-code-actions t "Enables understanding when to invoke code actions")
  (lsp-ui-sideline-enable nil "Noisy to show symbol information in the sideline")
  ;; Hide diagnostics when typing because they can be intrusive
  (lsp-ui-sideline-show-diagnostics nil "Flycheck/flymake already highlights errors")
  (lsp-ui-doc-max-width 72 "150 (default) is too wide")
  (lsp-ui-doc-delay 0.75 "0.2 (default) is too naggy")
  (lsp-ui-peek-enable nil)
  :config
  ;; Enabling the sideline creates flickering with Corfu popon.

  ;; (if (and (display-graphic-p) (eq sb/capf 'corfu))
  ;;   (setq lsp-ui-sideline-enable nil)
  ;;   (setq lsp-ui-sideline-enable t))
  )

;; Sync workspace folders and treemacs projects

;; (use-package lsp-treemacs
;;   :if (eq sb/lsp-provider 'lsp-mode)
;;   :commands (lsp-treemacs-errors-list lsp-treemacs-sync-mode)
;;   :config (lsp-treemacs-sync-mode 1)
;;   :bind
;;   (:map
;;     lsp-command-map
;;     ("S" . lsp-treemacs-symbols)
;;     ("F" . lsp-treemacs-references)
;;     ("Y" . lsp-treemacs-sync-mode)
;;     ("C" . lsp-treemacs-call-hierarchy)
;;     ("T" . lsp-treemacs-type-hierarchy)
;;     ("E" . lsp-treemacs-errors-list)))

;; Try to delete `lsp-java-workspace-dir' if the JDTLS fails
(use-package lsp-java
  :if (eq sb/lsp-provider 'lsp-mode)
  :commands
  (lsp-java-organize-imports
    lsp-java-build-project
    lsp-java-update-project-configuration
    lsp-java-actionable-notifications
    lsp-java-update-user-settings
    lsp-java-update-server
    lsp-java-generate-to-string
    lsp-java-generate-equals-and-hash-code
    lsp-java-generate-overrides
    lsp-java-generate-getters-and-setters
    lsp-java-type-hierarchy
    lsp-java-dependency-list
    lsp-java-extract-to-constant
    lsp-java-add-unimplemented-methods
    lsp-java-create-parameter
    lsp-java-create-field
    lsp-java-create-local
    lsp-java-extract-method
    lsp-java-add-import)
  :hook
  (java-mode-hook
    .
    (lambda ()
      (setq-local
        c-basic-offset 4
        c-set-style "java")
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :custom
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-format-settings-profile "Swarnendu")
  (lsp-java-format-settings-url
    (expand-file-name "github/dotfiles/java/eclipse-format-swarnendu.xml" sb/user-home-directory))
  :config
  (with-eval-after-load "dap-mode"
    (require 'dap-java)))

;; We need to enable lsp workspace to allow `lsp-grammarly' to work, which makes it ineffective for
;; temporary text files. `lsp-grammarly' supports PRO Grammarly accounts. If there are failures,
;; then try logging out of Grammarly and logging in again. Make sure to run "M-x keytar-install".

(use-package lsp-grammarly
  ;; The ":after" clause does not work with the ":hook", `lsp-mode' is not started automatically
  :if (eq sb/lsp-provider 'lsp-mode)
  :defines (lsp-grammarly-active-modes lsp-grammarly-user-words)
  :commands
  (lsp-grammarly--server-command
    lsp-grammarly--init
    lsp-grammarly--get-credentials
    lsp-grammarly--get-token
    lsp-grammarly--store-token
    lsp-grammarly--show-error
    lsp-grammarly--update-document-state)
  :hook ((text-mode-hook markdown-mode-hook org-mode-hook LaTeX-mode-hook) . lsp-deferred)
  :custom
  (lsp-grammarly-suggestions-oxford-comma t)
  (lsp-grammarly-suggestions-passive-voice t)
  (lsp-grammarly-suggestions-informal-pronouns-academic t)
  (lsp-grammarly-suggestions-preposition-at-the-end-of-sentence t)
  (lsp-grammarly-suggestions-conjunction-at-start-of-sentence t)
  (lsp-grammarly-user-words '(Swarnendu Biswas))
  ;; :config (defvar lsp-grammarly-active-modes)
  ;; (lsp-register-client
  ;;   (make-lsp-client
  ;;     :new-connection (lsp-tramp-connection #'lsp-grammarly--server-command)
  ;;     :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-grammarly-active-modes))
  ;;     :priority -1
  ;;     :remote? t
  ;;     :add-on? t
  ;;     :server-id 'grammarly-r
  ;;     :download-server-fn
  ;;     (lambda (_client callback error-callback _update?)
  ;;       (lsp-package-ensure 'grammarly-ls callback error-callback))
  ;;     :after-open-fn #'lsp-grammarly--init
  ;;     :async-request-handlers
  ;;     (ht
  ;;       ("$/getCredentials" #'lsp-grammarly--get-credentials)
  ;;       ("$/getToken" #'lsp-grammarly--get-token)
  ;;       ("$/storeToken" #'lsp-grammarly--store-token)
  ;;       ("$/showError" #'lsp-grammarly--show-error)
  ;;       ("$/updateDocumentState" #'lsp-grammarly--update-document-state))))
  )

(use-package lsp-ltex
  ;; The ":after" clause does not work with the ":hook", `lsp-mode' is not started automatically
  :if (eq sb/lsp-provider 'lsp-mode)
  :defines (lsp-ltex-enabled lsp-ltex-check-frequency lsp-ltex-dictionary lsp-ltex-java-path)
  :commands (lsp-ltex--downloaded-extension-path lsp-ltex--execute)
  :hook ((text-mode-hook markdown-mode-hook org-mode-hook LaTeX-mode-hook) . lsp-deferred)
  :custom
  ;; https://valentjn.github.io/ltex/settings.html#ltexlanguage
  (lsp-ltex-language "en" "Recommended to set a generic language to disable spell check")
  (lsp-ltex-check-frequency "save")
  (lsp-ltex-java-path "/usr/lib/jvm/java-17-openjdk-amd64")
  (lsp-ltex-version "16.0.0")
  (lsp-ltex-dictionary (expand-file-name "company-dict/text-mode" user-emacs-directory))
  :config
  ;; https://github.com/ggbaker/doom-emacs-config/blob/main/config.el
  ;; https://github.com/emacs-languagetool/lsp-ltex/issues/5
  ;; https://www.reddit.com/r/emacs/comments/ril2m4/comment/hqqbxbm/

  ;; Disable spell checking since we cannot get `lsp-ltex' to work with custom dict words.
  ;; Furthermore, we also use `flyspell' and `spell-fu'.

  (setq lsp-ltex-disabled-rules #s(hash-table size 30 data ("en-US" ["MORFOLOGIK_RULE_EN_US"])))

  ;; (setq lsp-ltex-disabled-rules
  ;;       (json-parse-string
  ;;        "{\"en-US\": [\"MORFOLOGIK_RULE_EN_US\"]}"))

  ;; (defvar lsp-ltex-active-modes)

  ;; (lsp-register-client
  ;;   (make-lsp-client
  ;;     :new-connection
  ;;     (lsp-tramp-connection
  ;;       (expand-file-name "lsp/server/ltex-ls/latest/bin/ltex-ls" no-littering-var-directory))
  ;;     :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-ltex-active-modes))
  ;;     :priority -2
  ;;     :add-on? t
  ;;     :remote? t
  ;;     :server-id 'ltex-r
  ;;     :download-server-fn
  ;;     (lambda (_client _callback error-callback _update?)
  ;;       (lsp-package-ensure
  ;;         'ltex-ls
  ;;         (lambda ()
  ;;           (let ((dest (f-dirname (lsp-ltex--downloaded-extension-path))))
  ;;             (unless
  ;;               (lsp-ltex--execute "tar" "-xvzf" (lsp-ltex--downloaded-extension-path) "-C" dest)
  ;;               (error "Error during the unzip process: tar"))))
  ;;         error-callback))))
  )

;; (use-package dap-mode
;;   :after lsp-mode
;;   :commands (dap-step-in dap-next dap-continue dap-debug dap-hydra)
;;   :init (dap-auto-configure-mode)
;;   :hook ((lsp-mode-hook . dap-ui-mode) (lsp-mode-hook . dap-mode))
;;   ;; :bind
;;   ;; ("<f7>" . dap-step-in)
;;   ;; ("<f8>" . dap-next)
;;   ;; ("<f9>" . dap-continue)
;;   )

;; Install with "python3 -m pip install -U pyright --user". Create stubs for a package with "pyright
;; --createstub pandas".

;; (use-package lsp-pyright
;;   :if
;;   (and (eq sb/lsp-provider 'lsp-mode)
;;     (eq sb/python-langserver 'pyright)
;;     (executable-find "pyright"))
;;   :commands (lsp-pyright-locate-python lsp-pyright-locate-venv)
;;   :hook (python-mode-hook . (lambda () (require 'lsp-pyright)))
;;   :custom
;;   (lsp-pyright-python-executable-cmd "python3")
;;   (lsp-pyright-typechecking-mode "basic")
;;   (lsp-pyright-auto-import-completions t)
;;   (lsp-pyright-auto-search-paths t)
;;   :config
;;   (lsp-register-client
;;     (make-lsp-client
;;       :new-connection
;;       (lsp-tramp-connection
;;         (lambda () (cons "pyright-langserver" lsp-pyright-langserver-command-args)))
;;       :major-modes '(python-mode)
;;       :remote? t
;;       :server-id 'pyright-r
;;       :multi-root lsp-pyright-multi-root
;;       :priority 3
;;       :initialization-options
;;       (lambda ()
;;         (ht-merge (lsp-configuration-section "pyright") (lsp-configuration-section "python")))
;;       :initialized-fn
;;       (lambda (workspace)
;;         (with-lsp-workspace
;;           workspace
;;           (lsp--set-configuration
;;             (ht-merge (lsp-configuration-section "pyright") (lsp-configuration-section "python")))))
;;       :download-server-fn
;;       (lambda (_client callback error-callback _update?)
;;         (lsp-package-ensure 'pyright callback error-callback))
;;       :notification-handlers
;;       (lsp-ht
;;         ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
;;         ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
;;         ("pyright/endProgress" 'lsp-pyright--end-progress-callback)))))

;; `lsp-latex' provides better support for the `texlab' server compared to `lsp-tex'. On the other
;; hand, `lsp-tex' supports `digestif'. `lsp-latex' does not require `auctex'. However, the server
;; performance is very poor, so I continue to prefer `auctex'.

(use-package lsp-latex
  :after lsp-mode
  :defines
  (lsp-latex-bibtex-formatter
    lsp-latex-latex-formatter
    lsp-latex-bibtex-formatter-line-length
    lsp-latex-chktex-on-open-and-save
    lsp-latex-build-on-save
    lsp-latex-build-is-continuous
    lsp-latex-build-args
    lsp-latex-diagnostics-delay)
  :hook
  (latex-mode-hook
    .
    (lambda ()
      (require 'lsp-latex)
      (lsp-deferred)))
  :custom
  (lsp-latex-bibtex-formatter "latexindent")
  (lsp-latex-latex-formatter "latexindent")
  (lsp-latex-bibtex-formatter-line-length sb/fill-column)
  (lsp-latex-chktex-on-open-and-save t)
  ;; Delay time in milliseconds before reporting diagnostics
  (lsp-latex-diagnostics-delay 2000)

  ;; Support forward search with Evince. Inverse search is already configured with evince-synctex,
  ;; use Ctrl+Click in the PDF document.
  ;; (lsp-latex-forward-search-executable "evince-synctex")
  ;; “%f” is replaced with "The path of the current TeX file", "%p" with "The path of the current
  ;; PDF file", "%l" with "The current line number" by texlab
  ;; (lsp-latex-forward-search-args '("-f" "%l" "%p" "\"emacsclient +%l %f\""))

  ;; Support forward search with Okular. Perform inverse search with Shift+Click in the PDF.
  (lsp-latex-forward-search-executable "okular")
  (lsp-latex-forward-search-args '("--unique" "file:%p#src:%l%f"))
  :config
  (add-to-list 'lsp-latex-build-args "-c")
  (add-to-list 'lsp-latex-build-args "-pvc")

  ;; (lsp-register-client
  ;;   (make-lsp-client
  ;;     :new-connection (lsp-tramp-connection "texlab")
  ;;     :major-modes '(tex-mode latex-mode LaTeX-mode bibtex-mode)
  ;;     :remote? t
  ;;     :server-id 'texlab-r))
  )

(use-package eglot
  :when (eq sb/lsp-provider 'eglot)
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
  ;; Eglot overwrites `company-backends' to only include `company-capf'
  (eglot-stay-out-of '(flymake company eldoc))
  (eglot-ignored-server-capabilities
    '
    (:codeLensProvider
      :executeCommandProvider
      :hoverProvider ; Automatic documentation popups can be distracting
      :foldingRangeProvider
      :documentOnTypeFormattingProvider
      :documentLinkProvider
      ;; Inlay hints are distracting
      :inlayHintProvider))
  :config
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

;; (use-package eglot-ltex
;;   :straight (:host github :repo "emacs-languagetool/eglot-ltex")
;;   :init
;;   (setq eglot-languagetool-server-path
;;     (expand-file-name "software/ltex-ls-16.0.0" sb/user-home-directory))
;;   :hook
;;   ((text-mode-hook LaTeX-mode-hook org-mode-hook markdown-mode-hook)
;;     .
;;     (lambda ()
;;       (require 'eglot-ltex)
;;       (eglot-ensure)))
;;   :custom (eglot-languagetool-active-modes '(text-mode LaTex-mode org-mode markdown-mode))
;;   :config
;;   ;; (setq eglot-server-programs (delete (car eglot-server-programs) eglot-server-programs))
;;   ;; (add-to-list
;;   ;;   'eglot-server-programs
;;   ;;   `(,eglot-languagetool-active-modes . ,(eglot-languagetool--server-command))
;;   ;;   'append)
;;   ;; (add-to-list 'eglot-server-programs (pop eglot-server-programs) 'append)
;;   ;;   `((:ltex ((:language "en-US") (:disabledRules (:en-US ["MORFOLOGIK_RULE_EN_US"]))))))
;;   )

(use-package eglot-java
  :when (eq sb/lsp-provider 'eglot)
  :hook
  (java-mode-hook
    .
    (lambda ()
      (eglot-ensure)
      (eglot-java-mode))))

(provide 'init-lsp)

;;; init-lsp.el ends here
