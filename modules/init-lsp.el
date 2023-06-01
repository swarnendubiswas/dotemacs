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
  :commands
  (lsp--set-configuration
    lsp-completion--regex-fuz
    lsp-register-client
    lsp-tramp-connection
    make-lsp-client
    lsp-format-buffer
    lsp-configuration-section
    lsp
    lsp-deferred
    lsp--set-configuration
    lsp-package-ensure
    lsp-signature-help
    lsp-enable-which-key-integration
    lsp-modeline-diagnostics-mode
    lsp-modeline-code-actions-mode
    lsp-symbol-highlight
    ht-merge
    lsp-completion--regex-fuz
    lsp-describe-thing-at-point
    lsp-find-type-definition)
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind
  ;; `lsp-imenu-create-categorised-index' - sorts the items by kind.
  ;; `lsp-imenu-create-uncategorized-index' - will have the items sorted by position.
  (("M-." . lsp-find-definition)
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
    ("c" . lsp-imenu-create-categorised-index)
    ("u" . lsp-imenu-create-uncategorised-index)
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
      "--malloc-trim" ; Release memory periodically
      "--pch-storage=memory" ; Increases memory usage but can improve performance
      "--pretty"))
  (lsp-completion-provider :none "Enable integration of custom backends other than `capf'")
  (lsp-completion-show-detail nil "Disable completion metadata since they can be very long")
  (lsp-completion-show-kind nil "Disable completion kind to shorten popup width")
  (lsp-completion-show-label-description nil "Disable description of completion candidates")
  (lsp-eldoc-enable-hover t)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-on-type-formatting nil "Reduce unexpected modifications to code")
  (lsp-enable-folding nil "I do not find the feature useful")
  (lsp-headerline-breadcrumb-enable nil "Breadcrumb is not useful for all modes")
  ;; Do not customize breadcrumb faces based on errors
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-html-format-wrap-line-length sb/fill-column)
  (lsp-html-format-end-with-newline t)
  (lsp-html-format-indent-inner-html t)
  (lsp-imenu-sort-methods '(position))
  (lsp-lens-enable nil)
  ;; We have `flycheck' error summary listed on the modeline, but the `lsp' server may report
  ;; additional errors. The problem is that the modeline can get too congested.
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-diagnostics-scope :file "Focus on the errors at hand")
  ;; (lsp-modeline-code-actions-enable (display-graphic-p))
  (lsp-modeline-workspace-status-enable nil)
  ;; Sudden changes in the height of the echo area causes the cursor to lose position,
  ;; manually request via `lsp-signature-activate'
  (lsp-signature-auto-activate nil)
  ;; Disable showing function documentation with `eldoc'
  ;; (lsp-signature-render-documentation nil)
  ;; Avoid annoying questions, we expect a server restart to succeed more often than not
  (lsp-restart 'auto-restart)
  (lsp-xml-logs-client nil)
  (lsp-yaml-print-width sb/fill-column)
  (lsp-warn-no-matched-clients nil)
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
    (add-hook 'text-mode-hook (lambda () (setq-local lsp-completion-enable nil)))
    (add-hook 'lsp-completion-mode-hook #'sb/lsp-mode-setup-completion))
  :diminish)

(use-package lsp-ui
  :after lsp-mode
  :defines lsp-ui-modeline-code-actions-enable
  :commands (lsp-ui-doc-mode lsp-ui-mode lsp-ui-doc--hide-frame lsp-ui-peek-find-implementation lsp-ui-imenu)
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
  ;; Invoke doc on demand with `lsp-ui-doc-show'
  ;; (lsp-ui-doc-enable nil "Disable on-hover dialogs")
  (lsp-ui-doc-include-signature t)
  (lsp-ui-imenu-auto-refresh 'after-save)
  ;; Showing code actions in the sideline enables understanding when to invoke them
  (lsp-ui-sideline-show-code-actions t)
  ;; Hide diagnostics when typing because they can be intrusive
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-max-width 72 "150 (default) is too wide")
  (lsp-ui-doc-delay 0.75 "0.2 (default) is too naggy")
  (lsp-ui-peek-enable nil)
  :config
  ;; Enabling the sideline creates flickering with Corfu popon.
  (if (and (display-graphic-p) (eq sb/capf 'corfu))
    (setq lsp-ui-sideline-enable nil)
    (setq lsp-ui-sideline-enable t)))

;; Sync workspace folders and treemacs projects
(use-package lsp-treemacs
  :if (eq sb/lsp-provider 'lsp-mode)
  :commands (lsp-treemacs-errors-list lsp-treemacs-sync-mode)
  :config (lsp-treemacs-sync-mode 1)
  :bind
  (:map
    lsp-command-map
    ("S" . lsp-treemacs-symbols)
    ("F" . lsp-treemacs-references)
    ("Y" . lsp-treemacs-sync-mode)
    ("C" . lsp-treemacs-call-hierarchy)
    ("T" . lsp-treemacs-type-hierarchy)
    ("E" . lsp-treemacs-errors-list)))

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :bind (:map lsp-command-map ("G" . lsp-ivy-global-workspace-symbol) ("W" . lsp-ivy-workspace-symbol)))

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

(use-package consult-lsp
  :after (consult lsp)
  :commands (consult-lsp-diagnostics consult-lsp-symbols consult-lsp-file-symbols)
  :bind (:map lsp-mode-map ([remap xref-find-apropos] . consult-lsp-symbols)))

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
  :config (defvar lsp-grammarly-active-modes)

  (lsp-register-client
    (make-lsp-client
      :new-connection (lsp-tramp-connection #'lsp-grammarly--server-command)
      :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-grammarly-active-modes))
      :priority -1
      :remote? t
      :add-on? t
      :server-id 'grammarly-r
      :download-server-fn
      (lambda (_client callback error-callback _update?)
        (lsp-package-ensure 'grammarly-ls callback error-callback))
      :after-open-fn #'lsp-grammarly--init
      :async-request-handlers
      (ht
        ("$/getCredentials" #'lsp-grammarly--get-credentials)
        ("$/getToken" #'lsp-grammarly--get-token)
        ("$/storeToken" #'lsp-grammarly--store-token)
        ("$/showError" #'lsp-grammarly--show-error)
        ("$/updateDocumentState" #'lsp-grammarly--update-document-state)))))

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

  (lsp-register-client
    (make-lsp-client
      :new-connection
      (lsp-tramp-connection
        (expand-file-name "lsp/server/ltex-ls/latest/bin/ltex-ls" no-littering-var-directory))
      :activation-fn (lambda (&rest _) (apply #'derived-mode-p lsp-ltex-active-modes))
      :priority -2
      :add-on? t
      :remote? t
      :server-id 'ltex-r
      :download-server-fn
      (lambda (_client _callback error-callback _update?)
        (lsp-package-ensure
          'ltex-ls
          (lambda ()
            (let ((dest (f-dirname (lsp-ltex--downloaded-extension-path))))
              (unless
                (lsp-ltex--execute "tar" "-xvzf" (lsp-ltex--downloaded-extension-path) "-C" dest)
                (error "Error during the unzip process: tar"))))
          error-callback)))))

(use-package dap-mode
  :after lsp-mode
  :commands (dap-step-in dap-next dap-continue dap-debug dap-hydra)
  :init (dap-auto-configure-mode)
  :hook ((lsp-mode-hook . dap-ui-mode) (lsp-mode-hook . dap-mode))
  ;; :bind
  ;; ("<f7>" . dap-step-in)
  ;; ("<f8>" . dap-next)
  ;; ("<f9>" . dap-continue)
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
