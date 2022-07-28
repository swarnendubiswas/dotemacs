;;; init-languages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/capf)

;; Registering `lsp-format-buffer' makes sense only if the server is active. We may not always want
;; to format unrelated files and buffers (e.g., commented YAML files in out-of-project locations).
(use-package lsp-mode
  :preface
  ;; https://github.com/minad/corfu/wiki
  (defun sb/lsp-mode-setup-completion ()
    (if (featurep 'orderless)
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
              '(orderless))
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(flex))))
  :defines (lsp-perl-language-server-path
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
  :commands (lsp--set-configuration lsp-completion--regex-fuz
                                    lsp-register-client
                                    lsp-tramp-connection
                                    make-lsp-client
                                    lsp-format-buffer
                                    lsp-configuration-section lsp
                                    lsp-deferred
                                    lsp--set-configuration
                                    lsp-package-ensure
                                    lsp-signature-help
                                    lsp-enable-which-key-integration
                                    lsp-modeline-diagnostics-mode
                                    lsp-modeline-code-actions-mode
                                    lsp-symbol-highlight ht-merge
                                    lsp-completion--regex-fuz
                                    lsp-describe-thing-at-point
                                    lsp-find-type-definition)
  :init
  ;;https://github.com/emacs-lsp/lsp-mode/issues/3550
  (when (eq sb/capf 'corfu)
    (add-hook 'text-mode-hook (lambda ()
                                (setq-local lsp-completion-enable nil))))
  :hook
  ((lsp-mode-hook . lsp-enable-which-key-integration)
   (lsp-mode-hook . lsp-dired-mode))
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :bind
  ;; `lsp-imenu-create-categorised-index' - sorts the items by kind.
  ;; `lsp-imenu-create-uncategorized-index' - will have the items sorted by position.
  (("M-." . lsp-find-definition)
   :map lsp-command-map
   ("=" . nil)
   ("w" . nil)
   ("g" . nil)
   ("G" . nil)
   ("a" . nil)
   ("F" . nil)
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
  :custom-face
  ;; Reduce the height
  (lsp-headerline-breadcrumb-symbols-face ((t (:inherit
                                               font-lock-doc-face :weight bold :height 0.9))))
  (lsp-headerline-breadcrumb-prefix-face ((t (:inherit font-lock-string-face :height 0.9))))
  (lsp-headerline-breadcrumb-project-prefix-face ((t (:inherit font-lock-string-face
                                                               :weight bold :height 0.9))))
  :custom
  (lsp-completion-enable nil)
  ;; We can add "--compile-commands-dir=<build-dir>" option to indicate the directory where
  ;; "compile_commands.json" reside. If path is invalid, clangd will look in the current directory
  ;; and parent paths of each source file.
  (lsp-clients-clangd-args '("-j=4"
                             "--all-scopes-completion"
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
                             "--pretty"))
  ;; Enable integration of custom backends other than `capf'
  (lsp-completion-provider :none)
  (lsp-completion-show-detail t "Disable completion metadata since they can be very long")
  ;; (lsp-completion-show-kind nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-on-type-formatting nil "Reduce unexpected modifications to code")
  (lsp-enable-folding nil "I do not find the feature useful")
  (lsp-enable-text-document-color t)
  ;; (lsp-semantic-tokens-enable t)
  (lsp-headerline-breadcrumb-enable nil "Breadcrumb is not useful for all modes")
  ;; Do not customize breadcrumb faces based on errors
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-html-format-wrap-line-length sb/fill-column)
  (lsp-html-format-end-with-newline t)
  (lsp-html-format-indent-inner-html t)
  (lsp-imenu-sort-methods '(position))
  ;; (lsp-keep-workspace-alive nil)
  (lsp-lens-enable nil)
  (lsp-log-io nil "Increases memory usage because of JSON parsing if enabled")
  ;; We have `flycheck' error summary listed on the modeline, but the `lsp' server may report
  ;; additional errors. The problem is that the modeline can get too congested.
  (lsp-modeline-diagnostics-enable (display-graphic-p))
  (lsp-modeline-diagnostics-scope :file "Focus on the errors at hand")
  (lsp-modeline-code-actions-enable (display-graphic-p))
  (lsp-modeline-workspace-status-enable nil)
  ;; Sudden changes in the height of the echo area causes the cursor to lose position,
  ;; manually request via `lsp-signature-activate'
  (lsp-signature-auto-activate nil)
  ;; Disable showing function documentation with `eldoc'
  ;; (lsp-signature-render-documentation nil)
  ;; (lsp-signature-function 'lsp-signature-posframe)
  ;; Avoid annoying questions, we expect a server restart to succeed more often than not
  (lsp-restart 'auto-restart)
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (lsp-use-plists nil)
  (lsp-xml-logs-client nil)
  (lsp-yaml-print-width sb/fill-column)
  (lsp-warn-no-matched-clients nil)
  :config
  ;; Autocomplete parentheses
  (when (featurep 'yasnippet)
    (setq lsp-enable-snippet t))

  (dolist (ignore-dirs '("/build\\'"
                         "/\\.metadata\\'"
                         "/\\.recommenders\\'"
                         "/\\.clangd\\'"
                         "/\\.cache\\'"
                         "/__pycache__\\'"
                         "/\\.log\\'"))
    (add-to-list 'lsp-file-watch-ignored-directories ignore-dirs))

  (with-eval-after-load "lsp-lens"
    (diminish 'lsp-lens-mode))

  (when (eq sb/capf 'corfu)
    (add-hook 'lsp-completion-mode-hook #'sb/lsp-mode-setup-completion))
  :diminish)

(use-package lsp-ui
  :after lsp-mode
  :demand t
  :defines lsp-ui-modeline-code-actions-enable
  :commands (lsp-ui-doc-mode lsp-ui-mode lsp-ui-doc--hide-frame
                             lsp-ui-peek-find-implementation lsp-ui-imenu)
  :hook
  (lsp-mode-hook . lsp-ui-mode)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references]  . lsp-ui-peek-find-references)
        :map lsp-command-map
        ("D" . lsp-ui-doc-show))
  :custom
  ;; Invoke doc on demand with `lsp-ui-doc-show'
  (lsp-ui-doc-enable nil "Disable on-hover dialogs")
  (lsp-ui-doc-include-signature t)
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-imenu-window-width 16)
  ;; Showing code actions in the sideline enables understanding when to invoke them
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover nil)
  ;; Hide diagnostics when typing because they can be intrusive
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-max-height 8)
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
  :commands (lsp-treemacs-errors-list lsp-treemacs-sync-mode)
  :config (lsp-treemacs-sync-mode 1)
  :bind
  (:map lsp-command-map
        ("S" . lsp-treemacs-symbols)
        ("F" . lsp-treemacs-references)
        ("Y" . lsp-treemacs-sync-mode)
        ("C" . lsp-treemacs-call-hierarchy)
        ("T" . lsp-treemacs-type-hierarchy)
        ("E" . lsp-treemacs-errors-list)))

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :bind
  (:map lsp-command-map
        ("G" . lsp-ivy-global-workspace-symbol)
        ("W" . lsp-ivy-workspace-symbol)))

(use-package dap-mode
  :commands (dap-debug dap-hydra)
  :hook
  ((lsp-mode-hook . dap-mode)
   (lsp-mode-hook . dap-ui-mode)))

;; Try to delete `lsp-java-workspace-dir' if the JDTLS fails
(use-package lsp-java
  :commands (lsp-java-organize-imports lsp-java-build-project
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
  (java-mode-hook . (lambda ()
                      (setq-local c-basic-offset 4
                                  c-set-style "java")
                      (lsp-deferred)))
  :custom
  (lsp-java-inhibit-message t)
  ;; Requires Java 11+, Java 11 is the LTS
  (lsp-java-java-path "/usr/lib/jvm/java-11-openjdk-amd64/bin/java")
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-format-settings-profile "Swarnendu")
  (lsp-java-format-settings-url (expand-file-name
                                 "github/dotfiles/java/eclipse-format-swarnendu.xml"
                                 sb/user-home-directory)))

;; "pyright --createstub pandas"
(use-package lsp-pyright
  :if (and (eq sb/python-langserver 'pyright) (executable-find "pyright"))
  :commands (lsp-pyright-locate-python lsp-pyright-locate-venv)
  :hook
  (python-mode-hook . (lambda ()
                        (require 'lsp-pyright)))
  :custom
  (lsp-pyright-python-executable-cmd "python3")
  (lsp-pyright-typechecking-mode "basic")
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection
  ;;                    (lambda ()
  ;;                      (cons "pyright-langserver"
  ;;                            lsp-pyright-langserver-command-args)))
  ;;   :major-modes '(python-mode)
  ;;   :remote? t
  ;;   :server-id 'pyright-r
  ;;   :multi-root lsp-pyright-multi-root
  ;;   :priority 3
  ;;   :initialization-options (lambda ()
  ;;                             (ht-merge (lsp-configuration-section "pyright")
  ;;                                       (lsp-configuration-section "python")))
  ;;   :initialized-fn (lambda (workspace)
  ;;                     (with-lsp-workspace workspace
  ;;                       (lsp--set-configuration
  ;;                        (ht-merge (lsp-configuration-section "pyright")
  ;;                                  (lsp-configuration-section "python")))))
  ;;   :download-server-fn (lambda (_client callback error-callback _update?)
  ;;                         (lsp-package-ensure 'pyright callback error-callback))
  ;;   :notification-handlers
  ;;   (lsp-ht
  ;;    ("pyright/beginProgress"  'lsp-pyright--begin-progress-callback)
  ;;    ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
  ;;    ("pyright/endProgress"    'lsp-pyright--end-progress-callback))))
  )

(use-package consult-lsp
  :if (eq sb/minibuffer-completion 'vertico)
  :after (consult lsp)
  :commands
  (consult-lsp-diagnostics consult-lsp-symbols
                           consult-lsp-file-symbols
                           consult-lsp-marginalia-mode)
  :init (consult-lsp-marginalia-mode 1)
  :bind
  (:map lsp-mode-map
        ([remap xref-find-apropos] . consult-lsp-symbols)))

(provide 'init-lsp)

;;; init-lsp.el ends here
