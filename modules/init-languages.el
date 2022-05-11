;;; init-languages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/fill-column)
(defvar hs-isearch-open)
(defvar sb/minibuffer-completion)
(defvar sb/user-home-directory)
(defvar sb/python-langserver)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.conf\\'" . conf-unix-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; Highlight and allow to open http links in strings and comments in programming
            ;; buffers.
            (goto-address-prog-mode 1)
            ;; Native from Emacs 27+, disable in TUI since the line characters also get copied.
            (when (display-graphic-p)
              (display-fill-column-indicator-mode 1))))

(progn
  (unless (fboundp 'subword-mode)
    (autoload #'subword-mode "subword" nil t))

  (add-hook 'prog-mode-hook #'subword-mode)

  (with-eval-after-load "subword"
    (diminish 'subword-mode)))

;; Edit outlines
(progn
  (unless (fboundp 'outline-minor-mode)
    (autoload #'outline-minor-mode "outline" nil t))

  (add-hook 'prog-mode-hook #'outline-minor-mode)

  (with-eval-after-load "outline"
    (diminish 'outline-minor-mode)))

;; Hide top-level code blocks. Enable code folding, which is useful for browsing large files. This
;; module is part of Emacs, and is better maintained than other alternatives like `origami'.
(progn
  (declare-function hs-show-all "hideshow")
  (declare-function hs-show-block "hideshow")
  (declare-function hs-hide-initial-comment-block "hideshow")
  (declare-function hs-hide-all "hideshow")

  (unless (fboundp 'hs-hide-all)
    (autoload #'hs-hide-all "hideshow" nil t))
  (unless (fboundp 'hs-hide-initial-comment-block)
    (autoload #'hs-hide-initial-comment-block "hideshow" nil t))
  (unless (fboundp 'hs-show-all)
    (autoload #'hs-show-all "hideshow" nil t))
  (unless (fboundp 'hs-show-block)
    (autoload #'hs-show-block "hideshow" nil t))

  ;; (add-hook 'prog-mode-hook #'hs-minor-mode)

  (with-eval-after-load "hideshow"
    (setq hs-isearch-open t) ;; Open all folds while searching

    (diminish 'hs-minor-mode)))

(defvar tags-revert-without-query)

(setq large-file-warning-threshold (* 500 1024 1024) ; MB
      tags-add-tables nil
      tags-case-fold-search nil ; t=case-insensitive, nil=case-sensitive
      ;; Do not ask before rereading the `TAGS' files if they have changed
      tags-revert-without-query t)

(use-package xref
  :commands xref-etags-mode
  :bind
  (("M-'"   . xref-find-definitions)
   ("M-?"   . xref-find-references)
   ("C-M-." . xref-find-apropos)
   ("M-,"   . xref-pop-marker-stack)
   :map xref--xref-buffer-mode-map
   ("C-o"   . xref-show-location-at-point)
   ("<tab>" . xref-quit-and-goto-xref)
   ("r"     . xref-query-replace-in-results)))

(use-package dumb-jump
  :after xref
  :demand t
  :commands dumb-jump-xref-activate
  :custom
  (dumb-jump-quiet t)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package ivy-xref
  :after (ivy xref)
  :demand t
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function       #'ivy-xref-show-xrefs))

(use-package counsel-etags
  :defines (counsel-etags-ignore-directories counsel-etags-ignore-filenames)
  :commands counsel-etags-virtual-update-tags
  :if (and (symbol-value 'sb/IS-LINUX) (eq sb/minibuffer-completion 'ivy) (executable-find "ctags"))
  :bind
  (("M-]"     . counsel-etags-find-tag-at-point)
   ("C-c g s" . counsel-etags-find-symbol-at-point)
   ("C-c g f" . counsel-etags-find-tag)
   ("C-c g l" . counsel-etags-list-tag)
   ("C-c g c" . counsel-etags-scan-code))
  :config
  (defalias 'list-tags 'counsel-etags-list-tag-in-current-file)

  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'counsel-etags-virtual-update-tags 'append 'local)))

  (dolist (ignore-dirs '(".vscode" "build" ".metadata" ".recommenders" ".clangd" ".cache"))
    (add-to-list 'counsel-etags-ignore-directories ignore-dirs))

  (dolist (ignore-files '(".clang-format" ".clang-tidy" "*.json" "*.html" "*.xml"))
    (add-to-list 'counsel-etags-ignore-filenames ignore-files)))

(use-package highlight-indentation
  :commands highlight-indentation-mode
  :diminish (highlight-indentation-mode highlight-indentation-current-column-mode)
  :hook ((yaml-mode-hook python-mode-hook) . highlight-indentation-mode))

(use-package aggressive-indent ; Claims to be better than `electric-indent-mode'
  :commands aggressive-indent-mode
  :hook (emacs-lisp-mode-hook . aggressive-indent-mode)
  :diminish
  :custom
  (aggressive-indent-comments-too t)
  ;; Never use `electric-indent-mode'
  (aggressive-indent-dont-electric-modes t))

(use-package symbol-overlay ; Highlight symbol under point
  :diminish
  :commands (symbol-overlay-mode)
  :hook (prog-mode-hook . symbol-overlay-mode)
  :bind
  (("M-p" . symbol-overlay-jump-prev)
   ("M-n" . symbol-overlay-jump-next))
  :custom
  ;; Delay highlighting to allow for transient cursor placements
  (symbol-overlay-idle-time 2))

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :commands ws-butler-mode
  :diminish
  :hook (prog-mode-hook . ws-butler-mode))

(use-package highlight-escape-sequences
  :commands hes-mode
  :hook (prog-mode-hook . hes-mode))

(unless (fboundp 'ini-mode)
  (autoload #'ini-mode "ini-mode" nil t))

(progn
  (unless (fboundp 'emacs-lisp-mode)
    (autoload #'emacs-lisp-mode "elisp-mode" nil t))
  (unless (fboundp 'elisp-byte-code-mode)
    (autoload #'elisp-byte-code-mode "elisp-mode" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.el\\'"  . emacs-lisp-mode))
;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.elc\\'" . elisp-byte-code-mode))

  (dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
    (add-hook hook (lambda ()
                     (when buffer-file-name
                       (add-hook 'after-save-hook #'check-parens nil t)
                       (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-checkdoc 'append))))))

(use-package yaml-mode
  :defines lsp-ltex-enabled lsp-disabled-clients
  :commands yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'" ".clang-format" ".clang-tidy")
  :hook
  (yaml-mode-hook .
                  (lambda ()
                    ;; `yaml-mode' is derived from `text-mode', so disable grammar and spell
                    ;; checking.
                    (make-local-variable 'lsp-disabled-clients)
                    (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                    (spell-fu-mode -1)
                    (flyspell-mode -1)
                    (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("yaml-language-server" "--stdio"))
    :major-modes '(yaml-mode)
    :remote? t
    :server-id 'yamlls-r)))

(use-package yaml-imenu
  :after yaml-mode
  :demand t
  :config (yaml-imenu-enable))

(use-package css-mode
  :commands css-mode
  :defines sb/flycheck-local-checkers
  :hook (css-mode-hook . lsp-deferred)
  :custom
  (css-indent-offset 2)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("css-languageserver" "--stdio"))
    :major-modes '(css-mode)
    :remote? t
    :server-id 'cssls-r)))

(progn
  (unless (fboundp 'makefile-mode)
    (autoload #'makefile-mode "make-mode" nil t))
  (unless (fboundp 'makefile-gmake-mode)
    (autoload #'makefile-gmake-mode "make-mode" nil t))

;;;###autoload
  (progn
    (add-to-list 'auto-mode-alist '("\\Makefile\\'"       . makefile-mode))
    ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
    (add-to-list 'auto-mode-alist '("makefile\\.rules\\'" . makefile-gmake-mode)))

  (add-hook 'makefile-mode-hook (lambda()
                                  (setq-local indent-tabs-mode t))))

;; Align fields with "C-c C-a"
(use-package csv-mode
  :defines lsp-disabled-clients
  :commands csv-mode
  :hook
  (csv-mode-hook . (lambda ()
                     (make-local-variable 'lsp-disabled-clients)
                     (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                     (spell-fu-mode -1)
                     (flyspell-mode -1)))
  :custom
  (csv-separators '("," ";" "|" " ")))

(progn
  (unless (fboundp 'antlr-mode)
    (autoload #'antlr-mode "antlr-mode" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode)))

(use-package bison-mode
  :mode ("\\.bison\\'"))

(progn
  (eval-when-compile
    (if (bound-and-true-p sb/disable-package.el)
        (use-package llvm-mode
          ;; :straight (llvm-mode :type git :host github
          ;;                      :repo "llvm/llvm-project"
          ;;                      :files "llvm/utils/emacs/llvm-mode.el")
          :straight nil)
      (use-package llvm-mode
        :ensure nil
        :load-path "extras")))

  (declare-function llvm-mode "llvm-mode")

  (unless (fboundp 'llvm-mode)
    (autoload #'llvm-mode "llvm-mode" nil t)))

(progn
  (eval-when-compile
    (if (bound-and-true-p sb/disable-package.el)
        (use-package tablegen-mode
          :straight nil)
      (use-package tablegen-mode
        :ensure nil
        :load-path "extras"))

    (declare-function tablegen-mode "tablegen-mode")

    (unless (fboundp 'tablegen-mode)
      (autoload #'tablegen-mode "tablegen-mode" nil t))))

(use-package autodisass-llvm-bitcode
  :commands autodisass-llvm-bitcode
  :mode "\\.bc\\'")

;; Enable live preview with "C-c C-c l" (`markdown-live-preview-mode'). The following page lists
;; more shortcuts.
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :commands (markdown-mode gfm-mode markdown-insert-bold
                           markdown-insert-italic
                           markdown-insert-blockquote
                           markdown-insert-pre
                           markdown-insert-code markdown-move-up
                           markdown-insert-link
                           markdown-insert-wiki-link
                           markdown-demote
                           markdown-move-down
                           markdown-insert-header-dwim
                           markdown-insert-reference-link-dwim
                           markdown-insert-header-atx-1
                           markdown-insert-header-atx-2
                           markdown-insert-header-atx-3
                           markdown-insert-header-atx-4
                           markdown-promote
                           markdown-insert-list-item
                           markdown-insert-uri
                           markdown-insert-footnote)
  :mode
  ;; The order is important to associate "README.md" with `gfm-mode'
  (("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  ;; :init
  ;; Looks good, but hiding markup makes it difficult to be consistent while editing
  ;; (setq-default markdown-hide-markup t)
  :custom
  (markdown-command
   "pandoc -f markdown -s --mathjax --standalone --quiet --highlight-style=pygments")
  (markdown-enable-math t "Syntax highlight for LaTeX fragments")
  (markdown-enable-wiki-links t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-indent-on-enter 'indent-and-new-item)
  (markdown-list-indent-width 2)
  (markdown-split-window-direction 'horizontal)
  ;; (markdown-make-gfm-checkboxes-buttons nil)
  (markdown-hide-urls t)
  :bind
  (:map markdown-mode-map
        ("C-c C-j" . nil)))

;; ;; Generate TOC with `markdown-toc-generate-toc'
;; (use-package markdown-toc
;;   :after markdown-mode
;;   :commands (markdown-toc-refresh-toc markdown-toc-generate-toc
;;                                       markdown-toc-generate-or-refresh-toc))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf
;; Convert `markdown' to `org': "pandoc -f markdown -t org -o output-file.org input-file.md"
(use-package pandoc-mode
  :commands (pandoc-load-default-settings pandoc-mode)
  :diminish
  :hook (markdown-mode-hook . pandoc-mode)
  :config (pandoc-load-default-settings))

;; Open preview of markdown file in a browser
(use-package markdown-preview-mode
  :disabled t
  :commands markdown-preview-mode)

;; Registering `lsp-format-buffer' makes sense only if the server is active. We may not always want
;; to format unrelated files and buffers (e.g., commented YAML files in out-of-project locations).
(use-package lsp-mode
  :diminish
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
  :preface
  ;; https://github.com/minad/corfu/wiki
  (defun sb/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  ((lsp-completion-mode-hook . sb/lsp-mode-setup-completion)
   ;; (lsp-mode-hook . lsp-lens-mode)
   (lsp-mode-hook . lsp-enable-which-key-integration))
  :custom-face
  ;; Reduce the height
  (lsp-headerline-breadcrumb-symbols-face ((t (:inherit
                                               font-lock-doc-face :weight bold :height 0.9))))
  (lsp-headerline-breadcrumb-prefix-face ((t (:inherit font-lock-string-face :height 0.9))))
  (lsp-headerline-breadcrumb-project-prefix-face ((t (:inherit font-lock-string-face
                                                               :weight bold :height 0.9))))
  :config
  ;; We can add "--compile-commands-dir=<build-dir>" option to indicate the directory where
  ;; "compile_commands.json" reside. If path is invalid, clangd will look in the current directory
  ;; and parent paths of each source file.
  (setq lsp-clients-clangd-args '("-j=4"
                                  "--all-scopes-completion"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--cross-file-rename"
                                  "--fallback-style=LLVM"
                                  "--header-insertion=never"
                                  "--log=error"
                                  "--malloc-trim" ;; Release memory periodically
                                  ;; Increases memory usage but can improve performance
                                  "--pch-storage=memory"
                                  "--pretty")
        ;; Enable integration of custom backends other than `company-capf'
        lsp-completion-provider :none
        lsp-completion-show-detail nil ; Disable completion metadata since they can be very long
        ;; lsp-completion-show-kind nil
        lsp-eldoc-enable-hover nil
        lsp-enable-dap-auto-configure nil
        lsp-enable-on-type-formatting nil ; Reduce unexpected modifications to code
        lsp-enable-folding nil ; I do not find the feature useful
        lsp-enable-text-document-color nil
        ;; lsp-semantic-tokens-enable t
        lsp-headerline-breadcrumb-enable nil ; Breadcrumb is not useful for all modes
        ;; Do not customize breadcrumb faces based on errors
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-html-format-wrap-line-length sb/fill-column
        lsp-html-format-end-with-newline t
        lsp-html-format-indent-inner-html t
        lsp-imenu-sort-methods '(position)
        ;; lsp-keep-workspace-alive nil
        lsp-log-io nil ; Increases memory usage because of JSON parsing if enabled
        ;; We have `flycheck' error summary listed on the modeline, but the `lsp' server may report
        ;; additional errors. The problem is that the modeline can get too congested.
        lsp-modeline-diagnostics-enable (display-graphic-p)
        lsp-modeline-diagnostics-scope :file ; Focus on the errors at hand
        lsp-modeline-workspace-status-enable nil
        ;; Sudden changes in the height of the echo area causes the cursor to lose position,
        ;; manually request via `lsp-signature-activate'
        lsp-signature-auto-activate nil
        ;; Disable showing function documentation with `eldoc'
        ;; lsp-signature-render-documentation nil
        ;; lsp-signature-function 'lsp-signature-posframe
        ;; Avoid annoying questions, we expect a server restart to succeed more often than not
        lsp-restart 'auto-restart
        ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
        lsp-use-plists nil
        lsp-xml-logs-client nil
        lsp-yaml-print-width sb/fill-column
        lsp-modeline-code-actions-enable (display-graphic-p))

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
  :bind-keymap ("C-c l" . lsp-command-map)
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
   ("b" . lsp-workspace-blacklist-remove)))

(use-package lsp-ui
  :defines lsp-ui-modeline-code-actions-enable
  :commands (lsp-ui-doc-mode lsp-ui-mode lsp-ui-doc--hide-frame
                             lsp-ui-peek-find-implementation lsp-ui-imenu)
  :after lsp-mode
  :demand t
  :custom
  (lsp-ui-doc-enable t "Enable/disable on-hover dialogs")
  (lsp-ui-doc-include-signature t)
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-imenu-window-width 16)
  (lsp-ui-sideline-enable t "Enable/disable whole sideline")
  ;; Showing code actions in the sideline enables understanding when to invoke them
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-hover nil)
  ;; Show/hide diagnostics when typing because they can be intrusive
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-doc-max-height 8)
  (lsp-ui-doc-max-width 72 "150 (default) is too wide")
  (lsp-ui-doc-delay 0.75 "0.2 (default) is too naggy")
  :config
  (when (not (display-graphic-p))
    (setq lsp-ui-doc-enable nil
          lsp-ui-peek-enable nil))

  (lsp-ui-mode 1)
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references]  . lsp-ui-peek-find-references)
        :map lsp-command-map
        ("D" . lsp-ui-doc-mode)))

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
  :commands (dap-debug dap-hydra dap-mode dap-ui-mode)
  :hook
  ((lsp-mode-hook . dap-mode)
   (lsp-mode-hook . dap-ui-mode)))

(use-package docstr
  :diminish
  :hook ((c++-mode-hook python-mode-hook java-mode-hook) . docstr-mode))

(progn
  (declare-function c-beginning-of-defun "cc-mode")
  (declare-function c-end-of-defun "cc-mode")
  (declare-function c-fill-paragraph "cc-mode")

  (unless (fboundp 'c++-mode)
    (autoload #'c++-mode "cc-mode" nil t))
  (unless (fboundp 'c-beginning-of-defun)
    (autoload #'c-beginning-of-defun "cc-mode" nil t))
  (unless (fboundp 'c-end-of-defun)
    (autoload #'c-end-of-defun "cc-mode" nil t))
  (unless (fboundp 'c-fill-paragraph)
    (autoload #'c-fill-paragraph "cc-mode" nil t))

;;;###autoload
  (dolist (pattern '("\\.h\\'" "\\.c\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'c++-mode)))

  (add-hook 'c++-mode-hook #'lsp-deferred)

  (with-eval-after-load "cc-mode"
    (defvar c-electric-indent)
    (defvar c-set-style)
    (defvar c-basic-offset)
    (defvar c-enable-auto-newline)
    (defvar c-electric-brace)
    (defvar c-auto-newline)
    (defvar c-electric-flag)
    (defvar c-syntactic-indentation)

    (setq c-set-style "cc-mode"
          c-basic-offset 2)

    ;; Disable electric indentation and on-type formatting
    (add-hook 'c++-mode-hook (lambda nil
                               (setq-local c-auto-newline nil
                                           c-electric-brace nil
                                           c-electric-flag nil
                                           c-electric-indent nil
                                           c-enable-auto-newline nil
                                           c-syntactic-indentation nil)))

    (unbind-key "C-M-a" c-mode-map)

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection "clangd")
      :major-modes '(c-mode c++-mode)
      :remote? t
      :server-id 'clangd-r))
    )

  (defvar c-mode-base-map)
  (bind-keys :package cc-mode :map c-mode-base-map
             ("C-c c a" . c-beginning-of-defun)
             ("C-c c e" . c-end-of-defun)
             ("M-q"     . c-fill-paragraph)))

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode-hook . modern-c++-font-lock-mode))

(use-package cuda-mode
  :commands cuda-mode
  :mode
  (("\\.cu\\'"  . c++-mode)
   ("\\.cuh\\'" . c++-mode)))

(use-package opencl-mode
  :commands opencl-mode
  :mode "\\.cl\\'")

(use-package cmake-mode
  :if (executable-find "cmake")
  :commands cmake-mode
  :mode "\(CMakeLists\.txt|\.cmake\)$"
  :hook
  (cmake-mode-hook . (lambda ()
                       (make-local-variable 'lsp-disabled-clients)
                       (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                       (spell-fu-mode -1)
                       (flyspell-mode -1)
                       (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "cmake-language-server")
    :major-modes '(cmake-mode)
    :remote? t
    :server-id 'cmakels-r)))

(use-package cmake-font-lock
  :hook (cmake-mode-hook . cmake-font-lock-activate))

(progn
  (declare-function python-nav-backward-block "python")
  (declare-function python-nav-forward-block "python")
  (declare-function python-indent-shift-left "python")
  (declare-function python-indent-shift-right "python")

  (unless (fboundp 'python-nav-backward-block)
    (autoload #'python-nav-backward-block "python" nil t))
  (unless (fboundp 'python-nav-forward-block)
    (autoload #'python-nav-forward-block "python" nil t))
  (unless (fboundp 'python-indent-shift-left)
    (autoload #'python-indent-shift-left "python" nil t))
  (unless (fboundp 'python-indent-shift-right)
    (autoload #'python-indent-shift-right "python" nil t))

  (add-hook 'python-mode-hook #'lsp-deferred)

  (with-eval-after-load "python"
    (setenv "PYTHONPATH" "python3")

    (defvar python-shell-completion-native-enable)
    (defvar python-fill-docstring-style)
    (defvar python-indent-guess-indent-offset)
    (defvar python-indent-guess-indent-offset-verbose)
    (defvar python-indent-offset)
    (defvar python-shell-exec-path)
    (defvar python-shell-interpreter)

    (setq python-shell-completion-native-enable nil ; Disable readline based native completion
          python-fill-docstring-style 'django
          python-indent-guess-indent-offset-verbose nil ; Remove guess indent python message
          python-indent-guess-indent-offset nil
          python-indent-offset 4
          python-shell-exec-path "python3"
          python-shell-interpreter "python3")

    ;; (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint))))))

    (setq auto-mode-alist (append
                           '(("SCon\(struct\|script\)$" . python-mode)
                             ("SConscript\\'" . python-mode)
                             ("[./]flake8\\'" . conf-mode)
                             ("/Pipfile\\'" . conf-mode))
                           auto-mode-alist))

    (with-eval-after-load "lsp-mode"
      (defvar lsp-pylsp-configuration-sources)
      (defvar lsp-pylsp-plugins-autopep8-enable)
      (defvar lsp-pylsp-plugins-mccabe-enabled)
      (defvar lsp-pylsp-plugins-pycodestyle-enabled)
      (defvar lsp-pylsp-plugins-pycodestyle-max-line-length)
      (defvar lsp-pylsp-plugins-pydocstyle-convention)
      (defvar lsp-pylsp-plugins-pydocstyle-enabled)
      (defvar lsp-pylsp-plugins-pydocstyle-ignore)
      (defvar lsp-pylsp-plugins-pyflakes-enabled)
      (defvar lsp-pylsp-plugins-pylint-args)
      (defvar lsp-pylsp-plugins-pylint-enabled)
      (defvar lsp-pylsp-plugins-yapf-enabled)
      (defvar lsp-pyright-langserver-command-args)
      (defvar lsp-pylsp-plugins-preload-modules)
      (defvar lsp-pylsp-plugins-flake8-enabled)
      (defvar lsp-pylsp-plugins-jedi-use-pyenv-environment)

      (when (eq sb/python-langserver 'pylsp)
        (setq lsp-pylsp-configuration-sources []
              lsp-pylsp-plugins-mccabe-enabled nil
              ;; We can also set this per-project
              lsp-pylsp-plugins-preload-modules ["numpy", "csv", "pandas", "statistics", "json"]
              lsp-pylsp-plugins-pycodestyle-enabled nil
              lsp-pylsp-plugins-pycodestyle-max-line-length sb/fill-column
              lsp-pylsp-plugins-pydocstyle-convention "pep257"
              lsp-pylsp-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213"))
              lsp-pylsp-plugins-pyflakes-enabled nil
              lsp-pylsp-plugins-pylint-args (vconcat
                                             (list "-j 2"
                                                   (concat "--rcfile="
                                                           (expand-file-name ".config/pylintrc"
                                                                             sb/user-home-directory))))
              lsp-pylsp-plugins-pylint-enabled t ; Pylint can be expensive
              lsp-pylsp-plugins-yapf-enabled t
              lsp-pylsp-plugins-flake8-enabled nil
              lsp-pylsp-plugins-jedi-use-pyenv-environment t)))

    ;; Assigning a keybinding such as "C-[" is involved, `[' is treated as `meta'
    ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c
    (defvar python-mode-map)

    ;; TODO: Bind other keys suitably
    ;; python-nav-beginning-of-block
    ;; python-nav-end-of-block
    ;; python-nav-beginning-of-defun
    ;; python-nav-end-of-defun
    ;; python-nav-backward-defun
    ;; python-nav-forward-defun
    ;; python-nav-backward-statement
    ;; python-nav-forward-statement
    (bind-keys :package python :map python-mode-map
               ("M-a"   . python-nav-backward-block)
               ("M-e"   . python-nav-forward-block)
               ("C-c <" . python-indent-shift-left)
               ("C-c >" . python-indent-shift-right))))

(use-package python-docstring
  :after python-mode
  :demand t
  :commands (python-docstring-mode python-docstring-install)
  :diminish
  :config (python-docstring-install))

(use-package pip-requirements
  :commands pip-requirements-mode)

(use-package pyvenv
  :commands (pyvenv-mode pyvenv-tracking-mode)
  :hook (python-mode-hook . pyvenv-mode)
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:"
                                                         pyvenv-virtual-env-name "] ")))
  (pyvenv-post-activate-hooks (list
                               (lambda ()
                                 (setq python-shell-interpreter
                                       (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks (list
                                 (lambda ()
                                   (setq python-shell-interpreter "python3")))))

(use-package py-isort
  :disabled t
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :commands (py-isort-before-save py-isort-buffer py-isort-region)
  :hook
  (python-mode-hook . (lambda ()
                        (add-hook 'before-save-hook #'py-isort-before-save)))
  :custom
  (py-isort-options '("-lines=100")))

;; "pyright --createstub pandas"
(use-package lsp-pyright
  :if (and (eq sb/python-langserver 'pyright) (executable-find "pyright"))
  :disabled t
  :commands (lsp-pyright-locate-python lsp-pyright-locate-venv)
  :hook
  (python-mode-hook . (lambda ()
                        (require 'lsp-pyright)))
  :custom
  (lsp-pyright-python-executable-cmd "python3")
  (lsp-pyright-typechecking-mode "basic")
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     (lambda ()
                       (cons "pyright-langserver"
                             lsp-pyright-langserver-command-args)))
    :major-modes '(python-mode)
    :remote? t
    :server-id 'pyright-r
    :multi-root lsp-pyright-multi-root
    :priority 3
    :initialization-options (lambda ()
                              (ht-merge (lsp-configuration-section "pyright")
                                        (lsp-configuration-section "python")))
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (ht-merge (lsp-configuration-section "pyright")
                                   (lsp-configuration-section "python")))))
    :download-server-fn (lambda (_client callback error-callback _update?)
                          (lsp-package-ensure 'pyright callback error-callback))
    :notification-handlers
    (lsp-ht
     ("pyright/beginProgress"  'lsp-pyright--begin-progress-callback)
     ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
     ("pyright/endProgress"    'lsp-pyright--end-progress-callback)))))

;; Yapfify works on the original file, so that any project settings supported by YAPF itself are
;; used.
(use-package yapfify
  :diminish yapf-mode
  :if (and (eq sb/python-langserver 'pyright) (executable-find "yapf"))
  :commands yapf-mode
  :hook (python-mode-hook . yapf-mode))

(progn
  (unless (fboundp 'cperl-mode)
    (autoload #'cperl-mode "cperl-mode" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("latexmkrc\\'" . cperl-mode))

  (add-hook 'cperl-mode-hook #'lsp-deferred)

  (with-eval-after-load "cperl-mode"
    ;; Prefer CPerl mode to Perl mode
    (fset 'perl-mode 'cperl-mode))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     (lambda ()
                       (list lsp-perl-language-server-path
                             "-MPerl::LanguageServer" "-e"
                             "Perl::LanguageServer::run" "--"
                             (format "--port %d --version %s"
                                     lsp-perl-language-server-port
                                     lsp-perl-language-server-client-version))))
    :major-modes '(perl-mode cperl-mode)
    :remote? t
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration
                         (lsp-configuration-section "perl"))))
    :priority -1
    :server-id 'perlls-r)))

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
                      (setq-default c-basic-offset 4
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

(use-package ant
  :commands (ant ant-clean ant-compile ant-test))

(use-package autodisass-java-bytecode ; Can disassemble ".class" files from within jars
  :commands autodisass-java-bytecode
  :mode "\\.class\\'")

(use-package groovy-mode ; Syntax highlighting for Gradle files
  :commands groovy-mode
  :mode "\\.gradle\\'")

;; Shell script mode
(progn
  (unless (fboundp 'sh-mode)
    (autoload #'sh-mode "sh-script" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.zsh\\'"   . sh-mode))
;;;###autoload
  (add-to-list 'auto-mode-alist '("\\bashrc\\'" . sh-mode))

  (add-hook 'sh-mode-hook #'lsp-deferred)

  (with-eval-after-load "sh-script"
    (defvar sh-basic-offset)
    (defvar sh-indent-comment)
    (defvar sh-indent-after-continuation)

    (setq sh-basic-offset 2
          sh-indent-after-continuation 'always
          ;; Indent comments as a regular line
          sh-indent-comment t)

    ;; (unbind-key "C-c C-d" sh-mode-map) ; Was bound to `sh-cd-here'

    (flycheck-add-next-checker 'sh-bash 'sh-shellcheck)

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection
                       '("bash-language-server" "start"))
      :major-modes '(sh-mode)
      :remote? t
      :server-id 'bashls-r))))

(use-package fish-mode
  :mode "\\.fish\\'"
  :interpreter "fish"
  :commands (fish-mode fish_indent-before-save)
  :hook
  (fish-mode-hook . (lambda ()
                      (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package shfmt
  :hook (sh-mode-hook . shfmt-on-save-mode)
  :custom
  ;; p: Posix, ci: indent case labels, i: indent with spaces
  (shfmt-arguments '("-i" "4" "-p" "-ci")))

(progn
  (unless (fboundp 'bat-mode)
    (autoload #'bat-mode "bat-mode" nil t))

;;;###autoload
  (progn
    (add-to-list 'auto-mode-alist '("\\.bat\\'" . bat-mode))
    (add-to-list 'auto-mode-alist '("\\.cmd\\'" . bat-mode))))

(use-package web-mode
  :commands web-mode
  :mode "\\.html?\\'"
  :hook (web-mode-hook . lsp-deferred)
  :custom
  (web-mode-enable-auto-closing              t)
  (web-mode-enable-auto-pairing              nil "Prefer `smartparens'")
  (web-mode-enable-auto-quoting              t)
  (web-mode-enable-block-face                t)
  (web-mode-enable-css-colorization          t)
  (web-mode-enable-current-element-highlight t "Highlight the element under the cursor")
  (web-mode-enable-current-column-highlight  t)
  (web-mode-markup-indent-offset             2) ; HTML
  (web-mode-css-indent-offset                2) ; CSS
  (web-mode-code-indent-offset               2) ; Script
  (web-mode-style-padding                    2) ; For `<style>' tag
  (web-mode-script-padding                   2) ; For `<script>' tag
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("html-languageserver" "--stdio"))
    :major-modes '(html-mode web-mode mhtml-mode)
    :remote? t
    :server-id 'htmlls-r)))

(use-package emmet-mode
  :defines emmet-move-cursor-between-quote
  :hook ((web-mode-hook css-mode-hook html-mode-hook) . emmet-mode)
  :custom (emmet-move-cursor-between-quote t))

(progn
  (unless (fboundp 'nxml-mode)
    (autoload #'nxml-mode "nxml-mode" nil t))

  (add-hook 'nxml-mode-hook
            (lambda()
              ;; `xml-mode' is derived from `text-mode', so disable grammar and spell checking.
              (make-local-variable 'lsp-disabled-clients)
              (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
              (spell-fu-mode -1)
              (flyspell-mode -1)
              (lsp-deferred)))

;;;###autoload
  (dolist (pattern '("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$"))
    (add-to-list 'auto-mode-alist (cons pattern 'nxml-mode)))

  (with-eval-after-load "nxml-mode"
    (fset 'xml-mode 'nxml-mode)

    (defvar nxml-auto-insert-xml-declaration-flag)
    (defvar nxml-slash-auto-complete-flag)

    (setq nxml-auto-insert-xml-declaration-flag t
          nxml-slash-auto-complete-flag t)

    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection
                       '("java" "-jar" lsp-xml-jar-file))
      :major-modes '(xml-mode nxml-mode)
      :remote? t
      :server-id 'xmlls-r))))

(use-package json-mode
  :commands (json-mode jsonc-mode json-mode-beautify)
  :mode
  (("\\.json\\'"                  . json-mode)
   ("pyrightconfig.json"          . jsonc-mode)
   (".*/vscode/settings.json$"    . jsonc-mode)
   (".*/\\.vscode/settings.json$" . jsonc-mode)
   ("User/settings.json$"         . jsonc-mode))
  :hook
  ((json-mode-hook jsonc-mode-hook) . (lambda ()
                                        (make-local-variable 'js-indent-level)
                                        (setq js-indent-level 2)
                                        (lsp-deferred)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection
                     '("vscode-json-languageserver" "--stdio"))
    :major-modes '(json-mode jsonc-mode)
    :remote? t
    :server-id 'jsonls-r)))

(use-package json-reformat
  :after (:any json-mode jsonc-mode)
  :demand t
  :custom
  (json-reformat:indent-width 2)
  (js-indent-level 2))

(use-package bazel
  :if (executable-find "bazel")
  :commands (bazel-mode bazelrc-mode bazel-buildifier)
  :hook
  ((bazel-mode-hook . (lambda ()
                        (add-hook 'before-save-hook #'bazel-buildifier nil t)))
   (bazel-mode-hook . flycheck-mode)))

(use-package protobuf-mode
  :commands protobuf-mode
  :mode "\\.proto$"
  :hook (protobuf-mode-hook . flycheck-mode))

(progn
  (declare-function mlir-mode "mlir-mode")

  (unless (fboundp 'mlir-mode)
    (autoload #'mlir-mode "mlir-mode")))

(use-package clang-format
  :if (executable-find "clang-format")
  :after (mlir-mode)
  :commands (clang-format clang-format-buffer clang-format-region)
  :custom (clang-format-style "file"))

(use-package clang-format+
  :if (executable-find "clang-format")
  :defines clang-format+-always-enable
  :hook (mlir-mode-hook . clang-format+-mode)
  :custom (clang-format+-always-enable t))

;; Tree-sitter provides advanced syntax highlighting features
(eval-when-compile
  (if (bound-and-true-p sb/disable-package.el)
      (use-package tree-sitter
        :straight tree-sitter-langs
        :straight t)
    (use-package tree-sitter
      :ensure tree-sitter-langs
      :ensure t)))

(use-package tree-sitter
  :functions tree-sitter-hl-mode
  :commands (global-tree-sitter-mode tree-sitter-hl-mode)
  :diminish tree-sitter-mode
  :preface
  (defun sb/enable-tree-sitter ()
    "Delay enabling tree-sitter to avoid slowing down Emacs startup."
    (dolist (hook '(sh-mode-hook c-mode-hook c++-mode-hook
                                 css-mode-hook html-mode-hook
                                 java-mode-hook json-mode-hook
                                 jsonc-mode-hook php-mode-hook
                                 python-mode-hook))
      (add-hook hook (lambda ()
                       (require 'tree-sitter-langs)
                       (global-tree-sitter-mode 1)))))
  ;; :init (run-with-idle-timer 2 nil #'sb/enable-tree-sitter)
  :hook (after-init-hook . sb/enable-tree-sitter)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package dotenv-mode
  :mode "\\.env\\'")

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-compile.el
(with-eval-after-load "compile"
  (defvar compilation-filter-start)
  (declare-function ansi-color-apply-on-region "ansi-color")

  (defun sb/colorize-compilation-buffer ()
    "Colorize compile mode output."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (defun sanityinc/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  ;; (add-hook 'compilation-filter-hook #'sb/colorize-compilation-buffer)
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer))

(progn
  (declare-function info-colors-fontify-node "info-colors")

  (unless (fboundp 'info-colors-fontify-node)
    (autoload #'info-colors-fontify-node "info-colors" nil t))

  (with-eval-after-load "info"
    (add-hook 'Info-selection-hook #'info-colors-fontify-node)))

(use-package consult-lsp
  :if (eq sb/minibuffer-completion 'vertico)
  :after (consult lsp)
  :commands (consult-lsp-diagnostics consult-lsp-symbols
                                     consult-lsp-file-symbols consult-lsp-marginalia-mode)
  :init (consult-lsp-marginalia-mode 1))

(use-package rainbow-delimiters
  :hook ((prog-mode-hook latex-mode-hook LaTeX-mode-hook
                         org-src-mode-hook) . rainbow-delimiters-mode))

;; The following section helper ensures that files are given `+x' permissions when they are saved,
;; if they contain a valid shebang line.
(progn
  (unless (fboundp 'executable-make-buffer-file-executable-if-script-p)
    (autoload #'executable-make-buffer-file-executable-if-script-p "executable" nil t))

  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p))

;; LATER: Prettier times out setting up the process on a remote machine. I am using `format-all'
;; for now.
;; https://github.com/jscheid/prettier.el/issues/84
(use-package prettier
  :if (executable-find "prettier")
  :disabled t
  :commands prettier-mode
  :hook
  ;; Should work with `gfm-mode', `css-mode', and `html-mode' as they are derived modes
  ((markdown-mode-hook web-mode-hook json-mode-hook jsonc-mode-hook js2-mode-hook)
   . (lambda ()
       (when (and buffer-file-name ; Returns `nil' if not visiting a file
                  (not (file-remote-p buffer-file-name)))
         (prettier-mode 1))))
  :config (setq prettier-lighter nil))

(use-package highlight-doxygen
  :commands highlight-doxygen-global-mode
  :init (highlight-doxygen-global-mode))

(use-package apt-sources-list
  :commands apt-sources-list-mode)

(use-package ssh-config-mode
  :commands (ssh-config-mode ssh-known-hosts-mode ssh-authorized-keys-mode)
  :hook (ssh-config-mode-hook . turn-on-font-lock))

(provide 'init-languages)

;;; init-languages.el ends here
