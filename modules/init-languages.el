;;; init-languages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/fill-column)
(defvar hs-isearch-open)
(defvar sb/minibuffer-completion)
(defvar sb/user-home-directory)
(defvar sb/python-langserver)

(declare-function spell-fu-mode "spell-fu")

(use-package ini-mode
  :commands
  (ini-mode))

(use-package conf-mode
  :straight (:type built-in)
  :mode "\\.cfg\\'" "\\.conf\\'")

(use-package elisp-mode
  :straight (:type built-in)
  :mode ("\\.el\\'"  . emacs-lisp-mode)
  :mode (".dir-locals" . emacs-lisp-mode)
  :hook
  ((lisp-mode-hook emacs-lisp-mode-hook) .
   (lambda ()
     (when buffer-file-name
       (add-hook 'after-save-hook #'check-parens nil t)
       (flycheck-add-next-checker 'emacs-lisp 'emacs-lisp-checkdoc 'append)))))

(use-package yaml-mode
  :defines
  (lsp-ltex-enabled lsp-disabled-clients)
  :commands
  (yaml-mode)
  :mode ("\\.yml\\'" "\\.yaml\\'" ".clang-format" ".clang-tidy" ".clangd")
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
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection
  ;;                    '("yaml-language-server" "--stdio"))
  ;;   :major-modes '(yaml-mode)
  ;;   :remote? t
  ;;   :server-id 'yamlls-r))
  )

(use-package yaml-imenu
  :hook
  (yaml-mode-hook . yaml-imenu-enable))

(use-package css-mode
  :commands css-mode
  :hook
  (css-mode-hook . lsp-deferred)
  :custom
  (css-indent-offset 2)
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection
  ;;                    '("css-languageserver" "--stdio"))
  ;;   :major-modes '(css-mode)
  ;;   :remote? t
  ;;   :server-id 'cssls-r))
  )

(use-package make-mode
  :straight (:type built-in)
  :mode
  (("\\Makefile\\'"       . makefile-mode)
   ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
   ("makefile\\.rules\\'" . makefile-gmake-mode))
  :hook
  (makefile-mode-hook . (lambda()
                          (setq-local indent-tabs-mode t))))

(use-package makefile-executor
  :hook
  (makefile-mode-hook . makefile-executor-mode))

;; Align fields with "C-c C-a"
(use-package csv-mode
  :defines
  (lsp-disabled-clients)
  :commands
  (csv-mode)
  :hook
  (csv-mode-hook . (lambda ()
                     (make-local-variable 'lsp-disabled-clients)
                     (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                     (spell-fu-mode -1)
                     (flyspell-mode -1)))
  :custom
  (csv-separators '("," ";" "|" " ")))

(use-package antlr-mode
  :straight (:type built-in)
  :mode "\\.g4\\'")

(use-package bison-mode
  :mode ("\\.bison\\'"))

(use-package llvm-mode
  ;; :straight (llvm-mode :type git :host github
  ;;                      :repo "llvm/llvm-project"
  ;;                      :files "llvm/utils/emacs/llvm-mode.el")
  :straight nil
  :load-path "extras"
  :commands
  (llvm-mode)
  :mode "\\.ll\\'")

(use-package tablegen-mode
  :straight nil
  :load-path "extras"
  :commands
  (tablegen-mode)
  :mode "\\.td\\'")

(use-package autodisass-llvm-bitcode
  :commands
  (autodisass-llvm-bitcode)
  :mode "\\.bc\\'")

;; Enable live preview with "C-c C-c l" (`markdown-live-preview-mode'). The following page lists
;; more shortcuts.
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode markdown-insert-bold
                 markdown-insert-italic
                 markdown-insert-blockquote markdown-insert-pre
                 markdown-insert-code markdown-move-up
                 markdown-insert-link markdown-insert-wiki-link
                 markdown-demote markdown-move-down
                 markdown-insert-header-dwim
                 markdown-insert-reference-link-dwim
                 markdown-insert-header-atx-1
                 markdown-insert-header-atx-2
                 markdown-insert-header-atx-3
                 markdown-insert-header-atx-4 markdown-promote
                 markdown-insert-list-item markdown-insert-uri
                 markdown-insert-footnote)
  ;; :init
  ;; Looks good, but hiding markup makes it difficult to be consistent while editing
  ;; (setq-default markdown-hide-markup t)
  :mode
  ;; The order is important to associate "README.md" with `gfm-mode'
  (("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-c C-d" . nil)
        ("C-c C-j" . nil))
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
  (markdown-hide-urls t))

;; Generate TOC with `markdown-toc-generate-toc'
(use-package markdown-toc
  :commands
  (markdown-toc-refresh-toc markdown-toc-generate-toc
                            markdown-toc-generate-or-refresh-toc)
  ;; :hook (markdown-mode-hook . markdown-toc-generate-toc)
  )

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf
;; Convert `markdown' to `org': "pandoc -f markdown -t org -o output-file.org input-file.md"
(use-package pandoc-mode
  :commands
  (pandoc-load-default-settings pandoc-mode)
  :hook
  (markdown-mode-hook . pandoc-mode)
  :config (pandoc-load-default-settings)
  :diminish)

;; Open preview of markdown file in a browser
(use-package markdown-preview-mode
  :disabled t
  :commands
  markdown-preview-mode)

(use-package docstr
  :hook
  ((c++-mode-hook python-mode-hook java-mode-hook) . docstr-mode)
  :diminish)

(use-package cperl-mode
  :mode ("latexmkrc\\'")
  :hook
  (cperl-mode-hook . lsp-deferred)
  :config
  ;; Prefer CPerl mode to Perl mode
  (fset 'perl-mode 'cperl-mode)

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection
  ;;                    (lambda ()
  ;;                      (list lsp-perl-language-server-path
  ;;                            "-MPerl::LanguageServer" "-e"
  ;;                            "Perl::LanguageServer::run" "--"
  ;;                            (format "--port %d --version %s"
  ;;                                    lsp-perl-language-server-port
  ;;                                    lsp-perl-language-server-client-version))))
  ;;   :major-modes '(perl-mode cperl-mode)
  ;;   :remote? t
  ;;   :initialized-fn (lambda (workspace)
  ;;                     (with-lsp-workspace workspace
  ;;                       (lsp--set-configuration
  ;;                        (lsp-configuration-section "perl"))))
  ;;   :priority -1
  ;;   :server-id 'perlls-r))
  )

(use-package ant
  :commands
  (ant ant-clean ant-compile ant-test))

(use-package autodisass-java-bytecode ; Can disassemble ".class" files from within jars
  :commands
  (autodisass-java-bytecode)
  :mode "\\.class\\'")

(use-package groovy-mode ; Syntax highlighting for Gradle files
  :commands
  (groovy-mode)
  :mode "\\.gradle\\'")

(use-package sh-script ; Shell script mode
  :straight (:type built-in)
  :commands
  (flycheck-add-next-checker)
  :mode
  (("\\.zsh\\'"   . sh-mode)
   ("\\bashrc\\'" . sh-mode))
  :hook
  (sh-mode-hook . lsp-deferred)
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  (sh-indent-after-continuation 'always)
  (sh-indent-comment t "Indent comments as a regular line")
  :config
  (unbind-key "C-c C-d" sh-mode-map) ; Was bound to `sh-cd-here'

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection
  ;;                    '("bash-language-server" "start"))
  ;;   :major-modes '(sh-mode)
  ;;   :remote? t
  ;;   :server-id 'bashls-r))
  )

(use-package fish-mode
  :mode "\\.fish\\'"
  :interpreter "fish"
  :commands
  (fish-mode fish_indent-before-save)
  :hook
  (fish-mode-hook . (lambda ()
                      (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package shfmt
  :hook
  (sh-mode-hook . shfmt-on-save-mode)
  :custom
  ;; p: Posix, ci: indent case labels, i: indent with spaces
  (shfmt-arguments '("-i" "4" "-p" "-ci")))

(use-package bat-mode
  :straight (:type built-in)
  :mode
  (("\\.bat\\'" . bat-mode)
   ("\\.cmd\\'" . bat-mode)))

(use-package web-mode
  :commands
  (web-mode)
  :mode "\\.html?\\'"
  :hook
  (web-mode-hook . lsp-deferred)
  :bind
  ("C-c C-d" . nil)
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
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection
  ;;                    '("html-languageserver" "--stdio"))
  ;;   :major-modes '(html-mode web-mode mhtml-mode)
  ;;   :remote? t
  ;;   :server-id 'htmlls-r))
  )

(use-package emmet-mode
  :defines
  (emmet-move-cursor-between-quote)
  :hook
  ((web-mode-hook css-mode-hook html-mode-hook) . emmet-mode)
  :custom (emmet-move-cursor-between-quote t))

(use-package nxml-mode
  :straight (:type built-in)
  :commands
  (nxml-mode)
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$")
  :hook
  (nxml-mode-hook . (lambda ()
                      ;; `xml-mode' is derived from `text-mode', so disable grammar and spell
                      ;; checking.
                      (make-local-variable 'lsp-disabled-clients)
                      (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                      (spell-fu-mode -1)
                      (flyspell-mode -1)
                      (lsp-deferred)))
  :custom
  (nxml-auto-insert-xml-declaration-flag t)
  (nxml-slash-auto-complete-flag t)
  :config
  (fset 'xml-mode 'nxml-mode)

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection
  ;;                    '("java" "-jar" lsp-xml-jar-file))
  ;;   :major-modes '(xml-mode nxml-mode)
  ;;   :remote? t
  ;;   :server-id 'xmlls-r))
  )

(use-package json-mode
  :commands
  (json-mode jsonc-mode json-mode-beautify)
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
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection
  ;;                    '("vscode-json-languageserver" "--stdio"))
  ;;   :major-modes '(json-mode jsonc-mode)
  ;;   :remote? t
  ;;   :server-id 'jsonls-r))
  )

(use-package json-reformat
  :after (:any json-mode jsonc-mode)
  :demand t
  :custom
  (json-reformat:indent-width 2)
  (js-indent-level 2))

(use-package bazel
  :if (executable-find "bazel")
  :commands
  (bazel-mode bazelrc-mode bazel-buildifier)
  :hook
  ((bazel-mode-hook . (lambda ()
                        (add-hook 'before-save-hook #'bazel-buildifier nil t)))
   (bazel-mode-hook . flycheck-mode)))

(use-package protobuf-mode
  :commands
  (protobuf-mode)
  :mode "\\.proto$"
  :hook
  (protobuf-mode-hook . flycheck-mode))

(use-package mlir-mode
  :straight nil
  :commands
  (mlir-mode)
  :load-path "extras"
  :mode "\\.mlir\\'")

;; Tree-sitter provides advanced syntax highlighting features

;; git clone https://github.com/ubolonton/emacs-tree-sitter/
;; cd emacs-tree-sitter
;; ./bin/setup; ./bin/build
(use-package tree-sitter
  :hook
  ((tree-sitter-after-on-hook . tree-sitter-hl-mode)
   (emacs-startup-hook . global-tree-sitter-mode))
  :config
  (use-package tree-sitter-langs
    :demand t)
  :diminish tree-sitter-mode)

(use-package dotenv-mode
  :mode "\\.env\\'")

;; Files are given `+x' permissions when they are saved, if they contain a valid shebang line.
(use-package executable
  :hook
  (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package highlight-doxygen
  :commands
  (highlight-doxygen-global-mode)
  :init (highlight-doxygen-global-mode))

(use-package apt-sources-list
  :commands apt-sources-list-mode)

(use-package ssh-config-mode
  :commands
  (ssh-config-mode ssh-known-hosts-mode ssh-authorized-keys-mode)
  :hook
  (ssh-config-mode-hook . turn-on-font-lock))

(use-package elf-mode
  :mode
  (("\\.so\\'"  . elf-mode)
   ("\\.a\\'"   . elf-mode)))

(provide 'init-languages)

;;; init-languages.el ends here
