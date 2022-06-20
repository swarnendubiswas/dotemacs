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

(use-package subword
  :straight (:type built-in)
  :diminish
  :hook (prog-mode-hook . subword-mode))

(use-package outline ; Edit outlines
  :hook (prog-mode-hook . outline-minor-mode)
  :diminish outline-minor-mode)

;; Hide top-level code blocks. Enable code folding, which is useful for browsing large files. This
;; module is part of Emacs, and is better maintained than other alternatives like `origami'.
(use-package hideshow
  :straight (:type built-in)
  :commands (hs-hide-all hs-hide-initial-comment-block hs-show-all hs-show-block)
  :diminish hs-minor-mode
  :hook
  ;; Hideshow is not defined for `ini-mode'.
  ((python-mode-hook emacs-lisp-mode-hook java-mode-hook sh-mode-hook) . hs-minor-mode)
  :custom
  (hs-isearch-open t "Open all folds while searching"))

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
  :if (and (symbol-value 'sb/IS-LINUX) (eq sb/minibuffer-completion 'ivy) (executable-find "ctags"))
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

(use-package highlight-indentation
  :diminish (highlight-indentation-current-column-mode highlight-indentation-mode)
  :hook ((yaml-mode-hook python-mode-hook) . highlight-indentation-mode))

(use-package aggressive-indent ; Claims to be better than `electric-indent-mode'
  :hook (emacs-lisp-mode-hook . aggressive-indent-mode)
  :diminish
  :custom
  (aggressive-indent-comments-too t)
  ;; Never use `electric-indent-mode'
  (aggressive-indent-dont-electric-modes t))

(use-package symbol-overlay ; Highlight symbol under point
  :diminish
  :commands transient-define-prefix
  :hook (prog-mode-hook . symbol-overlay-mode)
  :bind
  (("M-p" . symbol-overlay-jump-prev)
   ("M-n" . symbol-overlay-jump-next))
  :custom
  ;; Delay highlighting to allow for transient cursor placements
  (symbol-overlay-idle-time 2)
  :config
  (transient-define-prefix sb/symbol-overlay-transient ()
    "Symbol Overlay transient"
    ["Symbol Overlay"
     ["Overlays"
      ("." "Add/Remove at point" symbol-overlay-put)
      ("k" "Remove All" symbol-overlay-remove-all)
      ]
     ["Move to Symbol"
      ("n" "Next" symbol-overlay-jump-next)
      ("p" "Previous" symbol-overlay-jump-prev)
      ]
     ["Other"
      ("m" "Highlight symbol-at-point" symbol-overlay-mode)
      ]
     ]
    )
  (bind-key "M-o" #'sb/symbol-overlay-transient))

(use-package highlight-escape-sequences
  :hook (prog-mode-hook . hes-mode))

(use-package ini-mode
  :commands ini-mode)

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
  :defines lsp-ltex-enabled lsp-disabled-clients
  :commands yaml-mode
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
  :after yaml-mode
  :demand t
  :config (yaml-imenu-enable))

(use-package css-mode
  :commands css-mode
  :hook (css-mode-hook . lsp-deferred)
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
  :hook (makefile-mode-hook . makefile-executor-mode))

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
  :commands llvm-mode
  :mode "\\.ll\\'")

(use-package tablegen-mode
  :straight nil
  :load-path "extras"
  :commands tablegen-mode
  :mode "\\.td\\'")

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
        ("C-c C-d" . nil)
        ("C-c C-j" . nil)))

;; Generate TOC with `markdown-toc-generate-toc'
(use-package markdown-toc
  ;; :hook (markdown-mode-hook . markdown-toc-generate-toc)
  :commands (markdown-toc-refresh-toc markdown-toc-generate-toc
                                      markdown-toc-generate-or-refresh-toc))

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

(use-package docstr
  :diminish
  :hook ((c++-mode-hook python-mode-hook java-mode-hook) . docstr-mode))

(use-package cc-mode
  :straight (:type built-in)
  :defines (c-electric-brace c-enable-auto-newline c-set-style)
  :commands (c-fill-paragraph c-end-of-defun c-beginning-of-defun c++-mode)
  :mode
  (("\\.h\\'" . c++-mode)
   ("\\.c\\'" . c++-mode))
  :hook (c++-mode-hook . (lambda ()
                           (setq-local c-set-style "cc-mode"
                                       c-basic-offset 2)
                           (lsp-deferred)))
  :config
  (defvar c-electric-indent)

  ;; Disable electric indentation and on-type formatting
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq-local c-auto-newline nil
                          c-electric-brace nil
                          c-electric-flag nil
                          c-electric-indent nil
                          c-enable-auto-newline nil
                          c-syntactic-indentation nil)))

  (unbind-key "C-M-a" c-mode-map)

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection "clangd")
  ;;   :major-modes '(c-mode c++-mode)
  ;;   :remote? t
  ;;   :server-id 'clangd-r))
  :bind
  (:map c-mode-base-map
        ("C-c c a" . c-beginning-of-defun)
        ("C-c c e" . c-end-of-defun)
        ("M-q"     . c-fill-paragraph)
        ("C-c C-d" . nil)))

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
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection "cmake-language-server")
  ;;   :major-modes '(cmake-mode)
  ;;   :remote? t
  ;;   :server-id 'cmakels-r))
  )

(use-package cmake-font-lock
  :hook (cmake-mode-hook . cmake-font-lock-activate))

(use-package python
  :straight (:type built-in)
  :hook (python-mode-hook . lsp-deferred)
  :mode
  (("SCon\(struct\|script\)$" . python-mode)
   ("[./]flake8\\'" . conf-mode)
   ("/Pipfile\\'" . conf-mode))
  :bind
  ;; Assigning a keybinding such as "C-[" is involved, `[' is treated as `meta'
  ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c
  ;; TODO: Bind other keys suitably
  ;; python-nav-beginning-of-block
  ;; python-nav-end-of-block
  ;; python-nav-beginning-of-defun
  ;; python-nav-end-of-defun
  ;; python-nav-backward-defun
  ;; python-nav-forward-defun
  ;; python-nav-backward-statement
  ;; python-nav-forward-statement
  (:map python-mode-map
        ("C-c C-d")
        ("M-a"   . python-nav-backward-block)
        ("M-e"   . python-nav-forward-block)
        ("C-c <" . python-indent-shift-left)
        ("C-c >" . python-indent-shift-right))
  :custom
  (python-shell-completion-native-enable nil "Disable readline based native completion")
  (python-fill-docstring-style 'django)
  (python-indent-guess-indent-offset-verbose nil "Remove guess indent python message")
  (python-indent-guess-indent-offset nil)
  (python-indent-offset 4)
  (python-shell-exec-path "python3")
  (python-shell-interpreter "python3")
  :config
  (setenv "PYTHONPATH" "python3")

  ;; (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint))))))

  ;; (setq auto-mode-alist (append
  ;;                        '(("SCon\(struct\|script\)$" . python-mode)
  ;;                          ("SConscript\\'" . python-mode)
  ;;                          ("[./]flake8\\'" . conf-mode)
  ;;                          ("/Pipfile\\'" . conf-mode))
  ;;                        auto-mode-alist))

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
      (setq lsp-pylsp-configuration-sources ["setup.cfg"]
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
            lsp-pylsp-plugins-jedi-use-pyenv-environment t))))


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

;; FIXME: There is an error in passing options to isort
(use-package py-isort
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :disabled t
  :commands (py-isort-before-save py-isort-buffer py-isort-region)
  :hook
  (python-mode-hook . (lambda ()
                        (add-hook 'before-save-hook #'py-isort-before-save)))
  :custom
  (py-isort-options '("-l 100"
                      "--up" ; Use parentheses
                      "--tc" ; Use a trailing comma on multiline imports
                      )))

(use-package python-isort
  :straight (python-isort :type git :host github :repo "wyuenho/emacs-python-isort")
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :custom
  (python-isort-arguments '("--stdout" "--atomic" "-l 100" "--up" "--tc" "-"))
  :hook (python-mode-hook . python-isort-on-save-mode))

;; Yapfify works on the original file, so that any project settings supported by YAPF itself are
;; used. We do not use `lsp-format-buffer' since `pyright' does not support document formatting.
(use-package yapfify
  :diminish yapf-mode
  :if (executable-find "yapf")
  :hook (python-mode-hook . yapf-mode))

(use-package cperl-mode
  :mode ("latexmkrc\\'")
  :hook (cperl-mode-hook . lsp-deferred)
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
  :commands (ant ant-clean ant-compile ant-test))

(use-package autodisass-java-bytecode ; Can disassemble ".class" files from within jars
  :commands autodisass-java-bytecode
  :mode "\\.class\\'")

(use-package groovy-mode ; Syntax highlighting for Gradle files
  :commands groovy-mode
  :mode "\\.gradle\\'")

(use-package sh-script ; Shell script mode
  :straight (:type built-in)
  :commands flycheck-add-next-checker
  :mode
  (("\\.zsh\\'"   . sh-mode)
   ("\\bashrc\\'" . sh-mode))
  :hook (sh-mode-hook . lsp-deferred)
  :custom
  (sh-basic-offset 2)
  (sh-indent-after-continuation 'always)
  (sh-indent-comment t "Indent comments as a regular line")
  :config
  (unbind-key "C-c C-d" sh-mode-map) ; Was bound to `sh-cd-here'
  (flycheck-add-next-checker 'sh-bash 'sh-shellcheck)

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
  :commands (fish-mode fish_indent-before-save)
  :hook
  (fish-mode-hook . (lambda ()
                      (add-hook 'before-save-hook #'fish_indent-before-save))))

(use-package shfmt
  :hook (sh-mode-hook . shfmt-on-save-mode)
  :custom
  ;; p: Posix, ci: indent case labels, i: indent with spaces
  (shfmt-arguments '("-i" "4" "-p" "-ci")))

(use-package bat-mode
  :straight (:type built-in)
  :mode
  (("\\.bat\\'" . bat-mode)
   ("\\.cmd\\'" . bat-mode)))

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
  :defines emmet-move-cursor-between-quote
  :hook ((web-mode-hook css-mode-hook html-mode-hook) . emmet-mode)
  :custom (emmet-move-cursor-between-quote t))

(use-package nxml-mode
  :straight (:type built-in)
  :commands nxml-mode
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
  :commands (bazel-mode bazelrc-mode bazel-buildifier)
  :hook
  ((bazel-mode-hook . (lambda ()
                        (add-hook 'before-save-hook #'bazel-buildifier nil t)))
   (bazel-mode-hook . flycheck-mode)))

(use-package protobuf-mode
  :commands protobuf-mode
  :mode "\\.proto$"
  :hook (protobuf-mode-hook . flycheck-mode))

(use-package mlir-mode
  :straight nil
  :commands mlir-mode
  :load-path "extras"
  :mode "\\.mlir\\'")

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
(use-package tree-sitter
  :diminish tree-sitter-mode
  :hook
  ((tree-sitter-after-on-hook . tree-sitter-hl-mode)
   (after-init-hook . global-tree-sitter-mode))
  :config
  (use-package tree-sitter-langs
    :demand t))

(use-package dotenv-mode
  :mode "\\.env\\'")

(with-eval-after-load "compile"
  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-compile.el
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
  (add-hook 'compilation-filter-hook 'sanityinc/colourise-compilation-buffer)

  ;; https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
  (defun sb/bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings "
    (when (and
           (buffer-live-p buffer)
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           (not
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                      buffer)))
  (add-hook 'compilation-finish-functions #'bury-compile-buffer-if-successful))

(use-package rainbow-delimiters
  :hook ((prog-mode-hook latex-mode-hook LaTeX-mode-hook
                         org-src-mode-hook) . rainbow-delimiters-mode))

;; The following section helper ensures that files are given `+x' permissions when they are saved,
;; if they contain a valid shebang line.
(use-package executable
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package highlight-doxygen
  :commands highlight-doxygen-global-mode
  :init (highlight-doxygen-global-mode))

(use-package apt-sources-list
  :commands apt-sources-list-mode)

(use-package ssh-config-mode
  :commands (ssh-config-mode ssh-known-hosts-mode ssh-authorized-keys-mode)
  :hook (ssh-config-mode-hook . turn-on-font-lock))

(use-package string-inflection
  :bind
  (:map prog-mode-map
        ("C-c C-u" . string-inflection-all-cycle)))

(provide 'init-languages)

;;; init-languages.el ends here
