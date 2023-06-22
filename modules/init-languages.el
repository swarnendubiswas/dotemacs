;;; init-languages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:
;;; utf-8; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary: This file contains configurations related to individual language modes.

;;; Code:

(defvar sb/fill-column)
(defvar hs-isearch-open)
(defvar sb/minibuffer-completion)
(defvar sb/user-home-directory)
(defvar sb/python-langserver)

(declare-function spell-fu-mode "spell-fu")

(add-hook
  'prog-mode-hook
  (lambda ()
    (auto-fill-mode 1) ; Autofill comments

    ;; Native from Emacs 27+, disable in TUI since the line characters also get copied.
    (when (display-graphic-p)
      (display-fill-column-indicator-mode 1))))

(use-package which-func
  :hook (prog-mode-hook . which-function-mode)
  :custom (which-func-modes '(emacs-lisp-mode c-mode c++-mode python-mode makefile-mode sh-mode java-mode)))

(use-package subword
  :straight (:type built-in)
  :hook (prog-mode-hook . subword-mode)
  :diminish)

;; Hide top-level code blocks. Enable code folding, which is useful for browsing large files. This
;; module is part of Emacs, and is better maintained than other alternatives like `origami'.

;; (use-package hideshow
;;   :straight (:type built-in)
;;   :commands (hs-hide-all hs-hide-initial-comment-block hs-show-all hs-show-block)
;;   :hook
;;   ;; Hideshow is not defined for `ini-mode'.
;;   ((python-mode-hook c-mode-hook c++-mode-hook emacs-lisp-mode-hook java-mode-hook sh-mode-hook)
;;     .
;;     hs-minor-mode)
;;   :custom (hs-isearch-open t "Open all folds while searching")
;;   :diminish hs-minor-mode)

(use-package symbol-overlay ; Highlight symbol under point
  :commands (transient-define-prefix)
  :hook (prog-mode-hook . symbol-overlay-mode)
  :bind (("M-p" . symbol-overlay-jump-prev) ("M-n" . symbol-overlay-jump-next))
  :custom (symbol-overlay-idle-time 2 "Delay highlighting to allow for transient cursor placements")
  :diminish)

(use-package highlight-escape-sequences
  :hook (prog-mode-hook . hes-mode))

(use-package compile
  :straight (:type built-in)
  :bind
  ;; "<f10>" and "<f11>" conflict with Gnome window manager keybindings
  (("<f10>" . compile) ("<f11>" . recompile))
  :custom
  (compilation-always-kill t "Kill a compilation process before starting a new one")
  (compilation-ask-about-save nil "Save all modified buffers without asking")
  ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first
  ;; error.
  (compilation-scroll-output 'first-error))

(use-package fancy-compilation
  :straight (:host codeberg :repo "ideasman42/emacs-fancy-compilation" :branch "main")
  :after compile
  :init (fancy-compilation-mode 1))

(use-package rainbow-delimiters
  :hook ((prog-mode-hook latex-mode-hook LaTeX-mode-hook org-src-mode-hook) . rainbow-delimiters-mode))

;; Tree-sitter provides advanced syntax highlighting features

(use-package tree-sitter
  :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode) (prog-mode-hook . global-tree-sitter-mode))
  :config
  (use-package tree-sitter-langs
    :demand t)
  :diminish tree-sitter-mode)

;; https://www.reddit.com/r/emacs/comments/10iuim1/getting_emacs_29_to_automatically_use_treesitter/
;; https://github.com/renzmann/treesit-auto

;; (use-package treesit
;;   :straight (:type built-in)
;;   :demand t
;;   :config
;;   (setq treesit-language-source-alist
;;     '
;;     ((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;       (c "https://github.com/tree-sitter/tree-sitter-c")
;;       (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;       (cmake "https://github.com/uyha/tree-sitter-cmake")
;;       (css "https://github.com/tree-sitter/tree-sitter-css")
;;       (docker "https://github.com/camdencheek/tree-sitter-dockerfile")
;;       (emacs-lisp "https://github.com/Wilfred/tree-sitter-elisp")
;;       (html "https://github.com/tree-sitter/tree-sitter-html")
;;       (java "https://github.com/tree-sitter/tree-sitter-java")
;;       (js "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;       (json "https://github.com/tree-sitter/tree-sitter-json")
;;       (make "https://github.com/alemuller/tree-sitter-make")
;;       (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;       (org "https://github.com/milisims/tree-sitter-org")
;;       (python "https://github.com/tree-sitter/tree-sitter-python")
;;       (rust "https://github.com/tree-sitter/tree-sitter-rust")
;;       (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;   (add-to-list 'major-mode-remap-alist '(bash-mode . bash-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(make-mode . make-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(org-mode . org-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

;;   (setq
;;     bash-ts-mode-hook bash-mode-hook
;;     c-ts-mode-hook c-mode-hook
;;     cpp-ts-mode-hook c++-mode-hook
;;     cmake-ts-mode-hook cmake-mode-hook
;;     css-ts-mode-hook css-mode-hook
;;     html-ts-mode-hook html-mode-hook
;;     java-ts-mode-hook java-mode-hook
;;     json-ts-mode-hook json-mode-hook
;;     make-ts-mode-hook make-mode-hook
;;     markdown-ts-mode-hook markdown-mode-hook
;;     org-ts-mode-hook org-mode-hook
;;     python-ts-mode-hook python-mode-hook
;;     yaml-ts-mode-hook yaml-ts-mode-hook))

(use-package eldoc
  :straight (:type built-in)
  :hook (prog-mode-hook . turn-on-eldoc-mode)
  :custom (eldoc-area-prefer-doc-buffer t "Disable popups")
  ;; The variable-height minibuffer and extra eldoc buffers are distracting. We can limit ElDoc
  ;; messages to one line which prevents the echo area from resizing itself unexpectedly when point
  ;; is on a variable with a multiline docstring, but then it cuts of useful information.
  ;; (eldoc-echo-area-use-multiline-p nil)
  :config
  ;; Allow eldoc to trigger after completions
  (with-eval-after-load "company"
    (eldoc-add-command
      'company-complete-selection
      'company-complete-common
      'company-capf
      'company-abort))
  :diminish)

;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(setenv "SHELL" shell-file-name) ; Recommended to connect with Bash

;; Available C styles: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Built_002din-Styles
;;   "gnu": The default style for GNU projects
;;   "k&r": What Kernighan and Ritchie, the authors of C used in their book
;;   "bsd": What BSD developers use, aka "Allman style" after Eric Allman.
;;   "whitesmith": Popularized by the examples that came with Whitesmiths C, an early commercial C
;;   compiler.
;;   "stroustrup": What Stroustrup, the author of C++ used in his book
;;   "ellemtel": Popular C++ coding standards as defined by "Programming in C++, Rules and
;;   Recommendations," Erik Nyquist and Mats Henricson, Ellemtel
;;   "linux": What the Linux developers use for kernel development
;;   "python": What Python developers use for extension modules
;;   "java": The default style for java-mode (see below)
;;   "user": When you want to define your own style

(use-package cc-mode
  :straight (:type built-in)
  :defines (c-electric-brace c-enable-auto-newline c-set-style)
  :commands (c-fill-paragraph c-end-of-defun c-beginning-of-defun c++-mode)
  :mode
  ;; By default, files ending in ".h" are treated as C files.
  (("\\.h\\'" . c++-mode) ("\\.c\\'" . c++-mode))
  :hook
  (c++-mode-hook
    .
    (lambda ()
      (setq-local
        c-set-style "cc-mode"
        c-basic-offset 2)
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :bind
  (:map
    c-mode-base-map
    ("C-M-a" . c-beginning-of-defun)
    ("C-M-e" . c-end-of-defun)
    ("C-c C-d")
    :map
    c-mode-map
    ("C-M-a"))
  :config
  ;; Disable electric indentation and on-type formatting
  (add-hook
    'c++-mode-hook
    (lambda ()
      (setq-local
        c-auto-newline nil
        ;; c-electric-brace nil
        c-electric-flag nil
        ;; c-electric-indent nil
        c-enable-auto-newline nil
        c-syntactic-indentation nil)))

  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection "clangd")
  ;;       :major-modes '(c-mode c++-mode)
  ;;       :remote? t
  ;;       :server-id 'clangd-r)))
  )

(use-package modern-cpp-font-lock ; Better highlight for modern C++
  :hook (c++-mode-hook . modern-c++-font-lock-mode)
  :diminish modern-c++-font-lock-mode)

(use-package cuda-mode
  :commands cuda-mode
  :mode (("\\.cu\\'" . c++-mode) ("\\.cuh\\'" . c++-mode)))

(use-package opencl-mode
  :mode "\\.cl\\'")

(use-package cmake-mode
  :if (executable-find "cmake")
  :commands cmake-mode
  :mode "\(CMakeLists\.txt|\.cmake\)$"
  :hook
  (cmake-mode-hook
    .
    (lambda ()
      (when (fboundp 'spell-fu-mode)
        (spell-fu-mode -1))
      (flyspell-mode -1)
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (progn
            ;; Disable text checkers
            (make-local-variable 'lsp-disabled-clients)
            (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
            (lsp-deferred))))))
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection "cmake-language-server")
  ;;       :major-modes '(cmake-mode)
  ;;       :remote? t
  ;;       :server-id 'cmakels-r)))
  )

(use-package cmake-font-lock ; Advanced syntax coloring support for CMake scripts
  :hook (cmake-mode-hook . cmake-font-lock-activate))

;; (use-package rmsbolt
;;   :commands rmsbolt-mode)

(use-package python
  :straight (:type built-in)
  :mode
  (("SCon\(struct\|script\)$" . python-mode)
    ("[./]flake8\\'" . conf-mode)
    ("/Pipfile\\'" . conf-mode))
  :hook
  (python-mode-hook
    .
    (lambda ()
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :bind
  ;; Assigning a keybinding such as "C-[" is involved, "[" is treated as `meta'
  ;; https://emacs.stackexchange.com/questions/64839/assign-a-keybinding-with-c

  ;; TODO: Bind other functions suitably: python-nav-beginning-of-block, python-nav-end-of-block,
  ;; python-nav-backward-defun, python-nav-forward-defun, python-nav-backward-statement,
  ;; python-nav-forward-statement
  (:map
    python-mode-map
    ("C-c C-d")
    ("C-M-a" . python-nav-beginning-of-defun)
    ("C-M-e" . python-nav-end-of-defun)
    ("M-a" . python-nav-backward-block)
    ("M-e" . python-nav-forward-block)
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

    (setq
      lsp-pylsp-configuration-sources ["setup.cfg"]
      lsp-pylsp-plugins-mccabe-enabled nil
      ;; We can also set this per-project
      lsp-pylsp-plugins-preload-modules ["numpy" , "csv" , "pandas" , "statistics" , "json"]
      lsp-pylsp-plugins-pycodestyle-enabled nil
      lsp-pylsp-plugins-pycodestyle-max-line-length sb/fill-column
      lsp-pylsp-plugins-pydocstyle-convention "pep257"
      lsp-pylsp-plugins-pydocstyle-ignore (vconcat (list "D100" "D101" "D103" "D213"))
      lsp-pylsp-plugins-pyflakes-enabled nil
      lsp-pylsp-plugins-pylint-args
      (vconcat
        (list
          "-j 2"
          (concat "--rcfile=" (expand-file-name ".config/pylintrc" sb/user-home-directory))))
      lsp-pylsp-plugins-pylint-enabled t
      lsp-pylsp-plugins-yapf-enabled t
      lsp-pylsp-plugins-flake8-enabled nil
      lsp-pylsp-plugins-black-enabled nil
      lsp-pylsp-plugins-jedi-use-pyenv-environment t)))

(use-package python-docstring
  :after python-mode
  :demand t
  :commands (python-docstring-mode python-docstring-install)
  :config (python-docstring-install)
  :diminish)

(use-package pip-requirements
  :commands (pip-requirements-mode))

(use-package pyvenv
  :commands (pyvenv-tracking-mode)
  :hook (python-mode-hook . pyvenv-mode)
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-post-activate-hooks
    (list (lambda () (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks (list (lambda () (setq python-shell-interpreter "python3")))))

(use-package python-isort
  :straight (:host github :repo "wyuenho/emacs-python-isort")
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :hook (python-mode-hook . python-isort-on-save-mode)
  :custom
  (python-isort-arguments
    '
    ("--stdout" "--atomic" "-l 100"
      "--up" ; Use parentheses
      "--tc" ; Use a trailing comma on multiline imports
      "-")))

;; Yapfify works on the original file, so that any project settings supported by YAPF itself are
;; used. We do not use `lsp-format-buffer' or `eglot-format-buffer' since `pyright' does not support
;; document formatting.
(use-package yapfify
  :if (executable-find "yapf")
  :hook (python-mode-hook . yapf-mode)
  :diminish yapf-mode)

(use-package cperl-mode
  :mode ("latexmkrc\\'")
  :hook
  (cperl-mode-hook
    .
    (lambda ()
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :config
  ;; Prefer CPerl mode to Perl mode
  (fset 'perl-mode 'cperl-mode)

  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection
  ;;       (lsp-tramp-connection
  ;;         (lambda ()
  ;;           (list
  ;;             lsp-perl-language-server-path
  ;;             "-MPerl::LanguageServer"
  ;;             "-e"
  ;;             "Perl::LanguageServer::run"
  ;;             "--"
  ;;             (format "--port %d --version %s"
  ;;               lsp-perl-language-server-port
  ;;               lsp-perl-language-server-client-version))))
  ;;       :major-modes '(perl-mode cperl-mode)
  ;;       :remote? t
  ;;       :initialized-fn
  ;;       (lambda (workspace)
  ;;         (with-lsp-workspace
  ;;           workspace
  ;;           (lsp--set-configuration (lsp-configuration-section "perl"))))
  ;;       :priority -1
  ;;       :server-id 'perlls-r)))
  )

(use-package ant
  :commands (ant ant-clean ant-compile ant-test))

;; (use-package autodisass-java-bytecode ; Can disassemble ".class" files from within jars
;;   :mode "\\.class\\'")

(use-package sh-script ; Shell script mode
  :straight (:type built-in)
  :mode (("\\.zsh\\'" . sh-mode) ("\\bashrc\\'" . sh-mode))
  :hook
  (sh-mode-hook
    .
    (lambda ()
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :bind (:map sh-mode-map ("C-c C-d"))
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2)
  (sh-indent-after-continuation 'always)
  (sh-indent-comment t "Indent comments as a regular line")
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("bash-language-server" "start"))
  ;;       :major-modes '(sh-mode)
  ;;       :remote? t
  ;;       :server-id 'bashls-r)))
  )

(use-package fish-mode
  :mode "\\.fish\\'"
  :interpreter "fish"
  :commands (fish-mode fish_indent-before-save)
  :hook (fish-mode-hook . (lambda () (add-hook 'before-save-hook #'fish_indent-before-save))))

;; Files are given `+x' permissions when they are saved, if they contain a valid shebang line.
(use-package executable
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package highlight-doxygen
  :commands (highlight-doxygen-global-mode)
  :hook ((c-mode-hook c++-mode-hook) . highlight-doxygen-mode))

(use-package elisp-mode
  :straight (:type built-in)
  :mode ("\\.el\\'" . emacs-lisp-mode)
  :hook
  (emacs-lisp-mode-hook
    .
    (lambda ()
      (when buffer-file-name
        (add-hook 'after-save-hook #'check-parens nil t)))))

(use-package ini-mode
  :commands (ini-mode))

(use-package conf-mode
  :straight (:type built-in)
  :mode
  "\\.cfg\\'"
  "\\.conf\\'")

(use-package yaml-mode
  :defines (lsp-ltex-enabled lsp-disabled-clients)
  :commands (yaml-mode)
  :mode ("\\.yml\\'" "\\.yaml\\'" ".clang-format" ".clang-tidy" ".clangd")
  :hook
  (yaml-mode-hook
    .
    (lambda ()
      ;; `yaml-mode' is derived from `text-mode', so disable grammar and spell
      ;; checking.
      (when (fboundp 'spell-fu-mode)
        (spell-fu-mode -1))
      (when (fboundp 'flyspell-mode)
        (flyspell-mode -1))
      (when (fboundp 'jinx-mode)
        (jinx-mode -1))
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (progn
            (make-local-variable 'lsp-disabled-clients)
            (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
            (lsp-deferred))))))
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("yaml-language-server" "--stdio"))
  ;;       :major-modes '(yaml-mode)
  ;;       :remote? t
  ;;       :server-id 'yamlls-r)))
  )

(use-package yaml-imenu
  :hook (yaml-mode-hook . yaml-imenu-enable))

(use-package css-mode
  :commands css-mode
  :hook
  (css-mode-hook
    .
    (lambda ()
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :custom (css-indent-offset 2)
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("css-languageserver" "--stdio"))
  ;;       :major-modes '(css-mode)
  ;;       :remote? t
  ;;       :server-id 'cssls-r)))
  )

(use-package make-mode
  :straight (:type built-in)
  :mode
  (("\\Makefile\\'" . makefile-mode)
    ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
    ("makefile\\.rules\\'" . makefile-gmake-mode))
  :hook (makefile-mode-hook . (lambda () (setq-local indent-tabs-mode t))))

(use-package makefile-executor
  :hook (makefile-mode-hook . makefile-executor-mode))

;; Align fields with "C-c C-a"
(use-package csv-mode
  :defines (lsp-disabled-clients)
  :commands (csv-mode)
  :hook
  (csv-mode-hook
    .
    (lambda ()
      (make-local-variable 'lsp-disabled-clients)
      (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
      (when (fboundp 'spell-fu-mode)
        (spell-fu-mode -1))
      (when (fboundp 'flyspell-mode)
        (flyspell-mode -1))
      (when (fboundp 'jinx-mode)
        (jinx-mode -1))))
  :custom (csv-separators '("," ";" "|" " ")))

(use-package antlr-mode
  :straight (:type built-in)
  :mode "\\.g4\\'")

(use-package bison-mode
  :mode ("\\.flex\\'" . flex-mode)
  :mode ("\\.bison\\'" . bison-mode)
  :hook
  (flex-mode-hook
    .
    (lambda ()
      ;; Disable electric indentation and on-type formatting
      (setq-local
        c-auto-newline nil
        ;; c-electric-brace nil
        c-electric-flag nil
        ;; c-electric-indent nil
        c-enable-auto-newline nil
        c-syntactic-indentation nil))))

;; (use-package llvm-mode
;;   ;; :straight (llvm-mode :type git :host github
;;   ;;                      :repo "llvm/llvm-project"
;;   ;;                      :files "llvm/utils/emacs/llvm-mode.el")
;;   :straight nil
;;   :load-path "extras"
;;   :commands (llvm-mode)
;;   :mode "\\.ll\\'")

;; (use-package autodisass-llvm-bitcode
;;   :commands (autodisass-llvm-bitcode)
;;   :mode "\\.bc\\'")

;; Enable live preview with "C-c C-c l" (`markdown-live-preview-mode'). The following page lists
;; more shortcuts.
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :commands
  (markdown-mode
    gfm-mode
    markdown-insert-bold
    markdown-insert-italic
    markdown-insert-blockquote
    markdown-insert-pre
    markdown-insert-code
    markdown-move-up
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
  ;; :init
  ;; Looks good, but hiding markup makes it difficult to be consistent while editing
  ;; (setq-default markdown-hide-markup t)
  :mode
  ;; The order is important to associate "README.md" with `gfm-mode'
  (("\\.md\\'" . markdown-mode) ("\\.markdown\\'" . markdown-mode) ("README\\.md\\'" . gfm-mode))
  :bind (:map markdown-mode-map ("C-c C-d") ("C-c C-j"))
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

;; (use-package markdown-toc
;;   :commands (markdown-toc-refresh-toc markdown-toc-generate-toc markdown-toc-generate-or-refresh-toc))

;; Use `pandoc-convert-to-pdf' to export markdown file to pdf. Convert `markdown' to `org': "pandoc
;; -f markdown -t org -o output-file.org input-file.md"
(use-package pandoc-mode
  :commands (pandoc-load-default-settings pandoc-mode)
  :hook (markdown-mode-hook . pandoc-mode)
  :config (pandoc-load-default-settings)
  :diminish)

;; Open preview of markdown file in a browser

;; (use-package markdown-preview-mode
;;   :disabled t
;;   :commands markdown-preview-mode)

;; (use-package bat-mode
;;   :straight (:type built-in)
;;   :mode
;;   (("\\.bat\\'" . bat-mode)
;;    ("\\.cmd\\'" . bat-mode)))

(use-package web-mode
  :commands (web-mode)
  :mode "\\.html?\\'"
  :hook
  (web-mode-hook
    .
    (lambda ()
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :bind ("C-c C-d")
  :custom
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing nil "Prefer smartparens")
  (web-mode-enable-auto-quoting t)
  (web-mode-enable-block-face t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t "Highlight the element under the cursor")
  (web-mode-enable-current-column-highlight t)
  (web-mode-markup-indent-offset 2) ; HTML
  (web-mode-css-indent-offset 2) ; CSS
  (web-mode-code-indent-offset 2) ; Script
  (web-mode-style-padding 2) ; For <style> tag
  (web-mode-script-padding 2) ; For <script> tag
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("html-languageserver" "--stdio"))
  ;;       :major-modes '(html-mode web-mode mhtml-mode)
  ;;       :remote? t
  ;;       :server-id 'htmlls-r)))
  )

(use-package emmet-mode
  :defines (emmet-move-cursor-between-quote)
  :hook ((web-mode-hook css-mode-hook html-mode-hook) . emmet-mode)
  :custom (emmet-move-cursor-between-quote t))

(use-package nxml-mode
  :straight (:type built-in)
  :commands (nxml-mode)
  :mode ("\\.xml\\'" "\\.xsd\\'" "\\.xslt\\'" "\\.pom$")
  :hook
  (nxml-mode-hook
    .
    (lambda ()
      ;; `xml-mode' is derived from `text-mode', so disable grammar and spell
      ;; checking.
      (when (fboundp 'spell-fu-mode)
        (spell-fu-mode -1))
      (flyspell-mode -1)
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (progn
            (make-local-variable 'lsp-disabled-clients)
            (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
            (lsp-deferred))))))
  :custom
  (nxml-auto-insert-xml-declaration-flag t)
  (nxml-slash-auto-complete-flag t)
  (nxml-sexp-element-flag t)
  :config (fset 'xml-mode 'nxml-mode)

  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("java" "-jar" lsp-xml-jar-file))
  ;;       :major-modes '(xml-mode nxml-mode)
  ;;       :remote? t
  ;;       :server-id 'xmlls-r)))
  )

(use-package json-mode
  :commands (json-mode jsonc-mode json-mode-beautify)
  :mode
  (("\\.json\\'" . json-mode)
    ("pyrightconfig.json" . jsonc-mode)
    (".*/vscode/settings.json$" . jsonc-mode)
    (".*/\\.vscode/settings.json$" . jsonc-mode)
    ("User/settings.json$" . jsonc-mode))
  :hook
  ((json-mode-hook jsonc-mode-hook)
    .
    (lambda ()
      (make-local-variable 'js-indent-level)
      (setq js-indent-level 2)
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection '("vscode-json-languageserver" "--stdio"))
  ;;       :major-modes '(json-mode jsonc-mode)
  ;;       :remote? t
  ;;       :server-id 'jsonls-r)))
  )

(use-package json-reformat
  :after (:any json-mode jsonc-mode)
  :demand t
  :custom
  (json-reformat:indent-width 2)
  (js-indent-level 2))

;; (use-package bazel
;;   :if (executable-find "bazel")
;;   :commands
;;   (bazel-mode bazelrc-mode bazel-buildifier)
;;   :hook
;;   ((bazel-mode-hook . (lambda ()
;;                         (add-hook 'before-save-hook #'bazel-buildifier nil t)))
;;    (bazel-mode-hook . flycheck-mode)))

;; (use-package protobuf-mode
;;   :commands
;;   (protobuf-mode)
;;   :mode "\\.proto$"
;;   :hook
;;   (protobuf-mode-hook . flycheck-mode))

;; (use-package mlir-mode
;;   :straight nil
;;   :commands
;;   (mlir-mode)
;;   :load-path "extras"
;;   :mode "\\.mlir\\'")

;; (use-package dotenv-mode
;;   :mode "\\.env\\'")

(use-package apt-sources-list
  :commands apt-sources-list-mode)

(use-package ssh-config-mode
  :mode ("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
  :mode ("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys\\'" . ssh-authorized-keys-mode)
  :hook (ssh-config-mode-hook . turn-on-font-lock))

(provide 'init-languages)

;;; init-languages.el ends here
