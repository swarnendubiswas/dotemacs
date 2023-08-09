;;; init-languages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:
;;; utf-8; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary: This file contains configurations related to individual language modes.

;;; Code:

(defvar tags-revert-without-query)
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
  :custom
  (which-func-modes
    '
    (c-mode
      c-ts-mode
      c++-mode
      c++-ts-mode
      python-mode
      python-ts-mode
      sh-mode
      bash-ts-mode
      java-mode
      java-ts-mode)))

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
  (compile-command (format "make -k -j%s " (num-processors)))
  (compilation-always-kill t "Kill a compilation process before starting a new one")
  (compilation-ask-about-save nil "Save all modified buffers without asking")
  ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first error.
  (compilation-scroll-output 'first-error))

(use-package fancy-compilation
  :after compile
  :init (fancy-compilation-mode 1))

(use-package rainbow-delimiters
  :hook ((prog-mode-hook latex-mode-hook LaTeX-mode-hook org-src-mode-hook) . rainbow-delimiters-mode))

;; Tree-sitter provides advanced syntax highlighting features. Run
;; `tree-sitter-langs-install-grammars' to install the grammar files for languages for tree-sitter.
;; Run `tree-sitter-langs-install-grammars' periodically to install new grammars.

;; https://www.reddit.com/r/emacs/comments/10iuim1/getting_emacs_29_to_automatically_use_treesitter/
;; https://github.com/renzmann/treesit-auto
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(use-package treesit
  :straight (:type built-in)
  :when (executable-find "tree-sitter")
  :demand t
  :bind (("C-M-a" . treesit-beginning-of-defun) ("C-M-e" . treesit-end-of-defun))
  :custom (treesit-font-lock-level 4 "Increase default font locking")
  :config
  (setq treesit-language-source-alist
    '
    ((bash "https://github.com/tree-sitter/tree-sitter-bash")
      (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
      (c "https://github.com/tree-sitter/tree-sitter-c")
      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
      (cmake "https://github.com/uyha/tree-sitter-cmake")
      (css "https://github.com/tree-sitter/tree-sitter-css")
      (docker "https://github.com/camdencheek/tree-sitter-dockerfile")
      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
      (html "https://github.com/tree-sitter/tree-sitter-html")
      (java "https://github.com/tree-sitter/tree-sitter-java")
      (js "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
      (json "https://github.com/tree-sitter/tree-sitter-json")
      (latex "https://github.com/latex-lsp/tree-sitter-latex")
      (make "https://github.com/alemuller/tree-sitter-make")
      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
      (org "https://github.com/milisims/tree-sitter-org")
      (perl "https://github.com/tree-sitter-perl/tree-sitter-perl")
      (python "https://github.com/tree-sitter/tree-sitter-python")
      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Old language servers do not support tree-sitter yet.

  (add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
  ;; ;; (add-to-list 'major-mode-remap-alist '(bibtex-mode . bibtex-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode))
  ;; (add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  ;; ;; (add-to-list 'major-mode-remap-alist '(js2-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
  ;; ;; (add-to-list 'major-mode-remap-alist '(latex-mode . latex-ts-mode))
  (add-to-list 'major-mode-remap-alist '(makefile-mode . make-ts-mode))
  (add-to-list 'major-mode-remap-alist '(makefile-gmake-mode . make-ts-mode))
  (add-to-list 'major-mode-remap-alist '(markdown-mode . markdown-ts-mode))
  ;; ;; (add-to-list 'major-mode-remap-alist '(org-mode . org-ts-mode))
  ;; ;; (add-to-list 'major-mode-remap-alist '(perl-mode . perl-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  ;; ;; (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
  (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

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
  ;;     yaml-ts-mode-hook yaml-ts-mode-hook)
  )

(use-package tree-sitter
  :when (executable-find "tree-sitter")
  :hook
  ((tree-sitter-after-on-hook . tree-sitter-hl-mode)
    ((c-mode-hook c++-mode-hook) . tree-sitter-mode))
  :init (advice-add 'tsc-dyn-get--log :around #'sb/inhibit-message-call-orig-fun)
  :config
  (use-package tree-sitter-langs
    :demand t
    :init (advice-add 'tree-sitter-langs-install-grammars :around #'sb/inhibit-message-call-orig-fun))
  :diminish tree-sitter-mode)

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
  :mode (("\\.h\\'" . c++-mode) ("\\.c\\'" . c++-mode))
  :hook
  ((c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook)
    .
    (lambda ()
      (setq-local
        c-set-style "cc-mode"
        c-basic-offset 2
        ;; Disable electric indentation and on-type formatting
        c-auto-newline nil
        ;; c-electric-brace nil
        c-electric-flag nil
        ;; c-electric-indent nil
        c-enable-auto-newline nil
        c-syntactic-indentation nil)
      (cond
        ((eq sb/lsp-provider 'eglot)
          (eglot-ensure))
        ((eq sb/lsp-provider 'lsp-mode)
          (lsp-deferred)))))
  :bind (:map c-mode-base-map ("C-c C-d"))
  ;; :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (lsp-register-client
  ;;     (make-lsp-client
  ;;       :new-connection (lsp-tramp-connection "clangd")
  ;;       :major-modes '(c-mode c++-mode)
  ;;       :remote? t
  ;;       :server-id 'clangd-r)))
  )

;; (use-package modern-cpp-font-lock ; Better highlight for modern C++
;;   :hook (c++-mode-hook . modern-c++-font-lock-mode)
;;   :diminish modern-c++-font-lock-mode)

(use-package cuda-mode
  :commands cuda-mode
  :mode (("\\.cu\\'" . c++-mode) ("\\.cuh\\'" . c++-mode)))

(use-package opencl-mode
  :mode "\\.cl\\'")

(use-package cmake-mode
  :if (executable-find "cmake")
  :mode ("\(CMakeLists\.txt|\.cmake\)$" . cmake-ts-mode)
  :hook
  ((cmake-mode-hook cmake-ts-mode-hook)
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

;; (use-package cmake-font-lock ; Advanced syntax coloring support for CMake scripts
;;   :hook (cmake-mode-hook . cmake-font-lock-activate))

;; (use-package rmsbolt
;;   :commands rmsbolt-mode)

(use-package python
  :straight (:type built-in)
  :mode
  (("SCon\(struct\|script\)$" . python-ts-mode)
    ("[./]flake8\\'" . conf-mode)
    ("/Pipfile\\'" . conf-mode))
  :hook
  ((python-mode-hook python-ts-mode-hook)
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
  (python-shell-interpreter "python3"))

(use-package python-docstring
  :after python-mode
  :demand t
  :config (python-docstring-install)
  :diminish)

(use-package pip-requirements
  :commands (pip-requirements-mode))

(use-package pyvenv
  :hook ((python-mode-hook python-ts-mode-hook) . pyvenv-mode)
  :custom
  (pyvenv-mode-line-indicator '(pyvenv-virtual-env-name (" [venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-post-activate-hooks
    (list (lambda () (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (pyvenv-post-deactivate-hooks (list (lambda () (setq python-shell-interpreter "python3")))))

(use-package python-isort
  :straight (:host github :repo "wyuenho/emacs-python-isort")
  :if (and (executable-find "isort") (eq sb/python-langserver 'pyright))
  :hook ((python-mode-hook python-ts-mode-hook) . python-isort-on-save-mode)
  :custom
  (python-isort-arguments
    '
    ("--stdout" "--atomic" "-l 100"
      "--up" ; Use parentheses
      "--tc" ; Use a trailing comma on multiline imports
      "-")))

;; We cannot use `lsp-format-buffer' or `eglot-format-buffer' with `pyright' since it does not
;; support document formatting. So, we have to use yapf with pyright. Yapfify works on the original
;; file, so that any project settings supported by YAPF itself are used.
(use-package yapfify
  :if (and (executable-find "yapf") (eq sb/python-langserver 'pyright))
  :hook ((python-mode-hook python-ts-mode-hook) . yapf-mode)
  :diminish yapf-mode)

(use-package cperl-mode
  :mode "latexmkrc\\'"
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
  :after java-mode
  :commands (ant ant-clean ant-compile ant-test))

;; (use-package autodisass-java-bytecode ; Can disassemble ".class" files from within jars
;;   :mode "\\.class\\'")

(use-package sh-script ; Shell script mode
  :straight (:type built-in)
  :mode ("\\bashrc\\'" . bash-ts-mode)
  :hook
  ((sh-mode-hook bash-ts-mode-hook)
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
  :hook (fish-mode-hook . (lambda () (add-hook 'before-save-hook #'fish_indent-before-save))))

;; Files are given `+x' permissions when they are saved, if they contain a valid shebang line.
(use-package executable
  :hook (after-save-hook . executable-make-buffer-file-executable-if-script-p))

(use-package highlight-doxygen
  :commands (highlight-doxygen-global-mode)
  :hook ((c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook) . highlight-doxygen-mode))

(use-package lisp-mode
  :straight (:type built-in)
  :mode ("\\.dir-locals\\(?:-2\\)?\\.el\\'" . lisp-data-mode)
  :hook
  (lisp-data-mode-hook
    .
    (lambda ()
      (when buffer-file-name
        (add-hook 'after-save-hook #'check-parens nil t)))))

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
  :mode
  (("\\.yml\\'" . yaml-ts-mode)
    ("\\.yaml\\'" . yaml-ts-mode)
    (".clang-format" . yaml-ts-mode)
    (".clang-tidy" . yaml-ts-mode)
    (".clangd" . yaml-ts-mode))
  :hook
  ((yaml-mode-hook yaml-ts-mode-hook)
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
  ((css-mode-hook css-ts-mode-hook)
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
  (("\\Makefile\\'" . make-ts-mode)
    ("\\Makefile.common\\'" . make-ts-mode)
    ;; Add "makefile.rules" to `makefile-gmake-mode' for Intel Pin
    ("makefile\\.rules\\'" . make-ts-mode))
  :hook ((makefile-mode-hook make-ts-mode-hook) . (lambda () (setq-local indent-tabs-mode t))))

(use-package makefile-executor
  :hook ((makefile-mode-hook make-ts-mode-hook) . makefile-executor-mode))

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
  (("\\.md\\'" . markdown-ts-mode)
    ("\\.markdown\\'" . markdown-ts-mode)
    ("README\\.md\\'" . gfm-mode))
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
  :commands pandoc-load-default-settings
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
  :hook ((web-mode-hook css-mode-hook css-ts-mode-hook html-mode-hook html-ts-mode-hook) . emmet-mode)
  :custom (emmet-move-cursor-between-quote t))

(use-package nxml-mode
  :straight (:type built-in)
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
  :commands json-mode-beautify
  :mode
  (("\\.json\\'" . json-ts-mode)
    ("pyrightconfig.json" . jsonc-mode)
    (".*/vscode/settings.json$" . jsonc-mode)
    (".*/\\.vscode/settings.json$" . jsonc-mode)
    ("User/settings.json$" . jsonc-mode))
  :hook
  ((json-mode-hook json-ts-mode-hook jsonc-mode-hook)
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
  :after (:any json-mode jsonc-mode json-ts-mode)
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

;; Links in org-mode by default are displayed as "descriptive" links, meaning they hide their target
;; URLs. While this looks great, it makes it a bit tricky to figure out how you can edit their URL.
;; There are two easy options: (i) press "C-c C-l" (`org-insert-link') while your point is within a
;; link and you will be prompted to edit its URL in the minibuffer. You can use the same command to
;; create new links (when your point is not on an existing link). (ii) You can convert the
;; "descriptive" links to "literal" links by invoking the command "M-x org-toggle-link-display". You
;; can also toggle between the two display modes for links via the mode's menu (under "Hyperlinks").

;; https://orgmode.org/manual/In_002dbuffer-Settings.html
(use-package org
  :defer 2
  :defines
  (org-hide-leading-stars-before-indent-mode
    org-src-strip-leading-and-trailing-blank-lines
    org-src-tabs-acts-natively)
  :commands (org-indent-mode)
  :custom
  (org-fontify-whole-heading-line nil)
  (org-fontify-emphasized-text t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t "Hide *, ~, and / in Org text unless you edit")
  (org-hide-leading-stars nil "Show every star as it helps identify the indentation level")
  (org-hide-leading-stars-before-indent-mode nil)
  ;; Code block fontification using the major-mode of the code
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tabs-acts-natively t "TAB behavior depends on the major mode")
  (org-src-window-setup 'current-window)
  ;; There is a lot of visible distortion with `org-indent-mode' enabled. Emacs performance
  ;; feels better with the mode disabled.
  (org-startup-indented nil "Indentation looks nice, but is sometimes misaligned")
  (org-startup-truncated nil)
  ;; https://orgmode.org/manual/Initial-visibility.html
  (org-startup-folded 'showeverything)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  ;; See `org-speed-commands-default' for a list of the keys and commands enabled at the
  ;; beginning of headlines. `org-babel-describe-bindings' will display a list of the code
  ;; blocks commands and their related keys.
  (org-use-speed-commands t)
  (org-src-strip-leading-and-trailing-blank-lines t)
  ;; Display entities like `\tilde' and `\alpha' in UTF-8 characters
  (org-pretty-entities t)
  ;; Render subscripts and superscripts in org buffers
  (org-pretty-entities-include-sub-superscripts t)
  ;; Automatically sorted and renumbered whenever I insert a new one
  (org-footnote-auto-adjust t)
  (org-return-follows-link t)
  (org-adapt-indentation nil)
  (org-odd-levels-only t "Use odd levels to add more indentation")
  (org-export-with-smart-quotes t "#+OPTIONS ':t")
  (org-export-with-section-numbers nil "#+OPTIONS num:nil")
  ;; #+OPTIONS toc:nil, use "#+TOC: headlines 2" or similar if you need a headline
  (org-export-with-toc nil)
  (org-export-with-sub-superscripts nil "#+OPTIONS ^:{}")
  ;; This exports broken links as [BROKEN LINK %s], so we can actually find them. The default value
  ;; nil just aborts the export process with an error message "Unable to resolve link: nil". This
  ;; doesn't give any hint on which line the broken link actually is.
  (org-export-with-broken-links 'mark)
  (org-indent-indentation-per-level 1)
  (org-latex-listings 'minted "Syntax coloring is more extensive than listings")
  :config
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-pdf-process
    '
    ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (with-eval-after-load "org-indent"
    (diminish 'org-indent-mode))
  :bind-keymap ("C-c o" . org-mode-map)
  :bind
  (:map
    org-mode-map
    ("M-<left>")
    ("M-<right>")
    ("M-<up>")
    ("M-<down>")
    ("C-'")
    ("C-c C-d") ; Was bound to `org-deadline', I prefer to use it for `duplicate-thing'
    ;; Was bound to `org-goto', I prefer to use it for `imenu' and its variants
    ("C-c C-j")
    ;; Was bound to `org-forward-paragraph', I prefer to use it for `forward-sentence'
    ("M-e")
    ("<tab>" . org-indent-item)
    ("<backtab>" . org-outdent-item)
    ("M-{" . org-backward-element)
    ("M-}" . org-forward-element)
    ("C-c C-," . org-insert-structure-template)))

;; Disable the package to get consistent styles across themes.

;; (use-package org-bullets
;;   :disabled t
;;   :hook (org-mode-hook . org-bullets-mode))

(use-package org-appear ; Make invisible parts of Org elements appear visible
  :straight (:host github :repo "awth13/org-appear")
  :hook (org-mode-hook . org-appear-mode)
  :custom
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autolinks t)
  (org-appear-autoemphasis t)
  (org-appear-autokeywords t))

(use-package ox-gfm
  :after org
  :commands (org-gfm-export-as-markdown org-gfm-export-to-markdown))

(use-package ox-pandoc
  :after org
  :commands
  (org-pandoc-export-to-markdown
    org-pandoc-export-as-markdown
    org-pandoc-export-to-markdown-and-open))

;; (use-package org-modern
;;   :disabled t
;;   :hook (org-mode-hook . org-modern-mode))

;; (use-package org-modern-indent
;;   :straight (:host github :repo "jdtsmith/org-modern-indent")
;;   :disabled t
;;   :hook (org-mode-hook . org-modern-indent-mode))

;; Use zero-width space "C-x 8 zero width space" to treat Org markup as plain text.
;; https://orgmode.org/manual/Escape-Character.html
;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-unicode.el

;; (use-package org-superstar
;;   :disabled t
;;   :hook (org-mode-hook . org-superstar-mode))

;; (use-package org-block-capf
;;   :straight (:host github :repo "xenodium/org-block-capf")
;;   :hook (org-mode-hook . org-block-capf-add-to-completion-at-point-functions)
;;   :custom (org-block-capf-edit-style 'inline))

(defvar sb/minibuffer-completion)
(defvar sb/user-tmp-directory)

;; Auctex provides enhanced versions of `tex-mode' and `latex-mode', which automatically replace the
;; vanilla ones. Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides
;; the tex package.

(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :defines
  (tex-fontify-script
    font-latex-fontify-script
    font-latex-fontify-sectioning
    TeX-syntactic-comment
    TeX-save-query
    LaTeX-item-indent
    LaTeX-syntactic-comments
    LaTeX-fill-break-at-separators)
  :functions (TeX-active-process)
  :hook
  (((latex-mode-hook LaTeX-mode-hook) . LaTeX-math-mode)
    ((latex-mode-hook LaTeX-mode-hook) . TeX-PDF-mode) ; Use `pdflatex'
    ;; Revert PDF buffer after TeX compilation has finished
    (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
    ;; Enable rainbow mode after applying styles to the buffer
    (TeX-update-style-hook . rainbow-delimiters-mode)
    ;; Jump between editor and pdf viewer
    ((latex-mode-hook LaTeX-mode-hook) . TeX-source-correlate-mode)
    ((latex-mode-hook LaTeX-mode-hook) . turn-on-auto-fill)
    ((latex-mode-hook LaTeX-mode-hook)
      .
      (lambda ()
        (cond
          ((eq sb/lsp-provider 'eglot)
            (eglot-ensure))
          ((eq sb/lsp-provider 'lsp-mode)
            (lsp-deferred))))))
  :bind
  (:map
    TeX-mode-map
    ("C-c ;")
    ("C-c C-d")
    ("C-c C-c" . TeX-command-master)
    ("$" . self-insert-command)
    ("C-c x q" . TeX-insert-quote))
  :custom
  (TeX-auto-save t "Enable parse on save, stores parsed information in an `auto' directory")
  (TeX-auto-untabify t "Remove all tabs before saving")
  (TeX-clean-confirm nil)
  ;; Automatically insert braces after typing ^ and _ in math mode
  (TeX-electric-sub-and-superscript t)
  (TeX-electric-math t "Inserting $ completes the math mode and positions the cursor")
  (TeX-parse-self t "Parse documents")
  (TeX-quote-after-quote nil "Allow original LaTeX quotes")
  (TeX-save-query nil "Save buffers automatically when compiling")
  (TeX-source-correlate-method 'synctex)
  ;; Do not start the Emacs server when correlating sources
  (TeX-source-correlate-start-server t)
  (TeX-syntactic-comment t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (LaTeX-item-indent 0 "Indent lists by two spaces")
  (LaTeX-syntactic-comments t)
  (LaTeX-fill-break-at-separators nil "Do not insert line-break at inline math")
  (tex-fontify-script nil "Avoid raising of superscripts and lowering of subscripts")
  ;; Avoid superscripts and subscripts from being displayed in a different font size
  (font-latex-fontify-script nil)
  (font-latex-fontify-sectioning 1.0 "Avoid emphasizing section headers")
  :config
  (when (executable-find "okular")
    (setq
      TeX-view-program-list
      '(("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
      TeX-view-program-selection '((output-pdf "Okular"))))

  ;; Always query for the master file
  (setq-default TeX-master nil)
  (with-eval-after-load "auctex"
    (bind-key "C-c C-e" LaTeX-environment LaTeX-mode-map)
    (bind-key "C-c C-s" LaTeX-section LaTeX-mode-map)
    (bind-key "C-c C-m" TeX-insert-macro LaTeX-mode-map)))

(use-package bibtex
  :straight (:type built-in)
  :hook
  ((bibtex-mode-hook . turn-on-auto-revert-mode)
    (bibtex-mode-hook
      .
      (lambda ()
        (cond
          ((eq sb/lsp-provider 'eglot)
            (eglot-ensure))
          ((eq sb/lsp-provider 'lsp-mode)
            (lsp-deferred))))))
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-maintain-sorted-entries t)
  (bibtex-comma-after-last-field nil))

;; Reftex is useful to view ToC even with LSP support
;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493

(use-package reftex
  :preface
  (defun sb/get-bibtex-keys (file)
    (with-current-buffer (find-file-noselect file)
      (mapcar 'car (bibtex-parse-keys))))

  (defun sb/reftex-add-all-bibitems-from-bibtex ()
    (interactive)
    (mapc
      'LaTeX-add-bibitems
      (apply 'append (mapcar 'sb/get-bibtex-keys (reftex-get-bibfile-list)))))

  (defun sb/find-bibliography-file ()
    "Try to find a bibliography file using RefTeX.
      Returns a string with text properties (as expected by read-file-name) or
empty string if no file can be found"
    (interactive)
    (let ((bibfile-list nil))
      (condition-case nil
        (setq bibfile-list (reftex-get-bibfile-list))
        (error
          (ignore-errors
            (setq bibfile-list (reftex-default-bibliography)))))
      (if bibfile-list
        (car bibfile-list)
        "")))

  (defun sb/reftex-try-add-all-bibitems-from-bibtex ()
    "Try to find a bibliography file using RefTex and parse the bib keys.
Ignore if no file is found."
    (interactive)
    (let ((bibfile-list nil))
      (condition-case nil
        (setq bibfile-list (reftex-get-bibfile-list))
        (error
          (ignore-errors
            (setq bibfile-list (reftex-default-bibliography)))))
      ;; (message "%s" bibfile-list)
      (mapc 'LaTeX-add-bibitems (apply 'append (mapcar 'sb/get-bibtex-keys bibfile-list)))))
  :straight (:type built-in)
  :commands
  (reftex-get-bibfile-list
    bibtex-parse-keys
    reftex-mode
    reftex-toc-rescan
    reftex-toc-Rescan
    reftex-default-bibliography)
  :hook ((LaTeX-mode-hook latex-mode-hook) . turn-on-reftex)
  :bind
  (("C-c [" . reftex-citation)
    ("C-c )" . reftex-reference)
    ("C-c (" . reftex-label)
    ("C-c =" . reftex-toc)
    ("C-c -" . reftex-toc-recenter)
    ("C-c &" . reftex-view-crossref))
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-enable-partial-scans t)
  (reftex-highlight-selection 'both)
  (reftex-save-parse-info t "Save parse info to avoid reparsing every time a file is visited")
  (reftex-revisit-to-follow t)
  (reftex-auto-recenter-toc t "Center on the section currently being edited")
  (reftex-toc-follow-mode t "Other buffer follows the point in TOC buffer")
  (reftex-toc-split-windows-fraction 0.6 "Give TOC buffer more room")
  (reftex-toc-split-windows-horizontally t) ; Show reftex TOC on the left
  (reftex-ref-macro-prompt nil) ; No unnecessary prompts
  ;; (reftex-guess-label-type t "Try to guess the label type before prompting")
  (reftex-use-fonts t "Use nice fonts for TOC")
  ;; (reftex-revisit-to-follow t "Revisit files if necessary when browsing toc")
  (reftex-use-multiple-selection-buffers t "Cache selection buffers for faster access")
  ;; Throw away buffers created for parsing, but keep the ones created for lookup
  (reftex-keep-temporary-buffers 1)
  (reftex-trust-label-prefix '("fn:" "eq:" "sec:" "fig:" "tab:"))
  (reftex-allow-automatic-rescan nil)
  (reftex-enable-partial-scans t)
  :config
  ;; (sb/reftex-try-add-all-bibitems-from-bibtex)
  ;; (add-hook 'reftex-load-hook #'sb/reftex-add-all-bibitems-from-bibtex)

  (with-eval-after-load "reftex-toc"
    (bind-keys
      :package reftex-toc
      :map
      reftex-toc-mode-map
      ("n" . reftex-toc-next)
      ("p" . reftex-toc-previous)
      ("r" . reftex-toc-rescan)
      ("R" . reftex-toc-Rescan)
      ("g" . revert-buffer)
      ("q" . reftex-toc-quit)
      ("z" . reftex-toc-jump)
      (">" . reftex-toc-demote)
      ("<" . reftex-toc-promote))

    ;; Rescan the entire document, not only the current file (`reftex-toc-rescan'), to be consistent
    ;; but this is expensive.
    (add-hook 'reftex-toc-mode-hook #'reftex-toc-rescan))
  :diminish)

;; Read document like a hypertext document, supports mouse highlighting

;; (use-package bib-cite
;;   :straight auctex
;;   :hook
;;   ((LaTeX-mode-hook latex-mode-hook) . (lambda()
;;                                          (bib-cite-minor-mode 1)))
;;   ;; :bind
;;   ;; (:map bib-cite-minor-mode-map
;;   ;;       ("C-c b") ; We use `C-c b' for `comment-box'
;;   ;;       ("C-c l a" . bib-apropos)
;;   ;;       ("C-c l b" . bib-make-bibliography)
;;   ;;       ("C-c l d" . bib-display)
;;   ;;       ("C-c l t" . bib-etags)
;;   ;;       ("C-c l f" . bib-find)
;;   ;;       ("C-c l n" . bib-find-next))
;;   :custom
;;   (bib-cite-use-reftex-view-crossref t "Use RefTeX functions for finding bibliography files")
;;   :diminish bib-cite-minor-mode)

(use-package auctex-latexmk
  :after tex-mode
  :when (executable-find "latexmk")
  :demand t
  :custom (auctex-latexmk-inherit-TeX-PDF-mode t "Pass the '-pdf' flag when `TeX-PDF-mode' is active")
  :config
  (setq-default TeX-command-default "LatexMk")
  (auctex-latexmk-setup))

(with-eval-after-load "latex"
  (defvar LaTeX-mode-map)

  ;; Disable `LaTeX-insert-item' in favor of `imenu'
  (unbind-key "C-c C-j" LaTeX-mode-map)

  (bind-key "C-c x q" #'TeX-insert-quote LaTeX-mode-map))

;; `math-preview' requires external nodejs program "math-preview". Make sure that "math-preview" is
;; in "$PATH".

;; (use-package math-preview
;;   :straight (:host gitlab :repo "matsievskiysv/math-preview")
;;   :commands (math-preview-all math-preview-at-point math-preview-region)
;;   :custom (math-preview-command (expand-file-name "node_modules/.bin/math-preview" sb/user-tmp-directory)))

;; TODO: Try pcakages like `bibtex-capf' and `citar'
;; https://github.com/emacs-citar/citar

(use-package bibtex-capf
  :straight (:type git :host github :repo "mclear-tools/bibtex-capf")
  :when (eq sb/capf 'corfu)
  :hook ((org-mode markdown-mode tex-mode latex-mode reftex-mode) . bibtex-capf-mode))

(setq
  large-file-warning-threshold (* 500 1024 1024) ; MB
  tags-add-tables nil
  tags-case-fold-search nil ; "t"=case-insensitive, "nil"=case-sensitive
  ;; Do not ask before rereading the "TAGS" files if they have changed
  tags-revert-without-query t)

;; In Emacs Lisp mode, `xref-find-definitions' will by default find only functions and variables
;; from Lisp packages which are loaded into the current Emacs session or are auto-loaded.
(use-package xref
  :bind
  (("M-." . xref-find-definitions)
    ("M-?" . xref-find-references)
    ("C-M-." . xref-find-apropos) ; Find all identifiers whose name matches pattern
    ("M-," . xref-go-back)
    :map
    xref--xref-buffer-mode-map
    ("C-o" . xref-show-location-at-point)
    ("<tab>" . xref-quit-and-goto-xref)
    ("r" . xref-query-replace-in-results))
  :custom
  (xref-search-program 'ripgrep)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package dumb-jump
  :after xref
  :commands dumb-jump-xref-activate
  :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-prefer-searcher 'rg))

;; https://github.com/universal-ctags/citre/wiki/Use-Citre-together-with-lsp-mode

(use-package citre
  :preface
  (defun sb/citre-jump+ ()
    "Jump to the definition of the symbol at point using `citre-jump' first. Falls back to `xref-find-definitions' on failure."
    (interactive)
    (condition-case _
      (citre-jump)
      (error
        (let* ((xref-prompt-for-identifier nil))
          (call-interactively #'xref-find-definitions)))))

  (defun sb/citre-jump-back+ ()
    "Go back to the position before last `citre-jump'.
Fallback to `xref-go-back'."
    (interactive)
    (condition-case _
      (citre-jump-back)
      (error
        (if (fboundp #'xref-go-back)
          (call-interactively #'xref-go-back)
          (call-interactively #'xref-pop-marker-stack)))))

  (defun sb/push-point-to-xref-marker-stack (&rest r)
    (xref-push-marker-stack (point-marker)))

  (defun sb/lsp-citre-capf-function ()
    "A capf backend that tries lsp first, then Citre."
    (let
      (
        (lsp-result
          (cond
            ((bound-and-true-p lsp-mode)
              (and (fboundp #'lsp-completion-at-point) (lsp-completion-at-point)))
            ((bound-and-true-p eglot--managed-mode)
              (and (fboundp #'eglot-completion-at-point) (eglot-completion-at-point))))))
      (if
        (and lsp-result
          (try-completion
            (buffer-substring (nth 0 lsp-result) (nth 1 lsp-result))
            (nth 2 lsp-result)))
        lsp-result
        (citre-completion-at-point))))

  (defun sb/enable-lsp-citre-capf-backend ()
    "Enable the lsp + Citre capf backend in current buffer."
    (add-hook 'completion-at-point-functions #'sb/lsp-citre-capf-function nil t))
  :commands (citre-create-tags-file citre-update-tags-file)
  :hook
  ;; Using "(require citre-config)" will enable `citre-mode' for all files as long as it finds a
  ;; tags backend, which is not desired for plain text files.
  (prog-mode-hook . citre-mode)
  :bind
  (("C-x c j" . citre-jump)
    ("M-'" . sb/citre-jump+)
    ("C-x c b" . sb/citre-jump-back+)
    ("C-x c p" . citre-peek)
    ("C-x c c" . citre-create-tags-file)
    ("C-x c u" . citre-update-this-tags-file)
    ("C-x c e" . citre-edit-tags-file-recipe))
  :custom
  (citre-use-project-root-when-creating-tags t)
  (citre-default-create-tags-file-location 'project-cache)
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-enable-capf-integration nil)
  ;; Enabling this breaks imenu for Elisp files, it cannot identify `use-package' definitions
  (citre-enable-imenu-integration nil)
  (citre-enable-xref-integration t)
  (citre-edit-cmd-buf-default-cmd
    "ctags
-o
%TAGSFILE%
;; Edit the relevant programming languages to keep the tags file size reasonable
--languages=BibTeX,C,C++,CUDA,CMake,EmacsLisp,Java,Make,Python,Sh,TeX
--kinds-all=*
--fields=*
--extras=*
-R
;; -e
--exclude=@./.ctagsignore
;; add exclude by: --exclude=target
;; add dirs/files to scan here, one line per dir/file")
  :config
  ;; (add-hook 'citre-mode-hook #'sb/enable-lsp-citre-capf-backend)

  (dolist
    (func
      '(find-function counsel-imenu projectile-grep counsel-rg lsp-ivy-workspace-symbol citre-jump))
    (advice-add func :before 'sb/push-point-to-xref-marker-stack))

  ;; Try lsp first, then use Citre
  (with-no-warnings
    (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
      (let
        (
          (fetcher (apply -fn -args))
          (citre-fetcher
            (let ((xref-backend-functions '(citre-xref-backend t)))
              (apply -fn -args))))
        (lambda ()
          (or
            (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher))))))

  (with-eval-after-load "company"
    (defmacro citre-backend-to-company-backend (backend)
      "Create a company backend from Citre completion backend BACKEND.
The result is a company backend called
`company-citre-<backend>' (like `company-citre-tags') and can be
used in `company-backends'."
      (let
        (
          (backend-name (intern (concat "company-citre-" (symbol-name backend))))
          (docstring
            (concat
              "`company-mode' backend from the `"
              (symbol-name backend)
              "' Citre backend.\n"
              "`citre-mode' needs to be enabled to use this.")))
        `
        (defun ,backend-name (command &optional arg &rest ignored)
          ,docstring
          (pcase command
            ('interactive (company-begin-backend ',backend-name))
            ('prefix
              (and (bound-and-true-p citre-mode)
                (citre-backend-usable-p ',backend)
                ;; We shouldn't use this as it's defined for getting definitions/references. But the
                ;; Citre completion backend design is not fully compliant with company's design so
                ;; there's no simple "right" solution, and this works for tags/global backends.
                (or (citre-get-symbol-at-point-for-backend ',backend) 'stop)))
            ('meta (citre-get-property 'signature arg))
            ('annotation (citre-get-property 'annotation arg))
            ('candidates
              (let ((citre-completion-backends '(,backend)))
                (all-completions arg (nth 2 (citre-completion-at-point)))))))))

    (citre-backend-to-company-backend tags))
  :diminish)

(provide 'init-languages)

;;; init-languages.el ends here
