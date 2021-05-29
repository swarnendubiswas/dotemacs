;;; core-packages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/core-packages)

;; Check number of installed packages with `(length package-alist)'
(setq sb/core-packages '(ace-window
                         ;; adoc-mode
                         aggressive-indent
                         all-the-icons
                         all-the-icons-dired
                         all-the-icons-ibuffer
                         all-the-icons-ivy
                         all-the-icons-ivy-rich
                         amx
                         ant
                         anzu
                         apt-sources-list
                         async
                         auctex
                         auctex-latexmk
                         auto-dim-other-buffers
                         ;; autodisass-java-bytecode
                         ;; autodisass-llvm-bitcode
                         avy
                         ;; bazel-mode
                         ;; beacon
                         beginend
                         bibtex-completion
                         bibtex-utils
                         bind-key
                         bison-mode
                         bm
                         ;; boogie-friends
                         ;; bug-hunter
                         ;; clang-format
                         ;; clang-format+
                         cmake-font-lock
                         cmake-mode
                         company
                         company-auctex
                         ;; company-bibtex
                         company-math
                         company-posframe
                         company-quickhelp
                         company-reftex
                         company-shell
                         counsel
                         counsel-fd
                         crux
                         csv-mode
                         ;; ctrlf
                         cuda-mode
                         ;; cython-mode
                         dap-mode
                         deadgrep
                         default-text-scale
                         define-word
                         diff-hl
                         diminish
                         dired+
                         dired-efap
                         dired-narrow
                         disable-mouse
                         discover-my-major
                         doom-modeline
                         doom-themes
                         dotenv-mode
                         dumb-jump
                         duplicate-thing
                         ;; editorconfig
                         ;; ein
                         emmet-mode
                         ;; ess
                         ;; ess-smart-underscore
                         esup
                         exec-path-from-shell
                         expand-region
                         fasd
                         fish-mode
                         flycheck
                         ;; flycheck-clang-analyzer
                         ;; flycheck-clang-tidy
                         flycheck-grammarly
                         flyspell-correct
                         flyspell-correct-ivy
                         flyspell-popup
                         format-all
                         free-keys
                         gcmh
                         git-commit
                         git-gutter
                         gitattributes-mode
                         gitconfig-mode
                         gitignore-mode
                         ;; gnuplot
                         goto-last-change
                         grammarly
                         ;; graphviz-dot-mode
                         grip-mode
                         groovy-mode
                         helpful
                         ;; hide-mode-line
                         highlight-doxygen
                         highlight-escape-sequences
                         highlight-indentation
                         highlight-numbers
                         hl-todo
                         hungry-delete
                         hydra
                         ibuffer-projectile
                         iedit
                         immortal-scratch
                         ini-mode
                         isearch-dabbrev
                         isearch-symbol-at-point
                         ivy
                         ivy-avy
                         ivy-bibtex
                         ivy-hydra
                         ivy-rich
                         ivy-xref
                         ivy-yasnippet
                         ;; jgraph-mode
                         ;; jinja2-mode
                         ;; js2-mode
                         ;; js2-refactor
                         json-reformat
                         json-snatcher
                         key-chord
                         langtool
                         logview
                         lsp-ivy
                         lsp-java
                         lsp-mode
                         lsp-pyright
                         ;; lsp-treemacs
                         lsp-ui
                         magit
                         manage-minor-mode
                         markdown-mode
                         markdown-preview-mode
                         markdown-toc
                         math-preview
                         math-symbols
                         ;; Required by `ac-math' and `company-math'
                         math-symbol-lists
                         ;; matlab-mode
                         modern-cpp-font-lock
                         modus-themes
                         moody
                         move-text
                         multiple-cursors
                         ;; nix-mode
                         no-littering
                         nvm
                         opencl-mode
                         orderless
                         org-bullets
                         pandoc-mode
                         paradox
                         pdf-tools
                         persistent-scratch
                         ;; php-mode
                         ;; pip-requirements
                         ;; pkg-info
                         ;; pkgbuild-mode
                         popup
                         ;; popwin
                         pos-tip
                         posframe
                         powerline
                         prescient
                         projectile
                         ;; protobuf-mode
                         py-isort
                         pylint
                         python-docstring
                         pyvenv
                         rainbow-delimiters
                         rainbow-mode
                         reformatter
                         request
                         rich-minority
                         ripgrep
                         ;; rust-mode
                         ;; sass-mode
                         saveplace-pdf-view
                         ;; scss-mode
                         shfmt
                         smart-mark
                         spell-fu
                         ssh-config-mode
                         ;; super-save
                         swiper
                         symbol-overlay
                         ;; toml-mode
                         ;; treemacs
                         ;; treemacs-all-the-icons
                         ;; treemacs-magit
                         ;; treemacs-projectile
                         undo-tree
                         visual-regexp
                         vlf
                         wc-mode
                         web-mode
                         wgrep
                         which-key
                         which-key-posframe
                         whitespace-cleanup-mode
                         whole-line-or-region
                         writegood-mode
                         ws-butler
                         xref
                         ;; xref-js2
                         ;; xref-rst
                         yaml-imenu
                         yaml-mode
                         yapfify
                         yasnippet
                         yasnippet-snippets
                         ;; z3-mode
                         ))

(defun sb/install-packages ()
  "Install the listed packages."
  (interactive)
  (when (cl-find-if-not #'package-installed-p sb/core-packages)
    (package-refresh-contents)
    (mapc #'package-install sb/core-packages)))

(provide 'core-packages)
;;; core-packages.el ends here
