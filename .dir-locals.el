;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html

((nil
  .
  ((fill-column . 80)

   (make-local-variable 'completion-ignored-extensions)
   ;; Directories
   (eval . (add-to-list 'completion-ignored-extensions "eln-cache/"))
   (eval . (add-to-list 'completion-ignored-extensions "tree-sitter/"))
   (eval .
         (add-to-list
          'completion-ignored-extensions "eglot-java-eclipse-jdt-cache/"))
   (eval . (add-to-list 'completion-ignored-extensions "share/"))
   (eval . (add-to-list 'completion-ignored-extensions "auto-save-list/"))
   (eval . (add-to-list 'completion-ignored-extensions "\\.cache/"))
   (eval . (add-to-list 'completion-ignored-extensions "auto/"))

   ;; Files
   (eval . (add-to-list 'completion-ignored-extensions "\\.tags/"))
   (eval . (add-to-list 'completion-ignored-extensions ".pptx"))
   (eval . (add-to-list 'completion-ignored-extensions ".xml"))
   (eval . (add-to-list 'completion-ignored-extensions ".drawio"))

   (compile-command . "cmake -S . -B build; cmake --build build; ")

   (eval
    .
    (add-hook
     'lsp-managed-mode-hook
     (lambda ()
       ;; (add-to-list lsp-file-watch-ignored-directories "/build")

       ;; Update the chain of checkers based on requirement
       (when (derived-mode-p 'markdown-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (markdown-markdownlint-cli)))))))

       (when (derived-mode-p 'yaml-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (yaml-yamllint)))))))

       (when (derived-mode-p 'json-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (json-jsonlint)))))))

       ;; Basedpyright checks types, so we ignore mypy.
       (when (and (eq sb/python-langserver 'basedpyright)
                  (or (derived-mode-p 'python-mode)
                      (derived-mode-p 'python-ts-mode)))
         (setq sb/flycheck-local-checkers
               '((lsp
                  .
                  ((next-checkers
                    .
                    (python-pylint . (python-ruff . (hl-todo)))))))))

       ;; Other linters (pylint, mypy, and ruff) are included as plugins in
       ;; `pylsp' and are configured through the `lsp-mode' package.
       (when (and (eq sb/python-langserver 'pylsp)
                  (or (derived-mode-p 'python-mode)
                      (derived-mode-p 'python-ts-mode)))
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (hl-todo)))))))

       (when (derived-mode-p 'c++-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (c/c++-cppcheck)))))))

       ;; (when (derived-mode-p 'latex-mode)
       ;;   (setq sb/flycheck-local-checkers
       ;;         '((lsp
       ;;            . ((next-checkers . (tex-textidote . (tex-chktex))))))))

       (when (derived-mode-p 'html-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (html-tidy)))))))

       (when (derived-mode-p 'xml-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (xml-xmllint))))))))))

   ;; Associate a mode with a file
   (eval .
         (when (and
                (buffer-file-name) ; Ensure the buffer is visiting a file
                (string=
                 (file-name-nondirectory (buffer-name)) "setup_environment"))
           (sh-mode)))))

 (dired-mode
  .
  ((dired-omit-mode . t)
   (dired-omit-extensions
    . (".fasl" ".bbl" ".toc" ".fdb_latexmk" ".aux" ".fls" ".out" ".o" ".exe"))
   ;;   (dired-omit-files
   ;;    .
   ;;    "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.git\\'|\\.cache\\'|eln-cache|eglot-java-eclipse-jdt-cache|elisp-autofmt-cache|tree-sitter|share|elpa|\\.directory\\'"))
   ))

 (emacs-lisp-mode
  .
  ((no-byte-compile . t)
   (elisp-autofmt-on-save-p . always)
   (elisp-autofmt-load-packages-local . ("use-package-core"))
   ;; The special `subdirs' element is not a variable, but a special keyword
   ;; which indicates that the mode settings are only to be applied in the
   ;; current directory, not in any subdirectories.
   (subdirs . nil)))

 (lisp-data-mode
  .
  ((elisp-autofmt-on-save-p . always)
   (elisp-autofmt-load-packages-local . ("use-package-core"))
   ;; The special `subdirs' element is not a variable, but a special keyword
   ;; which indicates that the mode settings are only to be applied in the
   ;; current directory, not in any subdirectories.
   (subdirs . nil)))

 (c-mode . ((mode . c++)))

 ;; Some systems may not have treesitter libraries installed
 (c++-mode
  .
  ((clang-format-style . "file")
   (flycheck-gcc-language-standard . "c++17")
   (flycheck-clang-language-standard . "c++17")
   (flycheck-clang-tidy-build-path . ".")
   (flycheck-gcc-openmp . t)

   ;; (load-file "./util/emacs/m5-c-style.el")
   ;; (eval ignore-errors (require 'm5-c-style))
   ;; (c-default-style . "m5")
   ;; (c-set-style . "m5")

   ;; (eval add-hook 'hack-local-variables-hook (lambda () (when (string=
   ;; major-mode 'c++-mode) (lsp))))

   (eval .
         (let ((clang-args (list "-std=c++17" "-I./src"))
               (include-path (list "./src")))
           (setq-local
            company-clang-arguments clang-args
            flycheck-clang-args clang-args
            flycheck-clang-include-path include-path
            flycheck-gcc-args clang-args
            flycheck-gcc-include-path include-path
            flycheck-cppcheck-include-path include-path
            flycheck-cuda-include-path include-path)))

   ;; Alternatively, define clangd-args and set lsp-clients-clangd-args.
   (eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)
            (let ((compdir (file-name-directory buffer-file-name)))
              (add-to-list
               'lsp-clients-clangd-args (concat "--compile-commands-dir=./.")
               'append)))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (c++-ts-mode
  .
  ((clang-format-style . "file")
   (flycheck-gcc-language-standard . "c++17")
   (flycheck-clang-language-standard . "c++17")
   (flycheck-clang-tidy-build-path . ".")
   (flycheck-gcc-openmp . t)

   ;; (load-file "./util/emacs/m5-c-style.el")
   ;; (eval ignore-errors (require 'm5-c-style))
   ;; (c-default-style . "m5")
   ;; (c-set-style . "m5")

   (eval .
         (let ((clang-args (list "-std=c++17" "-I./src"))
               (include-path (list "./src")))
           (setq-local
            company-clang-arguments clang-args
            flycheck-clang-args clang-args
            flycheck-clang-include-path include-path
            flycheck-gcc-args clang-args
            flycheck-gcc-include-path include-path
            flycheck-cppcheck-include-path include-path
            flycheck-cuda-include-path include-path)))

   (eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)
            (let ((compdir (file-name-directory buffer-file-name)))
              (add-to-list
               'lsp-clients-clangd-args (concat "--compile-commands-dir=./.")
               'append)))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (python-mode
  .
  ((python-shell-exec-path . "python3")
   (python-shell-interpreter . "python3") (flycheck-pylintrc . "setup.cfg")
   ;; (pyvenv-workon . "./")
   ;; (pyvenv-activate . "./venv/")

   ;; (eval .
   ;;       (setenv "PYTHONPATH"
   ;;               (concat "src/python:ext/ply:src/sim:" (getenv "PYTHONPATH"))))

   ;; We can define lsp-pyright-extra-paths and lsp-pyright-venv-path
   ;; (eval .
   ;;       (let ((paths (vconcat (list "./src/python" "ext/ply" "src/sim"))))
   ;;         (setq lsp-pyright-extra-paths paths)))

   (eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)
            ;; (add-hook 'before-save-hook #'lsp-organize-imports nil t)
            )))

   ;; https://gist.github.com/doolio/8c1768ebf33c483e6d26e5205896217f
   ;; https://paste.sr.ht/~meow_king/df83c4dd8541e54befe511ddaf0eeee7cb59eaba
   ;;  https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md
   (eglot-workspace-configuration
    .
    (:pylsp
     (:configurationSources
      ["pyproject.toml" "setup.cfg"]
      :plugins
      (:autopep8
       (:enabled :json-false)
       :black (:cache_config t :enabled :json-false :line_length 80)
       :flake8
       (:config
        t
        :enabled
        :json-false
        :exclude []
        :executable "flake8"
        :extendIgnore []
        :filename nil ; string: null (default)
        :hangClosing nil
        :ignore []
        :indentSize nil
        :maxComplexity nil
        :maxLineLength 80
        :perFileIgnores [] ; e.g. ["file_path.py:W305,W304"]
        :select nil)
       :jedi
       (:auto_import_modules
        []
        :env_vars nil ; (:SOME_ENV_VAR "/some/path")
        :environment nil ; "./.venv/"
        :extra_paths [])
       :jedi_completion
       (:cache_for
        []
        :eager
        :json-false
        :enabled t
        :fuzzy t
        :include_class_objects
        :json-false
        :include_function_objects
        :json-false
        :include_params t
        :resolve_at_most 25)
       :jedi_definition
       (:enabled
        t
        :follow_builtin_definitions t
        :follow_builtin_imports t
        :follow_imports t)
       :jedi_hover (:enabled t)
       :jedi_references (:enabled t)
       :jedi_signature_help (:enabled t)
       :jedi_symbols (:enabled t :all_scopes t :include_import_symbols t)
       :mccabe (:enabled :json-false :threshold 15)
       :mypy (:enabled :json-false)
       :preload (:enabled :json-false :modules [])
       :pycodestyle
       (:enabled
        :json-false
        :exclude []
        :filename []
        :hangClosing nil
        :ignore []
        :indentSize nil
        :maxLineLength 80
        :select nil)
       :pydocstyle
       (:addIgnore
        []
        :addSelect []
        :convention "numpy"
        :enabled
        :json-false
        :ignore []
        :match "(?!test_).*\\.py"
        :matchDir "[^\\.].*"
        :select nil)
       :pyflakes (:enabled :json-false)
       :pylint (:args [] :enabled t)
       :pylsp_black (:cache_config t :enabled :json-false :line_length 80)
       :pylsp_isort (:enabled t)
       :pylsp_mypy (:enabled t :live_mode :json-false :report_progress t)
       :pylsp_ruff (:enabled t :formatEnabled :json-false :lineLength 80)
       :rope_autoimport
       (:code_actions
        (:enabled :json-false)
        :completions (:enabled :json-false)
        :enabled
        :json-false
        :memory
        :json-false)
       :rope_completion (:eager :json-false :enabled :json-false)
       :ruff (:enabled :json-false :formatEnabled :json-false :lineLength 80)
       :yapf
       (:enabled
        t
        :based_on_style "pep8"
        :column_limit 80
        :indent_width 4
        :split_before_logical_operator t
        :use_tabs
        :json-false)
       :rope (:extensionModules nil :ropeFolder nil))
      ;; A pyrightconfig.json or an entry in pyproject.toml gets priority over
      ;; LSP configuration for basedpyright.
      :basedpyright
      (:checkOnlyOpenFiles
       t
       :reportDuplicateImport t
       :typeCheckingMode "recommended"
       :useLibraryCodeForTypes t)
      :basedpyright.analysis
      (:diagnosticSeverityOverrides
       (:reportUnusedCallResult "none" :reportInvalidCast :json-false)
       :inlayHints
       (:callArgumentNames
        :json-false
        :functionReturnTypes
        :json-false
        :variableTypes
        :json-false
        :genericTypes
        :json-false)))))

   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)
            ;; (add-hook 'before-save-hook #'eglot-code-action-organize-imports
            ;;           nil
            ;;           t)
            )))))

 (python-ts-mode
  .
  ((python-shell-exec-path . "python3")
   (python-shell-interpreter . "python3") (flycheck-pylintrc . "setup.cfg")
   ;; (pyvenv-workon . "./")
   ;; (pyvenv-activate . "./venv/")

   ;; (eval .
   ;;       (setenv "PYTHONPATH"
   ;;               (concat "src/python:ext/ply:src/sim:" (getenv "PYTHONPATH"))))

   ;; We can define lsp-pyright-extra-paths and lsp-pyright-venv-path
   ;; (eval .
   ;;       (let ((paths (vconcat (list "./src/python" "ext/ply" "src/sim"))))
   ;;         (setq lsp-pyright-extra-paths paths)))

   (eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)
            ;; (add-hook 'before-save-hook #'lsp-organize-imports nil t)
            )))

   ;; https://gist.github.com/doolio/8c1768ebf33c483e6d26e5205896217f
   ;; https://paste.sr.ht/~meow_king/df83c4dd8541e54befe511ddaf0eeee7cb59eaba
   ;;  https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md   
   (eglot-workspace-configuration
    .
    (:pylsp
     (:configurationSources
      ["pyproject.toml" "setup.cfg"]
      :plugins
      (:autopep8
       (:enabled :json-false)
       :black (:cache_config t :enabled :json-false :line_length 80)
       :flake8
       (:config
        t
        :enabled
        :json-false
        :exclude []
        :executable "flake8"
        :extendIgnore []
        :filename nil ; string: null (default)
        :hangClosing nil
        :ignore []
        :indentSize nil
        :maxComplexity nil
        :maxLineLength 80
        :perFileIgnores [] ; e.g. ["file_path.py:W305,W304"]
        :select nil)
       :jedi
       (:auto_import_modules
        []
        :env_vars nil ; (:SOME_ENV_VAR "/some/path")
        :environment nil ; "./.venv/"
        :extra_paths [])
       :jedi_completion
       (:cache_for
        []
        :eager
        :json-false
        :enabled t
        :fuzzy t
        :include_class_objects
        :json-false
        :include_function_objects
        :json-false
        :include_params t
        :resolve_at_most 25)
       :jedi_definition
       (:enabled
        t
        :follow_builtin_definitions t
        :follow_builtin_imports t
        :follow_imports t)
       :jedi_hover (:enabled t)
       :jedi_references (:enabled t)
       :jedi_signature_help (:enabled t)
       :jedi_symbols (:all_scopes t :enabled t :include_import_symbols t)
       :mccabe (:enabled :json-false :threshold 15)
       :mypy (:enabled :json-false)
       :preload (:enabled :json-false :modules [])
       :pycodestyle
       (:enabled
        :json-false
        :exclude []
        :filename []
        :hangClosing nil
        :ignore []
        :indentSize nil
        :maxLineLength 80
        :select nil)
       :pydocstyle
       (:addIgnore
        []
        :addSelect []
        :convention "numpy"
        :enabled
        :json-false
        :ignore []
        :match "(?!test_).*\\.py"
        :matchDir "[^\\.].*"
        :select nil)
       :pyflakes (:enabled :json-false)
       :pylint (:args [] :enabled t)
       :pylsp_black (:cache_config t :enabled :json-false :line_length 80)
       :pylsp_isort (:enabled t)
       :pylsp_mypy (:enabled t :live_mode :json-false :report_progress t)
       :pylsp_ruff (:enabled t :formatEnabled :json-false :lineLength 80)
       :rope_autoimport
       (:code_actions
        (:enabled :json-false)
        :completions (:enabled :json-false)
        :enabled
        :json-false
        :memory
        :json-false)
       :rope_completion (:eager :json-false :enabled :json-false)
       :ruff (:enabled :json-false :formatEnabled :json-false :lineLength 80)
       :yapf
       (:enabled
        t
        :based_on_style "pep8"
        :column_limit 80
        :indent_width 4
        :split_before_logical_operator t
        :use_tabs
        :json-false)
       :rope (:extensionModules nil :ropeFolder nil))
      ;; A pyrightconfig.json or an entry in pyproject.toml gets priority over
      ;; LSP configuration for basedpyright.
      :basedpyright
      (:checkOnlyOpenFiles
       t
       :reportDuplicateImport t
       :typeCheckingMode "recommended"
       :useLibraryCodeForTypes t)
      :basedpyright.analysis
      (:diagnosticSeverityOverrides
       (:reportUnusedCallResult "none" :reportInvalidCast :json-false)
       :inlayHints
       (:callArgumentNames
        :json-false
        :functionReturnTypes
        :json-false
        :variableTypes
        :json-false
        :genericTypes
        :json-false)))))

   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)
            ;; (add-hook 'before-save-hook #'eglot-code-action-organize-imports
            ;;           nil
            ;;           t)
            )))))

 (sh-mode . ((subdirs . nil)))

 (bash-ts-mode . ((subdirs . nil)))

 (yaml-mode
  .
  ((eglot-workspace-configuration
    .
    (:yaml
     (:format
      (:enable
       t
       :singleQuote nil
       :bracketSpacing t
       :proseWrap "preserve"
       :printWidth 80)
      :validate t
      :hover t
      :completion t)))
   (eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (yaml-ts-mode
  .
  ((eglot-workspace-configuration
    .
    (:yaml
     (:format
      (:enable
       t
       :singleQuote nil
       :bracketSpacing t
       :proseWrap "preserve"
       :printWidth 80)
      :validate t
      :hover t
      :completion t)))
   (eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (toml-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (toml-ts-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (json-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (json-ts-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (jsonc-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (html-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (html-ts-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (cmake-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (cmake-ts-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

 (LaTeX-mode
  .
  ( ;; (TeX-master . (expand-file-name "./paper.tex"))
   (eval .
         (let ((bibpath (expand-file-name "./references/references.bib"))
               (projectroot (expand-file-name "./")))
           (setq-local
            reftex-default-bibliography bibpath
            company-bibtex-bibliography bibpath
            bibtex-completion-bibliography bibpath
            bibtex-capf-bibliography bibpath
            citar-bibliography bibpath)))

   ;; (eval .
   ;;       (add-hook
   ;;        'lsp-managed-mode-hook
   ;;        (lambda ()
   ;;          (make-local-variable 'before-save-hook)
   ;;          (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

   ;; (eval .
   ;;       (add-hook
   ;;        'eglot-managed-mode-hook
   ;;        (lambda ()
   ;;          (make-local-variable 'before-save-hook)
   ;;          (add-hook 'before-save-hook #'eglot-format-buffer nil t))))
   ))

 (kdl-ts-mode . (mode . kdl-format-on-save))

 (java-mode
  .
  ((eglot-workspace-configuration
    .
    (:java
     (:project
      (:sourcePaths ["src"] :referencedLibraries ["lib/*.jar"])
      :dependencies
      ["libs/**/*.jar" "libs/*.jar"]
      :output-dir "build")
     :jdtls (:workspaceFolder "~/java/")))
   (eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)))))))


;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
