;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html

((nil
  .
  ((fill-column . 80)

   (make-local-variable 'completion-ignored-extensions)
   (eval . (add-to-list 'completion-ignored-extensions "eln-cache/"))
   (eval . (add-to-list 'completion-ignored-extensions "tree-sitter/"))
   (eval .
         (add-to-list
          'completion-ignored-extensions "eglot-java-eclipse-jdt-cache/"))
   (eval . (add-to-list 'completion-ignored-extensions "share/"))
   (eval . (add-to-list 'completion-ignored-extensions "auto-save-list/"))
   (eval . (add-to-list 'completion-ignored-extensions "\\.tags/"))
   (eval . (add-to-list 'completion-ignored-extensions "\\.cache/"))
   (eval . (add-to-list 'completion-ignored-extensions "auto/"))
   (eval . (add-to-list 'completion-ignored-extensions ".xml"))
   (eval . (add-to-list 'completion-ignored-extensions ".drawio"))
   (eval . (add-to-list 'completion-ignored-extensions ".pptx"))

   (compile-command . "cmake -S . -B build; cmake --build build; ")

   (eval
    .
    (add-hook
     'lsp-managed-mode-hook
     (lambda ()
       ;; (add-to-list lsp-file-watch-ignored-directories "/build")

       (when (derived-mode-p 'markdown-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (markdown-markdownlint-cli)))))))

       (when (derived-mode-p 'yaml-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (yaml-yamllint)))))))

       (when (derived-mode-p 'json-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (json-jsonlint)))))))

       (when (derived-mode-p 'python-mode)
         (setq sb/flycheck-local-checkers
               '((lsp . ((next-checkers . (python-pylint)))))))

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
   (dired-omit-extensions . (".out" ".vect" ".o"))
   (dired-omit-files
    .
    "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.git\\'|\\.cache\\'|eln-cache|eglot-java-eclipse-jdt-cache|elisp-autofmt-cache|tree-sitter|share|elpa|\\.directory\\'")))

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
            flycheck-cppcheck-include-path include-path)))

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
            flycheck-cppcheck-include-path include-path)))

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
  ((python-shell-exec-path . "/usr/bin/python3")
   (python-shell-interpreter . "/usr/bin/python3")
   (flycheck-pylintrc . "setup.cfg")
   (py-isort-options . '("--settings-path=setup.cfg"))
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
            (add-hook 'before-save-hook #'lsp-organize-imports nil t))))

   ;;  https://github.com/python-lsp/python-lsp-server/blob/develop/CONFIGURATION.md
   (eglot-workspace-configuration
    .
    ((:python
      (:pythonPath "./.venv/bin/python")
      (:venvPath (expand-absolute-name "~/.local/share/conda/envs"))
      (:analysis
       (:diagnosticMode
        "openFilesOnly"
        :stubPath (expand-absolute-name "~/.local/lib/python-type-stubs"))))
     (:pylsp
      (:configurationSources
       ["setup.cfg"]
       :plugins
       (:autopep8
        (:enabled :json-false)
        :black
        (:enabled :json-false)
        :flake8
        (:enabled :json-false :config t :maxLineLength 80)
        :jedi
        (:environment "./.venv/" :extra_paths [])
        :jedi_completion
        (:fuzzy
         t
         :include_params t
         :include_class_objects t
         :cache_for
         ["pandas" "numpy" "matplotlib"])
        :jedi_definition
        (:enabled t :follow_imports t :follow_builtin_imports t)
        :jedi_hover
        (:enabled t)
        :jedi_references
        (:enabled t)
        :jedi_signature_help
        (:enabled t)
        :jedi_symbols
        (:enabled t :all_scopes t :include_import_symbols t)
        :mccabe
        (:enabled :json-false :threshold 15)
        :mypy
        (:enabled :json-false)
        :preload
        (:enabled t :modules ["pandas" "numpy" "matplotlib"])
        :pycodestyle
        (:enabled :json-false :maxLineLength 80)
        :pydocstyle
        (:enabled t :convention "numpy")
        :pyflakes
        (:enabled :json-false)
        :pylint
        (:enabled t)
        :pylsp_black
        (:enabled :json-false)
        :pylsp_isort
        (:enabled t)
        :pylsp_mypy
        (:enabled t :report_progress t :live_mode :json-false)
        :rope_autoimport
        (:code_actions (:enabled t) :completions (:enabled t) :enabled t)
        :rope_completion
        (:enabled t :eager :json-false)
        :ruff
        (:enabled :json-false :formatEnabled t :lineLength 80)
        :yapf
        (:enabled t))))))

   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)
            (add-hook
             'before-save-hook #'eglot-code-action-organize-imports))))))

 (python-ts-mode
  .
  ((python-shell-exec-path . "/usr/bin/python3")
   (python-shell-interpreter . "/usr/bin/python3")
   (flycheck-pylintrc . "setup.cfg")
   (py-isort-options . '("--settings-path=setup.cfg"))
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
            (add-hook 'before-save-hook #'lsp-organize-imports nil t))))

   (eglot-workspace-configuration
    .
    ((:python
      (:pythonPath "./.venv/bin/python")
      (:venvPath (expand-absolute-name "~/.local/share/conda/envs"))
      (:analysis
       (:diagnosticMode
        "openFilesOnly"
        :stubPath (expand-absolute-name "~/.local/lib/python-type-stubs"))))
     (:pylsp
      (:configurationSources
       ["setup.cfg"]
       :plugins
       (:autopep8
        (:enabled :json-false)
        :black
        (:enabled :json-false)
        :flake8
        (:enabled :json-false :config t :maxLineLength 80)
        :jedi
        (:environment "./.venv/" :extra_paths [])
        :jedi_completion
        (:fuzzy
         t
         :include_params t
         :include_class_objects t
         :cache_for
         ["pandas" "numpy" "matplotlib"])
        :jedi_definition
        (:enabled t :follow_imports t :follow_builtin_imports t)
        :jedi_hover
        (:enabled t)
        :jedi_references
        (:enabled t)
        :jedi_signature_help
        (:enabled t)
        :jedi_symbols
        (:enabled t :all_scopes t :include_import_symbols t)
        :mccabe
        (:enabled :json-false :threshold 15)
        :mypy
        (:enabled :json-false)
        :preload
        (:enabled t :modules ["pandas" "numpy" "matplotlib"])
        :pycodestyle
        (:enabled :json-false :maxLineLength 80)
        :pydocstyle
        (:enabled t :convention "numpy")
        :pyflakes
        (:enabled :json-false)
        :pylint
        (:enabled t)
        :pylsp_black
        (:enabled :json-false)
        :pylsp_isort
        (:enabled t)
        :pylsp_mypy
        (:enabled t :report_progress t :live_mode :json-false)
        :rope_autoimport
        (:code_actions (:enabled t) :completions (:enabled t) :enabled t)
        :rope_completion
        (:enabled t :eager :json-false)
        :ruff
        (:enabled :json-false :formatEnabled t :lineLength 80)
        :yapf
        (:enabled t))))))

   (eval .
         (add-hook
          'eglot-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'eglot-format-buffer nil t)
            (add-hook
             'before-save-hook #'eglot-code-action-organize-imports))))))

 (sh-mode . ((subdirs . nil)))

 (bash-ts-mode . ((subdirs . nil)))

 (yaml-mode
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

 (yaml-ts-mode
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
      :output-dir "build")))
   (jdtls (:workspaceFolder "~/java/"))
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
