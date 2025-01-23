((nil
  .
  ((fill-column . 80)

   (make-local-variable 'completion-ignored-extensions)
   (eval . (add-to-list 'completion-ignored-extensions "elisp-autofmt-cache/"))
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
               '((lsp . ((next-checkers . (xml-xmllint))))))))))))

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
   (elisp-autofmt-load-packages-local . ("use-package"))
   ;; The special `subdirs' element is not a variable, but a special keyword which
   ;; indicates that the mode settings are only to be applied in the current
   ;; directory, not in any subdirectories.
   (subdirs . nil)))

 (lisp-data-mode
  .
  ((elisp-autofmt-on-save-p . always)
   (elisp-autofmt-load-packages-local . ("use-package"))
   ;; The special `subdirs' element is not a variable, but a special keyword which
   ;; indicates that the mode settings are only to be applied in the current
   ;; directory, not in any subdirectories.
   (subdirs . nil)))

 (c-mode . ((mode . c++)))

 (c++-mode
  .
  ((clang-format-style . "file")
   (flycheck-gcc-language-standard . "c++17")
   (flycheck-clang-language-standard . "c++17")
   (flycheck-clang-tidy-build-path . ".")
   (flycheck-gcc-openmp . t)

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
               'append)))))))

 (c++-ts-mode
  .
  ((clang-format-style . "file")
   (flycheck-gcc-language-standard . "c++17")
   (flycheck-clang-language-standard . "c++17")
   (flycheck-clang-tidy-build-path . ".")
   (flycheck-gcc-openmp . t)

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
               'append)))))))

 (python-mode
  .
  ((python-shell-exec-path . "/usr/bin/python3")
   (python-shell-interpreter . "/usr/bin/python3")
   (flycheck-pylintrc . "setup.cfg")
   (py-isort-options . '("--settings-path=setup.cfg"))

   (eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (sh-mode . ((subdirs . nil)))

 (bash-ts-mode . ((subdirs . nil)))

 (yaml-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (yaml-ts-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (toml-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (toml-ts-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (json-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (json-ts-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (jsonc-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (html-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (html-ts-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (cmake-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (cmake-ts-mode
  .
  ((eval .
         (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (make-local-variable 'before-save-hook)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t))))))

 (latex-mode . ((lsp-latex-build-on-save . nil)))

 (kdl-ts-mode . (mode . kdl-format-on-save)))


;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
