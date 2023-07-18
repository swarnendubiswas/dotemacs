(
  (nil
    .
    ((make-local-variable 'completion-ignored-extensions)
      (eval . (add-to-list 'completion-ignored-extensions "elisp-autofmt-cache/"))
      (eval . (add-to-list 'completion-ignored-extensions "eln-cache/"))
      (eval . (add-to-list 'completion-ignored-extensions "tree-sitter/"))
      (eval . (add-to-list 'completion-ignored-extensions "eglot-java-eclipse-jdt-cache/"))
      (eval . (add-to-list 'completion-ignored-extensions "share/"))
      (eval . (add-to-list 'completion-ignored-extensions "auto-save-list/"))
      (eval . (add-to-list 'completion-ignored-extensions "\\.tags/"))
      (eval . (add-to-list 'completion-ignored-extensions "\\.cache/"))

      (eval .
        (setq-local counsel-find-file-ignore-regexp
          (regexp-opt
            '
            ("\\(?:\\`[#.]\\)"
              "\\(?:\\`.+?[#~]\\'\\)"
              "__pycache__"
              ".elc"
              ".o"
              ".out"
              "TAGS"
              "GPATH"
              "GRTAGS"
              "GTAGS"
              "tramp"
              ".vscode"
              ".metadata"
              ".clangd"
              ".cache"
              "auto-save-list"
              "eclipse.jdt.ls"
              "session*"
              "request"
              "workspace"
              "eglot-java-eclipse-jdt-cache"
              "elpa"
              "share"
              "tree-sitter"
              "eln-cache"
              "elisp-autofmt-cache"))))

      (eval .
        (add-hook
          'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'markdown-mode)
              (setq sb/flycheck-local-checkers
                '((lsp . ((next-checkers . (markdown-markdownlint-cli)))))))

            (when (derived-mode-p 'yaml-mode)
              (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (yaml-yamllint)))))))

            (when (derived-mode-p 'json-mode)
              (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (json-jsonlint)))))))

            (when (derived-mode-p 'python-mode)
              (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint)))))))

            (when (derived-mode-p 'c++-mode)
              (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (c/c++-cppcheck)))))))

            (when (derived-mode-p 'latex-mode)
              (setq sb/flycheck-local-checkers
                '((lsp . ((next-checkers . (tex-textidote . (tex-chktex))))))))

            (when (derived-mode-p 'html-mode)
              (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (html-tidy)))))))

            (when (derived-mode-p 'xml-mode)
              (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (xml-xmllint))))))))))

      (eglot-workspace-configuration
        .
        ((ltex . (:disabledRules (:en-US ["MORFOLOGIK_RULE_EN_US"])))))))

  (dired-mode
    .
    (
      (dired-omit-files
        .
        "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.git\\'|\\.cache\\'|eln-cache|eglot-java-eclipse-jdt-cache|elisp-autofmt-cache|tree-sitter|share|elpa|\\.directory\\'")))

  (emacs-lisp-mode
    .
    ((no-byte-compile . t)
      (elisp-autofmt-on-save-p . always)
      (elisp-autofmt-load-packages-local . ("use-package"))
      (sb/delete-trailing-whitespace-p . t)
      ;; The special `subdirs' element is not a variable, but a special keyword which
      ;; indicates that the mode settings are only to be applied in the current
      ;; directory, not in any subdirectories.
      (subdirs . nil)))

  (sh-mode
    .
    (
      ;; Bash language server does not support formatting
      ;; (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
      (sb/delete-trailing-whitespace-p . t)
      (subdirs . nil)))

  (yaml-mode
    .
    (
      (eval .
        (add-hook
          'lsp-managed-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

      (eval .
        (add-hook
          'eglot-managed-mode-hook
          (lambda () (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

  (json-mode
    .
    (
      (eval .
        (add-hook
          'lsp-managed-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

      (eval .
        (add-hook
          'eglot-managed-mode-hook
          (lambda () (add-hook 'before-save-hook #'eglot-format-buffer nil t))))))

  (jsonc-mode
    .
    (
      (eval .
        (add-hook
          'lsp-managed-mode-hook
          (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

      (eval .
        (add-hook
          'eglot-managed-mode-hook
          (lambda () (add-hook 'before-save-hook #'eglot-format-buffer nil t)))))))


;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
