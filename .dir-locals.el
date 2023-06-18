(
  (nil
    .
    (
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
              (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (xml-xmllint))))))))))))

  (dired-mode
    .
    (
      (dired-omit-files
        .
        "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.git\\'|\\.cache\\'|\\eln-cache\\'|\\eglot-java-eclipse-jdt-cache\\'|\\elisp-autofmt-cache\\'")))

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

  (sh-mode . ((sb/delete-trailing-whitespace-p . t) (subdirs . nil)))

  (json-mode
    .
    (
      (eval .
        (add-hook 'before-save-hook
          (lambda ()
            (when (bound-and-true-p lsp-managed-mode)
              (lsp-format-buffer))
            (when (bound-and-true-p eglot--managed-mode)
              (eglot-format-buffer)))
          nil t))))

  (jsonc-mode
    .
    (
      (eval .
        (add-hook 'before-save-hook
          (lambda ()
            (when (bound-and-true-p lsp-managed-mode)
              (lsp-format-buffer))
            (when (bound-and-true-p eglot--managed-mode)
              (eglot-format-buffer)))
          nil t))))


  (org-mode . ((eglot-workspace-configuration . ((:ltex-ls . (:language . "en")))))))


;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
