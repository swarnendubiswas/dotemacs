(
 (nil . (
         (eval . (add-hook 'lsp-managed-mode-hook
                           (lambda ()
                             (when (derived-mode-p 'markdown-mode)
                               (setq sb/flycheck-local-checkers
                                     '((lsp . ((next-checkers . (markdown-markdownlint-cli)))))))

                             (when (derived-mode-p 'sh-mode)
                               (setq sb/flycheck-local-checkers
                                     '((lsp . ((next-checkers . (sh-shellcheck)))))))

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

                             (when (derived-mode-p 'html-mode)
                               (setq sb/flycheck-local-checkers
                                     '((lsp . ((next-checkers . (html-tidy)))))))

                             (when (derived-mode-p 'xml-mode)
                               (setq sb/flycheck-local-checkers
                                     '((lsp . ((next-checkers . (xml-xmllint)))))))

                             ;; Chktex can report lot of errors
                             ;; (when (derived-mode-p 'latex-mode)
                             ;;   (setq sb/flycheck-local-checkers
                             ;;         '((lsp . ((next-checkers . (tex-chktex)))))))

                             ;; (when (derived-mode-p 'org-mode)
                             ;;   (setq sb/flycheck-local-checkers
                             ;;         '((lsp . ((next-checkers . (org-lint)))))))
                             ))
               )))

 (dired-mode . (
                (dired-omit-files . "\\`[.]?#\\|\\`[.][.]?\\'\\|\\.git\\'|\\.cache\\'|\\eln-cache\\'")
                ))

 (emacs-lisp-mode . (
                     (no-byte-compile . t)
                     (elisp-autofmt-on-save-p . always)
                     (elisp-autofmt-load-packages-local . ("use-package"))
                     (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
                     ;; The special `subdirs' element is not a variable, but a special keyword which
                     ;; indicates that the mode settings are only to be applied in the current
                     ;; directory, not in any subdirectories.
                     (subdirs . nil)
                     ))

 ;; Bash language server does not format files
 (sh-mode . (
             (sb/delete-trailing-whitespace-p . t)
             ;; (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
             ;; (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
             (subdirs . nil)
             ))

 (json-mode . (
               (eval . (add-hook 'before-save-hook
                                 (lambda ()
                                   (when (bound-and-true-p lsp-managed-mode)
                                     (lsp-format-buffer))
                                   (when (bound-and-true-p eglot--managed-mode)
                                     (eglot-format-buffer))) nil t))
               ))

 (jsonc-mode . (
                (eval . (add-hook 'before-save-hook
                                  (lambda ()
                                    (when (bound-and-true-p lsp-managed-mode)
                                      (lsp-format-buffer))
                                    (when (bound-and-true-p eglot--managed-mode)
                                      (eglot-format-buffer))) nil t))
                ))

 ;; Does not support formatting
 (yaml-mode . (
               ;; (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
               ))
 )


;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
