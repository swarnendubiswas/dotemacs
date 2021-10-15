(
 (emacs-lisp-mode . (
                     (eval . (format-all-mode 1))
                     ;; The special `subdirs' element is not a variable, but a special keyword which
                     ;; indicates that the mode settings are only to be applied in the current
                     ;; directory, not in any subdirectories.
                     (subdirs . nil)
                     ))

 (nil . (
         (eval . (add-hook 'lsp-managed-mode-hook
                           (lambda ()
                             (when (derived-mode-p 'markdown-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (markdown-markdownlint-cli))))))
                               (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly))

                             (when (derived-mode-p 'gfm-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (markdown-markdownlint-cli))))))
                               (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly))

                             (when (derived-mode-p 'sh-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (sh-shellcheck)))))))

                             (when (derived-mode-p 'yaml-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (yaml-yamllint)))))))

                             (when (derived-mode-p 'json-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (json-jsonlint)))))))

                             (when (derived-mode-p 'python-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (python-pylint)))))))

                             (when (derived-mode-p 'c++-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (c/c++-cppcheck)))))))

                             (when (derived-mode-p 'html-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (html-tidy)))))))

                             (when (derived-mode-p 'xml-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (xml-xmllint)))))))
                             )))
         ))

 (sh-mode . (
             (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
             ))

 (json-mode . (
               (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
               ))

 (jsonc-mode . (
                (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
                ))

 )
