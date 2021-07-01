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
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (markdown-markdownlint-cli)))))))

                             (when (derived-mode-p 'gfm-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (markdown-markdownlint-cli)))))))

                             (when (derived-mode-p 'sh-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (sh-shellcheck)))))))

                             (when (derived-mode-p 'json-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (json-jsonlint)))))))
                             )))
         ))
 )
