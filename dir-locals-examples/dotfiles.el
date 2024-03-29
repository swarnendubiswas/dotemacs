;; ((nil
;;   (eval
;;    (lambda ()
;;      (when (string= (file-name-nondirectory buffer-file-name)
;;                     "dotprettierrc")
;;        (json-mode))))))

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
                             )))
         ))

 (yaml-mode . (
               (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
               ))

 (json-mode . (
               (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
               ))

 (jsonc-mode . (
                (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
                ))
 )


;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
