(
 (nil . (
         (eval . (add-hook 'lsp-managed-mode-hook
                           (lambda ()
                             (when (derived-mode-p 'markdown-mode)
                               (setq sb/flycheck-local-checkers
                                     '((lsp . ((next-checkers
                                                . (markdown-markdownlint-cli))))))
                               ;; Use this if `grammarly-ls' is disabled
                               ;; (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly)
                               )

                             ;; Another form

                             ;; (when (derived-mode-p 'markdown-mode)
                             ;;   (setq sb/flycheck-local-cache
                             ;;         '((lsp . ((next-checkers
                             ;;                    . (markdown-markdownlint-cli
                             ;;                       . (grammarly))))))))

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
                             ;;         '((lsp . ((next-checkers . (tex-chktex))))))
                             ;;   (flycheck-add-next-checker 'tex-chktex 'grammarly))
                             )))
         ))

 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
