(
 (nil . (
         (counsel-find-file-ignore-regexp . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|.dvi$\\|.fdb_latexmk$\\|.fls$\\|.lof$\\|.log$\\|.lot$\\|.out$\\|.rel$\\|.rip$\\|.synctex$\\|.synctex.gz$\\|.toc$")
         (projectile-project-compilation-dir . ".")
         (projectile-project-compilation-cmd . "latexmk -f paper.tex")
         (eval . (add-hook 'lsp-managed-mode-hook
                           (lambda ()
                             (when (derived-mode-p 'markdown-mode)
                               (setq sb/flycheck-local-checkers
                                     '((lsp . ((next-checkers
                                                . (markdown-markdownlint-cli))))))
                               ;; Use this if `grammarly-ls' is disabled
                               ;; (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly)
                               )

                             ;; (when (derived-mode-p 'latex-mode)
                             ;;   (setq sb/flycheck-local-checkers
                             ;;         '((lsp . ((next-checkers
                             ;;                    . (tex-chktex))))))
                             ;;   (flycheck-add-next-checker 'tex-chktex 'grammarly))

                             (when (derived-mode-p 'sh-mode)
                               (setq sb/flycheck-local-checkers
                                     '((lsp . ((next-checkers
                                                . (sh-shellcheck)))))))
                             )))
         ))

 (latex-mode . (
                ;; (flycheck-checker . tex-chktex)
                (TeX-master . (expand-file-name "paper.tex"))

                (eval . (progn
                          (let (
                                (bibpath (expand-file-name "./references/references.bib"))
                                (projectroot (expand-file-name "."))
                                )
                            (setq-local reftex-default-bibliography    bibpath
                                        company-bibtex-bibliography    bibpath
                                        bibtex-completion-bibliography bibpath
                                        lsp-latex-root-directory       projectroot))
                          ))
                ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
