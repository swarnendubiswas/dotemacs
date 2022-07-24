(
 (nil . (
         (make-local-variable 'completion-ignored-extensions)
         (eval . (add-to-list 'completion-ignored-extensions ".out"))
         (eval . (add-to-list 'completion-ignored-extensions ".log"))
         (counsel-find-file-ignore-regexp . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|.dvi$\\|.fdb_latexmk$\\|.fls$\\|.lof$\\|.log$\\|.lot$\\|.out$\\|.rel$\\|.rip$\\|.synctex$\\|.synctex.gz$\\|.toc$\\|.aux$\\|.bbl$\\|.blg$")
         (projectile-project-compilation-dir . ".")
         (projectile-project-compilation-cmd . "latexmk -f paper.tex")
         (projectile-enable-caching . t)
         (compile-command . "latexmk -f paper.tex")

         (eval . (add-hook 'lsp-managed-mode-hook
                           (lambda ()
                             (when (derived-mode-p 'markdown-mode)
                               (setq sb/flycheck-local-checkers
                                     '((lsp . ((next-checkers
                                                . (markdown-markdownlint-cli))))))
                               ;; Use this if `grammarly-ls' is disabled
                               ;; (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly)
                               )

                             (when (derived-mode-p 'latex-mode)
                               (setq sb/flycheck-local-checkers
                                     '((lsp . ((next-checkers .
                                                              (tex-textidote . (tex-chktex))))))))
                             )))
         ))

 (latex-mode . (                
                (TeX-master . (expand-file-name "paper.tex"))

                (eval . (progn
                          (let (
                                (bibpath (expand-file-name "./references/references.bib"))
                                (projectroot (expand-file-name "./"))
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
