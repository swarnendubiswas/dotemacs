(
 (nil . (
         (counsel-find-file-ignore-regexp . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|.dvi$\\|.fdb_latexmk$\\|.fls$\\|.lof$\\|.log$\\|.lot$\\|.out$\\|.rel$\\|.rip$\\|.synctex$\\|.synctex.gz$\\|.toc$")

         ;; (eval . (add-hook 'lsp-managed-mode-hook
         ;;                   (lambda ()

         ;;                     (when (derived-mode-p 'latex-mode)
         ;;                       (setq sb/flycheck-local-checkers '((lsp . ((next-checkers . (tex-chktex))))))
         ;;                       (flycheck-add-next-checker 'tex-chktex 'grammarly))

         ;;                     (when (derived-mode-p 'markdown-mode)
         ;;                       (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (markdown-markdownlint-cli))))))
         ;;                       (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly))

         ;;                     (when (derived-mode-p 'gfm-mode)
         ;;                       (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (markdown-markdownlint-cli))))))
         ;;                       (flycheck-add-next-checker 'markdown-markdownlint-cli 'grammarly))

         ;;                     (when (derived-mode-p 'sh-mode)
         ;;                       (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (sh-shellcheck)))))))

         ;;                     )))
         ))

 (latex-mode . (
                (eval . (setq TeX-master (expand-file-name "main.tex" (projectile-project-root))
                              reftex-default-bibliography (expand-file-name
                                                           "references/references.bib"
                                                           (projectile-project-root))
                              company-bibtex-bibliography (expand-file-name
                                                           "references/references.bib"
                                                           (projectile-project-root))
                              bibtex-completion-bibliography (expand-file-name
                                                              "references/references.bib"
                                                              (projectile-project-root))
                              lsp-latex-root-directory (expand-file-name (projectile-project-root))
                              ))
                ))
 )

