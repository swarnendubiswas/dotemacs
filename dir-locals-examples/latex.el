(
 (nil . (
         (counsel-find-file-ignore-regexp . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|.dvi$\\|.fdb_latexmk$\\|.fls$\\|.lof$\\|.log$\\|.lot$\\|.out$\\|.rel$\\|.rip$\\|.synctex$\\|.synctex.gz$\\|.toc$")
         (projectile-enable-caching . t)
         ))

 (latex-mode . (
                (eval . (setq TeX-master (expand-file-name "paper.tex" (projectile-project-root))
                              reftex-default-bibliography (expand-file-name "references.bib" (projectile-project-root))
                              company-bibtex-bibliography (expand-file-name "references.bib" (projectile-project-root))
                              bibtex-completion-bibliography (expand-file-name "references.bib" (projectile-project-root))
                              lsp-latex-root-directory (expand-file-name (projectile-project-root))
                              ))
                ))
 )
