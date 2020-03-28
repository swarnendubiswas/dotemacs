(
 (nil . (
         (projectile-project-compilation-cmd . latexmk)
         ))

 (latex-mode . (
                (TeX-master . "/home/swarnendu/plass-workspace/arc-paper/paper.tex")
                (reftex-default-bibliography . ("/home/swarnendu/plass-workspace/bib/plass.bib"))
                (company-bibtex-bibliography . ("/home/swarnendu/plass-workspace/bib/plass.bib"))
                (bibtex-completion-bibliography . ("/home/swarnendu/plass-workspace/bib/plass.bib"))
                (eval . (add-hook 'before-save-hook #'sb/save-buffer-and-run-latexmk)))
             )
 )
