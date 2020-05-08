;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

;; ((nil . ((eval . (progn BODY)))))

(
 (nil . ((counsel-find-file-ignore-regexp
          . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|__pycache__\\|.aux$\\|.bbl$\\|.blg$\\|.cb$\\|.cb2$\\|.dvi$\\|.elc$\\|.fdb_latexmk$\\|.fls$\\|.jar$\\|.lof$\\|.lot$\\|.o$\\|.out$\\|.pdf$\\|.pyc$\\|.rel$\\|.rip$\\|.synctex$\\|.synctex.gz$\\|.tar.gz$\\|.toc$\\|TAGS\\|GPATH\\|GRTAGS\\|GTAGS\\|tramp\\|.metadata\\|.clangd\\|.recommenders\\|auto-save-list\\|eclipse.jdt.ls\\|session*\\|request\\|workspace")
         (eval . (setq-local counsel-etags-project-root (projectile-project-root)
                             tags-table-list (list (projectile-project-root))
                             ))
         (eval . (add-hook 'before-save-hook #'delete-trailing-whitespace))
         ))
 (sh-mode . ((eval ignore-errors (require 'shfmt))
             (mode . shfmt-on-save)
             ))
 ;; (gfm-mode . ((mode . prettier-js)))
 )
