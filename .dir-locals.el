(
 (nil . ((counsel-find-file-ignore-regexp
          . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|.elc$\\|.jar$\\|.pdf$\\|.pyc$\\|.tar.gz$\\|TAGS\\|GPATH\\|GRTAGS\\|GTAGS\\|tramp\\|.metadata\\|auto-save-list\\|eclipse.jdt.ls\\|session*\\|request\\|workspace")
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
