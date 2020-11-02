(
 (nil . (
         (counsel-find-file-ignore-regexp
          . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|.elc$\\|.jar$\\|.pdf$\\|.tar.gz$\\|TAGS\\|GPATH\\|GRTAGS\\|GTAGS\\|tramp\\|auto-save-list\\|eclipse.jdt.ls\\|session*\\|request\\|^workspace")
         ;; (eval . (setq-local counsel-etags-project-root (projectile-project-root)
         ;;                     tags-table-list (list (projectile-project-root))))
         ;; (eval . (add-hook 'before-save-hook #'delete-trailing-whitespace))
         (mode . whitespace-cleanup)
         (counsel-projectile-default-file . "init.el")
         ))
 (emacs-lisp-mode . ((no-byte-compile . t)))
 (sh-mode . (
             (eval ignore-errors (require 'shfmt))
             (mode . shfmt-on-save)
             ))
 )
