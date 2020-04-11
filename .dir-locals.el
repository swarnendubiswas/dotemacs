;; -*- enable-local-variables: all; enable-local-eval: t -*-

;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

;; ((nil . ((eval . (progn BODY)))))

;; (
;; (nil . (

;;         ;; (tags-table-list . (eval . (projectile-project-root)))

;;         ;; (eval . (let ((tags-list '(`,(expand-file-name "TAGS" (projectile-project-root)))))
;;         ;;           (setq-local tags-table-list tags-list))

;;         )
;;      )
;; )

(
 (nil . (
         ;; (enable-local-eval . t)

         (counsel-find-file-ignore-regexp
          . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|__pycache__\\|.aux$\\|.bbl$\\|.blg$\\|.cb$\\|.cb2$\\|.dvi$\\|.elc$\\|.fdb_latexmk$\\|.fls$\\|.jar$\\|.lof$\\|.lot$\\|.o$\\|.out$\\|.pdf$\\|.pyc$\\|.rel$\\|.rip$\\|.synctex$\\|.synctex.gz$\\|.tar.gz$\\|.toc$\\|TAGS\\|GPATH\\|GRTAGS\\|GTAGS\\|tramp\\|.metadata\\|.clangd\\|.recommenders\\|auto-save-list\\|eclipse.jdt.ls\\|session*\\|request\\|workspace")

         (tags-table-list . ("."))

         (eval . (setq-local counsel-etags-project-root (projectile-project-root)))
         ))
 )
