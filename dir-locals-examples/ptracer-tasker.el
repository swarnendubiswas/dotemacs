(
 (nil . (
         (projectile-project-compilation-dir . "./..")
         (projectile-project-compilation-cmd . "bash build_fastpar.sh;")
         ))

 (c++-mode . (
              ;; (lsp-clients-clangd-args . ("-j=4"
              ;;                             "--background-index"
              ;;                             "--clang-tidy"
              ;;                             "--fallback-style=LLVM"
              ;;                             "--header-insertion=never"
              ;;                             "--header-insertion-decorators=0"
              ;;                             "--log=error"
              ;;                             "--pch-storage=memory"
              ;;                             "--pretty"
              ;;                             "--compile-commands-dir=."))

              (eval . (progn
                        (add-to-list 'lsp-clients-clangd-args
                                     (concat "--compile-commands-dir=./")
                                     'append)
                        ))
              ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
