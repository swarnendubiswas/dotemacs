(
 (nil . (
         (projectile-project-compilation-dir . "./..")
         (projectile-project-compilation-cmd . "bash build_fastpar.sh;")
         ))

 (c++-mode . (
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
