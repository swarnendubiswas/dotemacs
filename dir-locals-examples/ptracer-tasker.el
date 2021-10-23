(
 (c++-mode . (
              (eval . (progn
                        (add-to-list 'lsp-clients-clangd-args
                                     (concat "--compile-commands-dir="
                                             (expand-file-name "./" (projectile-project-root)))
                                     'append)
                        ))
              ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
