(
 (c++-mode . (
              (flycheck-gcc-language-standard   . "c11")
              (flycheck-clang-language-standard . "c11")
              (flycheck-clang-tidy-build-path   . ".")
              (eval . (let (
                            (clang-args (list
                                         "-std=c11"
                                         "-I/usr/include"
                                         "-I/usr/include/linux"
                                         "-I/usr/include/boost"
                                         (concat "-I" (expand-file-name "src"
                                                                        (projectile-project-root)))
                                         ))
                            (include-path (list
                                           "/usr/include"
                                           "/usr/include/linux"
                                           "/usr/include/boost"
                                           (expand-file-name "src" (projectile-project-root))
                                           ))
                            )
                        (setq-local company-clang-arguments clang-args
                                    flycheck-clang-args clang-args
                                    flycheck-gcc-args clang-args
                                    flycheck-gcc-include-path include-path
                                    flycheck-clang-include-path include-path
                                    flycheck-cppcheck-include-path include-path)
                        ))
              (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
              (eval . (add-to-list 'lsp-clients-clangd-args
                                   (concat "--compile-commands-dir="
                                           (concat "." (projectile-project-root)))
                                   'append))
              ))
 )

