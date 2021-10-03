(
 (nil . (
         (lsp-file-watch-ignored-directories . ("/\\.git$" "/\\.clangd$" "build" "obj"))
         ))

 (c++-mode . (
              (flycheck-gcc-language-standard . "c++11")
              (flycheck-clang-language-standard . "c++11")
              (eval . (let (
                            (clang-args (list
                                         "-std=c++11"
                                         (concat "-I" "/usr/include/linux")
                                         (concat "-I" (expand-file-name "core" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "core/utils" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "." (projectile-project-root)))
                                         ))
                            (include-path (list
                                           "/usr/include/linux"
                                           (expand-file-name "core" (projectile-project-root))
                                           (expand-file-name "core/utils" (projectile-project-root))
                                           (expand-file-name "." (projectile-project-root))
                                           )))
                        (setq-local company-clang-arguments clang-args
                                    flycheck-clang-args clang-args
                                    flycheck-gcc-args clang-args
                                    flycheck-gcc-include-path include-path
                                    flycheck-clang-include-path include-path
                                    flycheck-cuda-include-path include-path)
                        ))
              ))
 )

