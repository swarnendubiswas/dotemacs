(
 (nil . ((eval . (setq-local counsel-etags-project-root (projectile-project-root)
                             tags-table-files (list (projectile-project-root)))
               ))
      )

 (c++-mode . (
              (flycheck-gcc-language-standard . "c++11")
              (flycheck-clang-language-standard . "c++11")
              (eval . (let ((clang-args (list
                                         "-std=c++11"
                                         "-I/usr/include"
                                         "-I/usr/include/boost"
                                         "-I/usr/include/linux"
                                         "-I/home/swarnendu/prospar-workspace/abseil-cpp"
                                         "-I/home/swarnendu/prospar-workspace/protobuf/src"
                                         (concat "-I" (expand-file-name (projectile-project-root)))
                                         ))
                            (include-path
                             (list
                              "/usr/include"
                              "/usr/include/boost"
                              "/usr/include/linux"
                              "/home/swarnendu/prospar-workspace/abseil-cpp"
                              "/home/swarnendu/prospar-workspace/protobuf/src"
                              (expand-file-name (projectile-project-root))
                              )))
                        (setq-local company-clang-arguments clang-args
                                    flycheck-clang-args clang-args
                                    flycheck-gcc-args clang-args
                                    flycheck-gcc-include-path include-path
                                    flycheck-clang-include-path include-path)
                        )))
           )
 )
