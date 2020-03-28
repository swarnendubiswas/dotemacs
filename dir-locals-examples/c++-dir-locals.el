((nil . ((eval . (setq projectile-enable-caching t))))
 (c++-mode . ((flycheck-gcc-language-standard . "c++11")
              (flycheck-clang-language-standard . "c++11")
              (eval . (let ((clang-args '("-std=c++11"
                                          "-I/usr/include"
                                          "-I/usr/include/boost"
                                          "-I/usr/include/linux"
                                          (concat "-I" (expand-file-name "tdebug-llvm/llvm/include" (projectile-project-root)))
                                          (concat "-I" (expand-file-name "tdebug-llvm/clang/include" (projectile-project-root)))
                                          (concat "-I" (expand-file-name "tdebug-lib/include" (projectile-project-root)))
                                          (concat "-I"(expand-file-name "spd3-lib/include" (projectile-project-root)))
                                          (concat "-I" (expand-file-name "fasttrack/include" (projectile-project-root)))
                                          (concat "-I" (expand-file-name "newfasttrack/include" (projectile-project-root)))
                                          (concat "-I" (expand-file-name "new_algo/include" (projectile-project-root)))
                                          ))
                            (include-path
                             (list
                              "/usr/include"
                              "/usr/include/boost"
                              "/usr/include/linux"
                              (expand-file-name "tdebug-llvm/llvm/include" (projectile-project-root))
                              (expand-file-name "tdebug-llvm/clang/include" (projectile-project-root))
                              (expand-file-name "tdebug-lib/include" (projectile-project-root))
                              (expand-file-name "spd3-lib/include" (projectile-project-root))
                              (expand-file-name "fasttrack/include" (projectile-project-root))
                              (expand-file-name "newfasttrack/include" (projectile-project-root))
                              (expand-file-name "new_algo/include" (projectile-project-root)))))
                        (setq-local company-clang-arguments clang-args
                                    flycheck-clang-args clang-args
                                    flycheck-gcc-args clang-args
                                    flycheck-gcc-include-path include-path
                                    flycheck-clang-include-path include-path))))))
