(
 (nil . (
         (counsel-find-file-ignore-regexp . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|__pycache__\\|.o$\\|.out$\\|.pyc$\\|.toc$\\|tramp\\|.metadata\\|.clangd\\|.recommenders\\|eclipse.jdt.ls\\|session*\\|request\\|^workspace")
         (lsp-file-watch-ignored-directories . ("/\\.git$" "/\\.clangd$" "build"))
         ))

 (c++-mode . (
              (flycheck-gcc-language-standard   . "c++11")
              (flycheck-clang-language-standard . "c++11")
              (flycheck-gcc-openmp . t)

              ;; (eval add-hook 'hack-local-variables-hook (lambda () (when (string= major-mode 'c++-mode) (lsp))))

              ;; https://stackoverflow.com/questions/33063008/define-new-variable-in-dir-locals-el
              (eval . (let (
                            (clang-args (list
                                         "-std=c++11"
                                         (concat "-I" "/usr/include")
                                         (concat "-I" "/usr/include/linux")
                                         (concat "-I" "/usr/include/boost")
                                         (concat "-I" "/usr/lib/llvm-12/include")
                                         (concat "-I" (expand-file-name "." (projectile-project-root)))
                                         ))
                            (include-path (list
                                           "/usr/include"
                                           "/usr/include/linux"
                                           "/usr/include/boost"
                                           "/usr/lib/llvm-12/include"
                                           (expand-file-name "." (projectile-project-root))
                                           ))
                            (clangd-args (list
                                          "-j=2"
                                          "--background-index"
                                          "--clang-tidy"
                                          "--fallback-style=LLVM"
                                          "--compile-commands-dir=build"
                                          "--header-insertion=never"
                                          "--header-insertion-decorators=0"
                                          "--log=error"
                                          "--pch-storage=memory"
                                          ;; "--suggest-missing-includes"
                                          "--pretty"
                                          ))
                            )
                        (setq-local company-clang-arguments clang-args
                                    flycheck-clang-args clang-args
                                    flycheck-gcc-args clang-args
                                    flycheck-gcc-include-path include-path
                                    flycheck-clang-include-path include-path
                                    flycheck-cppcheck-include-path include-path
                                    lsp-clients-clangd-args clangd-args)
                        ))
              ))

 (python-mode . (
                 (flycheck-pylintrc . "setup.cfg")
                 (lsp-pyright-extra-paths . ["./src"])
                 (lsp-pyright-venv-path . ["./src"])
                 (python-shell-exec-path . "/usr/bin/python3")
                 (python-shell-interpreter . "/usr/bin/python3")
                 ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
