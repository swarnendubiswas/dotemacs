(
 (nil . (
         (fill-column . 79)
         (indent-tabs-mode . nil)
         (projectile-enable-caching . t)
         (lsp-file-watch-ignored-directories . ("/\\.git$" "/\\.clangd$" "/build$" "/\\.cache$"))
         ))

 (c++-mode . (
              (load-file "./util/emacs/m5-c-style.el")
              (eval ignore-errors (require 'm5-c-style))
              (c-default-style . "m5")
              (c-set-style . "m5")

              ;; (c-basic-offset . 4)
              ;; (indent-tabs-mode . nil)
              ;; (c-offsets-alist . ((substatement-open . 0)
              ;;                     (inline-open . 0)
              ;;                     (block-open . -4)
              ;;                     (case-label . 2)
              ;;                     (label . 2)
              ;;                     (statement-case-intro . 2)
              ;;                     (statement-case-open . 2)
              ;;                     (access-label . -2)
              ;;                     (innamespace . 0)))

              (flycheck-gcc-language-standard . "c++11")
              (flycheck-clang-language-standard . "c++11")

              (eval . (let (
                            (clang-args (list
                                         "-std=c++11"
                                         (concat "-I" (expand-file-name "src" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "src/mem/ruby/structures/" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "src/mem" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "src/sim" (projectile-project-root)))
                                         (concat "-I" (expand-file-name "build/X86/params" (projectile-project-root)))
                                         ))
                            (include-path (list
                                           (expand-file-name "src" (projectile-project-root))
                                           (expand-file-name "src/mem/ruby/structures" (projectile-project-root))
                                           (expand-file-name "src/mem" (projectile-project-root))
                                           (expand-file-name "src/sim" (projectile-project-root))
                                           (expand-file-name "build/X86/params" (projectile-project-root))
                                           )))
                        (setq-local company-clang-arguments clang-args
                                    flycheck-clang-args clang-args
                                    flycheck-gcc-args clang-args
                                    flycheck-gcc-include-path include-path
                                    flycheck-clang-include-path include-path)
                        ))

              (lsp-clients-clangd-args . ("-j=2"
                                          "--background-index"
                                          "--clang-tidy"
                                          "--fallback-style=LLVM"
                                          "--header-insertion=never"
                                          "--header-insertion-decorators=0"
                                          "--log=error"
                                          "--pch-storage=memory"
                                          "--pretty"
                                          "--compile-commands-dir=."))

              ))

 (python-mode . (
                 (flycheck-pylintrc . "setup.cfg")
                 (eval . (let (
                               (paths
                                (vconcat (list
                                          (expand-file-name "src/python" (projectile-project-root))
                                          (expand-file-name "ext/ply" (projectile-project-root))
                                          (expand-file-name "src/sim" (projectile-project-root))
                                          ))
                                ))
                           (setq lsp-pyright-extra-paths paths)
                           ))
                 (eval . (setenv "PYTHONPATH"
                                 (concat
                                  (expand-file-name "src/python" (projectile-project-root))
                                  ":"
                                  (expand-file-name "ext/ply" (projectile-project-root))
                                  ":"
                                  (expand-file-name "src/sim" (projectile-project-root))
                                  ":"
                                  (getenv "PYTHONPATH"))))
                 (pyvenv-activate . "/home/swarnendu/tmp/virtualenvs/fs-python2-venv")
                 (py-isort-options . '("--settings-path=setup.cfg"))
                 ))
 )

