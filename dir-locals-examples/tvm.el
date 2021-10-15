(
 (python-mode . (
                 (pyvenv-activate . "/home/swarnendu/tmp/virtualenvs/flextensor-venv")
                 (eval . (let (
                               (paths
                                (vconcat (list
                                          (expand-file-name "flextensor" (projectile-project-root))
                                          (expand-file-name "tvm-examples" (projectile-project-root))
                                          (expand-file-name "TVM_Codes" (projectile-project-root))
                                          (expand-file-name "tvm" (projectile-project-root))
                                          (expand-file-name "tvm/python" (projectile-project-root))
                                          (expand-file-name "tvm/python/tvm" (projectile-project-root))
                                          ))))
                           (setq lsp-pyright-extra-paths paths)))
                 (eval . (progn
                           (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                           (add-hook 'before-save-hook #'lsp-organize-imports nil t)
                           ))
                 ))

 (c++-mode . (
              (flycheck-gcc-language-standard   . "c++11")
              (flycheck-clang-language-standard . "c++11")
              (eval . (let (
                            (clang-args (list
                                         "-std=c++11"
                                         (concat "-I" (expand-file-name "" (projectile-project-root)))
                                         ))
                            (include-path (list
                                           (expand-file-name "" (projectile-project-root))
                                           ))
                            )
                        (setq-local company-clang-arguments     clang-args
                                    flycheck-clang-args         clang-args
                                    flycheck-gcc-args           clang-args
                                    flycheck-gcc-include-path   include-path
                                    flycheck-clang-include-path include-path)
                        ))
              ))

 )

