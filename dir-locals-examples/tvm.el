(
 (python-mode . (
                 ;; TODO: Update this as needed
                 (pyvenv-activate . "/home/swarnendu/tmp/virtualenvs/flextensor-venv")
                 (eval . (progn
                           (let ((paths
                                  (vconcat
                                   (list
                                    (expand-file-name "." (projectile-project-root))
                                    (expand-file-name "./python" (projectile-project-root))
                                    (expand-file-name "./python/tvm" (projectile-project-root))
                                    (expand-file-name "./python/tvm/relay/testing"
                                                      (projectile-project-root))
                                    (expand-file-name "./tests/python/unittest"
                                                      (projectile-project-root))

                                    ))))
                             (setq lsp-pyright-extra-paths paths))

                           ;; TODO: Pyright does not support formatting, add it to directories we
                           ;; will modify to reduce the diff

                           ;; (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                           ;; (add-hook 'before-save-hook #'lsp-organize-imports nil t)

                           (setenv "PYTHONPATH"
                                   (concat
                                    (expand-file-name "./python" (projectile-project-root))
                                    ":"
                                    (getenv "PYTHONPATH")))
                           ))
                 ))

 (c++-mode . (
              (flycheck-gcc-language-standard   . "c++11")
              (flycheck-clang-language-standard . "c++11")

              (eval . (progn
                        (let (
                              (clang-args
                               (list
                                "-std=c++11"
                                (concat "-I" (expand-file-name "" (projectile-project-root)))
                                ))
                              (include-path (list
                                             (expand-file-name "" (projectile-project-root))
                                             ))
                              )
                          (setq-local company-clang-arguments        clang-args
                                      flycheck-clang-args            clang-args
                                      flycheck-clang-include-path    include-path
                                      flycheck-gcc-args              clang-args
                                      flycheck-gcc-include-path      include-path
                                      flycheck-cppcheck-include-path include-path)
                          )
                        (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                        (add-to-list 'lsp-clients-clangd-args
                                     (concat "--compile-commands-dir="
                                             (expand-file-name "./" (projectile-project-root)))
                                     'append)
                        ))
              ))
 )

