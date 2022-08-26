(
 (nil . (
         (counsel-find-file-ignore-regexp
          . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|.o$\\|\\.clangd\\|^build")
         (projectile-project-compilation-dir . ".")
         (projectile-project-compilation-cmd .
                                             "mkdir -p build; cd build; cmake ..; cmake --build .;")
         (compile-command . "mkdir -p build; cd build; cmake ..; cmake --build .;")
         (eval . (progn
                   (add-to-list 'lsp-file-watch-ignored-directories "build")
                   ))
         ))

 (c++-mode . (
              (clang-format-style . "file")
              (flycheck-gcc-language-standard   . "c++14")
              (flycheck-clang-language-standard . "c++14")
              (flycheck-clang-tidy-build-path   . ".")
              (flycheck-gcc-openmp . t)

              ;; https://stackoverflow.com/questions/33063008/define-new-variable-in-dir-locals-el
              (eval . (progn
                        (let (
                              (clang-args (list
                                           "-std=c++14"
                                           ;; Using `(projectile-project-root)' will use the full
                                           ;; remote path
                                           "-I./src"
                                           ))
                              (include-path (list
                                             "./src"
                                             )))
                          (setq-local company-clang-arguments        clang-args
                                      flycheck-clang-args            clang-args
                                      flycheck-clang-include-path    include-path
                                      flycheck-gcc-args              clang-args
                                      flycheck-gcc-include-path      include-path
                                      flycheck-cppcheck-include-path include-path))
                        (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                        (add-to-list 'lsp-clients-clangd-args
                                     (concat "--compile-commands-dir="
                                             (expand-file-name "./build"))
                                     'append)
                        ))
              ))

 (cmake-mode . (
                (eval . (add-hook 'before-save-hook #'lsp-format-buffer nil t))
                ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
