(
 (nil . (
         (counsel-find-file-ignore-regexp
          . "\\(?:\\`[#.]\\)\\|\\(?:\\`.+?[#~]\\'\\)\\|.o$\\|\\.clangd\\|^build")
         (projectile-project-compilation-dir . ".")
         (projectile-project-compilation-cmd . "bear -a make")
         (eval . (progn
                   (add-to-list 'lsp-file-watch-ignored-directories "build")
                   ))
         (eval . (when (and
                        (buffer-file-name) ; Ensure the buffer is visiting a file
                        (string= (file-name-nondirectory (buffer-name)) "setup_environment"))
                   (sh-mode)))
         ))

 (c++-mode . (
              (flycheck-gcc-language-standard   . "c++11")
              (flycheck-clang-language-standard . "c++11")
              (flycheck-clang-tidy-build-path   . ".")

              (eval . (progn
                        (let (
                              (clang-args (list
                                           "-std=c++11"
                                           ;; Using `(projectile-project-root)' will use the full
                                           ;; remote path
                                           "-I./src"
                                           ))
                              (include-path (list
                                             "src"
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
                                             (expand-file-name "." (projectile-project-root)))
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
