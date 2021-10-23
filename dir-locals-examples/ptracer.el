(
 (nil . (
         (projectile-enable-caching . t)
         (eval . (progn
                   (setq-local counsel-etags-project-root (projectile-project-root)
                               tags-table-files (list (projectile-project-root)))

                   (add-to-list 'lsp-file-watch-ignored-directories "obj")
                   (add-to-list 'lsp-file-watch-ignored-directories "build")
                   ))
         ))

 (c++-mode . (
              (flycheck-gcc-language-standard   . "c++11")
              (flycheck-clang-language-standard . "c++11")
              (flycheck-clang-tidy-build-path   . ".")

              ;; Not needed we are already setting it from `init.el'
              ;; (eval add-hook 'hack-local-variables-hook
              ;;       (lambda () (when (string= major-mode 'c++-mode) (lsp))))

              (eval . (progn
                        (let (
                              (clang-args (list
                                           "-std=c++11"
                                           (concat "-I" (expand-file-name "tdebug-llvm/llvm/include"
                                                                          (projectile-project-root)))
                                           (concat "-I" (expand-file-name "tdebug-llvm/clang/include"
                                                                          (projectile-project-root)))
                                           (concat "-I" (expand-file-name "tbb-lib/include"
                                                                          (projectile-project-root)))
                                           (concat "-I" (expand-file-name "tdebug-lib/include"
                                                                          (projectile-project-root)))
                                           (concat "-I" (expand-file-name "spd3-lib/include"
                                                                          (projectile-project-root)))
                                           (concat "-I" (expand-file-name "fasttrack/include"
                                                                          (projectile-project-root)))
                                           (concat "-I" (expand-file-name "newfasttrack/include"
                                                                          (projectile-project-root)))
                                           (concat "-I" (expand-file-name "new_algo/include"
                                                                          (projectile-project-root)))
                                           ))
                              (include-path (list
                                             (expand-file-name "tdebug-llvm/llvm/include"
                                                               (projectile-project-root))
                                             (expand-file-name "tdebug-llvm/clang/include"
                                                               (projectile-project-root))
                                             (expand-file-name "tbb-lib/include"
                                                               (projectile-project-root))
                                             (expand-file-name "tdebug-lib/include"
                                                               (projectile-project-root))
                                             (expand-file-name "spd3-lib/include"
                                                               (projectile-project-root))
                                             (expand-file-name "fasttrack/include"
                                                               (projectile-project-root))
                                             (expand-file-name "newfasttrack/include"
                                                               (projectile-project-root))
                                             (expand-file-name "new_algo/include"
                                                               (projectile-project-root))
                                             )))
                          (setq-local company-clang-arguments        clang-args
                                      flycheck-gcc-args              clang-args
                                      flycheck-gcc-include-path      include-path
                                      flycheck-clang-args            clang-args
                                      flycheck-clang-include-path    include-path
                                      flycheck-cppcheck-include-path include-path))

                        (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                        (add-to-list 'lsp-clients-clangd-args
                                     (concat "--compile-commands-dir="
                                             (expand-file-name "./" (projectile-project-root)))
                                     'append)
                        ))
              ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
