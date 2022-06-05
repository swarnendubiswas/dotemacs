(
 (python-mode . (
                 (fill-column . 100)

                 (projectile-project-run-cmd . "python -m src")
                 ;; Do not prompt for command on `projectile-run-project'
                 (compilation-read-command . nil)

                 (python-shell-exec-path   . "/usr/bin/python3")
                 (python-shell-interpreter . "/usr/bin/python3")

                 (flycheck-pylintrc . "setup.cfg")

                 (lsp-pylsp-plugins-yapf-enable   . t)
                 (lsp-pylsp-plugins-pylint-enable . t)
                 ;; Default is jedi (slow and old)
                 (lsp-pylsp-rename-backend . 'rope)

                 (eval . (let (
                               (paths
                                (vconcat (list
                                          (expand-file-name "src")
                                          ))
                                )
                               )
                           (setq lsp-pyright-extra-paths paths)
                           ))

                 (eval . (lsp-deferred))
                 (eval . (add-hook 'before-save-hook
                                   #'lsp-format-buffer nil t))

                 (py-isort-options . '("--settings-path=setup.cfg"))

                 (eval . (require 'dap-python))
                 (eval . (dap-register-debug-template <...>))
                 ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
