(
 (python-mode . (
                 (flycheck-pylintrc . "setup.cfg")
                 (lsp-pylsp-plugins-yapf-enable   . t)
                 (lsp-pylsp-plugins-pylint-enable . t)
                 (python-shell-exec-path   . "/usr/bin/python3")
                 (python-shell-interpreter . "/usr/bin/python3")
                 (eval . (let (
                               (paths
                                (vconcat (list
                                          (expand-file-name "src" (projectile-project-root))
                                          ))
                                )
                               )
                           (setq lsp-pyright-extra-paths paths)
                           ))
                 (py-isort-options . '("--settings-path=setup.cfg"))
                 ))
 )
