(
 (python-mode . (
                 (fill-column . 100)

                 (projectile-project-run-cmd . "python -m src")
                 ;; Do not prompt for command on `projectile-run-project'
                 (compilation-read-command . nil)

                 (python-shell-exec-path   . "/usr/bin/python3")
                 (python-shell-interpreter . "/usr/bin/python3")

                 (flycheck-pylintrc . "setup.cfg")

                 (py-isort-options . '("--settings-path=setup.cfg"))

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
                           (setq lsp-pyright-extra-paths paths
                                 lsp-pyright-venv-path paths)
                           ))

                 (eval . (lsp-deferred))

                 ;; We do not use `lsp-format-buffer' since `pyright' does not support document
                 ;; formatting.

                 ;; (eval . (add-hook 'before-save-hook
                 ;;                   #'lsp-format-buffer nil t))


                 ;; (eval . (require 'dap-python))
                 ;; (eval . (dap-register-debug-template <...>))

                 (eglot-workspace-configuration .
                                                ((:python
                                                  :venvPath ,(expand-absolute-name "~/.local/share/conda/envs"))
                                                 :analysis (:diagnosticMode
                                                            "openFilesOnly"
                                                            :stubPath
                                                            ,(expand-absolute-name "~/.local/lib/python-type-stubs"))
                                                 (:pylsp .
                                                         (:plugins
                                                          (:jedi_completion
                                                           (:fuzzy t)
                                                           (:include_params t)
                                                           :pydocstyle (:enabled nil)
                                                           :pycodestyle (:enabled nil)
                                                           :mccabe (:enabled nil)
                                                           :pyflakes (:enabled nil)
                                                           :flake8 (:enabled t)
                                                           :black (:enabled t)
                                                           :pylint (:enabled t))
                                                          :configurationSources ["flake8"]))))
                 ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
