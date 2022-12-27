(
 (python-mode . (
                 (eglot-workspace-configuration
                  .
                  ((:python .
                            (:pythonPath ".venv/bin/python")
                            (:venvPath (expand-absolute-name "~/.local/share/conda/envs"))
                            (:analysis (:diagnosticMode
                                        "openFilesOnly"
                                        :stubPath
                                        (expand-absolute-name "~/.local/lib/python-type-stubs"))))
                   (:pylsp .
                           (:plugins
                            (:jedi_completion
                             (:fuzzy t)
                             (:include_params t)
                             :pylsp_isort (:enabled t)
                             :pylsp_mypy (:enabled t)
                             :pydocstyle (:enabled nil)
                             :pycodestyle (:enabled nil)
                             :mccabe (:enabled nil)
                             :pyflakes (:enabled nil)
                             :flake8 (:enabled t)
                             :black (:enabled t)
                             :pylint (:enabled t))
                            :configurationSources ["setup.cfg"]))
                   ))))
 (org-mode . (
              (eglot-workspace-configuration
               .
               ((:ltex-ls .
                          (:language . "en")))
               )))
 (markdown-mode . (
                   (eglot-workspace-configuration
                    .
                    ((:@emacs-grammarly/grammarly-languageserver .
                                                                 (:audience . "knowledgeable")))
                    )))
 (java-mode .
            (eglot-workspace-configuration
             .
             ((:java .
                     (:format (:settings (:url "./style.xml" :profile "GoogleStyle")))))))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
