(
 ((nil
  . ((eglot-workspace-configuration

      . (:pylsp (:plugins (:jedi_completion (:include_params t
                                             :fuzzy t)
                           :pylint (:enabled :json-false)))
         :gopls (:usePlaceholders t))))))
 (python-mode .
              ((eglot-workspace-configuration
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
                            (:jedi_completion (:fuzzy t
                                               :include_params t)
                             :pylsp_isort (:enabled t)
                             :pylsp_mypy (:enabled t)
                             :pydocstyle (:enabled :json-false)
                             :pycodestyle (:enabled :json-false)
                             :mccabe (:enabled :json-false)
                             :pyflakes (:enabled :json-false)
                             :flake8 (:enabled :json-false)
                             :black (:enabled :json-false)
                             :pylint (:enabled t)
                             :mypy (:enabled :json-false))
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
