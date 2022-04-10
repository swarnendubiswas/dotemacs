(
 (nil . (
         (eval . (when (and
                        (buffer-file-name) ; Ensure the buffer is visiting a file
                        (string= (file-name-nondirectory (buffer-name)) "wordlist.5"))
                   (when (or (not (derived-mode-p 'text-mode)) (derived-mode-p 'nroff-mode))
                     (make-local-variable 'lsp-disabled-clients)
                     (setq lsp-disabled-clients '(ltex-ls grammarly-ls))
                     (text-mode)
                     )))
         ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
