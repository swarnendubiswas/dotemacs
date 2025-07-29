((nil
  .
  ((eval .
         (when (and
                (buffer-file-name) ; Ensure the buffer is visiting a file
                (string= (file-name-nondirectory (buffer-name)) "wordlist.5"))
           (unless (and (derived-mode-p 'text-mode)
                        (not (derived-mode-p 'nroff-mode)))
             (text-mode))
           (when (boundp 'lsp-disabled-clients)
             (setq-local lsp-disabled-clients
                         (append '(ltex-ls-plus) lsp-disabled-clients))))))))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
