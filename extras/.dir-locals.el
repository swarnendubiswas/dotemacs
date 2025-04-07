((nil
  .
  ((eval .
         (when (and
                (buffer-file-name) ; Ensure the buffer is visiting a file
                (string= (file-name-nondirectory (buffer-name)) "wordlist.5"))
           (when (or (not (derived-mode-p 'text-mode))
                     (derived-mode-p 'nroff-mode))
             (setq-local lsp-disabled-clients '(ltex-ls-plus))
             (text-mode)))))))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
