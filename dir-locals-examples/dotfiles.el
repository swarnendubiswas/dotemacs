;; ((nil
;;   (eval
;;    (lambda ()
;;      (when (string= (file-name-nondirectory buffer-file-name)
;;                     "dotprettierrc")
;;        (json-mode))))))

(
 (nil . (
         ;; (mode . whitespace-cleanup)

         (eval . (add-hook 'lsp-managed-mode-hook
                           (lambda ()
                             (when (derived-mode-p 'markdown-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (markdown-markdownlint-cli)))))))

                             (when (derived-mode-p 'sh-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (sh-shellcheck)))))))

                             (when (derived-mode-p 'yaml-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (yaml-yamllint)))))))

                             (when (derived-mode-p 'json-mode)
                               (setq sb/flycheck-local-cache '((lsp . ((next-checkers . (json-jsonlint)))))))
                             )))
         ))
 )

