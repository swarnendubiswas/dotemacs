(
 (emacs-lisp-mode . (
                     (no-byte-compile . t)
                     (sb/delete-trailing-whitespace-p . t)
                     (eval . (progn
                               (format-all-mode 1)
                               ;; (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
                               ))
                     ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
