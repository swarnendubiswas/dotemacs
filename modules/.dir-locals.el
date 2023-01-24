(
 (emacs-lisp-mode . (
                     (no-byte-compile . t)
                     (eval . (progn
                           (format-all-mode 1)
                           (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))
                     ;; The special `subdirs' element is not a variable, but a special keyword which
                     ;; indicates that the mode settings are only to be applied in the current
                     ;; directory, not in any subdirectories.
                     (subdirs . nil)
                     ))
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
