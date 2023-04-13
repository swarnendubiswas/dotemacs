;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((elisp-autofmt-on-save-p . always)
                     (elisp-autofmt-load-packages-local . ("use-package"))
                     (no-byte-compile . t)
                     (sb/delete-trailing-whitespace-p . t)
                     ;; (eval . (progn
                     ;;           (format-all-mode 1)))
                     )))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
