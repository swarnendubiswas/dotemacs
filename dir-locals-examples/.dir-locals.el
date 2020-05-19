(
 (nil
  (eval
   (lambda ()
     (when (string= (file-name-nondirectory buffer-file-name)
                    ".*-dir-locals.el")
       (flycheck-mode 0)))))

(emacs-lisp-mode . ((no-byte-compile . t)))
 )
