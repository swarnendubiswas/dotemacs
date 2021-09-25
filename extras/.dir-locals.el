(
 (nil . (
         (eval . (when (and
                        (buffer-file-name) ; Ensure the buffer is visiting a file
                        (string= (file-name-nondirectory (buffer-name)) "wordlist.5"))
                   (when (or (not (derived-mode-p 'text-mode)) (derived-mode-p 'nroff-mode))
                     (text-mode))))

         ))
 )

;; (
;;  (nil
;;   (eval
;;    when (string= (file-name-nondirectory buffer-file-name)
;;                  "wordlist.5")
;;    (text-mode))))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
