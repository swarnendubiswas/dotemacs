;; (
;;  (nil .
;;       (eval . (lambda ()
;;                 (when (string= (file-name-nondirectory (buffer-name)) "wordlist")
;;                   (message "First eval form")
;;                   (text-mode)
;;                   (set-auto-mode)))

((nil
  (eval
   (lambda ()
     (when (string= (file-name-nondirectory buffer-file-name)
                    "wordlist.5")
       (text-mode))))))
