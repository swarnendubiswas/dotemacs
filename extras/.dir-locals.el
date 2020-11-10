;; (
;;  (nil .
;;       (eval . (lambda ()
;;                 (when (string= (file-name-nondirectory (buffer-name)) "wordlist")
;;                   (message "First eval form")
;;                   (text-mode)
;;                   (set-auto-mode)))

;;             (eval add-hook 'hack-local-variables-hook (lambda ()
;;                                                         (when (string= (file-name-directory (buffer-name)) "wordlist")
;;                                                           (message "Second eval form")
;;                                                           (text-mode))))
;;             ))
;;  )

;; ((nil
;;   (eval
;;    (lambda ()
;;      (when (string= (file-name-nondirectory buffer-file-name)
;;                     "wordlist")
;;        (message "Third eval form")
;;        (text-mode))))))
