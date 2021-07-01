;; Test functions

(defun sb/open-local-file-projectile (directory)
  "Open projectile file within DIRECTORY.
Specify by the keyword projectile-default-file define in `dir-locals-file'"
  (let ((default-file
          (f-join directory
                  (nth 1
                       (car (-tree-map (lambda (node)
                                         (when (eq (car node)
                                                   'dotemacs-projectile-default-file)
                                           (format "%s" (cdr node))))
                                       (dir-locals-get-class-variables (dir-locals-read-from-dir
                                                                        directory))))))))
    (if (f-exists? default-file)
        (counsel-find-file default-file)
      (message "The file %s doesn't exist in the select project" default-file))))

(defun sb/open-project-default-file1 (filepath)
  "Open projectile file with FILEPATH.
Specify by the keyword projectile-default-file define in `dir-locals-file'."
  (let ((liststring (with-temp-buffer
                      (insert-file-contents filepath)
                      (split-string (buffer-string) "\n"))))
    (mapcar (lambda (str)
              (when (cl-search "dotemacs-projectile-default-file" str)
                (let ((x (substring str (+
                                         13 (length "dotemacs-projectile-default-file")) (length
                                         str))))
                  (let ((default-file (expand-file-name (substring
                                                         x 1 -2) (projectile-project-root))))
                    (when (f-exists? default-file)
                      (let ((new-buffer (get-buffer-create default-file)))
                        (switch-to-buffer new-buffer)
                        (insert-file-contents default-file)))))))
            liststring)))
;; (sb/open-project-default-file1 "/home/swarnendu/.emacs.d/.dir-locals.el")

(defun sb/open-project-default-file2 ()
  "Open projectile file with FILEPATH.
Specify by the keyword projectile-default-file define in `dir-locals-file'."
  (interactive)
  (let ((mylist (dir-locals-get-class-variables (dir-locals-read-from-dir
                                                 (projectile-project-root)))))
    (mapcar (lambda (node)
              (when (eq (car node) nil)
                (let ((nillist (cdr node)))
                  (mapcar (lambda (elem)
                            (when (eq (car elem) 'dotemacs-projectile-default-file)
                              (let ((default-file (expand-file-name (cdr elem)
                                                                    (projectile-project-root))))
                                (when (f-exists? default-file)
                                  ;; (let ((new-buffer (get-buffer-create default-file)))
                                  ;;   (switch-to-buffer new-buffer)
                                  ;;   (insert-file-contents default-file))
                                  (find-file default-file)))))
                          nillist))))
            mylist)))
;; (sb/open-project-default-file2)

;; (with-eval-after-load "counsel-projectile"
;;   (add-to-list 'counsel-projectile-action '("d"
;;                                             sb/open-project-default-file2 "open default file") t))
