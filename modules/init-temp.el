;;; init-temp.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/custom-file)
(defvar sb/private-file)

;; Mark safe variables

(put 'bibtex-completion-bibliography          'safe-local-variable #'listp)
(put 'company-bibtex-bibliography             'safe-local-variable #'listp)
(put 'company-clang-arguments                 'safe-local-variable #'listp)
(put 'counsel-find-file-ignore-regexp         'safe-local-variable #'stringp)
(put 'flycheck-checker                        'safe-local-variable #'listp)
(put 'flycheck-clang-include-path             'safe-local-variable #'listp)
(put 'flycheck-gcc-include-path               'safe-local-variable #'listp)
(put 'flycheck-python-pylint-executable       'safe-local-variable #'stringp)
(put 'lsp-clients-clangd-args                 'safe-local-variable #'listp)
(put 'lsp-latex-root-directory                'safe-local-variable #'stringp)
(put 'lsp-pyright-extra-paths                 'safe-local-variable #'listp)
(put 'projectile-enable-caching               'safe-local-variable #'stringp)
(put 'projectile-globally-ignored-directories 'safe-local-variable #'listp)
(put 'projectile-project-root                 'safe-local-variable #'stringp)
(put 'pyvenv-activate                         'safe-local-variable #'stringp)
(put 'reftex-default-bibliography             'safe-local-variable #'listp)
(put 'tags-table-list                         'safe-local-variable #'listp)

(setq custom-file sb/custom-file)

(let ((gc-cons-threshold most-positive-fixnum))
  (when (file-exists-p custom-file)
    (load custom-file 'noerror 'nomessage))
  (when (file-exists-p sb/private-file)
    (load sb/private-file 'noerror 'nomessage)))

;; (defun sb/open-local-file-projectile (directory)
;;   "Open projectile file within DIRECTORY.
;; Specify by the keyword projectile-default-file define in `dir-locals-file'"
;;   (let ((default-file
;;           (f-join directory
;;                   (nth 1
;;                        (car (-tree-map (lambda (node)
;;                                          (when (eq (car node)
;;                                                    'dotemacs-projectile-default-file)
;;                                            (format "%s" (cdr node))))
;;                                        (dir-locals-get-class-variables (dir-locals-read-from-dir
;;                                                                         directory))))))))
;;     (if (f-exists? default-file)
;;         (counsel-find-file default-file)
;;       (message "The file %s doesn't exist in the select project" default-file))))

;; (defun sb/open-project-default-file1 (filepath)
;;   "Open projectile file with FILEPATH.
;; Specify by the keyword projectile-default-file define in `dir-locals-file'."
;;   (let ((liststring (with-temp-buffer
;;                       (insert-file-contents filepath)
;;                       (split-string (buffer-string) "\n"))))
;;     (mapcar (lambda (str)
;;               (when (cl-search "dotemacs-projectile-default-file" str)
;;                 (let ((x (substring str (+
;;                                          13 (length "dotemacs-projectile-default-file")) (length
;;                                          str))))
;;                   (let ((default-file (expand-file-name (substring
;;                                                          x 1 -2) (projectile-project-root))))
;;                     (when (f-exists? default-file)
;;                       (let ((new-buffer (get-buffer-create default-file)))
;;                         (switch-to-buffer new-buffer)
;;                         (insert-file-contents default-file)))))))
;;             liststring)))

;; (sb/open-project-default-file1 "/home/swarnendu/.emacs.d/.dir-locals.el")

;; (defun sb/open-project-default-file2 ()
;;   "Open projectile file with FILEPATH.
;; Specify by the keyword projectile-default-file define in `dir-locals-file'."
;;   (interactive)
;;   (let ((mylist (dir-locals-get-class-variables (dir-locals-read-from-dir
;;                                                  (projectile-project-root)))))
;;     (mapcar (lambda (node)
;;               (when (eq (car node) nil)
;;                 (let ((nillist (cdr node)))
;;                   (mapcar (lambda (elem)
;;                             (when (eq (car elem) 'dotemacs-projectile-default-file)
;;                               (let ((default-file (expand-file-name (cdr elem)
;;                                                                     (projectile-project-root))))
;;                                 (when (f-exists? default-file)
;;                                   ;; (let ((new-buffer (get-buffer-create default-file)))
;;                                   ;;   (switch-to-buffer new-buffer)
;;                                   ;;   (insert-file-contents default-file))
;;                                   (find-file default-file)))))
;;                           nillist))))
;;             mylist)))

;; (sb/open-project-default-file2)

;; (with-eval-after-load "counsel-projectile"
;;   (add-to-list 'counsel-projectile-action '("d"
;;     sb/open-project-default-file2 "open default file") t))

(provide 'init-temp)

;;; init-temp.el ends here
