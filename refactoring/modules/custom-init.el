;;; custom-init.el --- Part of emacs initialization

;;; Commentary:
;; Contains custom or temporary defintions.

;;; Code:

;; custom functions

;; kill all non-special buffers but the current one
(defun kill-other-buffers ()
  "Kill all buffers but the current one. Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

;; http://endlessparentheses.com/new-in-emacs-25-1-comment-line.html
(defun comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

;; tags

(use-package ctags
  :ensure t
  :defer t)

(use-package ctags-update
  :ensure t
  :defer t)

;; create tags for a latex project, no need to setup a keybinding
;; http://stackoverflow.com/questions/548414/how-to-programmatically-create-update-a-tags-file-with-emacs
(defun create-latex-etags ()
  "Create etags for the current latex project."
  (interactive)
  (compile "find . -name \"*.tex\" -print | etags -")
  )
(defun create-latex-ctags () ; (dir-name))
  "Create ctags for the current latex project."
  ;;(interactive "DDirectory: ")
  ;; (shell-command
  ;;  (format "ctags -o TAGS -R *.tex %s" (directory-file-name dir-name)))
  (interactive)
  ;;(compile "find . -name \"*.tex\" -print | ctags -a -u -o TAGS -")
  (compile "find . -name \"*.tex\" -print | xargs ctags -o TAGS")
  )

(provide 'custom-init)

;;; custom-init.el ends here
