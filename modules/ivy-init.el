;;; ivy-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup ivy mode for completion.

;;; Code:

(defvar dotemacs-selection)
(defvar recentf-list)

(use-package ivy
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :preface
  (defun dotemacs--ivy-recentf ()
    "Find a file on `recentf-list'."
    (interactive)
    (ivy-read "Recentf: " (mapcar #'abbreviate-file-name recentf-list)
              :action
              (lambda (f)
                (with-ivy-window
                  (find-file f)))
              :caller 'counsel-recentf))
  :config
  (setq ivy-use-virtual-buffers t ; When non-nil, add recentf-mode and bookmarks to ivy-switch-buffer completion
                                        ; candidates
        confirm-nonexistent-file-or-buffer t
        ivy-virtual-abbreviate 'name
        ivy-wrap t ; Useful to be able to wrap around boundary items
        ivy-action-wrap t
        ivy-case-fold-search t ; Ignore case while searching
        ivy-height 25 ; This seems a good number to see several options at a time
        ivy-fixed-height-minibuffer t ; It is distracting if the mini-buffer height keeps changing
        ivy-display-style 'fancy
        ivy-extra-directories nil ; Hide "." and ".."
        ivy-format-function 'ivy-format-function-line
        ;; ivy-count-format "(%d/%d) " ; There seems no added benefit
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)) ; ivy--regex-fuzzy adds noise
        ivy-flx-limit 200
        ;; Always ignore buffers set in ivy-ignore-buffers
        ivy-use-ignore-default 'always)
  (dolist (buffer '("^\\*Backtrace\\*$"
                    "^\\*Compile-Log\\*$"
                    "^\\*.+Completions\\*$"
                    "^\\*Help\\*$"
                    "^\\*Ibuffer\\*$"
                    "company-statistics-cache.el"))
    (add-to-list 'ivy-ignore-buffers buffer))
  (ivy-mode 1)
  (use-package ivy-hydra
    :ensure t)
  :bind
  (("C-c r" . ivy-resume)
   ("<f9>" . dotemacs--ivy-recentf)
   ("C-'" . ivy-avy)
   ([remap switch-to-buffer] . ivy-switch-buffer)
   ("<f3>" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<return>" . ivy-alt-done)
   ("<left>" . ivy-previous-line)
   ("<right>" . ivy-next-line))
  :diminish ivy-mode)

(use-package counsel
  :ensure t
  :after ivy
  :preface
  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
  (defun dotemacs-counsel-goto-recent-directory ()
    "Open recent directory with dired"
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
            (append (mapcar 'file-name-directory recentf-list)
                    ;; fasd history
                    (if (executable-find "fasd")
                        (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "directories:" collection :action 'dired)))
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap yank-pop] . counsel-yank-pop)
   ([remap describe-bindings] . counsel-descbinds)
   ([remap execute-extended-command] . counsel-M-x)
   ("<f1>" . counsel-M-x)
   ([remap find-file] . counsel-find-file)
   ("<f2>" . counsel-find-file)
   ([remap load-theme] . counsel-load-theme)
   ([remap load-library] . counsel-load-library)
   ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
   ("C-<f9>" . dotemacs-counsel-goto-recent-directory))
  :config
  (setq counsel-mode-override-describe-bindings t
        counsel-find-file-at-point nil
        counsel-find-file-ignore-regexp (concat
                                         "\\(?:\\`[#.]\\)" ; File names beginning with # or .
                                         "\\|\\(?:\\`.+?[#~]\\'\\)" ; File names ending with # or ~
                                         "\\|__pycache__"
                                         "\\|.aux$"
                                         "\\|.bbl$"
                                         "\\|.blg$"
                                         "\\|.elc$"
                                         "\\|.fdb_latexmk$"
                                         "\\|.fls$"
                                         "\\|.lof$"
                                         "\\|.log$"
                                         "\\|.lot$"
                                         "\\|.out$"
                                         "\\|.pdf$"
                                         "\\|.pyc$"
                                         "\\|.rel$"
                                         "\\|.rip$"
                                         "\\|.synctex.gz"
                                         "\\|.toc"))
  (counsel-mode 1)
  :diminish counsel-mode)

(provide 'ivy-init)

;;; ivy-init.el ends here
