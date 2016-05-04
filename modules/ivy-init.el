;;; ivy-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup ivy mode as a replacement for ido.

;;; Code:

(use-package ivy
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :preface
  ;; https://github.com/abo-abo/oremacs/blob/github/oleh/modes/ora-ivy.el
  (defun dotemacs--ivy-dired ()
    (interactive)
    (if ivy--directory
        (ivy-quit-and-run
         (dired ivy--directory)
         (when (re-search-forward
                (regexp-quote
                 (substring ivy--current 0 -1)) nil t)
           (goto-char (match-beginning 0))))
      (user-error
       "Not completing files currently")))
  :config
  (setq ivy-use-virtual-buffers t ; When non-nil, add recentf-mode and bookmarks to ivy-switch-buffer completion
                                        ; candidates.
        ivy-virtual-abbreviate 'full ; Easier to distinguish files
        ivy-wrap t ; Useful to be able to wrap around boundary items
        ivy-case-fold-search t ; Ignore case while searching
        ivy-height 25 ; This seems a good number to see several options at a time
        ivy-fixed-height-minibuffer t ; It is distracting if the mini-buffer height keeps changing
        ivy-display-style 'fancy
        ivy-extra-directories nil ; Hide "." and ".."
        ivy-format-function 'ivy-format-function-arrow
        ;; ivy-count-format "(%d/%d) " ; There seems no added benefit
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)) ;; ivy--regex-fuzzy adds noise
        ivy-flx-limit 200)
  (dolist (buffer '("^\\*Backtrace\\*$"
                    "^\\*Compile-Log\\*$"
                    "^\\*.+Completions\\*$"
                    "^\\*Help\\*$"
                    "^\\*Ibuffer\\*$"))
    (add-to-list 'ivy-ignore-buffers buffer))
  (ivy-mode 1)
  :bind
  (("C-c r" . ivy-resume)
   ("<f9>" . ivy-recentf)
   ("C-'" . ivy-avy)
   ([remap switch-to-buffer] . ivy-switch-buffer)
   ("<f4>" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<return>" . ivy-alt-done)
   ("C-:" . ivy-dired)
   ("<left>" . ivy-previous-line)
   ("<right>" . ivy-next-line))
  :diminish ivy-mode)

(use-package counsel
  :ensure t
  :after ivy
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap yank-pop] . counsel-yank-pop)
   ([remap describe-bindings] . counsel-descbinds)
   ([remap execute-extended-command] . counsel-M-x)
   ("<f1>" . counsel-M-x)
   ([remap find-file] . counsel-find-file)
   ("<f3>" . counsel-find-file))
  :config
  (setq counsel-mode-override-describe-bindings t
        counsel-find-file-at-point nil)
  (setq counsel-find-file-ignore-regexp (concat
                                         "\\(?:\\`[#.]\\)" ; File names beginning with # or .
                                         "\\|\\(?:\\`.+?[#~]\\'\\)" ; File names ending with # or ~
                                         "\\|__pycache__"
                                         "\\|.aux$"
                                         "\\|.bbl$"
                                         "\\|.blg$"
                                         "\\|.elc$"
                                         "\\|.fdb_latexmk$"
                                         "\\|.fls$"
                                         "\\|.log$"
                                         "\\|.out$"
                                         "\\|.pyc$"
                                         "\\|.rel$"
                                         "\\|.synctex.gz"))
  (counsel-mode 1)
  :diminish counsel-mode)

(provide 'ivy-init)

;;; ivy-init.el ends here
