;;; ivy-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup ivy mode for completion.

;;; Code:

(defvar dotemacs-selection)
(defvar recentf-list)
(defvar dotemacs-temp-directory)
(defvar savehist-additional-variables)

(use-package ivy
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :preface
  ;; https://github.com/abo-abo/swiper/wiki/Sort-files-by-mtime
  (defun eh-ivy-return-recentf-index (dir)
    (when (and (boundp 'recentf-list)
               recentf-list)
      (let ((files-list
             (cl-subseq recentf-list
                        0 (min (- (length recentf-list) 1) 20)))
            (index 0))
        (while files-list
          (if (string-match-p dir (car files-list))
              (setq files-list nil)
            (setq index (+ index 1))
            (setq files-list (cdr files-list))))
        index)))

  (defun eh-ivy-sort-file-function (x y)
    (let* ((x (concat ivy--directory x))
           (y (concat ivy--directory y))
           (x-mtime (nth 5 (file-attributes x)))
           (y-mtime (nth 5 (file-attributes y))))
      (if (file-directory-p x)
          (if (file-directory-p y)
              (let ((x-recentf-index (eh-ivy-return-recentf-index x))
                    (y-recentf-index (eh-ivy-return-recentf-index y)))
                (if (and x-recentf-index y-recentf-index)
                    ;; Directories is sorted by `recentf-list' index
                    (< x-recentf-index y-recentf-index)
                  (string< x y)))
            t)
        (if (file-directory-p y)
            nil
          ;; File is sorted by mtime
          (time-less-p y-mtime x-mtime)))))
  :config
  (setq ivy-use-virtual-buffers nil ; Add recent files and bookmarks to ivy-switch-buffer completion candidates
        confirm-nonexistent-file-or-buffer t
        ivy-virtual-abbreviate 'full
        ivy-wrap t ; Useful to be able to wrap around boundary items
        ivy-action-wrap t
        ivy-case-fold-search 'always ; Always ignore case while searching
        ivy-height 20 ; This seems a good number to see several options at a time without cluttering the view
        ivy-fixed-height-minibuffer t ; It is distracting if the mini-buffer height keeps changing
        ivy-display-style 'fancy
        ivy-extra-directories nil ; Hide "." and ".."
        ivy-format-function 'ivy-format-function-arrow
        ivy-count-format "(%d/%d) " ; This is beneficial to identify wrap arounds
        ivy-re-builders-alist '((counsel-find-file . ivy--regex-fuzzy)
                                (read-file-name-internal . ivy--regex-ignore-order)
                                (swiper . ivy--regex-plus)
                                (counsel-rg . ivy--regex-plus)
                                (counsel-grep-or-swiper . ivy--regex-plus)
                                (ivy-switch-buffer . ivy--regex-plus)
                                (t . ivy--regex-ignore-order))
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
  (add-to-list 'ivy-sort-functions-alist
               '(read-file-name-internal . eh-ivy-sort-file-function))
  ;; https://oremacs.com/2017/04/09/ivy-0.9.0/
  (setq ivy-switch-buffer-faces-alist
        '((emacs-lisp-mode . swiper-match-face-1)
          (dired-mode . ivy-subdir)
          (org-mode . org-level-4)))
  (ivy-mode 1)
  :bind
  (("C-c r" . ivy-resume)
   ("C-'" . ivy-avy)
   ([remap switch-to-buffer] . ivy-switch-buffer)
   ("<f3>" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<return>" . ivy-alt-done) ; Continue completion
   ("C-j" . ivy-immediate-done) ; View the current directory
   ("<left>" . ivy-previous-line)
   ("<right>" . ivy-next-line)
   ;; http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/
   ("M-y" . ivy-next-line))
  :diminish ivy-mode)

(use-package smex
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :config
  (setq smex-save-file (concat dotemacs-temp-directory "smex-items")
        smex-auto-update t)
  (smex-initialize))

(use-package counsel
  :ensure t
  :ensure ivy
  :after ivy
  :ensure smex
  :preface
  (defun dotemacs--counsel-recentf ()
    "Find a file on `recentf-list' and abbreviate the home directory."
    (interactive)
    (ivy-read "Recentf: " (mapcar #'abbreviate-file-name recentf-list)
              :action
              (lambda (f)
                (with-ivy-window
                  (find-file f)))
              :caller 'counsel-recentf))

  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
  (defun dotemacs--counsel-goto-recent-directory ()
    "Open recent directories with dired."
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
            (append (mapcar 'file-name-directory recentf-list)
                    ;; fasd history
                    (if (executable-find "fasd")
                        (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "Directories:" collection :action 'dired)))
  :bind
  (([remap describe-function] . counsel-describe-function)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap yank-pop] . counsel-yank-pop)
   ([remap describe-bindings] . counsel-descbinds)
   ;; counsel-M-x uses smex, I use amx
   ;; ([remap execute-extended-command] . counsel-M-x)
   ;; ("<f1>" . counsel-M-x)
   ([remap find-file] . counsel-find-file)
   ("<f2>" . counsel-find-file)
   ([remap load-theme] . counsel-load-theme)
   ([remap load-library] . counsel-load-library)
   ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
   ([remap completion-at-point] . counsel-company)
   ("<f9>" . dotemacs--counsel-recentf)
   ("C-<f9>" . dotemacs--counsel-goto-recent-directory)
   ("C-c s a" . counsel-ag)
   ("C-c s g" . counsel-git-grep) ; Shows only the first 200 results, use "C-c C-o" to save all the matches to a buffer.
   ("C-c s r" . counsel-rg)
   ("<f4>" . counsel-grep-or-swiper)
   ("C-c C-m" . counsel-mark-ring)
   ("C-c C-j" . counsel-imenu))
  :config
  (setq counsel-mode-override-describe-bindings t
        counsel-grep-swiper-limit 1000000 ; Number of characters in the buffer
        counsel-find-file-at-point nil
        counsel-yank-pop-separator "\n-----------------\n"
        counsel-find-file-ignore-regexp (concat
                                         "\\(?:\\`[#.]\\)" ; File names beginning with # or .
                                         "\\|\\(?:\\`.+?[#~]\\'\\)" ; File names ending with # or ~
                                         "\\|__pycache__"
                                         "\\|.aux$"
                                         "\\|.bbl$"
                                         "\\|.blg$"
                                         "\\|.cb$"
                                         "\\|.cb2$"
                                         "\\|.dvi$"
                                         "\\|.elc$"
                                         "\\|.fdb_latexmk$"
                                         "\\|.fls$"
                                         "\\|.lof$"
                                         "\\|.lot$"
                                         "\\|.o$"
                                         "\\|.out$"
                                         "\\|.pdf$"
                                         "\\|.pyc$"
                                         "\\|.rel$"
                                         "\\|.rip$"
                                         "\\|.synctex.gz$"
                                         "\\|.toc$"))
  (counsel-mode 1)
  :diminish counsel-mode)

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (setq ivy-rich-switch-buffer-align-virtual-buffer t
        ivy-rich-abbreviate-paths t
        ivy-rich-path-style 'relative
        ivy-rich-switch-buffer-name-max-length 48
        ivy-rich-switch-buffer-project-max-length 32)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(use-package ivy-historian
  :ensure t
  :after ivy
  :config
  (use-package historian
    :ensure t
    :config (setq historian-save-file (concat dotemacs-temp-directory "historian")))
  (ivy-historian-mode 1))

(use-package ivy-dired-history
  :ensure t
  :after ivy
  :after savehist
  :config (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))

(provide 'ivy-init)

;;; ivy-init.el ends here
