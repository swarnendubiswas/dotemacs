;;; init-ivy.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(use-package ivy
  :functions ivy-format-function-line
  :commands (ivy-read)
  :if (eq sb/minibuffer-completion 'ivy)
  :preface
  ;; https://github.com/abo-abo/swiper/wiki/Hiding-dired-buffers
  (defun sb/ignore-dired-buffers (str)
    "Return non-nil if STR names a Dired buffer.
  This function is intended for use with `ivy-ignore-buffers'."
    (let ((buf (get-buffer str)))
      (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))
  :hook (after-init-hook . ivy-mode)
  :custom
  (ivy-count-format "(%d/%d) " "Helps identify wrap around")
  (ivy-extra-directories nil "Hide . and ..")
  (ivy-fixed-height-minibuffer t "Distracting if the height keeps changing")
  (ivy-height 12)
  ;; Make the height of the minibuffer proportionate to the screen
  ;; ivy-height-alist '((t
  ;;                      lambda (_caller)
  ;;                      (/ (frame-height) 2)))
  ;; We update `ivy-re-builders-alist' after loading `orderless'
  ;; ivy-re-builders-alist '((counsel-M-x       . ivy--regex-fuzzy)
  ;;                         (counsel-find-file . ivy--regex-fuzzy)
  ;;                         (t                 . ivy--regex-ignore-order))
  (ivy-truncate-lines nil) ; `counsel-flycheck' output gets truncated
  (ivy-wrap t)
  (ivy-initial-inputs-alist nil "Do not start searches with ^")
  (ivy-use-virtual-buffers nil "Do not show recent files in `switch-buffer'")
  ;; The default sorter is much too slow and the default for `ivy-sort-max-size' is way too
  ;; big (30,000). Turn it down so big repos affect project navigation less.
  (ivy-sort-max-size 10000)
  :config
  (dolist (buffer
           '("TAGS" "magit-process" "*emacs*" "*xref*"
             ;; "*eldoc for use-package*" "^\\*Help\\*$" "^\\*Ibuffer\\*$" "*Warnings*"
             ;; "^\\*Compile-Log\\*$" "^\\*.+Completions\\*$" "^\\*Backtrace\\*$"
             ;; "*flycheck-posframe-buffer*" "^\\*prettier" "^\\*json*" "^\\*texlab*"
             ;; "^\\*clangd*" "^\\*shfmt*" "*company-documentation*"
             ))
    (add-to-list 'ivy-ignore-buffers buffer))

  (setq ivy-re-builders-alist '((counsel-rg        . ivy--regex-plus)
                                (counsel-M-x       . ivy--regex-fuzzy)
                                (counsel-find-file . ivy--regex-fuzzy)
                                (t                 . ivy--regex-plus)))

  ;; Ignore `dired' buffers from `ivy-switch-buffer'
  ;; (add-to-list 'ivy-ignore-buffers #'sb/ignore-dired-buffers)
  :diminish
  :bind
  (("C-c r"    . ivy-resume)
   ("<f3>"     . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("<RET>"    . ivy-alt-done) ; Continue completion
   ("<left>"   . ivy-previous-line)
   ("<right>"  . ivy-next-line)))

(use-package counsel
  :if (eq sb/minibuffer-completion 'ivy)
  :preface
  ;; http://blog.binchen.org/posts/use-ivy-to-open-recent-directories.html
  (defun sb/counsel-goto-recent-directory ()
    "Open recent directories with `dired'."
    (interactive)
    (unless recentf-mode (recentf-mode 1))
    (let ((collection
           (delete-dups
            (append (mapcar 'file-name-directory recentf-list)
                    (if (executable-find "fasd")
                        (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
      (ivy-read "Directories:" collection :action 'dired)))
  :bind
  (;; Counsel can use the sorting from `amx' or `smex' for `counsel-M-x'.
   ([remap execute-extended-command] . counsel-M-x)
   ("<f1>"                           . counsel-M-x)
   ([remap completion-at-point]      . counsel-company)
   ("C-M-i"                          . counsel-company)
   ([remap find-file]                . counsel-find-file)
   ("<f2>"                           . counsel-find-file)
   ("<f9>"                           . counsel-recentf)
   ("C-<f9>"                         . sb/counsel-goto-recent-directory)
   ("C-c d m"                        . counsel-minor)
   ("C-c s g"                        . counsel-git)
   ("C-c s G"                        . counsel-git-grep)
   ("C-c s r"                        . counsel-rg)
   ("<f4>"                           . counsel-grep-or-swiper)
   ([remap locate]                   . counsel-locate)
   ("C-c s l"                        . counsel-locate)
   ([remap yank-pop]                 . counsel-yank-pop)
   ("M-y"                            . counsel-yank-pop)
   ("C-c C-m"                        . counsel-mark-ring)
   ;; Enabling preview can make switching over remote buffers slow
   ("S-<f3>"                         . counsel-switch-buffer)
   ([remap imenu]                    . counsel-imenu)
   ("C-c C-j"                        . counsel-imenu)
   ([remap bookmark-jump]            . counsel-bookmark)
   ([remap apropos]                  . counsel-apropos)
   ("M-g o"                          . counsel-outline)
   ([remap load-theme]               . counsel-theme)
   ([remap load-library]             . counsel-load-library))
  :diminish
  :hook (ivy-mode-hook . counsel-mode)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (concat
                                    "\\(?:\\`[#.]\\)"
                                    "\\|\\(?:\\`.+?[#~]\\'\\)"
                                    "\\|.cb$"
                                    "\\|.cb2$"
                                    "\\|.class$"
                                    "\\|.djvu$"
                                    "\\|.doc$"
                                    "\\|.docx$"
                                    "\\|.elc$"
                                    "\\|.fdb_latexmk$"
                                    "\\|.fls$"
                                    "\\|.lof$"
                                    "\\|.lot$"
                                    "\\|.o$"
                                    "\\|.ppt$"
                                    "\\|.pptx$"
                                    "\\|.pyc$"
                                    "\\|.rel$"
                                    "\\|.rip$"
                                    "\\|.so$"
                                    "\\|.synctex$"
                                    "\\|.synctex.gz$"
                                    "\\|.toc$"
                                    "\\|.xls$"
                                    "\\|.xlsx$"
                                    "\\|tags"
                                    "\\|TAGS"
                                    "\\|GPATH"
                                    "\\|GRTAGS"
                                    "\\|GTAGS"
                                    "\\|tramp"
                                    "\\|.clangd"
                                    "\\|.cache"
                                    "\\|.metadata"
                                    "\\|.recommenders"
                                    "\\|typings"
                                    "\\|__pycache__"))
  (counsel-mode-override-describe-bindings t)
  (counsel-preselect-current-file t)
  (counsel-switch-buffer-preview-virtual-buffers nil "Removes recent files and bookmarks")
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n------------------------------------------\n"))

;; Enable before `ivy-rich-mode' for better performance. The new transformers (file permissions)
;; seem an overkill, and it hides long file names.
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode-hook . all-the-icons-ivy-rich-mode)
  :custom
  (all-the-icons-ivy-rich-icon-size 0.9)
  :config
  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-recentf
             '(:columns
               ((all-the-icons-ivy-rich-file-icon)
                (all-the-icons-ivy-rich-file-name (:width 0.70))
                (all-the-icons-ivy-rich-file-id (:width 10
                                                        :face all-the-icons-ivy-rich-file-owner-face
                                                        :align right))
                (ivy-rich-file-last-modified-time (:face all-the-icons-ivy-rich-time-face)))
               :delimiter "\t"))

  (plist-put all-the-icons-ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((all-the-icons-ivy-rich-file-icon)
                (all-the-icons-ivy-rich-file-name (:width 0.4))
                (all-the-icons-ivy-rich-file-id (:width 15
                                                        :face all-the-icons-ivy-rich-file-owner-face
                                                        :align right)))
               :delimiter "\t"))

  (plist-put
   all-the-icons-ivy-rich-display-transformers-list
   'ivy-switch-buffer
   '(:columns
     ((all-the-icons-ivy-rich-buffer-icon)
      (ivy-rich-candidate (:width 30))
      (ivy-rich-switch-buffer-indicators (:width 4
                                                 :face all-the-icons-ivy-rich-indicator-face
                                                 :align right))
      (all-the-icons-ivy-rich-switch-buffer-major-mode
       (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
      (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
      (ivy-rich-switch-buffer-path (:width
                                    (lambda (x)
                                      (ivy-rich-switch-buffer-shorten-path
                                       x
                                       (ivy-rich-minibuffer-width 0.3)))
                                    :face all-the-icons-ivy-rich-path-face)))
     :predicate (lambda (cand) (get-buffer cand))
     :delimiter "\t"))

  (with-eval-after-load "projectile"
    (plist-put
     all-the-icons-ivy-rich-display-transformers-list
     'projectile-completing-read
     '(:columns
       ((all-the-icons-ivy-rich-file-icon)
        (all-the-icons-ivy-rich-project-find-file-transformer (:width 0.4))
        (all-the-icons-ivy-rich-project-file-id (:width 15 :face
                                                        all-the-icons-ivy-rich-file-owner-face
                                                        :align right)))
       :delimiter "\t"))))

(use-package ivy-rich
  :commands (ivy-rich-mode ivy-rich-modify-column
                           ivy-rich-set-columns ivy-rich-modify-columns
                           ivy-format-function-line)
  :after (ivy counsel)
  :preface
  ;; Adapted from
  ;; https://github.com/tshu-w/.emacs.d/blob/master/lisp/editor-completion.el
  (defun sb/ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
           ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
           ((> size 1000) (format "%.1fk " (/ size 1000.0)))
           (t (format "%d " size)))))))

  (defun sb/ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
          ""
        (let* ((user-id (file-attribute-user-id (file-attributes candidate)))
               (user-name (user-login-name user-id)))
          (format "%s" user-name)))))
  :init (ivy-rich-mode 1)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

  (ivy-rich-project-root-cache-mode 1)

  ;; (if (display-graphic-p)
  ;;     (ivy-rich-set-columns 'counsel-find-file
  ;;                           '((all-the-icons-ivy-rich-file-icon)
  ;;                             (ivy-rich-candidate    (:width 0.70))
  ;;                             (sb/ivy-rich-file-size (:width 10 :align right
  ;;                                                            :face font-lock-doc-face))))
  ;;   (ivy-rich-set-columns 'counsel-find-file
  ;;                         '((ivy-rich-candidate    (:width 0.70))
  ;;                           (sb/ivy-rich-file-size (:width 10 :align right
  ;;                                                          :face font-lock-doc-face)))))

  ;; ;; Increase the width to see the major mode clearly
  ;; (ivy-rich-modify-columns 'ivy-switch-buffer
  ;;                          '((ivy-rich-switch-buffer-size (:align right))
  ;;                            (ivy-rich-switch-buffer-major-mode (:width 16 :face error))
  ;;                            (ivy-rich-switch-buffer-project (:width 0.24 :face success))))

  ;; (ivy-rich-set-columns 'counsel-recentf
  ;;                       '((file-name-nondirectory (:width 0.24))
  ;;                         (ivy-rich-candidate (:width 0.75))))
  )

(provide 'init-ivy)

;;; init-ivy.el ends here
