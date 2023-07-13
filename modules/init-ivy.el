;;; init-ivy.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar recentf-list)
(defvar sb/minibuffer-completion)

;; `amx-major-mode-commands' limits to commands that are relevant to the current major mode,
;; `amx-show-unbound-commands' shows frequently used commands that have no key bindings.

(use-package amx ; Sort most-used commands and show keyboard shortcuts
  :hook (emacs-startup-hook . amx-mode))

(use-package ivy
  :if (eq sb/minibuffer-completion 'ivy)
  :functions ivy-format-function-line
  :hook (emacs-startup-hook . ivy-mode)
  :bind
  (("C-c r" . ivy-resume)
    ("<f3>" . ivy-switch-buffer)
    :map ivy-minibuffer-map
    ("<RET>" . ivy-alt-done) ; Continue completion
    ("<left>" . ivy-previous-line) ("<right>" . ivy-next-line))
  :custom
  (ivy-count-format "(%d/%d) " "Helps identify wrap around")
  (ivy-extra-directories nil "Hide . and ..")
  (ivy-fixed-height-minibuffer t "Distracting if the height keeps changing")
  ;; Make the height of the minibuffer proportionate to the screen
  ;; (ivy-height-alist '((t lambda (_caller) (/ (frame-height) 2))))
  (ivy-height-alist
    '
    ((counsel-M-x . 10)
      (counsel-switch-buffer . 10)
      (counsel-yank-pop . 15)
      (swiper . 15)
      (swiper-isearch . 15)))
  (ivy-truncate-lines t) ; NOTE: `counsel-flycheck' output gets truncated
  (ivy-wrap t "Easy to navigate")
  (ivy-initial-inputs-alist nil "Do not start searches with ^")
  (ivy-use-virtual-buffers nil "Do not show recent files in `switch-buffer'")
  (ivy-use-selectable-prompt t "Easier to edit names with common prefixes")
  (ivy-sort-max-size 50000 "Increase the limit to allow sorting.")
  :config
  (dolist
    (buffer
      '
      ("TAGS" "magit-process" "*emacs*" "*xref*" "^\\*.+Completions\\*$" "^\\*Compile-Log\\*$"
        ;; "*eldoc for use-package*" "^\\*Help\\*$" "^\\*Ibuffer\\*$" "*Warnings*"
        ;;   "^\\*Backtrace\\*$"
        ;; "*flycheck-posframe-buffer*" "^\\*prettier" "^\\*json*" "^\\*texlab*"
        ;; "^\\*clangd*" "^\\*shfmt*" "*company-documentation*"
        ))
    (add-to-list 'ivy-ignore-buffers buffer))

  ;; ;; Other options: ivy--regex-ignore-order
  ;; (setq ivy-re-builders-alist '((counsel-rg        . ivy--regex-plus)
  ;;                               (counsel-M-x       . ivy--regex-fuzzy)
  ;;                               (counsel-find-file . ivy--regex-fuzzy)
  ;;                               (t                 . ivy--regex-plus)))

  ;; Ignore `dired' buffers from `ivy-switch-buffer'
  ;; https://github.com/abo-abo/swiper/wiki/Hiding-dired-buffers

  (defun sb/ignore-dired-buffers (str)
    "Return non-nil if STR names a Dired buffer.
  This function is intended for use with `ivy-ignore-buffers'."
    (let ((buf (get-buffer str)))
      (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

  ;; (add-to-list 'ivy-ignore-buffers #'sb/ignore-dired-buffers)

  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'ivy-views))
  :diminish)

;; (use-package all-the-icons-ivy
;;   :if (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p))
;;   :after ivy
;;   :hook (emacs-startup-hook . all-the-icons-ivy-setup))

(use-package counsel
  :preface
  ;; https://emacs.stackexchange.com/questions/33332/recursively-list-all-files-and-sub-directories
  (defun sb/counsel-all-files-recursively (dir-name)
    "List all files recursively in DIR-NAME."
    (interactive "DDirectory: ")
    (let*
      ((cands (split-string (shell-command-to-string (format "find %s -type f" dir-name)) "\n" t)))
      (ivy-read "File: " cands :action #'find-file :caller 'sb/counsel-all-files-recursively)))
  :if (eq sb/minibuffer-completion 'ivy)
  :hook (ivy-mode-hook . counsel-mode)
  :bind
  ( ;; Counsel can use the sorting from `amx' or `smex' for `counsel-M-x'.
    ([remap execute-extended-command] . counsel-M-x)
    ("<f1>" . counsel-M-x)
    ([remap completion-at-point] . counsel-company)
    ("C-M-i" . counsel-company)
    ([remap find-file] . counsel-find-file)
    ("<f2>" . counsel-find-file)
    ([remap dired] . counsel-dired)
    ([remap recentf-open-files] . counsel-recentf)
    ("<f9>" . counsel-recentf)
    ("C-c d m" . counsel-minor)
    ("C-c s g" . counsel-git)
    ("C-c s G" . counsel-git-grep)
    ("C-c s r" . counsel-rg)
    ("<f4>" . counsel-grep-or-swiper)
    ([remap locate] . counsel-locate)
    ("C-c s l" . counsel-locate)
    ([remap yank-pop] . counsel-yank-pop)
    ("M-y" . counsel-yank-pop)
    ("C-c C-m" . counsel-mark-ring)
    ("<f3>" . counsel-switch-buffer)
    ("S-<f3>" . counsel-switch-buffer)
    ([remap imenu] . counsel-imenu)
    ("C-c C-j" . counsel-imenu)
    ([remap bookmark-jump] . counsel-bookmark)
    ([remap apropos] . counsel-apropos)
    ("M-g o" . counsel-outline)
    ([remap load-theme] . counsel-theme)
    ([remap load-library] . counsel-load-library)
    ("C-x j" . sb/counsel-all-files-recursively)
    ([remap compile] . counsel-compile))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  (counsel-find-file-at-point t)
  (counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  (counsel-mode-override-describe-bindings t)
  (counsel-preselect-current-file t)
  (counsel-switch-buffer-preview-virtual-buffers nil)
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n---------------------------------------------------\n")
  :config
  (with-eval-after-load "savehist"
    (add-to-list 'savehist-additional-variables 'counsel-compile-history))
  :diminish)

;; Enable before `ivy-rich-mode' for better performance. The new transformers (file permissions)
;; seem an overkill, and it hides long file names.

;; (use-package all-the-icons-ivy-rich
;;   :hook (ivy-mode-hook . all-the-icons-ivy-rich-mode)
;;   :custom
;;   (all-the-icons-ivy-rich-icon nil "Disable icons")
;;   (all-the-icons-ivy-rich-icon-size 0.9)
;;   :config
;;   (plist-put
;;     all-the-icons-ivy-rich-display-transformers-list 'counsel-recentf
;;     '
;;     (:columns
;;       ((all-the-icons-ivy-rich-file-icon)
;;         (all-the-icons-ivy-rich-file-name (:width 0.70))
;;         (all-the-icons-ivy-rich-file-id
;;           (:width 10 :face all-the-icons-ivy-rich-file-owner-face :align right))
;;         (ivy-rich-file-last-modified-time (:face all-the-icons-ivy-rich-time-face)))
;;       :delimiter "\t"))

;;   (plist-put
;;     all-the-icons-ivy-rich-display-transformers-list 'counsel-find-file
;;     '
;;     (:columns
;;       ((all-the-icons-ivy-rich-file-icon)
;;         (all-the-icons-ivy-rich-file-name (:width 0.4))
;;         (all-the-icons-ivy-rich-file-id
;;           (:width 15 :face all-the-icons-ivy-rich-file-owner-face :align right)))
;;       :delimiter "\t"))

;;   (plist-put
;;     all-the-icons-ivy-rich-display-transformers-list 'ivy-switch-buffer
;;     '
;;     (:columns
;;       ((all-the-icons-ivy-rich-buffer-icon)
;;         (ivy-rich-candidate (:width 30))
;;         (ivy-rich-switch-buffer-indicators
;;           (:width 4 :face all-the-icons-ivy-rich-indicator-face :align right))
;;         (all-the-icons-ivy-rich-switch-buffer-major-mode
;;           (:width 18 :face all-the-icons-ivy-rich-major-mode-face))
;;         (ivy-rich-switch-buffer-project (:width 0.12 :face all-the-icons-ivy-rich-project-face))
;;         (ivy-rich-switch-buffer-path
;;           (:width
;;             (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))
;;             :face all-the-icons-ivy-rich-path-face)))
;;       :predicate (lambda (cand) (get-buffer cand))
;;       :delimiter "\t"))

;;   (with-eval-after-load "projectile"
;;     (plist-put
;;       all-the-icons-ivy-rich-display-transformers-list 'projectile-completing-read
;;       '
;;       (:columns
;;         ((all-the-icons-ivy-rich-file-icon)
;;           (all-the-icons-ivy-rich-project-find-file-transformer (:width 0.4))
;;           (all-the-icons-ivy-rich-project-file-id
;;             (:width 15 :face all-the-icons-ivy-rich-file-owner-face :align right)))
;;         :delimiter "\t"))))

(use-package nerd-icons-ivy-rich
  :hook (ivy-mode-hook . nerd-icons-ivy-rich-mode)
  :custom
  (nerd-icons-ivy-rich-icon nil "Disable icons")
  (nerd-icons-ivy-rich-icon-size 0.9)
  :config
  (plist-put
    nerd-icons-ivy-rich-display-transformers-list 'counsel-recentf
    '
    (:columns
      ((nerd-icons-ivy-rich-file-icon)
        (nerd-icons-ivy-rich-file-name (:width 0.75))
        (nerd-icons-ivy-rich-file-id
          (:width 10 :face nerd-icons-ivy-rich-file-owner-face :align right))
        (ivy-rich-file-last-modified-time (:face nerd-icons-ivy-rich-time-face)))
      :delimiter "\t"))

  (plist-put
    nerd-icons-ivy-rich-display-transformers-list 'counsel-find-file
    '
    (:columns
      ((nerd-icons-ivy-rich-file-icon)
        (nerd-icons-ivy-rich-file-name (:width 0.6))
        (nerd-icons-ivy-rich-file-id
          (:width 15 :face nerd-icons-ivy-rich-file-owner-face :align right))
        (ivy-rich-file-last-modified-time (:face nerd-icons-ivy-rich-time-face)))
      :delimiter "\t"))

  (plist-put
    nerd-icons-ivy-rich-display-transformers-list 'ivy-switch-buffer
    '
    (:columns
      ((nerd-icons-ivy-rich-buffer-icon)
        (ivy-rich-candidate (:width 30))
        (ivy-rich-switch-buffer-indicators
          (:width 4 :face nerd-icons-ivy-rich-indicator-face :align right))
        (nerd-icons-ivy-rich-switch-buffer-major-mode
          (:width 18 :face nerd-icons-ivy-rich-major-mode-face))
        (ivy-rich-switch-buffer-project (:width 0.12 :face nerd-icons-ivy-rich-project-face))
        (ivy-rich-switch-buffer-path
          (:width
            (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3)))
            :face nerd-icons-ivy-rich-path-face)))
      :predicate (lambda (cand) (get-buffer cand))
      :delimiter "\t"))

  (with-eval-after-load "projectile"
    (plist-put
      nerd-icons-ivy-rich-display-transformers-list 'projectile-completing-read
      '
      (:columns
        ((nerd-icons-ivy-rich-file-icon)
          (nerd-icons-ivy-rich-project-find-file-transformer (:width 0.6))
          (nerd-icons-ivy-rich-project-file-id
            (:width 15 :face nerd-icons-ivy-rich-file-owner-face :align right))
          (nerd-icons-ivy-rich-project-file-size (:width 7 :face nerd-icons-ivy-rich-size-face)))
        :delimiter "\t")))

  (nerd-icons-ivy-rich-reload))

(use-package ivy-rich
  :preface
  ;; Adapted from https://github.com/tshu-w/.emacs.d/blob/master/lisp/editor-completion.el
  (defun sb/ivy-rich-file-size (candidate)
    "Displays the file size of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        ""
        (let ((size (file-attribute-size (file-attributes candidate))))
          (cond
            ((> size 1000000)
              (format "%.1fM " (/ size 1000000.0)))
            ((> size 1000)
              (format "%.1fk " (/ size 1000.0)))
            (t
              (format "%d " size)))))))

  (defun sb/ivy-rich-file-user (candidate)
    "Displays the file user of the candidate for ivy-rich."
    (let ((candidate (expand-file-name candidate ivy--directory)))
      (if (or (not (file-exists-p candidate)) (file-remote-p candidate))
        ""
        (let*
          (
            (user-id (file-attribute-user-id (file-attributes candidate)))
            (user-name (user-login-name user-id)))
          (format "%s" user-name)))))
  :after (ivy counsel)
  :init (ivy-rich-mode 1)
  :custom (ivy-rich-parse-remote-buffer nil)
  :config (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow-line)

  ;; (ivy-rich-project-root-cache-mode 1)

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

(use-package counsel-fd ; Counsel interface for fd
  :when (and (eq sb/minibuffer-completion 'ivy) (executable-find "fd"))
  :bind
  (("C-x d" . counsel-fd-dired-jump) ; Jump to a directory below the current directory
    ;; Jump to a file below the current directory
    ("C-x f" . counsel-fd-file-jump)))

;; This package adds a "C-'" binding to the Ivy minibuffer that uses Avy
(use-package ivy-avy
  :after ivy
  :bind (:map ivy-minibuffer-map ("C-'" . ivy-avy)))

(use-package ivy-yasnippet
  :if (eq sb/minibuffer-completion 'ivy)
  :after (ivy yasnippet)
  :bind ("C-M-y" . ivy-yasnippet))

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :bind (:map lsp-command-map ("G" . lsp-ivy-global-workspace-symbol) ("W" . lsp-ivy-workspace-symbol)))

(use-package ivy-xref
  :after (ivy xref)
  :demand t
  :custom
  (xref-show-definitions-function #'ivy-xref-show-defs)
  (xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package counsel-tramp
  :after counsel
  :bind ("C-c d t" . counsel-tramp))

(use-package ivy-bibtex
  :if (eq sb/minibuffer-completion 'ivy)
  :after latex
  :defines
  (ivy-bibtex-default-action
    bibtex-completion-cite-default-as-initial-input
    bibtex-completion-cite-prompt-for-optional-arguments
    bibtex-completion-display-formats)
  :bind ("C-c x b" . ivy-bibtex)
  :custom (ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  :config (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

  (require 'bibtex-completion)

  (setq
    bibtex-completion-cite-default-as-initial-input t
    bibtex-completion-cite-prompt-for-optional-arguments nil
    bibtex-completion-display-formats '((t . "${author:24} ${title:*} ${=key=:16} ${=type=:12}"))))

;; "M-n" will search for the word under cursor, "C-s" will search for the next occurrence, "C-s"
;; will search for a previously searched string.
(use-package swiper
  :if (eq sb/minibuffer-completion 'ivy)
  :commands (swiper swiper-isearch swiper-isearch-thing-at-point)
  :custom
  (swiper-action-recenter t)
  (swiper-verbose nil))

(provide 'init-ivy)

;;; init-ivy.el ends here
