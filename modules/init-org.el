;;; init-org.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(use-package org
  :defines (org-hide-leading-stars-before-indent-mode
            org-src-strip-leading-and-trailing-blank-lines
            org-src-tabs-acts-natively)
  :commands (org-indent-mode org-indent-item org-outdent-item)
  :hook (org-mode-hook . visual-line-mode)
  :custom
  (org-fontify-done-headline nil)
  (org-fontify-whole-heading-line nil)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-hide-leading-stars-before-indent-mode t)
  ;; Code block fontification using the major-mode of the code
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tabs-acts-natively t)
  (org-src-window-setup 'current-window)
  ;; There is a lot of visible distortion with `org-indent-mode' enabled. Emacs performance
  ;; feels better with the mode disabled.
  (org-startup-indented nil)
  (org-startup-truncated nil)
  (org-startup-folded 'showeverything)
  (org-startup-with-inline-images t)
  (org-support-shift-select t)
  ;; See `org-speed-commands-default' for a list of the keys and commands enabled at the
  ;; beginning of headlines. `org-babel-describe-bindings' will display a list of the code
  ;; blocks commands and their related keys.
  (org-use-speed-commands t)
  (org-src-strip-leading-and-trailing-blank-lines t)
  ;; Display entities like `\tilde' and `\alpha' in UTF-8 characters
  (org-pretty-entities t)
  ;; Render subscripts and superscripts in org buffers
  (org-pretty-entities-include-sub-superscripts t)
  ;; Automatically sorted and renumbered whenever I insert a new one
  (org-footnote-auto-adjust t)
  :config
  (with-eval-after-load "org-indent"
    (diminish 'org-indent-mode))
  :bind-keymap ("C-c o" . org-mode-map)
  :bind
  (:map org-mode-map
        ("M-<left>"  . nil)
        ("M-<right>" . nil)
        ("M-<up>"    . nil)
        ("M-<down>"  . nil)
        ("C-'"       . nil)
        ("C-c C-d"   . nil) ; Was bound to `org-deadline'
        ("C-c C-j"   . nil) ; Was bound to `org-goto'
        ("<tab>"     . org-indent-item)
        ("<backtab>" . org-outdent-item)))

;; Disable the package to get consistent styles across themes.
(use-package org-bullets
  :hook (org-mode-hook . org-bullets-mode))

;; Make invisible parts of Org elements appear visible
(use-package org-appear ; Make invisible parts of Org elements appear visible
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode-hook . org-appear-mode)
  :custom
  (org-appear-autosubmarkers t)
  (org-appear-autoentities   t)
  (org-appear-autolinks      t)
  (org-appear-autoemphasis   t))

(use-package ox-gfm
  :after org
  :demand t
  :commands (org-gfm-export-as-markdown org-gfm-export-to-markdown))

(use-package ox-pandoc
  :after org
  :demand t
  :commands (org-pandoc-export-to-markdown
             org-pandoc-export-as-markdown
             org-pandoc-export-to-markdown-and-open))

(use-package org-modern
  :hook (org-mode-hook . org-modern-mode))

(provide 'init-org)

;;; init-org.el ends here
