;;; init-org.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(unless (fboundp 'org-indent-mode)
  (autoload #'org-indent-mode "org" nil t))
(unless (fboundp 'org-indent-item)
  (autoload #'org-indent-item "org" nil t))
(unless (fboundp 'org-outdent-item)
  (autoload #'org-outdent-item "org" nil t))

(with-eval-after-load "org"
  (defvar org-fontify-done-headline)
  (defvar org-fontify-whole-heading-line)
  (defvar org-hide-emphasis-markers)
  (defvar org-hide-leading-stars)
  (defvar org-hide-leading-stars-before-indent-mode)
  (defvar org-src-fontify-natively)
  (defvar org-src-preserve-indentation)
  (defvar org-src-tabs-acts-natively)
  (defvar org-src-window-setup)
  (defvar org-startup-indented)
  (defvar org-startup-truncated)
  (defvar org-startup-folded)
  (defvar org-startup-with-inline-images)
  (defvar org-support-shift-select)
  (defvar org-use-speed-commands)
  (defvar org-src-strip-leading-and-trailing-blank-lines)
  (defvar org-pretty-entities)
  (defvar org-pretty-entities-include-sub-superscripts)
  (defvar org-footnote-auto-adjust)

  (setq org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        ;; Code block fontification using the major-mode of the code
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tabs-acts-natively t
        org-src-window-setup 'current-window
        ;; There is a lot of visible distortion with `org-indent-mode' enabled. Emacs performance
        ;; feels better with the mode disabled.
        org-startup-indented nil
        org-startup-truncated nil
        org-startup-folded 'showeverything
        org-startup-with-inline-images t
        org-support-shift-select t
        ;; See `org-speed-commands-default' for a list of the keys and commands enabled at the
        ;; beginning of headlines. `org-babel-describe-bindings' will display a list of the code
        ;; blocks commands and their related keys.
        org-use-speed-commands t
        org-src-strip-leading-and-trailing-blank-lines t
        ;; Display entities like `\tilde' and `\alpha' in UTF-8 characters
        org-pretty-entities t
        ;; Render subscripts and superscripts in org buffers
        org-pretty-entities-include-sub-superscripts t
        ;; Automatically sorted and renumbered whenever I insert a new one
        org-footnote-auto-adjust t)

  (defvar org-mode-map)
  ;; (unbind-key "M-<left>" org-mode-map)
  ;; (unbind-key "M-<right>" org-mode-map)

  (bind-keys :package org :map org-mode-map
             ("M-<left>")
             ("M-<right>")
             ("M-<up>")
             ("M-<down>")
             ("C-'")
             ("<tab>"      . org-indent-item)
             ("<backtab>"  . org-outdent-item))

  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode 1)
                             (prettify-symbols-mode 1))))

(with-eval-after-load "org-indent"
  (diminish 'org-indent-mode))

;; Disabled the package to get consistent styles across themes.
(use-package org-bullets
  :commands org-bullets-mode
  :hook (org-mode-hook . org-bullets-mode))

;; Make invisible parts of Org elements appear visible
(progn
  (eval-when-compile
    (if (bound-and-true-p sb/disable-package.el)
        (use-package org-appear
          :straight (org-appear :type git :host github :repo "awth13/org-appear"))
      (use-package org-appear)))

  (unless (fboundp 'org-appear-mode)
    (autoload #'org-appear-mode "org-appear" nil t))

  (add-hook 'org-mode-hook #'org-appear-mode)

  (with-eval-after-load "org-appear"
    (setq org-appear-autosubmarkers t
          org-appear-autoentities   t
          org-appear-autolinks      t
          org-appear-autoemphasis   t)))

(use-package ox-gfm
  :after org
  :demand t
  :commands (org-gfm-export-as-markdown org-gfm-export-to-markdown))

;; TODO: Use "C-c o" as the binding for `org-mode-map'

(provide 'init-org)

;;; init-org.el ends here
