;;; init-org.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; https://orgmode.org/manual/In_002dbuffer-Settings.html
(use-package org
  :defines (org-hide-leading-stars-before-indent-mode
            org-src-strip-leading-and-trailing-blank-lines
            org-src-tabs-acts-natively)
  :commands (org-indent-mode org-indent-item org-outdent-item)
  :hook (org-mode-hook . turn-on-visual-line-mode)
  :custom
  (org-fontify-whole-heading-line nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t "Hide *, ~, and / in Org text")
  (org-hide-leading-stars nil "Show every star as it helps identify the indentation level")
  (org-hide-leading-stars-before-indent-mode nil)
  ;; Code block fontification using the major-mode of the code
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tabs-acts-natively t "TAB behavior depends on the major mode")
  (org-src-window-setup 'current-window)
  ;; There is a lot of visible distortion with `org-indent-mode' enabled. Emacs performance
  ;; feels better with the mode disabled.
  (org-startup-indented t "Indentation looks nice")
  (org-startup-truncated nil)
  ;; https://orgmode.org/manual/Initial-visibility.html
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
  (org-return-follows-link t)
  (org-adapt-indentation t)
  (org-odd-levels-only t "Use odd levels to add more indentation")
  (org-export-with-smart-quotes t "#+OPTIONS ':t")
  (org-export-with-section-numbers nil "#+OPTIONS num:nil")
  ;; #+OPTIONS toc:nil, use "#+TOC: headlines 2" or similar if you need a headline
  (org-export-with-toc nil)
  (org-export-with-sub-superscripts nil "#+OPTIONS ^:{}")
  ;; This exports broken links as [BROKEN LINK %s], so we can actually find them. The default value
  ;; nil just aborts the export process with an error message "Unable to resolve link: nil". This
  ;; doesn't give any hint on which line the broken link actually is.
  (org-export-with-broken-links 'mark)
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
        ("<backtab>" . org-outdent-item)
        ("M-a" . org-backward-paragraph)
        ("M-e" . org-forward-paragraph)
        ("M-{" . org-backward-element)
        ("M-}" . org-forward-element)))

;; Disable the package to get consistent styles across themes.
(use-package org-bullets
  :disabled t
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
  :commands (org-gfm-export-as-markdown org-gfm-export-to-markdown))

(use-package ox-pandoc
  :after org
  :commands (org-pandoc-export-to-markdown
             org-pandoc-export-as-markdown
             org-pandoc-export-to-markdown-and-open))

(use-package org-modern
  :disabled t
  :hook (org-mode-hook . org-modern-mode))

(provide 'init-org)

;;; init-org.el ends here
