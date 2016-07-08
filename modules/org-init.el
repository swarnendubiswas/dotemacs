;;; org-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup org mode.

;;; Code:

(use-package org
  :ensure t
  :defer t
  :functions (org-mode org-toggle-blocks org-indent-mode org-set-emph-re)
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  (diminish 'visual-line-mode)
  (add-hook 'org-mode-hook #'turn-on-auto-fill)

  (setq org-completion-use-ido t
        org-src-fontify-natively t ; code block fontification using the major-mode of the code
        org-startup-indented t
        org-startup-truncated nil
        org-src-preserve-indentation t
        org-src-tabs-acts-natively t
        org-src-window-setup 'current-window
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-startup-folded 'showeverything ; options: nil
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        org-support-shift-select t ; use shift-select
        ;; See org-speed-commands-default for a list of the keys and commands enabled at the beginning of headlines. See
        ;; org-babel-describe-bindings will display a list of the code blocks commands and their related keys.
        org-use-speed-commands t
        org-src-strip-leading-and-trailing-blank-lines t
        ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
        org-pretty-entities t
        ;; Render subscripts and superscripts in org buffers
        org-pretty-entities-include-sub-superscripts t)

  ;; Allow syntax highlighting for parts of a word
  ;; http://stackoverflow.com/questions/1218238/how-to-make-part-of-a-word-bold-in-org-mode
  (setcar org-emphasis-regexp-components " \t('\"`{[:alpha:]=")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}=\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Does not work for me
  ;; disable including title if not explicitly specified, default was to use the buffer name
  ;; (defadvice org-export-grab-title-from-buffer (around org-export-grab-title-from-buffer-disable activate))

  (require 'org-inlinetask)

  (add-hook 'org-mode-hook #'org-toggle-blocks)
  (add-hook 'org-mode-hook #'which-function-mode)

  (use-package helm-org
    :if (eq dotemacs-selection 'helm)
    :config (setq helm-org-headings-fontify t))

  (use-package ox-latex
    :config
    ;; include the listings package
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    ;; if you want colored source code then you need to include the color package
    ;; (add-to-list 'org-latex-packages-alist '("" "color"))
    ;; Add minted to the defaults packages to include when exporting.
    ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
    ;; tell org to use listings, options: t, 'minted
    (setq org-latex-listings 't
          org-latex-table-caption-above nil))

  (use-package org-indent
    :diminish org-indent-mode
    :config (org-indent-mode 1))

  (use-package org-bullets
    :ensure t
    :init (add-hook 'org-mode-hook #'org-bullets-mode))

  (use-package org-autolist
    :ensure t)

  (use-package org-footnote
    :ensure nil
    :defer t
    :config
    (setq org-footnote-define-inline t
          org-footnote-auto-label 'random))

  (when (eq dotemacs-completion-in-buffer 'auto-complete)
    (use-package org-ac
      :ensure t
      :config (org-ac/config-default))))

(provide 'org-init)

;;; org-init.el ends here
