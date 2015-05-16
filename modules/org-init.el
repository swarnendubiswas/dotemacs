;;; org-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup org mode.

;;; Code:

(use-package org
  :ensure t
  :defer t
  :config
  (setq org-completion-use-ido t
        org-src-fontify-natively t ; code block fontification using the major-mode of the code
        org-src-preserve-indentation t
        org-src-tabs-acts-natively t
        org-src-window-setup 'current-window
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-startup-folded 'showeverything ; options: nil
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        org-completion-use-ido t
        org-support-shift-select t ; use shift-select
        ;; See org-speed-commands-default for a list of the keys and commands enabled at the beginning of headlines.
        ;; See org-babel-describe-bindings will display a list of the code blocks commands and their related keys.
        org-use-speed-commands t)

  ;; Allow syntax highlighting for parts of a word
  ;; http://stackoverflow.com/questions/1218238/how-to-make-part-of-a-word-bold-in-org-mode
  (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  
  (require 'org-inlinetask)
  
  ;; (eval-after-load 'org
  ;;   '(bind-key "C-c C-d" 'duplicate-thing org-mode-map))
  ;; (eval-after-load 'org
  ;;   '(define-key org-mode-map (kbd "C-c C-d") nil))
  ;; (eval-after-load 'org
  ;;   '(define-key org-mode-map (kbd "C-c C-d") 'duplicate-thing))
  ;; (bind-key "C-c C-d" 'duplicate-thing org-mode-map)
  ;; (bind-key "C-c SPC" 'ace-jump-mode org-mode-map)
  
  ;; turn on soft wrapping mode for org mode
  (add-hook 'org-mode-hook
            (lambda ()
              (setq truncate-lines nil))))

;; require ox-latex so that the following variables are defined
;;(require 'ox-latex)
(use-package ox-latex
  :defer t
  :config
  (with-eval-after-load 'org
    ;; include the listings package
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    ;; if you want colored source code then you need to include the color package
    (add-to-list 'org-latex-packages-alist '("" "color"))
    ;; Add minted to the defaults packages to include when exporting.
    (add-to-list 'org-latex-packages-alist '("" "minted")))
  ;; tell org to use listings, options: t, 'minted
  (setq org-latex-listings 't))

(use-package simple
  :diminish visual-line-mode auto-fill-function
  :config (add-hook 'org-mode-hook #'visual-line-mode))

(use-package org-beautify-theme
  :disabled t
  :ensure t)

(use-package org-indent
  :defer t
  :diminish org-indent-mode
  :config
  (with-eval-after-load 'org
    (org-indent-mode 1)))

(use-package org-ref
  :disabled t
  :load-path "lisp/org-ref"
  :init (org-babel-load-file "org-ref.org")
  :config (setq org-ref-default-bibliography '("~/workspace/bib/plass.bib")))

(use-package org-bullets
  :ensure t)

(use-package org-autolist
  :ensure t)

(provide 'org-init)

;;; org-init.el ends here
