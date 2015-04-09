;;; org-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

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
        org-src-window-setup 'current-window
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-startup-folded nil
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        org-completion-use-ido t)
  (bind-key "C-c C-d" 'duplicate-thing org-mode-map))

;; (eval-after-load 'org
;;   '(bind-key "C-c C-d" 'duplicate-thing org-mode-map))

;; (eval-after-load 'org
;;   '(define-key org-mode-map (kbd "C-c C-d") nil))
;; (eval-after-load 'org
;;   '(define-key org-mode-map (kbd "C-c C-d") 'duplicate-thing))

;; require ox-latex so that the following variables are defined
;;(require 'ox-latex)
(use-package ox-latex
  :defer t
  ;;:demand t
  :config (with-eval-after-load 'org
            (setq org-latex-listings t) ;; tell org to use listings
            ;; include the listings package
            (add-to-list 'org-latex-packages-alist '("" "listings"))
            ;; if you want colored source code then you need to include the color package
            (add-to-list 'org-latex-packages-alist '("" "color"))))

(add-hook 'org-mode-hook 'turn-on-font-lock)

(use-package simple
  :diminish visual-line-mode
  :config
  (add-hook 'org-mode-hook 'visual-line-mode))

;; turn on soft wrapping mode for org mode
(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil)))

(use-package org-beautify-theme
  :ensure t
  :defer t)

(use-package org-indent
  :defer t
  :diminish org-indent-mode
  :config (with-eval-after-load 'org
            (org-indent-mode 1)))

(provide 'org-init)

;;; org-init.el ends here
