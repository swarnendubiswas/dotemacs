;;; org-init.el --- Part of emacs initialization

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
        org-fontify-whole-heading-line t
        org-latex-listings t)) ;; tell org to use listings
;; requite org-latex so that the following variables are defined
(paradox-require 'ox-latex)


(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'visual-line-mode)
;; turn on soft wrapping mode for org mode
(add-hook 'org-mode-hook 
          (lambda () (setq truncate-lines nil)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode 1)))

;; include the listings package
(add-to-list 'org-latex-packages-alist '("" "listings"))

;; if you want colored source code then you need to include the color package
(add-to-list 'org-latex-packages-alist '("" "color"))

(use-package org-beautify-theme
  :ensure t
  :defer t)

(provide 'org-init)

;;; org-init.el ends here
