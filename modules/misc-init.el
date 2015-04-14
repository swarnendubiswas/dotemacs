;;; misc-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Miscellaneous package configurations.

;;; Code:

(use-package smooth-scrolling
  :ensure t
  :defer 10)

(use-package achievements
  :ensure t
  :disabled t
  :diminish achievements-mode
  :config
  (setq achievements-idle-time 600) ; seconds
  (achievements-mode 1))

;; speed up emacs for large files
(use-package vlf
  :ensure t
  :defer 5
  :config
  (progn
    ;; warn when opening files bigger than 50MB
    (setq large-file-warning-threshold 50000000)
    (use-package vlf-setup))) 

(use-package tabbar
  :ensure t
  :config
  (setq tabbar-use-images nil) ; speed up by not using images
  (tabbar-mode 1))

(use-package jgraph-mode
  :ensure t
  :defer t)

;; erase 'all' consecutive white space characters in a given direction
(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode 1))

(use-package fish-mode
  :ensure t
  :disabled t)

;; move text with M-up and M-down like eclipse
(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package duplicate-thing
  :ensure t
  :bind ("C-c C-d" . duplicate-thing))

;; identify weasel words, passive voice, and duplicate words
(use-package writegood-mode
  :ensure t
  :bind ("C-c g" . writegood-mode)
  :diminish writegood-mode
  :config
  (add-hook 'text-mode-hook #'writegood-mode)
  (add-hook 'LaTeX-mode-hook #'writegood-mode)
  (add-hook 'org-mode-hook #'writegood-mode))

(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package manage-minor-mode
  :ensure t
  :defer t)

(use-package jgraph-mode
  :ensure t
  :defer t)

(use-package graphviz-dot-mode
  :ensure t
  :defer t)

(use-package goto-last-change
  :ensure t
  :pin melpa
  ;;:load-path "lisp/" ; prefer melpa
  :bind ("C-x C-\\" . goto-last-change))

(use-package bug-hunter
  :ensure t
  :defer t)

(use-package popwin
  :ensure t
  :config
  (setq popwin:popup-window-height 35)
  (popwin-mode 1))

(provide 'misc-init)

;;; misc-init.el ends here
