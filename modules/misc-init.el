;;; misc-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Miscellaneous package configurations

;;; Code:

(use-package smooth-scrolling
  :ensure t
  :defer t
  :config (require 'smooth-scrolling))

(use-package achievements
  :ensure t
  :disabled t
  :diminish achievements-mode
  :init (achievements-mode 1)
  :config (setq achievements-idle-time 600)) ; seconds

;; speed up emacs for large files
(use-package vlf
  :ensure t
  :defer t
  :config
  (progn
    (setq large-file-warning-threshold 50000000)
    (use-package vlf-setup))) ; warn when opening files bigger than 50MB

(use-package tabbar
  :ensure t
  :init (tabbar-mode 1)
  :config (setq tabbar-use-images nil)) ; speed up by not using images

(use-package jgraph-mode
  :ensure
  :defer t)

;; erase 'all' consecutive white space characters in a given direction
(use-package hungry-delete
  :ensure t
  :defer t
  :init (global-hungry-delete-mode 1))

;; highlight certain words
(use-package fixme-mode
  :ensure t
  :defer t
  :config (fixme-mode 1))

(use-package fish-mode
  :ensure t
  :disabled t)

;; move text with M-up and M-down like eclipse
(use-package move-text
  :ensure t
  :defer t
  :init (move-text-default-bindings))

(use-package duplicate-thing
  :ensure t
  :defer t
  :bind ("C-c C-d" . duplicate-thing))

;; writegood-mode: identify weasel words, passive voice, and duplicate words
(use-package writegood-mode
  :ensure t
  :defer t
  :bind ("C-c g" . writegood-mode)
  :diminish writegood-mode)

(use-package discover-my-major
  :ensure t
  :defer t
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
  :defer t
  :bind ("C-x C-\\" . goto-last-change))

(provide 'misc-init)

;;; misc-init.el ends here
