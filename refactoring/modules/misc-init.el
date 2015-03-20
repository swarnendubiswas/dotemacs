;;; misc-init.el --- Part of emacs initialization

;;; Commentary:
;; Miscellaneous configurations

;;; Code:

(use-package smooth-scrolling
  :ensure t
  :init (require 'smooth-scrolling))

(use-package auto-compile
  :ensure t
  :config
  (progn 
    (setq auto-compile-display-buffer nil
          auto-compile-mode-line-counter nil)
    (auto-compile-on-load-mode 1)
    (auto-compile-on-save-mode 1)))

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

(use-package fixme-mode
             :ensure t
             :defer t
  :config (fixme-mode 1))

(use-package fish-mode
             :ensure t
  :disabled t)

(use-package move-text
  :ensure t
  :defer t)

(use-package duplicate-thing
  :ensure t
  :defer t
  :bind ("C-c C-d" . duplicate-thing))

;; setup writegood-mode, identify weasel words, passive voice, and duplicate words
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

(provide 'misc-init)

;;; misc-init.el ends here
