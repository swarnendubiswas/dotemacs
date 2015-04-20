;;; misc-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Miscellaneous package configurations.

;;; Code:

(use-package smooth-scrolling
  :ensure t
  :defer 10)

(use-package achievements
  :disabled t
  :ensure t
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
  ;; Add a buffer modification state indicator in the tab label, and place a
  ;; space around the label to make it looks less crowd.
  (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
    (setq ad-return-value
          (if (and (buffer-modified-p (tabbar-tab-value tab))
                   (buffer-file-name (tabbar-tab-value tab)))
              (concat " * " (concat ad-return-value " "))
            (concat " " (concat ad-return-value " ")))))
  ;; Customize the tabbar faces, inspired from
  ;; http://amitp.blogspot.com/2007/04/emacs-buffer-tabs.html
  ;; https://zhangda.wordpress.com/2012/09/21/tabbar-mode-rocks-with-customization/
  (set-face-attribute 'tabbar-default nil :background "gray60")
  (set-face-attribute 'tabbar-unselected nil :background "gray88" :foreground "gray30" :box nil :height 1.1)
  (set-face-attribute 'tabbar-selected nil :background "#f2f2f6" :foreground "black" :box nil :underline t :height 1.2 :bold t)
  (set-face-attribute 'tabbar-highlight nil :underline t)
  (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72" :style released-button))
  ;;(set-face-attribute 'tabbar-button-highlight ((t (:inherit tabbar-default))))
  (set-face-attribute 'tabbar-separator nil :height 0.7)
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
  :disabled t
  :ensure t)

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
  (if (string-equal system-name "XXX")
      (setq popwin:popup-window-height 15)
    (setq popwin:popup-window-height 15))
  (popwin-mode 1))

(provide 'misc-init)

;;; misc-init.el ends here
