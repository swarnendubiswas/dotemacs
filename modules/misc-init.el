;;; misc-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Miscellaneous package configurations.

;;; Code:

(use-package smooth-scrolling
  :ensure t)

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
  :config
  ;; warn when opening files bigger than 50MB
  (setq large-file-warning-threshold (* 50 1024 1024))
  (use-package vlf-setup))

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
  (set-face-attribute 'tabbar-default nil :background "gray80")
  (set-face-attribute 'tabbar-unselected nil :background "gray88" :foreground "gray30" :box nil :height 1.0)
  (set-face-attribute 'tabbar-selected nil :background "#f2f2f6" :foreground "black" :box nil :underline t :height 1.2 :bold t)
  (set-face-attribute 'tabbar-highlight nil :underline t)
  (set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72" :style released-button))
  ;;(set-face-attribute 'tabbar-button-highlight ((t (:inherit tabbar-default))))
  (set-face-attribute 'tabbar-separator nil :height 1.0)
  
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

;; move text with M-<up> and M-<down> like eclipse
(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package duplicate-thing
  :ensure t
  :bind* ("C-c C-d" . duplicate-thing))

(use-package discover-my-major
  :ensure t
  :bind* ("C-h C-m" . discover-my-major))

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
  :bind* ("C-x C-\\" . goto-last-change))

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

;; this package now provides ivy-mode
(use-package swiper
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (use-package swiper-helm
    :ensure t)
  :bind* (("C-f" . swiper)
          ("C-r" . swiper)))

(use-package pabbrev
  :disabled t
  :ensure t
  :diminish pabbrev-mode
  :config (global-pabbrev-mode 1))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t))

;; Edit file with sudo
(use-package sudo-edit
  :ensure t
  :bind ("M-s e" . sudo-edit))

(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-file (concat dotemacs-temp-directory "keyfreq"))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'misc-init)

;;; misc-init.el ends here
