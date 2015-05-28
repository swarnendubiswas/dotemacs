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

;; http://stackoverflow.com/questions/18511113/emacs-tabbar-customisation-making-unsaved-changes-visible
;; http://stackoverflow.com/questions/15735163/update-tabbar-when-nothing-to-save#
;; https://github.com/tomekowal/dotfiles/blob/master/.emacs.d/my-tabbar.el
(use-package tabbar
  :ensure t
  :preface
  (defun tabbar--modification-state-change ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  (defun tabbar--on-buffer-modification ()
    (set-buffer-modified-p t)
    (tabbar--modification-state-change))

  :config
  (add-hook 'after-save-hook #'tabbar--modification-state-change)
  (add-hook 'after-revert-hook #'tabbar--modification-state-change)
  (add-hook 'first-change-hook #'tabbar--on-buffer-modification)

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
  (set-face-attribute 'tabbar-default nil
                      :background "gray80")
  (set-face-attribute 'tabbar-unselected nil
                      :background "gray88"
                      :foreground "gray30"
                      :box nil
                      :height 1.1)
  (set-face-attribute 'tabbar-selected nil
                      :background "#f2f2f6"
                      :foreground "black"
                      :box '(:line-width 1 :color "black" :style pressed-button)
                      :height 1.2
                      :bold t
                      :underline nil)
  (set-face-attribute 'tabbar-highlight nil
                      :underline t
                      :background "lemon chiffon")
  (set-face-attribute 'tabbar-button nil
                      :box '(:line-width 1 :color "gray72" :style released-button))
  (set-face-attribute 'tabbar-separator nil
                      :height 1.0)
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

(use-package pabbrev
  :disabled t
  :ensure t
  :diminish pabbrev-mode
  :config (global-pabbrev-mode 1))

(use-package golden-ratio
  :disabled t
  :ensure t
  :diminish golden-ratio-mode
  :preface
  ;; http://tuhdo.github.io/helm-intro.html
  (defun dotemacs/helm-alive-p ()
    (if (boundp 'helm-alive-p)
        (symbol-value 'helm-alive-p)))
  :config
  (add-to-list 'golden-ratio-inhibit-functions #'dotemacs/helm-alive-p)
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t
        ;; https://truongtx.me/2014/11/15/auto-resize-windows-by-golden-ratio-in-emacs/
        split-width-threshold nil))

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

;; hide "Auto-saving...done" messages
;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun my-auto-save-wrapper (save-fn &rest args)
  (apply save-fn '(t)))

(advice-add 'do-auto-save :around #'my-auto-save-wrapper)

(provide 'misc-init)

;;; misc-init.el ends here
