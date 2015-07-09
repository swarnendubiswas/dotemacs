;;; appearance-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Tweak Emacs appearance.

;;; Code:

;; better frame titles
;; (setq frame-title-format (concat  "%b - GNU Emacs@" (system-name)))
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b") "  --  " "GNU Emacs " emacs-version "@" system-name))
(setq-default indicate-buffer-boundaries 'right)

(use-package tool-bar
  :init
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)))

;; SB: You can learn many shortcuts from the menu bar entries.
(use-package menu-bar
  :disabled t
  :init
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(use-package tooltip
  :init (tooltip-mode -1))

(use-package scroll-bar
  :init (scroll-bar-mode 1))

(use-package frame
  :init
  ;; enable/disable blinking cursor
  (blink-cursor-mode 1)
  :config
  ;; start with Emacs window maximized
  (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(fullscreen . fullheight)))

;; displays the time and date in the mode line
(use-package time
  :init
  (setq display-time-day-and-date t
        display-time-24hr-format nil)
  (display-time))

;; display line numbers in margin
(use-package linum
  :init (global-linum-mode 1))

(cond ((eq dotemacs--theme 'leuven) (use-package leuven-theme
                                      :ensure t
                                      :init (load-theme 'leuven t)
                                      :config
                                      ;; set font size, value is in 1/10pt, so 100 will give you 10pt
                                      (set-face-attribute 'default nil
                                                          :family "Dejavu Sans Mono"
                                                          :height 110)
                                      ;; customize the fringe marks on the sides
                                      ;; (set-face-background 'fringe "wheat")
                                      ;; (with-eval-after-load 'hl-line
                                      ;; (set-face-attribute 'hl-line nil :background "lavender"))
                                      ))

      ((eq dotemacs--theme 'professional) (use-package professional-theme
                                            :ensure t
                                            :init (load-theme 'professional t)
                                            :config (set-face-attribute 'default nil
                                                                        :height 110)))

      ((eq dotemacs--theme 'eclipse) (use-package eclipse-theme
                                       :ensure t
                                       :init (load-theme 'eclipse t)
                                       :config
                                       (set-face-attribute 'default nil
                                                           :family "Dejavu Sans Mono"
                                                           :height 110)
                                       (set-background-color "white")
                                       ;; (set-face-attribute 'mode-line nil
                                       ;;                     :background "grey88"
                                       ;;                     :foreground "black"
                                       ;;                     :box nil :bold nil)
                                       ;; (set-face-attribute 'mode-line-inactive nil
                                       ;;                     :box nil)
                                       (set-face-attribute 'region nil
                                                           :background "#164040"
                                                           :foreground "white")
                                       ;; (set-face-attribute 'linum nil
                                       ;;                     :background "#006666"
                                       ;;                     :foreground "#FFFFDD"
                                       ;;                     :height 1.0)
                                       (with-eval-after-load "hl-line"
                                         (set-face-attribute 'hl-line nil
                                                             :background "linen"))
                                       (with-eval-after-load "helm"
                                         (set-face-attribute 'helm-selection nil
                                                             :underline nil))
                                       ;; org-mode customizations inspired from leuven theme
                                       (with-eval-after-load "org"
                                         (set-face-attribute 'org-level-1 nil
                                                             :height 1.2
                                                             :overline "#A7A7A7"
                                                             ;;:weight bold
                                                             :foreground "#3C3C3C"
                                                             :background "#F5F5F5")
                                         (set-face-attribute 'org-level-2 nil
                                                             :height 1.1
                                                             :overline "#123555"
                                                             :foreground "#123555"
                                                             :background "#E5F4FB"))
                                       ;; customize the fringe marks on the sides
                                       (set-face-background 'fringe "lavender")))

      ;; default
      ((eq dotemacs--theme 'default)
       (set-face-attribute 'default nil
                           :family "Dejavu Sans Mono"
                           :height 115)
       (set-face-attribute 'region nil
                           :background "LemonChiffon"
                           :foreground "black")
       (with-eval-after-load "hl-line"
         (set-face-attribute 'hl-line nil
                             :background "linen"))))

(use-package display-theme
  :ensure t
  :if (not (eq dotemacs--theme 'default))
  :init (global-display-theme-mode))

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

  :init
  (setq tabbar-use-images nil ; speed up by not using images
        tabbar-auto-scroll-flag t
        tabbar-separator '(0.3))
  (tabbar-mode 1)

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
  ;; https://gist.github.com/ShingoFukuyama/7245914
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
                      :height 1.1
                      :bold t
                      :underline nil)
  (set-face-attribute 'tabbar-highlight nil
                      :underline t
                      :background "lemon chiffon")
  (set-face-attribute 'tabbar-button nil
                      :box '(:line-width 1 :color "gray72" :style released-button))
  (set-face-attribute 'tabbar-separator nil
                      :height 1.0))

(provide 'appearance-init)

;;; appearance-init.el ends here
