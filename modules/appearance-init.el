;;; appearance-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak Emacs appearance.

;;; Code:

;; Better frame titles
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b") "  --  " "GNU Emacs " emacs-version "@" system-name))
(setq-default indicate-buffer-boundaries 'right)

(use-package tool-bar
  :init
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)))

(use-package menu-bar ; Learn many shortcuts from the menu bar entries.
  :init
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode 1)))

(use-package tooltip
  :config (tooltip-mode -1))

(use-package scroll-bar
  :init (scroll-bar-mode 1))

(use-package frame
  :config
  ;; Start with Emacs window maximized:
  ;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
  ;; Only the frame that Emacs creates at startup, but will not touch any subsequent frames you create.
  ;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; It will maximize all frames: both the first one and any others you create.
  ;; options: fullheight, fullboth
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (blink-cursor-mode 0))

(use-package time ; Displays the time and date in the mode line
  :init
  (setq display-time-day-and-date t
        display-time-24hr-format nil
        display-time-default-load-average nil)
  (display-time))

(use-package linum ; Display line numbers in margin
  :init (global-linum-mode 1))

(cond ((eq dotemacs-theme 'leuven) (use-package leuven-theme
                                     :ensure t
                                     :init (load-theme 'leuven t)
                                     :config
                                     (with-eval-after-load "avy"
                                       (set-face-attribute 'avy-background-face nil
                                                           :background "WhiteSmoke"
                                                           :foreground "black"))
                                     ;; customize the fringe marks on the sides
                                     ;; (set-face-background 'fringe "wheat")
                                     ))

      ((eq dotemacs-theme 'professional) (use-package professional-theme
                                           :ensure t
                                           :init (load-theme 'professional t)))

      ((eq dotemacs-theme 'eclipse) (use-package eclipse-theme
                                      :ensure t
                                      :init (load-theme 'eclipse t)
                                      :config
                                      (set-background-color "white")
                                      (set-face-attribute 'region nil
                                                          :background "LemonChiffon"
                                                          :foreground "black")
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
                                      ;; FIXME: Change font does not work (biswass 01-15-16)
                                      (with-eval-after-load "helm-buffers"
                                        (set-face-attribute 'helm-buffer-file nil
                                                            :foreground "black"))
                                      (set-face-attribute 'mode-line nil
                                                          :background "grey88"
                                                          :foreground "black"
                                                          :box nil)
                                      ;; Org-mode customizations inspired from leuven theme
                                      (with-eval-after-load "org"
                                        (set-face-attribute 'org-level-1 nil
                                                            :height 1.2
                                                            :overline "#A7A7A7"
                                                            :foreground "#3C3C3C"
                                                            :background "#F5F5F5")
                                        (set-face-attribute 'org-level-2 nil
                                                            :height 1.1
                                                            :overline "#123555"
                                                            :foreground "#123555"
                                                            :background "#E5F4FB"))))

      ;; Default
      ((eq dotemacs-theme 'default)
       (set-face-attribute 'region nil
                           :background "LemonChiffon"
                           :foreground "black")
       (with-eval-after-load "hl-line"
         (set-face-attribute 'hl-line nil
                             :background "linen"))))

(use-package display-theme
  :ensure t
  :if (not (eq dotemacs-theme 'default))
  :config (global-display-theme-mode))

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
  :functions tabbar-display-update
  :init
  (setq tabbar-use-images nil ; Speed up by not using images
        tabbar-auto-scroll-flag t
        tabbar-separator '(0.3))
  (tabbar-mode 1)

  :config
  (add-hook 'after-save-hook #'tabbar--modification-state-change)
  (add-hook 'after-revert-hook #'tabbar--modification-state-change)
  (add-hook 'first-change-hook #'tabbar--on-buffer-modification)

  ;; Add a buffer modification state indicator in the tab label, and place a
  ;; space around the label to make it look less crowded.
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
                      :height 1.0)
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
                      :height 1.0)

  :bind* (("M-<left>" . tabbar-backward-tab)
          ("M-<right>" . tabbar-forward-tab)))

;; Set font face independent of the color theme, value is in 1/10pt, so 100 will give you 10pt
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
                        :family "Consolas"
                        :height 120)
  (set-face-attribute 'default nil
                      :family "Dejavu Sans Mono"
                      ;; :weight 'light
                      :height 110))

(provide 'appearance-init)

;;; appearance-init.el ends here
