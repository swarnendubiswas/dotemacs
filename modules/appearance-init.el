;;; appearance-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak Emacs appearance.

;;; Code:

(defvar dotemacs-theme)

;; Better frame title
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b") "  --  " "GNU Emacs " emacs-version "@" (system-name)))
(setq-default indicate-buffer-boundaries 'right)

(use-package tool-bar
  :if (fboundp 'tool-bar-mode)
  :config
  ;; Maximize the vertical space
  (tool-bar-mode -1))

(use-package menu-bar
  :if (fboundp 'menu-bar-mode)
  :config
  ;; One can learn many shortcuts from the menu bar entries.
  (menu-bar-mode 1))

(use-package tooltip
  :config (tooltip-mode -1))

(use-package scroll-bar
  :if (fboundp 'scroll-bar-mode)
  :config
  ;; Maximize the space for displaying the buffer.
  (scroll-bar-mode -1))

(use-package frame
  :config
  ;; Start with Emacs window maximized:
  ;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
  ;; Only the frame that Emacs creates at startup, but will not touch any subsequent frames you create.
  ;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; It will maximize all frames: both the first one and any others you create. Options: fullheight, fullboth
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Blinking cursor can be distracting
  (blink-cursor-mode 0))

(use-package time ; Display the time and date in the mode line
  :config
  (setq display-time-day-and-date t
        display-time-24hr-format nil
        display-time-default-load-average nil)
  (display-time))

;; Display line numbers in the margin
(or (use-package linum
      :disabled t ;; linum-mode can slow down Emacs for large files:
      ;; http://blog.binchen.org/posts/turn-off-linum-mode-when-file-is-too-big.html
      :config (global-linum-mode 1))

    (use-package nlinum ; Might improve performance with jit font locking.
      :ensure t
      :disabled t
      :config
      (global-nlinum-mode 1)
      (setq nlinum-highlight-current-line t)))

(use-package custom
  :config (setq custom-safe-themes t))

(cond ((eq dotemacs-theme 'leuven) (use-package leuven-theme
                                     :ensure t
                                     :config (load-theme 'leuven t)))

      ((eq dotemacs-theme 'professional) (use-package professional-theme
                                           :ensure t
                                           :config (load-theme 'professional t)))

      ((eq dotemacs-theme 'eclipse) (use-package eclipse-theme
                                      :ensure t
                                      :config
                                      (load-theme 'eclipse t)
                                      (set-background-color "white")
                                      (set-face-attribute 'region nil
                                                          :background "LemonChiffon"
                                                          :foreground "black")
                                      (with-eval-after-load "helm"
                                        (set-face-attribute 'helm-selection nil
                                                            :underline nil))
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

      ((eq dotemacs-theme 'spacemacs-light) (use-package spacemacs-common
                                              :ensure spacemacs-theme
                                              :config
                                              ;; https://github.com/nashamri/spacemacs-theme/issues/42
                                              (load-theme 'spacemacs-light t)
                                              (add-to-list 'default-frame-alist '(background-color . "#fbf8ef"))))

      ((eq dotemacs-theme 'solarized-light) (use-package solarized
                                              :ensure solarized-theme
                                              :config
                                              (setq solarized-distinct-fringe-background t)
                                              (load-theme 'solarized-light t)))

      ((eq dotemacs-theme 'solarized-dark) (use-package solarized
                                             :ensure solarized-theme
                                             :config
                                             (setq solarized-distinct-fringe-background t)
                                             (load-theme 'solarized-dark t)))

      ((eq dotemacs-theme 'zenburn) (use-package zenburn-theme
                                      :ensure t
                                      :config (load-theme 'zenburn t)))

      ((eq dotemacs-theme 'default) (progn
                                      (set-face-attribute 'region nil
                                                          :background "deep sky blue"
                                                          :foreground "white"))))

(use-package display-theme
  :ensure t
  :disabled t ; This shrinks other segment sizes on the modeline
  :if (not (eq dotemacs-theme 'default))
  :config (global-display-theme-mode))

;; http://stackoverflow.com/questions/18511113/emacs-tabbar-customisation-making-unsaved-changes-visible
;; http://stackoverflow.com/questions/15735163/update-tabbar-when-nothing-to-save#
;; https://github.com/tomekowal/dotfiles/blob/master/.emacs.d/my-tabbar.el
(use-package tabbar
  :ensure t
  :preface
  (defun dotemacs--tabbar-modification-state-change ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  (defun dotemacs--tabbar-on-buffer-modification ()
    (set-buffer-modified-p t)
    (dotemacs--tabbar-modification-state-change))
  :init (tabbar-mode 1)
  :config
  (setq tabbar-use-images nil ; Speed up by not using images
        tabbar-auto-scroll-flag t
        tabbar-background-color nil
        tabbar-separator '(0.3))

  (add-hook 'after-save-hook #'dotemacs--tabbar-modification-state-change)
  (add-hook 'after-revert-hook #'dotemacs--tabbar-modification-state-change)
  (add-hook 'first-change-hook #'dotemacs--tabbar-on-buffer-modification)

  ;; Add a buffer modification state indicator in the tab label, and place a space around the label to make it look less
  ;; crowded.
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

  (if (eq dotemacs-theme 'spacemacs-light)
      (progn
        (set-face-attribute 'tabbar-unselected nil
                            :inherit 'tabbar-unselected
                            :background "gray90"
                            :height 0.9)
        (set-face-attribute 'tabbar-selected nil
                            :inherit 'tabbar-default
                            :height 1.2
                            :bold t
                            :underline nil)
        (set-face-attribute 'tabbar-separator nil
                            :inherit 'tabbar-separator
                            :height 1.0)
        (set-face-attribute 'tabbar-modified nil
                            :inherit 'tabbar-modified
                            :foreground "red"
                            :height 1.1
                            :box '(:line-width 1 :color "black" :style sunken))
        (set-face-attribute 'tabbar-selected-modified nil
                            :inherit 'tabbar-selected-modified
                            :foreground "dark green"
                            :box '(:line-width 1 :color "black" :style sunken)
                            :height 1.1
                            ;; :bold t
                            ;; :underline nil
                            ))
    (progn
      ((set-face-attribute 'tabbar-default nil
                           :background "gray80")
       (set-face-attribute 'tabbar-unselected nil
                           :background "gray88"
                           :foreground "gray30"
                           :box nil
                           :height 1.0)
       (set-face-attribute 'tabbar-selected nil
                           :inherit 'tabbar-default
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
       (set-face-attribute 'tabbar-modified nil
                           :background "gray88"
                           :foreground "red"
                           :box '(:line-width 1 :color "black" :style sunken))
       (set-face-attribute 'tabbar-selected-modified nil
                           :background "#f2f2f6"
                           :foreground "dark green"
                           :box '(:line-width 1 :color "black" :style sunken)
                           :height 1.3
                           :bold t
                           :underline nil))))
  :bind (:map tabbar-mode-map
              ("M-<left>" . tabbar-backward-tab)
              ("M-<right>" . tabbar-forward-tab)))

;; Set font face independent of the color theme, value is in 1/10pt, so 100 will give you 10pt.
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil
                        :family "Consolas"
                        :height 120)
  (set-face-attribute 'default nil
                      :family "Dejavu Sans Mono"
                      :height 110))

(provide 'appearance-init)

;;; appearance-init.el ends here
