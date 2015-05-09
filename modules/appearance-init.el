;;; appearance-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Tweak emacs appearance.

;;; Code:

;; better frame titles
;;(setq frame-title-format (concat  "%b - emacs@" (system-name)))
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b") "  --  " "GNU Emacs " emacs-version "@" system-name))
(setq-default indicate-buffer-boundaries 'right)

(use-package tool-bar
  :config
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)))

(use-package menu-bar
  :config
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(use-package tooltip
  :config (tooltip-mode -1))

(use-package scroll-bar
  :config (scroll-bar-mode 1))

(use-package frame
  :config
  ;; enable/disable blinking cursor
  (blink-cursor-mode 1))

;; displays the time and date in the mode line
(use-package time
  :config
  (setq display-time-day-and-date t
        display-time-24hr-format nil)
  (display-time))

;; display line numbers in margin
(use-package linum
  :config (global-linum-mode 1))

(use-package simple
  :config (column-number-mode 1))

;; highlight current line
(use-package hl-line
  :ensure t
  ;;:if (not (eq dotemacs-theme 'default))
  :config
  ;;(global-hl-line-mode 1)
  ;; highlight only when idle
  (use-package hl-line+
    :ensure t
    :config (toggle-hl-line-when-idle 1)))

;; extension to linum-mode to highlight current line number in the margin
(use-package hlinum
  :ensure t
  :config (hlinum-activate))

(cond ((eq dotemacs-theme 'leuven) (use-package leuven-theme
                                     :ensure t
                                     :config
                                     (load-theme 'leuven t)
                                     (use-package smart-mode-line
                                       :ensure t
                                       :config
                                       (setq sml/theme 'light 
                                             sml/no-confirm-load-theme t
                                             sml/mode-width 'full
                                             sml/shorten-modes t
                                             sml/shorten-directory t)
                                       (sml/setup))
                                     ;; set font size, value is in 1/10pt, so 100 will give you 10pt
                                     (if (string-equal system-name "XXX")
                                         (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110)
                                       (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110))
                                     ;; customize the fringe marks on the sides
                                     (set-face-background 'fringe "lavender")
                                     (set-face-attribute 'hl-line nil :background "linen")))
      
      ((eq dotemacs-theme 'professional) (use-package professional-theme
                                           :ensure t
                                           :config
                                           (load-theme 'professional t)
                                           ;; set font size, value is in 1/10pt, so 100 will give you 10pt)
                                           (set-face-attribute 'default nil :height 110)))
      
      ((eq dotemacs-theme 'eclipse) (use-package eclipse-theme
                                      :ensure t
                                      :config
                                      (load-theme 'eclipse t)
                                      (use-package powerline
                                        :ensure t
                                        :config
                                        (setq powerline-display-mule-info nil
                                              powerline-display-buffer-size t
                                              powerline-display-hud nil)
                                        (powerline-default-theme))
                                      ;; set font size, value is in 1/10pt, so 100 will give you 10pt
                                      (if (string-equal system-name "rain.cse.ohio-state.edu")
                                          (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110)
                                        (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110))
                                      (set-background-color "white")
                                      (set-face-attribute 'mode-line nil :background "grey88" :foreground "black" :box nil :bold nil)
                                      (set-face-attribute 'mode-line-inactive nil :box nil)
                                      ;; (set-face-attribute 'region nil :background "LemonChiffon" :foreground "black")
                                      (set-face-attribute 'region nil :background "#164040" :foreground "white")
                                      (set-face-attribute 'linum nil :background "#006666" :foreground "#FFFFDD" :height 0.98)
                                      (set-face-attribute 'hl-line nil :background "linen")
                                      (with-eval-after-load 'helm
                                        (set-face-attribute 'helm-selection nil :underline t))
                                      ;; org-mode customizations inspired from leuven theme
                                      (with-eval-after-load "org"
                                        (set-face-attribute 'org-level-1 nil :height 1.2 :overline "#A7A7A7" ;;:weight bold
                                                            :foreground "#3C3C3C" :background "#F5F5F5")
                                        (set-face-attribute 'org-level-2 nil
                                                            :height 1.1 :overline "#123555" :foreground "#123555" :background "#E5F4FB"))
                                      ;; customize the fringe marks on the sides
                                      (set-face-background 'fringe "lavender")))

      ;; default
      ((eq dotemacs-theme 'default)
       (if (string= system-name "XXX")
           (set-face-attribute 'default nil  :height 110)
         (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110))
       (set-face-attribute 'region nil :background "LemonChiffon" :foreground "black")
       (set-face-attribute 'hl-line nil :background "linen")))

(use-package display-theme
  :ensure t
  :config (global-display-theme-mode))

(provide 'appearance-init)

;;; appearance-init.el ends here
