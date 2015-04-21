;;; appearance-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

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
  :config
  (tooltip-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode 1))

(use-package frame
  :config (blink-cursor-mode 1)) ; enable/disable blinking cursor

;; displays the time and date in the mode line
(use-package time
  :defer 2
  :config
  (setq display-time-day-and-date t
        display-time-24hr-format nil)
  (display-time))

(use-package linum
  :config (global-linum-mode 1)) ; display line numbers in margin

(use-package simple
  :config (column-number-mode 1))

;; highlight current line
(use-package hl-line
  :ensure t
  :config (global-hl-line-mode 1))

;; extension to linum-mode to highlight current line number in the margin
(use-package hlinum
  :ensure t
  :defer 5
  :config (hlinum-activate))

(or (use-package leuven-theme
      :disabled t
      :ensure t
      :config
      (load-theme 'leuven t)
      (use-package smart-mode-line
        :ensure t
        :config
        (progn
          (use-package smart-mode-line-powerline-theme
            :ensure t
            :defer t)
          (setq sml/theme 'light ; options: dark, light, respectful, automatic, powerline
                ;; sml/name-width 20
                sml/no-confirm-load-theme t
                sml/mode-width 'full
                sml/shorten-modes t
                sml/shorten-directory t)
          (sml/setup)))
      ;; set font size, value is in 1/10pt, so 100 will give you 10pt
      (if (string-equal system-name "XXX")
          (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110)
        (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110))
      ;; customize the fringe marks on the sides
      (set-face-background 'fringe "linen"))
    
    (use-package professional-theme
      :disabled t
      :ensure t
      :config
      (load-theme 'professional t)
      ;; set font size, value is in 1/10pt, so 100 will give you 10pt)
      (set-face-attribute 'default nil :height 115))

    (use-package eclipse-theme
      :ensure t
      :config
      (load-theme 'eclipse t)
      (use-package powerline
        :ensure t
        :config
        (powerline-default-theme))
      ;; set font size, value is in 1/10pt, so 100 will give you 10pt
      (if (string-equal system-name "XXX")
          (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110)
        (set-face-attribute 'default nil :family "Dejavu Sans Mono" :height 110))
      (set-face-attribute 'mode-line nil :background "grey87" :foreground "black" :box nil)
      (set-face-attribute 'region nil :background "LemonChiffon" :foreground "black")
      (set-face-attribute 'linum nil :background "#006666" :foreground "#FFFFDD")
      (set-face-attribute 'hl-line nil :background "lavender")
      ;; org-mode customizations inspired from leuven theme
      (with-eval-after-load "org"
        (set-face-attribute 'org-level-1 nil :height 1.2 :overline "#A7A7A7" ;;:weight bold 
                            :foreground "#3C3C3C" :background "#F5F5F5")
        (set-face-attribute 'org-level-2 nil :height 1.1 :overline "#123555" :foreground "#123555" :background "#E5F4FB"))
      ;; customize the fringe marks on the sides
      (set-face-background 'fringe "wheat")))

(use-package display-theme
  :ensure t
  :config (global-display-theme-mode))

(provide 'appearance-init)

;;; appearance-init.el ends here
