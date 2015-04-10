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

;; extension to linum-mode to highlight current line number in the margin
(use-package hlinum
  :ensure t
  :defer 5
  :config (hlinum-activate))

(or (use-package leuven-theme
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
                sml/shorten-modes t
                sml/shorten-directory t)
          (sml/setup)))
      ;; set font size, value is in 1/10pt, so 100 will give you 10pt
      (if (string-equal system-name "rain.cse.ohio-state.edu")
          (set-face-attribute 'default nil :height 110)
        (set-face-attribute 'default nil :height 118) )
      ;; hide the fringe mark on the left
      (set-face-background 'fringe "linen"))
    
    (use-package professional-theme
      :ensure t
      :disabled t
      :config
      (load-theme 'professional t)
      ;; set font size, value is in 1/10pt, so 100 will give you 10pt)
      (set-face-attribute 'default nil :height 115))

    (use-package eclipse-theme
      :ensure t
      :disabled t
      :config
      (load-theme 'eclipse t)
      (use-package powerline
        :ensure t
        :config
        (powerline-default-theme))
      ;; set font size, value is in 1/10pt, so 100 will give you 10pt
      (set-face-attribute 'default nil :height 115)
      (set-face-attribute 'mode-line nil :background "grey87" :foreground "black" :box nil)
      (set-face-attribute 'region nil :background "LemonChiffon" :foreground "black")
      (set-face-attribute 'linum nil :background "#006666" :foreground "#FFFFDD")))

;; highlight current line
(use-package hl-line
  :ensure t
  :defer 5
  :config (global-hl-line-mode 1))

(use-package display-theme
  :ensure t
  ;;:disabled t
  :config (global-display-theme-mode))

(provide 'appearance-init)

;;; appearance-init.el ends here
