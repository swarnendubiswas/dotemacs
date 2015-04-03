;;; appearance-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Tweak emacs appearance.

;;; Code:

;; better frame titles
;;(setq frame-title-format (concat  "%b - emacs@" (system-name)))
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b") "  --  " "GNU Emacs " emacs-version "@" system-name))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(tooltip-mode -1)
(scroll-bar-mode 1)
(blink-cursor-mode 1) ; enable/disable blinking cursor

;; displays the time and date in the mode line
(use-package time
  :defer 5
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

(use-package leuven-theme
  :ensure t
  :disabled t
  :config
  (load-theme 'leuven t)
  (set-face-attribute 'default nil :height 115) ; set font size, value is in 1/10pt, so 100 will give you 10pt)
  (set-face-attribute 'mode-line nil :background "grey88" :foreground "black" :box nil))

(use-package professional-theme
  :ensure t
  :disabled t
  :config
  (load-theme 'professional t)
  (set-face-attribute 'default nil :height 115)) ; set font size, value is in 1/10pt, so 100 will give you 10pt)

(use-package eclipse-theme
  :ensure t
  :config
  (load-theme 'eclipse t)
  (set-face-attribute 'default nil :height 115) ; set font size, value is in 1/10pt, so 100 will give you 10pt
  (set-face-attribute 'mode-line nil :background "grey87" :foreground "black" :box nil)
  (set-face-attribute 'region nil :background "LemonChiffon" :foreground "black")
  (set-face-attribute 'linum nil :background "#006666" :foreground "#FFFFDD"))

;; highlight current line
(use-package hl-line
  :ensure t
  :defer 5
  :config (global-hl-line-mode 1))

;;(set-face-background 'fringe "white") ; hide the fringe mark on the left
(setq-default indicate-buffer-boundaries 'right)

(use-package display-theme
  :ensure t
  :disabled t
  :config (display-theme-mode))

(provide 'appearance-init)

;;; appearance-init.el ends here
