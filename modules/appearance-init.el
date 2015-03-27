;;; appearance-init.el --- Part of emacs initialization

;;; Commentary:
;; Tweak emacs appearance.

;;; Code:

;; better frame titles
;;(setq frame-title-format (concat  "%b - emacs@" (system-name)))
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b") " -- " "GNU Emacs " emacs-version "@" system-name))

(use-package linum
  :init (global-linum-mode 1)) ; display line numbers in margin

(use-package simple
  :config (column-number-mode 1))

(tooltip-mode -1) 
(tool-bar-mode -1) 
(scroll-bar-mode 1) 
(menu-bar-mode -1)
(blink-cursor-mode 1) ;; enable/disable blinking cursor

;; displays the time and date in the mode line
(setq display-time-day-and-date t
      display-time-24hr-format nil)
(display-time)

;; extension to linum-mode to highlight current line number in the margin
(use-package hlinum
  :ensure t
  :config (hlinum-activate)) 

(use-package leuven-theme
  :ensure t
  :init (load-theme 'leuven t)
  :disabled t
  :config
  (set-face-attribute 'default nil :height 115) ; set font size, value is in 1/10pt, so 100 will give you 10pt)
  (set-face-attribute 'mode-line nil :background "grey90" :foreground "black" :box nil))
  
(use-package professional-theme
  :ensure t
  :init (load-theme 'professional t)
  :disabled t
  :config
  (set-face-attribute 'default nil :height 115)) ; set font size, value is in 1/10pt, so 100 will give you 10pt)

(use-package eclipse-theme
  :ensure t
  :init (load-theme 'eclipse t)
  :config
  (set-face-attribute 'default nil :height 115) ; set font size, value is in 1/10pt, so 100 will give you 10pt
  (set-face-attribute 'mode-line nil :background "grey90" :foreground "black" :box nil)
  (set-face-attribute 'region nil :background "LemonChiffon" :foreground "black"))

;; highlight current line
(use-package hl-line
  :init (global-hl-line-mode 1))

;;(set-face-background 'fringe "white") ; hide the fringe mark on the left
(setq-default indicate-buffer-boundaries 'right)

(use-package display-theme
  :ensure t
  :disabled t
  :init (display-theme-mode))

(provide 'appearance-init)

;;; appearance-init.el ends here
