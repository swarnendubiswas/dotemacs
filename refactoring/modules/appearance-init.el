;;; appearance-init.el --- Part of emacs initialization

;;; Commentary:
;; Tweak emacs appearance.

;;; Code:

;;  line and column numbers
;;(global-hl-line-mode 1) ; highlight current line
(global-linum-mode 1) ; display line numbers in margin
(column-number-mode 1)

(tooltip-mode -1) ;; tooltips
(tool-bar-mode -1) ; no toolbar with icons
(scroll-bar-mode -1) ; no scroll bars
(menu-bar-mode -1) ; no menu bar
(blink-cursor-mode 1) ;; enable/disable blinking cursor

;; displays the time and date in the mode line
(setq display-time-day-and-date t
      display-time-24hr-format nil)
(display-time)

(use-package hlinum
            :ensure t
            :init (hlinum-activate)) ; extension to linum-mode to highlight current line number in the margin


;; these are two nice themes: leuven and professional
(use-package leuven-theme
             :ensure t
             ;;:init (load-theme 'leuven t)
	     :disabled t)

(use-package professional-theme
             :ensure t
             :disabled t)

(use-package eclipse-theme
	     :ensure t
	     :init (require 'eclipse-theme))

(set-face-background 'fringe "white") ; hide the fringe mark on the left
(setq-default indicate-empty-lines t ; show empty lines after buffer end
              indicate-buffer-boundaries 'right)


(provide 'appearance-init)

;;; appearance-init.el ends here

