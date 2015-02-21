;;; mode-line-init.el --- Part of emacs initialization

;;; Commentary:
;; Tweak mode line.

;;; Code:

;; smart mode line

(use-package smart-mode-line-powerline-theme
             :ensure t
             :disable t
             )

(use-package smart-mode-line
             :ensure t
             :defer t
             :idle
             (progn
               (setq sml/theme 'light ; options: dark, light, respectful, automatic, powerline
                     ;; sml/name-width 20
                     sml/no-confirm-load-theme t
                     sml/shorten-modes t
                     sml/shorten-directory t
                     )
               (sml/setup)
               )
             )

;; flat-looking mode-line
;;(set-face-attribute 'mode-line nil :box nil)
;;(set-face-attribute 'mode-line-inactive nil :box nil)
;;(set-face-attribute 'mode-line-highlight nil :box nil)

(provide 'mode-line-init)

;;; mode-line-init.el ends here
