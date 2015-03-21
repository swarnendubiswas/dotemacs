;;; mode-line-init.el --- Part of emacs initialization

;;; Commentary:
;; Tweak mode line.

;;; Code:

;; smart mode line


(use-package powerline
  :ensure t
  :disabled t
  :config (powerline-default-theme))

(use-package smart-mode-line-powerline-theme
             :ensure t
  :disabled t)

(use-package smart-mode-line
             :ensure t
             :defer t
  :disabled t
  :config
             (progn
               (setq sml/theme 'light ; options: dark, light, respectful, automatic, powerline
                     ;; sml/name-width 20
                     sml/no-confirm-load-theme t
                     sml/shorten-modes t
                     sml/shorten-directory t
                     )
    (sml/setup)))

;; use a fork of powerline: https://github.com/jonathanchu/emacs-powerline/
(use-package powerline
  :ensure t
  :load-path "lisp/emacs-powerline/"
  :config
(setq powerline-arrow-shape 'arrow) ; curve, arrow, half, arrow14
(set-face-attribute 'mode-line nil :background "grey88" :foreground "black" :box nil)
  )

;; flat-looking mode-line
;;(set-face-attribute 'mode-line nil :box nil)
;;(set-face-attribute 'mode-line-inactive nil :box nil)
;;(set-face-attribute 'mode-line-highlight nil :box nil)

(provide 'mode-line-init)

;;; mode-line-init.el ends here
