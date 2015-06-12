;;; mode-line-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak mode line.

;;; Code:

(or (use-package powerline
      :disabled t
      :ensure t
      :config
      (setq powerline-display-mule-info nil
            powerline-display-buffer-size t
            powerline-display-hud nil)
      (powerline-default-theme))

    (use-package smart-mode-line
      :disabled t
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

    ;; Use a fork of powerline: https://github.com/jonathanchu/emacs-powerline/
    (use-package powerline
      :disabled t
      ;; If the path is relative, it is expanded within user-emacs-directory
      :load-path "lisp/emacs-powerline/"
      :config
      (setq powerline-arrow-shape 'arrow) ; curve, arrow, half, arrow14
      (set-face-attribute 'mode-line nil :background "grey88" :foreground "black" :box nil)))

;; flat-looking mode-line
;;(set-face-attribute 'mode-line nil :box nil)
;;(set-face-attribute 'mode-line-inactive nil :box nil)
;;(set-face-attribute 'mode-line-highlight nil :box nil)

(provide 'mode-line-init)

;;; mode-line-init.el ends here
