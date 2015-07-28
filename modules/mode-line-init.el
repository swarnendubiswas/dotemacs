;;; mode-line-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak mode line themes.

;;; Code:

(when (display-graphic-p)
  (or (use-package powerline
        :ensure t
        :if (eq dotemacs-theme 'eclipse)
        :ensure t
        :config
        (setq powerline-display-mule-info nil
              powerline-display-buffer-size t
              powerline-display-hud nil)
        (powerline-default-theme))

      (use-package smart-mode-line
        :ensure t
        :if (or (eq dotemacs-theme 'leuven) (eq dotemacs-theme 'default))
        :config
        (use-package smart-mode-line-powerline-theme
          :ensure t
          :defer t)
        (setq sml/theme 'light ; options: dark, light, respectful, automatic, powerline
              ;; sml/name-width 20
              sml/no-confirm-load-theme t
              sml/mode-width 'full
              sml/shorten-modes t
              sml/shorten-directory t)
        (sml/setup))

      ;; I find the melpa powerline package more difficult to configure than this fork:
      ;; https://github.com/jonathanchu/emacs-powerline/
      (use-package powerline
        :disabled t
        :if (eq dotemacs-theme 'default)
        :load-path "lisp/emacs-powerline/"
        :config
        (setq powerline-arrow-shape 'arrow) ; curve, arrow, half, arrow14
        (set-face-attribute 'mode-line nil :background "grey88" :foreground "black" :box nil))))

(provide 'mode-line-init)

;;; mode-line-init.el ends here
