;;; mode-line-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak mode line themes.

;;; Code:

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
      :if (eq dotemacs-theme 'leuven)
      :commands (sml/faces-from-theme sml/theme-p)
      :init
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
      (set-face-attribute 'mode-line nil :background "grey88" :foreground "black" :box nil))

    (use-package telephone-line
      :ensure t
      :if (eq dotemacs-theme 'default)
      :init
      (setq telephone-line-lhs
            '((nil    . (telephone-line-minor-mode-segment))
              (accent . (telephone-line-vc-segment
                         telephone-line-process-segment))
              (nil    . (telephone-line-buffer-segment))))
      (setq telephone-line-rhs
            '((nil    . (telephone-line-misc-info-segment))
              (accent . (telephone-line-major-mode-segment))
              (nil    . (telephone-line-airline-position-segment))))
      (telephone-line-mode 1)))

(provide 'mode-line-init)

;;; mode-line-init.el ends here
