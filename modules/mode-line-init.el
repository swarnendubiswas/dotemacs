;;; mode-line-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak mode line themes.

;;; Code:

(when (display-graphic-p)
  (or (use-package powerline
        :ensure t
        :disabled t
        :ensure t
        :config
        (setq powerline-display-mule-info nil
              powerline-display-buffer-size t
              powerline-display-hud nil)
        (powerline-default-theme))

      (use-package smart-mode-line
        :ensure t
        :if (eq dotemacs-theme 'leuven)
        :functions (sml/faces-from-theme sml/theme-p)
        :init
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
        :if (or (eq dotemacs-theme 'eclipse) (eq dotemacs-theme 'professional))
        :load-path "packages/emacs-powerline/"
        :config
        (setq powerline-arrow-shape 'arrow) ; curve, arrow, half, arrow14
        (set-face-attribute 'mode-line nil :background "grey88" :foreground "black" :box nil))

      (use-package telephone-line
        :ensure t
        :disabled t
        :if (or (eq dotemacs-theme 'default) (eq dotemacs-theme 'professional))
        :init
        ;; (setq telephone-line-lhs
        ;;       '((nil    . (telephone-line-minor-mode-segment))
        ;;         (accent . (telephone-line-vc-segment
        ;;                    telephone-line-process-segment))
        ;;         (nil    . (telephone-line-buffer-segment))))
        ;; (setq telephone-line-rhs
        ;;       '((nil    . (telephone-line-misc-info-segment))
        ;;         (accent . (telephone-line-major-mode-segment))
        ;;         (nil    . (telephone-line-airline-position-segment))))
        (telephone-line-mode 1))

      (use-package spaceline
        :ensure t
        :if (eq dotemacs-theme 'default)
        :init
        (require 'spaceline-config)
        (setq powerline-height 20
              ;; arrow, slant, chamfer, wave, brace, roundstub, zigzag, butt, rounded, contour, curve
              powerline-default-separator 'wave
              spaceline-anzu-p t)
        (spaceline-emacs-theme)
        (set-face-attribute 'spaceline-highlight-face nil
                            :background "burlywood"
                            :foreground "#3E3D31")
        (set-face-attribute 'powerline-active1 nil
                            :background "gray22"
                            :foreground "white"
                            :weight 'light)
        (set-face-attribute 'powerline-inactive1 nil
                            :background "grey11"
                            :foreground "white")))

  (use-package nyan-mode
    :ensure t
    :init
    (nyan-mode 1)
    ;; (nyan-start-animation)
    (setq-default nyan-wavy-trail nil)))

(provide 'mode-line-init)

;;; mode-line-init.el ends here
