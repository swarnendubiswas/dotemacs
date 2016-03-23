;;; mode-line-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak mode line themes.

;;; Code:

(cond ((eq dotemacs-mode-line-theme 'powerline) (or (use-package powerline
                                                      :ensure t
                                                      :config
                                                      (setq powerline-display-mule-info nil
                                                            powerline-display-buffer-size t
                                                            powerline-display-hud nil)
                                                      (powerline-default-theme))

                                                    ;; I find the melpa powerline package more difficult to configure than this fork:
                                                    ;; https://github.com/jonathanchu/emacs-powerline/
                                                    (use-package powerline
                                                      :disabled t
                                                      :load-path "packages/emacs-powerline/"
                                                      :config
                                                      (setq powerline-arrow-shape 'arrow) ; curve, arrow, half, arrow14
                                                      (set-face-attribute 'mode-line nil
                                                                          :background "grey88"
                                                                          :foreground "black"
                                                                          :box nil))))

      ((eq dotemacs-mode-line-theme 'sml) (use-package smart-mode-line
                                            :ensure t
                                            :functions (sml/faces-from-theme sml/theme-p)
                                            :config
                                            (setq sml/theme 'light ; options: dark, light, respectful, automatic, powerline
                                                  ;; sml/name-width 20
                                                  sml/no-confirm-load-theme t
                                                  sml/mode-width 'full
                                                  sml/shorten-modes t
                                                  sml/shorten-directory t)
                                            (sml/setup)))

      ((eq dotemacs-mode-line-theme 'telephone-line) (use-package telephone-line
                                                       :ensure t
                                                       :config (telephone-line-mode 1)))

      ((eq dotemacs-mode-line-theme 'spaceline) (use-package spaceline
                                                  :ensure t
                                                  :config
                                                  (require 'spaceline-config)
                                                  (setq powerline-height 20
                                                        ;; Options: arrow, slant, chamfer, wave, brace, roundstub,
                                                        ;; zigzag, butt, rounded, contour, curve
                                                        powerline-default-separator 'wave
                                                        spaceline-anzu-p t
                                                        spaceline-hud-p nil
                                                        spaceline-buffer-position-p nil)
                                                  (spaceline-emacs-theme)
                                                  (set-face-attribute 'spaceline-highlight-face nil
                                                                      :background "#1A4B77"
                                                                      :foreground "white")
                                                  (set-face-attribute 'powerline-active1 nil
                                                                      :background "gray22"
                                                                      :foreground "white"
                                                                      :weight 'light)
                                                  (set-face-attribute 'powerline-inactive1 nil
                                                                      :background "grey11"
                                                                      :foreground "white")
                                                  (when (eq dotemacs-selection 'helm)
                                                    (spaceline-helm-mode))))

      ((eq dotemacs-mode-line-theme 'ergoemacs-status) (use-package ergoemacs-status
                                                         :ensure t
                                                         :config (ergoemacs-status-mode 1)))

      ((eq dotemacs-mode-line-theme 'default)))

;; https://github.com/cemerick/.emacs.d#nyan-mode
(use-package nyan-mode
  :ensure t
  :init
  (defun toggle-nyan-mode (&optional frame)
    "Enable/disable nyan mode."
    (if (display-graphic-p frame)
        (progn
          (nyan-mode 1)
          (nyan-start-animation)
          (setq-default nyan-wavy-trail nil))
      (nyan-mode -1)))
  (add-hook 'after-make-frame-functions 'toggle-nyan-mode)
  (add-hook 'after-init-hook 'toggle-nyan-mode))

(provide 'mode-line-init)

;;; mode-line-init.el ends here
