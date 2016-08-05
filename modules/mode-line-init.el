;;; mode-line-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak mode line themes.

;;; Code:

(defvar spaceline-anzu-p)
(defvar spaceline-hud-p)
(defvar spaceline-buffer-position-p)
(defvar spaceline-projectile-root-p)
(defvar dotemacs-mode-line-theme)
(defvar dotemacs-selection)

(cond ((eq dotemacs-mode-line-theme 'powerline) (use-package powerline
                                                  :ensure t
                                                  :config
                                                  (setq powerline-display-mule-info nil
                                                        powerline-display-buffer-size t
                                                        powerline-display-hud nil
                                                        powerline-gui-use-vcs-glyph t
                                                        powerline-default-separator 'slant)
                                                  (powerline-default-theme)
                                                  (set-face-attribute 'powerline-active1 nil
                                                                      :background "gray22"
                                                                      :foreground "white"
                                                                      :weight 'light)
                                                  (set-face-attribute 'powerline-active2 nil
                                                                      :background "grey88"
                                                                      :foreground "black")))

      ((eq dotemacs-mode-line-theme 'sml) (use-package smart-mode-line
                                            :ensure t
                                            :functions (sml/faces-from-theme sml/theme-p)
                                            :config
                                            (setq sml/theme 'light
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
                                                        powerline-default-separator 'arrow
                                                        spaceline-anzu-p t
                                                        spaceline-hud-p nil ; Prefer nyan cat mode
                                                        spaceline-buffer-position-p nil
                                                        spaceline-projectile-root-p t)
                                                  ;; Adapted from https://github.com/lunaryorn/.emacs.d/blob/master/init.el
                                                  (spaceline-compile
                                                   'biswass
                                                   ;; Left side of the mode line
                                                   '(((buffer-modified buffer-size) :face highlight-face)
                                                     anzu
                                                     '(buffer-id remote-host)
                                                     major-mode
                                                     (process :when active)
                                                     ((flycheck-error flycheck-warning flycheck-info) :when active)
                                                     (minor-modes :when active)
                                                     nyan-cat)
                                                   ;; Right segment
                                                   '(((which-function projectile-root) :separator " | ")
                                                     ;; (python-pyvenv :fallback python-pyenv)
                                                     (version-control :when active)
                                                     (battery :when active)
                                                     selection-info
                                                     input-method
                                                     ;; ((buffer-encoding-abbrev point-position line-column) :separator " | ")
                                                     ((point-position line-column) :separator " | ")
                                                     (global :when active)
                                                     ,@additional-segments
                                                     buffer-position
                                                     hud))
                                                  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-biswass))))
                                                  (set-face-attribute 'spaceline-highlight-face nil
                                                                      :background "#1A4B77"
                                                                      :foreground "white")
                                                  (if (eq dotemacs-theme 'spacemacs-light)
                                                      (set-face-attribute 'powerline-active1 nil
                                                                          :background "gray32"
                                                                          :foreground "white"
                                                                          :weight 'light)
                                                    (set-face-attribute 'powerline-active1 nil
                                                                        :background "gray22"
                                                                        :foreground "white"
                                                                        :weight 'light))
                                                  (set-face-attribute 'powerline-inactive1 nil
                                                                      :background "grey11"
                                                                      :foreground "white")
                                                  (when (eq dotemacs-selection 'helm)
                                                    (spaceline-helm-mode))
                                                  (spaceline-info-mode)))

      ((eq dotemacs-mode-line-theme 'default) ))

(use-package mode-icons
  :ensure t
  :if (not (eq dotemacs-mode-line-theme 'spaceline)) ;; https://github.com/TheBB/spaceline/issues/84
  :config (mode-icons-mode 1))

(use-package nyan-mode
  :ensure t
  :functions nyan-start-animation
  :preface
  ;; https://github.com/cemerick/.emacs.d#nyan-mode
  (defun dotemacs--toggle-nyan-mode (&optional frame)
    "Enable/disable nyan mode."
    (if (display-graphic-p frame)
        (progn
          (nyan-mode 1)
          (nyan-start-animation)
          (setq-default nyan-wavy-trail nil
                        nyan-animate-nyancat t
                        nyan-bar-length 24))
      (nyan-mode -1)))
  :init
  (add-hook 'after-make-frame-functions 'dotemacs--toggle-nyan-mode)
  (add-hook 'after-init-hook 'dotemacs--toggle-nyan-mode))

(provide 'mode-line-init)

;;; mode-line-init.el ends here
