;;; modeline-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak mode line themes.

;;; Code:

(defvar spaceline-anzu-p)
(defvar spaceline-hud-p)
(defvar spaceline-buffer-position-p)
(defvar spaceline-projectile-root-p)
(defvar dotemacs-modeline-theme)
(defvar dotemacs-theme)

(use-package simple
  :config (size-indication-mode -1))

(cond ((eq dotemacs-modeline-theme 'powerline) (use-package powerline
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
                                                                     :foreground "black")
                                                 (when (eq dotemacs-theme 'leuven)
                                                   (set-face-attribute 'mode-line nil
                                                                       :background "grey88"
                                                                       :foreground "black")
                                                   (set-face-attribute 'mode-line-buffer-id nil
                                                                       :weight 'bold
                                                                       :foreground "black"
                                                                       :background "gray88"))))

      ((eq dotemacs-modeline-theme 'sml) (use-package smart-mode-line
                                           :ensure t
                                           :config
                                           (setq sml/theme 'light
                                                 sml/no-confirm-load-theme t
                                                 sml/mode-width 'full ; Everything after the minor-modes will be right-indented
                                                 sml/shorten-modes t
                                                 sml/shorten-directory t)
                                           (sml/setup)))

      ((eq dotemacs-modeline-theme 'spaceline) (use-package spaceline
                                                 :ensure t
                                                 :defer 5
                                                 :config
                                                 (require 'spaceline-config)
                                                 (setq powerline-height 20
                                                       powerline-default-separator 'slant
                                                       spaceline-anzu-p t
                                                       spaceline-hud-p nil
                                                       spaceline-buffer-modified-p t
                                                       spaceline-buffer-position-p t
                                                       spaceline-projectile-root-p t
                                                       spaceline-paradox-menu-p t)
                                                 ;; Adapted from https://github.com/lunaryorn/.emacs.d/blob/master/init.el
                                                 ;; (spaceline-compile
                                                 ;;   'compact
                                                 ;;   ;; Left side of the mode line
                                                 ;;   '(((buffer-modified buffer-size) :face highlight-face)
                                                 ;;     anzu
                                                 ;;     '(buffer-id remote-host)
                                                 ;;     major-mode
                                                 ;;     (process :when active)
                                                 ;;     ((flycheck-error flycheck-warning flycheck-info) :when active)
                                                 ;;     (paradox-menu :when active)
                                                 ;;     (minor-modes :when active)
                                                 ;;     ((nyan-cat buffer-position) :separator " | "))
                                                 ;;   ;; Right segment
                                                 ;;   '(;;((which-function projectile-root) :separator " | ")
                                                 ;;     (which-function :when active)
                                                 ;;     (projectile-root :when active)
                                                 ;;     (version-control :when active)
                                                 ;;     (battery :when active)
                                                 ;;     selection-info
                                                 ;;     input-method
                                                 ;;     ((point-position line-column) :separator " | ")
                                                 ;;     (global :when active)
                                                 ;;     ,@additional-segments
                                                 ;;     hud))
                                                 ;; (setq-default mode-line-format '("%e" (:eval (spaceline-ml-compact))))

                                                 (spaceline-emacs-theme)
                                                 (spaceline-info-mode)

                                                 (when (eq dotemacs-theme 'spacemacs-light)
                                                   (set-face-attribute 'powerline-active1 nil
                                                                       :background "gray22"
                                                                       :foreground "white"
                                                                       :weight 'light))

                                                 (when (eq dotemacs-theme 'leuven)
                                                   (set-face-attribute 'powerline-active1 nil
                                                                       :background "gray22"
                                                                       :foreground "white"
                                                                       :weight 'light)
                                                   ;;   ;; (set-face-attribute 'powerline-active2 nil
                                                   ;;   ;;                     :background "#1A4B7"
                                                   ;;   ;;                     :foreground "white")
                                                   ;;   (set-face-attribute 'mode-line nil
                                                   ;;                       :background "grey88"
                                                   ;;                       :foreground "black")
                                                   (set-face-attribute 'mode-line-inactive nil
                                                                       :background "grey88"
                                                                       :foreground "black")
                                                   ;;   (set-face-attribute 'mode-line-buffer-id nil
                                                   ;;                       :weight 'bold
                                                   ;;                       :foreground "black"
                                                   ;;                       :inherit 'mode-line)
                                                   )

                                                 ;; (when (eq dotemacs-theme 'default)
                                                 ;;   ;; (set-face-attribute 'spaceline-highlight-face nil
                                                 ;;   ;;                     :background "#1A4B77"
                                                 ;;   ;;                     :foreground "white")
                                                 ;;   (set-face-attribute 'powerline-active1 nil
                                                 ;;                       :background "gray22"
                                                 ;;                       :foreground "white"
                                                 ;;                       :weight 'light)
                                                 ;;   (set-face-attribute 'powerline-inactive1 nil
                                                 ;;                       :background "grey11"
                                                 ;;                       :foreground "white"))

                                                 ))

      ((eq dotemacs-modeline-theme 'default) ))

(use-package nyan-mode
  :ensure t
  :disabled t
  :preface
  ;; https://github.com/cemerick/.emacs.d#nyan-mode
  (defun sb/toggle-nyan-mode (&optional frame)
    "Enable/disable nyan mode."
    (if (display-graphic-p frame)
        (progn
          (nyan-mode 1)
          (nyan-start-animation)
          (setq-default nyan-wavy-trail nil
                        nyan-animate-nyancat t
                        nyan-bar-length 16
                        nyan-cat-face-number 5))
      (nyan-mode -1)))
  :config
  (add-hook 'after-make-frame-functions 'sb/toggle-nyan-mode)
  (add-hook 'after-init-hook 'sb/toggle-nyan-mode))

(provide 'modeline-init)

;;; modeline-init.el ends here