;;; init-ui.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/gui-theme)
(defvar sb/tui-theme)

(use-package doom-themes
  :if (or (eq sb/gui-theme 'doom-molokai)
          (eq sb/gui-theme 'doom-one)
          (eq sb/gui-theme 'doom-nord)
          (eq sb/gui-theme 'doom-gruvbox))
  :commands (doom-themes-org-config doom-themes-treemacs-config)
  :init
  (if (display-graphic-p)
      (cond
       ((eq sb/gui-theme 'doom-molokai) (load-theme 'doom-molokai t))
       ((eq sb/gui-theme 'doom-one) (load-theme 'doom-one t))
       ((eq sb/gui-theme 'doom-nord) (load-theme 'doom-nord t))
       ((eq sb/gui-theme 'doom-gruvbox) (load-theme 'doom-gruvbox t)))
    (cond
     ((eq sb/tui-theme 'doom-molokai) (load-theme 'doom-molokai t))
     ((eq sb/tui-theme 'doom-one) (load-theme 'doom-one t))
     ((eq sb/tui-theme 'doom-nord) (load-theme 'doom-nord t))
     ((eq sb/tui-theme 'doom-gruvbox) (load-theme 'doom-gruvbox t)))))

(use-package monokai-theme
  :if (or (and (display-graphic-p) (eq sb/gui-theme 'monokai))
          (and (not (display-graphic-p)) (eq sb/tui-theme 'monokai)))
  :init (load-theme 'monokai t))

(use-package modus-themes
  :if (or (and (display-graphic-p)
               (or (eq sb/gui-theme 'modus-operandi)
                   (eq sb/gui-theme 'modus-vivendi)))
          (and (not (display-graphic-p))
               (or (eq sb/tui-theme 'modus-operandi)
                   (eq sb/tui-theme 'modus-vivendi))))
  :defines (modus-themes-completions modus-themes-fringes
                                     modus-themes-prompts
                                     modus-themes-lang-checkers
                                     modus-themes-hl-line
                                     modus-themes-org-blocks
                                     modus-themes-mode-line)
  :init
  (when (eq sb/modeline-theme 'default)
    (setq modus-themes-mode-line 'accented-3d))

  (when (eq sb/modeline-theme 'moody)
    (setq modus-themes-mode-line 'borderless-moody))

  (when (display-graphic-p)
    (cond
     ((eq sb/gui-theme 'modus-operandi) (load-theme 'modus-operandi t))
     ((eq sb/gui-theme 'modus-vivendi) (load-theme 'modus-vivendi t))))

  (unless (display-graphic-p)
    (cond
     ((eq sb/tui-theme 'modus-operandi) (load-theme 'modus-operandi t))
     ((eq sb/tui-theme 'modus-vivendi) (load-theme 'modus-vivendi t))))
  :custom
  (modus-themes-hl-line '(accented intense))
  (modus-themes-subtle-line-numbers t)
  (modus-themes-paren-match '(intense bold))
  (modus-themes-lang-checkers '(intense))
  (modus-themes-prompts '(intense bold gray background))
  (modus-themes-fringes 'intense)
  (modus-themes-tabs-accented t)
  (modus-themes-org-blocks 'tinted-background))

(when (and (eq sb/gui-theme 'sb/customized)
           (display-graphic-p))
  (progn
    (set-foreground-color "#333333")
    (with-eval-after-load "hl-line"
      (set-face-attribute 'hl-line nil :background "light yellow"))
    (set-face-attribute 'region nil :background "gainsboro")))

;; ;; Set `sb/gui-theme' and `sb/tui-theme' to `none' if you use this package
;; (use-package circadian
;;   :commands circadian-setup
;;   :disabled t
;;   :custom
;;   (circadian-themes '((:sunrise . nano-light)
;;                       (:sunset  . modus-vivendi)))
;;   :hook (after-init-hook . circadian-setup))

(use-package catppuccin-theme
  :if (or (eq sb/gui-theme 'catppuccin)
          (eq sb/tui-theme 'catppuccin))
  :init (load-theme 'catppuccin))

(use-package ef-themes
  :straight (:type git :host github :repo "protesilaos/ef-themes")
  :if (or (and (display-graphic-p)
               (or (eq sb/gui-theme 'ef-dark) (eq sb/gui-theme 'ef-trio-dark)
                   (eq sb/gui-theme 'ef-bio)))
          (and (not (display-graphic-p))
               (or (eq sb/tui-theme 'ef-dark) (eq sb/tui-theme 'ef-trio-dark)
                   (eq sb/tui-theme 'ef-bio))))
  :init
  (when (display-graphic-p)
    (cond
     ((eq sb/gui-theme 'ef-dark) (load-theme 'ef-dark t))
     ((eq sb/gui-theme 'ef-trio-dark) (load-theme 'ef-trio-dark t))
     ((eq sb/gui-theme 'ef-bio) (load-theme 'ef-bio t))))

  (unless (display-graphic-p)
    (cond
     ((eq sb/tui-theme 'ef-dark) (load-theme 'ef-dark t))
     ((eq sb/tui-theme 'ef-trio-dark) (load-theme 'ef-trio-dark t))
     ((eq sb/tui-theme 'ef-bio) (load-theme 'ef-bio t)))))

(provide 'init-theme)

;;; init-theme.el ends here
