;;; init-ui.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/theme)

(use-package doom-themes
  :if (or (eq sb/theme 'doom-molokai)
          (eq sb/theme 'doom-one)
          (eq sb/theme 'doom-nord))
  :commands (doom-themes-org-config doom-themes-treemacs-config)
  :init
  (cond
   ((eq sb/theme 'doom-molokai) (load-theme 'doom-molokai t))
   ((eq sb/theme 'doom-one) (load-theme 'doom-one t))
   ((eq sb/theme 'doom-nord) (load-theme 'doom-nord t))))

(use-package modus-themes
  :if (or (eq sb/theme 'modus-operandi)
          (eq sb/theme 'modus-vivendi))
  :defines (modus-themes-completions modus-themes-fringes
                                     modus-themes-prompts
                                     modus-themes-lang-checkers
                                     modus-themes-hl-line
                                     modus-themes-org-blocks
                                     modus-themes-mode-line)
  :init
  (when (eq sb/modeline-theme 'default)
    (setq modus-themes-mode-line 'accented-3d))

  (cond
   ((eq sb/theme 'modus-operandi) (load-theme 'modus-operandi t))
   ((eq sb/theme 'modus-vivendi) (load-theme 'modus-vivendi t)))
  :custom
  (modus-themes-hl-line '(accented intense))
  (modus-themes-subtle-line-numbers t)
  (modus-themes-paren-match '(intense bold))
  (modus-themes-lang-checkers '(intense))
  (modus-themes-prompts '(intense bold gray background))
  (modus-themes-fringes 'intense)
  (modus-themes-tabs-accented t)
  (modus-themes-org-blocks 'tinted-background))

(when (and (eq sb/theme 'sb/customized)
           (display-graphic-p))
  (progn
    (set-foreground-color "#333333")
    (with-eval-after-load "hl-line"
      (set-face-attribute 'hl-line nil :background "light yellow"))
    (set-face-attribute 'region nil :background "gainsboro")))

;; ;; Set `sb/theme' to `none' if you use this package
;; (use-package circadian
;;   :commands circadian-setup
;;   :disabled t
;;   :custom
;;   (circadian-themes '((:sunrise . nano-light)
;;                       (:sunset  . modus-vivendi)))
;;   :hook (emacs-startup-hook . circadian-setup))

(use-package catppuccin-theme
  :if (eq sb/theme 'catppuccin)
  :init (load-theme 'catppuccin))

(use-package ef-themes
  :straight (:type git :host github :repo "protesilaos/ef-themes")
  :if (or (eq sb/theme 'ef-dark) (eq sb/theme 'ef-trio-dark)
          (eq sb/theme 'ef-bio))
  :init
  (cond
   ((eq sb/theme 'ef-dark) (load-theme 'ef-dark t))
   ((eq sb/theme 'ef-trio-dark) (load-theme 'ef-trio-dark t))
   ((eq sb/theme 'ef-bio) (load-theme 'ef-bio t))))

(use-package standard-themes
  :straight (:type git :host github :repo "protesilaos/standard-themes")
  :if (or (eq sb/theme 'standard-light) (eq sb/theme 'standard-dark))
  :init
  (cond
   ((eq sb/theme 'standard-light) (load-theme 'standard-light t))
   ((eq sb/theme 'standard-dark) (load-theme 'standard-dark t))))

(provide 'init-theme)

;;; init-theme.el ends here
