;;; init-ui.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/theme)

(use-package
  doom-themes
  :if (or (eq sb/theme 'doom-molokai) (eq sb/theme 'doom-one) (eq sb/theme 'doom-nord))
  :commands (doom-themes-org-config doom-themes-treemacs-config)
  :init
  (cond
    ((eq sb/theme 'doom-molokai)
      (load-theme 'doom-molokai t))
    ((eq sb/theme 'doom-one)
      (load-theme 'doom-one t))
    ((eq sb/theme 'doom-nord)
      (load-theme 'doom-nord t)))
  :config
  (with-eval-after-load "org-mode"
    (require 'doom-themes-ext-org)))

(use-package
  modus-themes
  :if (or (eq sb/theme 'modus-operandi) (eq sb/theme 'modus-vivendi))
  :defines
  (modus-themes-completions
    modus-themes-fringes
    modus-themes-prompts
    modus-themes-lang-checkers
    modus-themes-hl-line
    modus-themes-org-blocks
    modus-themes-mode-line)
  :init
  (cond
    ((eq sb/theme 'modus-operandi)
      (load-theme 'modus-operandi t))
    ((eq sb/theme 'modus-vivendi)
      (load-theme 'modus-vivendi t)))
  :custom (modus-themes-paren-match '(bold))
  ;; (modus-themes-lang-checkers '(intense))
  ;; (modus-themes-prompts '(intense bold gray background))
  ;; (modus-themes-fringes 'intense)
  ;; (modus-themes-org-blocks 'tinted-background)
  ;; (org-fontify-whole-block-delimiter-line nil)
  (modus-themes-italic-constructs t) (modus-themes-bold-constructs t))

(when (and (eq sb/theme 'sb/customized) (display-graphic-p))
  (progn
    (set-foreground-color "#333333")
    (with-eval-after-load "hl-line"
      (set-face-attribute 'hl-line nil :background "light yellow"))
    (set-face-attribute 'region nil :background "gainsboro")))

;; Set `sb/theme' to `none' if you use this package

(use-package
  circadian
  :hook (emacs-startup-hook . circadian-setup)
  :config
  (if (display-graphic-p)
    (setq circadian-themes '((:sunrise . modus-operandi) (:sunset . modus-vivendi)))
    (setq circadian-themes '((:sunrise . modus-operandi) (:sunset . modus-vivendi)))))

(use-package
  ef-themes
  :straight (:host github :repo "protesilaos/ef-themes")
  :if (or (eq sb/theme 'ef-trio-dark) (eq sb/theme 'ef-bio))
  :init
  (cond
    ((eq sb/theme 'ef-trio-dark)
      (load-theme 'ef-trio-dark t))
    ((eq sb/theme 'ef-bio)
      (load-theme 'ef-bio t))))

(use-package
  standard-themes
  :straight (:host github :repo "protesilaos/standard-themes")
  :if (or (eq sb/theme 'standard-light) (eq sb/theme 'standard-dark))
  :init
  (cond
    ((eq sb/theme 'standard-light)
      (load-theme 'standard-light t))
    ((eq sb/theme 'standard-dark)
      (load-theme 'standard-dark t))))

(use-package nordic-night-theme :init (load-theme 'nordic-night t))

(provide 'init-theme)

;;; init-theme.el ends here
