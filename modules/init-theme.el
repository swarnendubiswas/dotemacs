;;; init-ui.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/gui-theme)
(defvar sb/tui-theme)

(use-package leuven-theme
  :if (or (and (display-graphic-p) (eq sb/gui-theme 'leuven))
          (and (not (display-graphic-p)) (eq sb/tui-theme 'leuven)))
  :init (load-theme 'leuven t))

(use-package zenburn-theme
  :if (or (and (display-graphic-p) (eq sb/gui-theme 'zenburn))
          (and (not (display-graphic-p)) (eq sb/tui-theme 'zenburn)))
  :init (load-theme 'zenburn t))

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

(use-package nano-theme
  :straight
  (nano-theme :type git :host github :repo "rougier/nano-theme")
  :if (or (and (display-graphic-p)
               (or (eq sb/gui-theme 'nano-light)
                   (eq sb/gui-theme 'nano-dark)))
          (and (not (display-graphic-p)) (eq sb/tui-theme 'nano-dark)))
  :init
  (setq nano-fonts-use t)
  (when (display-graphic-p)
    (cond
     ((eq sb/gui-theme 'nano-light) (load-theme 'nano-light t))
     ((eq sb/gui-theme 'nano-dark) (load-theme 'nano-dark t))))
  (unless (display-graphic-p)
    (cond
     ((eq sb/tui-theme 'nano-dark) (load-theme 'nano-dark t)))))

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

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :if (or (and (display-graphic-p)
               (or (eq sb/gui-theme 'lambda-dark)
                   (eq sb/gui-theme 'lambda-dark-faded)))
          (and (not (display-graphic-p))
               (or (eq sb/tui-theme 'lambda-dark)
                   (eq sb/tui-theme 'lambda-dark-faded))))
  :init
  (when (display-graphic-p)
    (cond
     ((eq sb/gui-theme 'lambda-dark) (load-theme 'lambda-dark t))
     ((eq sb/gui-theme 'lambda-dark-faded) (load-theme 'lambda-dark-faded t))))
  (unless (display-graphic-p)
    (cond
     ((eq sb/tui-theme 'lambda-dark) (load-theme 'lambda-dark t))
     ((eq sb/tui-theme 'lambda-dark-faded) (load-theme 'lambda-dark-faded t))))
  :custom-face
  (company-tooltip ((t (:inherit default :background "##524f5c" :foreground "white"))))
  (writegood-weasels-face ((t (:inherit default :background "coral"))))
  (writegood-duplicates-face ((t (:inherit default :background "light pink"))))
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

(provide 'init-theme)

;;; init-theme.el ends here
