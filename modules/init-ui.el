;;; init-ui.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/gui-theme)
(defvar sb/tui-theme)
(defvar sb/modeline-theme)
(defvar sb/window-split)
(defvar sb/minibuffer-completion)

;; Install fonts with "M-x all-the-icons-install-fonts"
(use-package all-the-icons
  :preface
  ;; https://github.com/domtronn/all-the-icons.el/issues/120
  ;; FIXME: This seems to work only with GUI Emacs.
  (defun sb/font-installed-p (font-name)
    "Check if font with FONT-NAME is available."
    (if (find-font (font-spec :name font-name))
        t
      nil))
  :commands all-the-icons-install-fonts
  :init
  (if (and (display-graphic-p) (not (sb/font-installed-p "all-the-icons")))
      (all-the-icons-install-fonts t))
  :custom
  (all-the-icons-scale-factor 0.9)
  (all-the-icons-faicon-scale-factor 0.9)
  (all-the-icons-wicon-scale-factor 0.9)
  (all-the-icons-octicon-scale-factor 0.9)
  (all-the-icons-fileicon-scale-factor 0.9)
  (all-the-icons-material-scale-factor 0.9)
  (all-the-icons-alltheicon-scale-factor 0.9)
  (all-the-icons-color-icons t))

(use-package all-the-icons-ivy
  :if (and (eq sb/minibuffer-completion 'ivy) (display-graphic-p))
  :after ivy
  :hook
  (after-init-hook . all-the-icons-ivy-setup))

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

;; The Python virtualenv information is not shown on the modeline. The package is not being actively
;; maintained.
(use-package powerline
  :if (eq sb/modeline-theme 'powerline)
  :commands powerline-default-theme
  :init
  (setq powerline-display-hud nil ; Visualization of the position in the buffer is not useful
        ;; powerline-default-separator 'box
        powerline-display-buffer-size nil
        powerline-display-mule-info nil ; File encoding information is not useful
        powerline-gui-use-vcs-glyph t
        powerline-height 20)

  (when (eq sb/gui-theme 'leuven)
    (set-face-attribute 'mode-line nil :background "grey88" :foerground "black")
    (set-face-attribute 'mode-line-buffer-id nil :weight 'bold
                        :foreground "black" :background "gray88"))

  (if (or (eq sb/gui-theme 'nano-light) (eq sb/gui-theme 'nano-dark))
      (powerline-nano-theme)
    (powerline-nano-theme)))

(use-package doom-modeline
  :if (eq sb/modeline-theme 'doom-modeline)
  :init
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-checker-simple-format nil
        doom-modeline-indent-info nil
        doom-modeline-lsp t
        doom-modeline-minor-modes t
        doom-modeline-buffer-file-name-style 'truncate-with-project ; Reduce space on the modeline
        doom-modeline-unicode-fallback t)
  :hook
  (after-init-hook . doom-modeline-mode))

(use-package spaceline
  :defines (spaceline-hud-p spaceline-selection-info-p
                            spaceline-version-control-p spaceline-input-method-p
                            spaceline-persp-name-p
                            spaceline-buffer-encoding-abbrev-p
                            spaceline-buffer-encoding-p
                            spaceline-buffer-size-p)
  :if (eq sb/modeline-theme 'spaceline)
  :init
  (require 'spaceline-config)
  (setq spaceline-hud-p nil
        spaceline-selection-info-p nil
        spaceline-version-control-p t
        spaceline-input-method-p nil
        spaceline-buffer-size-p nil
        ;; Line ending convention used in the current buffer (unix, dos or mac)
        spaceline-buffer-encoding-abbrev-p nil
        ;; Line ending convention used in the current buffer (unix, dos or mac) without abbreviation
        spaceline-buffer-encoding-p nil
        spaceline-persp-name-p nil)

  (spaceline-emacs-theme))

;; Minimal modeline information
(use-package awesome-tray ; Minimal modeline information
  :straight (awesome-tray :type git :host github :repo "manateelazycat/awesome-tray")
  :if (eq sb/modeline-theme 'awesome-tray)
  :hook
  (after-init-hook . awesome-tray-mode)
  :custom
  (awesome-tray-active-modules '("file-path" "buffer-name" "mode-name" "location" "git"))
  (awesome-tray-git-update-duration 30 "Seconds")
  (awesome-tray-file-path-full-dirname-levels 1)
  :custom-face
  (awesome-tray-default-face ((t (:inherit default :height 0.8))))
  (awesome-tray-module-awesome-tab-face ((t (:foreground "#b83059" :weight bold :height 0.8))))
  (awesome-tray-module-buffer-name-face ((t (:foreground "#cc7700" :weight bold :height 0.8))))
  (awesome-tray-module-date-face ((t (:foreground "#717175" :weight bold :height 0.8))))
  (awesome-tray-module-file-path-face ((t (:foreground "#5e8e2e" :weight normal :height 0.8))))
  (awesome-tray-module-git-face ((t (:foreground "#cc2444" :weight normal :height 0.8))))
  (awesome-tray-module-last-command-face ((t (:foreground "#0061cc" :weight bold :height 0.8))))
  (awesome-tray-module-location-face ((t (:foreground "#cc7700" :weight normal :height 0.8))))
  (awesome-tray-module-mode-name-face ((t (:foreground "#00a400" :weight bold :height 0.8))))
  (awesome-tray-module-parent-dir-face ((t (:foreground "#5e8e2e" :weight bold :height 0.8)))))

(use-package moody
  :if (eq sb/modeline-theme 'moody)
  :commands
  (moody-replace-vc-mode moody-replace-mode-line-buffer-identification)
  :init
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package mini-modeline
  :if (eq sb/modeline-theme 'mini)
  :hook
  (after-init-hook . mini-modeline-mode)
  :config
  (setq mini-modeline-r-format '("%e" mode-line-front-space
                                 mode-line-client
                                 mode-line-modified
                                 mode-line-remote
                                 " " mode-line-buffer-identification " "
                                 ;; mode-line-position
                                 ;; mode-line-percent-position
                                 (:eval (string-trim (format-mode-line mode-line-modes)))
                                 mode-line-misc-info))
  :diminish mini-modeline-mode)

;; Display a minor-mode menu in the mode line. This is enabled if the full LSP state is shown, which
;; takes up lot of horizontal space.
(use-package minions
  :unless (bound-and-true-p doom-modeline-lsp)
  :hook
  (doom-modeline-mode-hook . minions-mode))

;; https://github.com/dbordak/telephone-line/blob/master/examples.org
(use-package telephone-line
  :if (eq sb/modeline-theme 'telephone)
  :init
  (setq telephone-line-primary-left-separator 'telephone-line-gradient
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-gradient
        telephone-line-secondary-right-separator 'telephone-line-nil
        telephone-line-height 24)
  (telephone-line-mode 1))

;; https://github.com/AnthonyDiGirolamo/airline-themes/issues/28
(use-package airline-themes
  :if (eq sb/modeline-theme 'airline)
  :demand t
  :custom
  (airline-display-directory 'airline-directory-shortened)
  :config (load-theme 'airline-doom-one t))

(use-package nano-modeline
  :straight (nano-modeline :type git :host github :repo "rougier/nano-modeline")
  :if (eq sb/modeline-theme 'nano)
  :init
  (when (eq sb/modeline-theme 'nano)
    (nano-modeline-mode 1)))

;; This does not work well with Treemacs, and it is difficult to make out the highlighted current
;; line.
(use-package auto-dim-other-buffers
  :commands adob--rescan-windows
  :hook
  (after-init-hook . auto-dim-other-buffers-mode))

;; Value is in 1/10pt, so 100 will give you 10pt
;; (set-frame-font "DejaVu Sans Mono" nil t)

;; (cond ((member "Inconsolata" (font-family-list))
;;        (set-face-attribute 'default nil :font "Inconsolata-18"))
;;       ((member "Monaco" (font-family-list))
;;        (set-face-attribute 'default nil :font "Monaco" :height 120)
;;        (setq default-frame-alist '((font . "Monaco-12")))))

(when (string= (system-name) "inspiron-7572")
  (set-face-attribute 'default nil :font "MesloLGS NF" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 130)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 130 :weight 'light)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120))

(when (string= (system-name) "dell-7506")
  (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120))

(when (string= (system-name) "swarnendu-Dell-XPS-L502X")
  (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 140)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 140 :weight 'light)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

;; Decrease minibuffer font
;; https://stackoverflow.com/questions/7869429/altering-the-font-size-for-the-emacs-minibuffer-separately-from-default-emacs
(progn
  (defun sb/minibuffer-font-setup ()
    "Customize minibuffer font."
    (set (make-local-variable 'face-remapping-alist) '((default :height 0.90))))

  (add-hook 'minibuffer-setup-hook #'sb/minibuffer-font-setup))

;; Changing height of the echo area is jarring, but limiting the height makes it difficult to see
;; useful information.

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq resize-mini-windows nil
;;                   max-mini-window-height 5)))

(use-package beacon
  :hook
  (after-init-hook . beacon-mode)
  :diminish)

(when (display-graphic-p)
  ;; Show dividers on the right of each window, more prominent than the default
  (add-hook 'after-init-hook #'window-divider-mode)
  ;; (display-battery-mode 1)
  ;; Copying text from the TUI includes the line numbers, which is an additional nuisance.
  (global-display-line-numbers-mode 1)

  ;; Default is 8 pixels, fringes do not work on the TUI. Having a fringe on the RHS seems pointless.
  (fringe-mode '(10 . 0))

  ;; Use a blinking bar for the cursor style to help identify it easily. This does not work on the TUI
  ;; Emacs because the cursor style then is controlled by the terminal application.
  (setq-default cursor-type 'box)
  ;; Set cursor color to white
  (set-cursor-color "#ffffff")
  (blink-cursor-mode 1))

;; horizontal - Split the selected window into two windows (e.g., `split-window-below'), one above
;; the other.
(when (eq sb/window-split 'vertical)
  (setq split-width-threshold nil
        split-height-threshold 0))

;; vertical - Split the selected window into two side-by-side windows (e.g., `split-window-right').
(when (eq sb/window-split 'horizontal)
  (setq split-height-threshold nil
        split-width-threshold 0))

;; Start with a window split to make use of wider screens
(when nil
  (when (string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
    (split-window-right)))

(use-package hl-line
  :commands hl-line-highlight
  :hook
  (after-init-hook . global-hl-line-mode))

;; This package disables the mouse completely which is an extreme.
(use-package disable-mouse
  :if (display-mouse-p)
  :hook
  (after-init-hook . global-disable-mouse-mode)
  :diminish disable-mouse-global-mode)

;; Move the cursor from the line of view
(use-package avoid
  :straight (:type built-in)
  :if (display-mouse-p)
  :commands mouse-avoidance-mode
  :init (mouse-avoidance-mode 'banish))

;; Icons for minibuffer completion
(use-package all-the-icons-completion
  :if (display-graphic-p)
  :commands all-the-icons-completion-mode
  :init (all-the-icons-completion-mode 1)
  :hook
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup))

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

(use-package lambda-line
  :straight (:type git :host github :repo "lambda-emacs/lambda-line")
  :if (eq sb/modeline-theme 'lambda-line)
  :hook
  (after-init-hook . lambda-line-mode)
  :custom
  (lambda-line-abbrev t "Abbreviate major modes")
  (lambda-line-space-top +0.15)
  (lambda-line-space-bottom -0.15))

(provide 'init-ui)

;;; init-ui.el ends here
