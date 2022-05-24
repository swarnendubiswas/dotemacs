;;; init-ui.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/gui-theme)
(defvar sb/tui-theme)
(defvar sb/modeline-theme)
(defvar sb/window-split)

;; Install fonts with "M-x all-the-icons-install-fonts"
;; https://github.com/domtronn/all-the-icons.el/issues/120
(defun sb/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (if (find-font (font-spec :name font-name))
      t
    nil))

;; https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
(defun sb/font-available-p (font-name)
  "Find font specified by FONT-NAME."
  (find-font (font-spec :name font-name)))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (sb/font-installed-p "all-the-icons")
    (all-the-icons-install-fonts t))
  :custom
  (all-the-icons-scale-factor 0.9)
  (all-the-icons-color-icons nil))

(use-package all-the-icons-ivy
  :after ivy
  :demand t
  :commands all-the-icons-ivy-setup
  :config (all-the-icons-ivy-setup))

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
          (eq sb/gui-theme 'doom-one-light)
          (eq sb/gui-theme 'doom-one)
          (eq sb/gui-theme 'doom-nord)
          (eq sb/gui-theme 'doom-gruvbox))
  :commands (doom-themes-org-config doom-themes-treemacs-config)
  :init
  (if (display-graphic-p)
      (cond
       ((eq sb/gui-theme 'doom-molokai) (load-theme 'doom-molokai t))
       ((eq sb/gui-theme 'doom-one-light) (load-theme 'doom-one-light t))
       ((eq sb/gui-theme 'doom-one) (load-theme 'doom-one t))
       ((eq sb/gui-theme 'doom-nord) (load-theme 'doom-nord t))
       ((eq sb/gui-theme 'doom-gruvbox) (load-theme 'doom-gruvbox t)))
    (cond
     ((eq sb/tui-theme 'doom-molokai)   (load-theme 'doom-molokai t))
     ((eq sb/tui-theme 'doom-one-light) (load-theme 'doom-one-light t))
     ((eq sb/tui-theme 'doom-one) (load-theme 'doom-one t))
     ((eq sb/tui-theme 'doom-nord)      (load-theme 'doom-nord t))
     ((eq sb/tui-theme 'doom-gruvbox)   (load-theme 'doom-gruvbox t)))))

(use-package monokai-theme
  :if (or (and (display-graphic-p) (eq sb/gui-theme 'monokai))
          (and (not (display-graphic-p)) (eq sb/tui-theme 'monokai)))
  :init (load-theme 'monokai t))

(use-package modus-themes
  :defines (modus-themes-completions modus-themes-fringes
                                     modus-themes-prompts
                                     modus-themes-lang-checkers
                                     modus-themes-hl-line
                                     modus-themes-org-blocks
                                     modus-themes-mode-line)
  :if (or (and (display-graphic-p)
               (or (eq sb/gui-theme 'modus-operandi)
                   (eq sb/gui-theme 'modus-vivendi)))
          (and (not (display-graphic-p))
               (or (eq sb/tui-theme 'modus-operandi)
                   (eq sb/tui-theme 'modus-vivendi))))
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
     ((eq sb/tui-theme 'modus-vivendi) (load-theme 'modus-vivendi t)))))

(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
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
    ;; (setq frame-background-mode 'light)
    ;; (set-background-color "#ffffff")
    (set-foreground-color "#333333")
    (with-eval-after-load "hl-line"
      (set-face-attribute 'hl-line nil :background "light yellow"))
    (set-face-attribute 'region nil :background "gainsboro")))

;; Set `sb/gui-theme' and `sb/tui-theme' to `none' if you use this package
(use-package circadian
  :commands circadian-setup
  :disabled t
  :init
  (require 'solar)
  (setq calendar-latitude 26.50
        calendar-location-name "Kanpur, UP, India"
        calendar-longitude 80.23
        circadian-themes '((:sunrise . nano-light)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))

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
    (set-face-attribute 'mode-line nil :background "grey88" :foreground "black")
    (set-face-attribute 'mode-line-buffer-id nil :weight 'bold
                        :foreground "black" :background "gray88"))

  (if (or (eq sb/gui-theme 'nano-light) (eq sb/gui-theme 'nano-dark))
      (powerline-nano-theme)
    (powerline-default-theme)))

(use-package doom-modeline
  :if (eq sb/modeline-theme 'doom-modeline)
  :commands doom-modeline-mode
  :init
  ;; Requires the fonts included with `all-the-icons', run "M-x all-the-icons-install-fonts".
  (when (and (display-graphic-p) (not (sb/font-installed-p "all-the-icons")))
    (all-the-icons-install-fonts t))

  (setq doom-modeline-buffer-encoding nil
        doom-modeline-checker-simple-format nil
        doom-modeline-indent-info nil
        doom-modeline-lsp t
        doom-modeline-minor-modes t
        ;; Reduce space on the modeline
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-unicode-fallback t)
  (doom-modeline-mode 1))

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
  :commands awesome-tray-mode
  :if (eq sb/modeline-theme 'awesome-tray)
  :hook (after-init-hook . awesome-tray-mode)
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
  :commands (moody-replace-vc-mode moody-replace-mode-line-buffer-identification)
  :init
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package mini-modeline
  :diminish mini-modeline-mode
  :if (eq sb/modeline-theme 'mini)
  :hook (after-init-hook . mini-modeline-mode)
  :config
  (setq mini-modeline-r-format '("%e" mode-line-front-space
                                 mode-line-client
                                 mode-line-modified
                                 mode-line-remote
                                 " " mode-line-buffer-identification " "
                                 ;; mode-line-position
                                 ;; mode-line-percent-position
                                 (:eval (string-trim (format-mode-line mode-line-modes)))
                                 mode-line-misc-info)))

;; Display a minor-mode menu in the mode line. This is enabled if the full LSP state is shown, which
;; takes up lot of horizontal space.
(use-package minions
  :if (not (bound-and-true-p doom-modeline-lsp))
  :hook (doom-modeline-mode-hook . minions-mode))

(use-package telephone-line
  :if (eq sb/modeline-theme 'telephone-line)
  :init (telephone-line-mode 1))

;; https://github.com/AnthonyDiGirolamo/airline-themes/issues/28
(use-package airline-themes
  :if (eq sb/modeline-theme 'airline)
  :demand t
  :config (load-theme 'airline-doom-one t)
  :custom
  (airline-display-directory 'airline-directory-shortened))

(use-package nano-modeline
  :straight (nano-modeline :type git :host github
                           :repo "rougier/nano-modeline")
  :if (eq sb/modeline-theme 'nano)
  :init
  (when (eq sb/modeline-theme 'nano)
    (nano-modeline-mode 1)))

;; This does not work well with Treemacs, and it is difficult to make out the highlighted current
;; line.
(use-package auto-dim-other-buffers
  :commands (adob--rescan-windows auto-dim-other-buffers-mode)
  ;; :init (run-with-idle-timer 3 nil #'auto-dim-other-buffers-mode)
  :hook (after-init-hook . auto-dim-other-buffers-mode))

;; (cond
;;  ((sb/font-available-p "Cascadia Code")
;;   (set-frame-font "Cascadia Code-14"))
;;  ((sb/font-available-p "Menlo")
;;   (set-frame-font "Menlo-14"))
;;  ((sb/font-available-p "Inconsolata")
;;   (set-frame-font "Inconsolata-14"))
;;  ((sb/font-available-p "DejaVu Sans Mono")
;;   (set-frame-font "DejaVu Sans Mono-14")))

;; Value is in 1/10pt, so 100 will give you 10pt
;; (set-frame-font "DejaVu Sans Mono" nil t)

(cond ((member "Inconsolata" (font-family-list))
       (set-face-attribute 'default nil :font "Inconsolata-18"))
      ((member "Monaco" (font-family-list))
       (set-face-attribute 'default nil :font "Monaco" :height 120)
       (setq default-frame-alist '((font . "Monaco-12")))))

(when (string= (system-name) "inspiron-7572")
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font "JetBrains Mono" :height 130)
        (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 130)
        (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 130 :weight 'light)
        (set-face-attribute 'mode-line nil :height 110)
        (set-face-attribute 'mode-line-inactive nil :height 110))
    (progn
      (set-face-attribute 'default nil :font "Cascadia Code" :height 120)
      (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 120)
      (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 120 :weight 'light)
      (set-face-attribute 'mode-line nil :height 100)
      (set-face-attribute 'mode-line-inactive nil :height 100))))

(when (string= (system-name) "dell-7506")
  (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120))

(when (string= (system-name) "swarnendu-Dell-XPS-L502X")
  (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 140)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 140)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 140 :weight 'light)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "vindhya")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "himalaya")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "aravalli")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "satpura")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "nilgiri")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "sivalik")
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'mode-line nil :height 110)
  (set-face-attribute 'mode-line-inactive nil :height 110))

(when (string= (system-name) "garwhal")
  (set-face-attribute 'default nil :height 160)
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
(when nil
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq resize-mini-windows nil))))

(use-package beacon
  :commands beacon-mode
  :diminish
  :hook (after-init-hook . beacon-mode))

;; Show dividers on the right of each window, more prominent than the default
(add-hook 'after-init-hook #'window-divider-mode)

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

;; Copying text from the TUI includes the line numbers, which is an additional nuisance.
(when (display-graphic-p)
  (global-display-line-numbers-mode 1))

;; Not a library/file, so `eval-after-load' does not work
(diminish 'auto-fill-function)

(with-eval-after-load "simple"
  (diminish 'visual-line-mode))

;; Default is 8 pixels, we have increased it to make it more prominent on the TUI
(if (display-graphic-p)
    (fringe-mode '(10 . 0))
  (fringe-mode '(10 . 10)))

;; Make the cursor a thin horizontal bar, not a block
;; (set-default 'cursor-type '(bar . 4))

(when (display-graphic-p)
  (display-battery-mode 1))

(use-package hl-line
  :commands hl-line-highlight
  :if (display-graphic-p)
  :hook (after-init-hook . global-hl-line-mode))

(use-package centaur-tabs
  :commands (centaur-tabs-group-by-projectile-project centaur-tabs-headline-match)
  :hook (emacs-startup-hook . centaur-tabs-mode)
  :custom
  (centaur-tabs-set-icons nil "The icons may not blend well with the theme")
  (centaur-tabs-set-modified-marker t)
  ;; (centaur-tabs-modified-marker "*")
  (centaur-tabs-modified-marker "•") ; Unicode Bullet (0x2022)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-gray-out-icons t)
  (centaur-tabs-set-close-button nil)
  (centaur-tabs-show-new-tab-button nil)
  (centaur-tabs-enable-ido-completion nil)
  ;; Other styles like "wave" is not rendered on the terminal, and also does not work well with many
  ;; themes
  (centaur-tabs-style "bar")
  :config
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  :bind*
  (("M-<right>" . centaur-tabs-forward-tab)
   ("M-<left>"  . centaur-tabs-backward-tab)))

;; This package disables the mouse completely which is an extreme.
(use-package disable-mouse
  :if (display-mouse-p)
  :commands global-disable-mouse-mode
  :diminish disable-mouse-global-mode
  :hook (after-init-hook . global-disable-mouse-mode))

;; Move the cursor from the line of view
(use-package avoid
  :straight nil
  :commands mouse-avoidance-mode
  :if (display-mouse-p)
  :init (mouse-avoidance-mode 'banish))

(use-package all-the-icons-completion
  :commands all-the-icons-completion-mode
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode 1))

(provide 'init-ui)

;;; init-ui.el ends here
