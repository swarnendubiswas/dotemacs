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

;; This does not work well with Treemacs, and it is difficult to make out the highlighted current
;; line.
(use-package auto-dim-other-buffers
  :commands adob--rescan-windows
  :hook
  (after-init-hook . auto-dim-other-buffers-mode))

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

  ;; Default is 8 pixels, fringes do not work on the TUI. Having a fringe on the RHS seems
  ;; pointless.
  (fringe-mode '(10 . 0))

  ;; Use a blinking bar for the cursor style to help identify it easily. This does not work on the
  ;; TUI Emacs because the cursor style then is controlled by the terminal application.
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

(use-package nerd-fonts
  :straight (nerd-fonts :type git :host github :repo "twlz0ne/nerd-fonts.el")
  :demand t)

(use-package all-the-icons-nerd-fonts
  :straight (all-the-icons-nerd-fonts :host github :repo "mohkale/all-the-icons-nerd-fonts")
  :after all-the-icons
  :demand t
  :config
  (all-the-icons-nerd-fonts-prefer))

;; Value is in 1/10pt, so 100 will give you 10pt
;; (set-frame-font "DejaVu Sans Mono" nil t)

;; (cond ((member "Inconsolata" (font-family-list))
;;        (set-face-attribute 'default nil :font "Inconsolata-18"))
;;       ((member "Monaco" (font-family-list))
;;        (set-face-attribute 'default nil :font "Monaco" :height 120)
;;        (setq default-frame-alist '((font . "Monaco-12")))))

;; These font preferences will be ignored when we use TUI Emacs. Then, the terminal font setting
;; will be used.

(when (display-graphic-p)
  (cond ((string= (system-name) "inspiron-7572")
         (progn
           (set-face-attribute 'default nil :font "MesloLGS NF" :height 140)
           (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 130)
           (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 130 :weight 'light)
           (set-face-attribute 'mode-line nil :height 120)
           (set-face-attribute 'mode-line-inactive nil :height 120)))

        ((string= (system-name) "dell-7506")
         (progn
           (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
           (set-face-attribute 'mode-line nil :height 120)
           (set-face-attribute 'mode-line-inactive nil :height 120)))

        ((string= (system-name) "swarnendu-Dell-XPS-L502X")
         (progn
           (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
           (set-face-attribute 'mode-line nil :height 110)
           (set-face-attribute 'mode-line-inactive nil :height 110)))

        ((string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
         (progn
           (set-face-attribute 'default nil :font "JetBrains Mono" :height 150)
           (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 140)
           (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 140 :weight 'light)
           (set-face-attribute 'mode-line nil :height 110)
           (set-face-attribute 'mode-line-inactive nil :height 110)))))

(provide 'init-ui)

;;; init-ui.el ends here
