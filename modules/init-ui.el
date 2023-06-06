;;; init-ui.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary: Configure UI and visual elements like icons and font size.

;;; Code:

(defvar sb/modeline-theme)
(defvar sb/window-split)
(defvar sb/minibuffer-completion)

;; Install fonts with "M-x all-the-icons-install-fonts"

;; (use-package all-the-icons
;;   :preface
;;   ;; FIXME: This seems to work only with GUI Emacs.
;;   (defun sb/font-installed-p (font-name)
;;     "Check if font with FONT-NAME is available."
;;     (if (find-font (font-spec :name font-name))
;;       t
;;       nil))
;;   :when (or (eq sb/icons-provider 'all-the-icons) (eq sb/tab-bar-handler 'centaur-tabs))
;;   :commands all-the-icons-install-fonts
;;   :init
;;   (if (and (display-graphic-p) (not (sb/font-installed-p "all-the-icons")))
;;     (all-the-icons-install-fonts t))
;;   :custom
;;   ;; Small icons look nicer
;;   (all-the-icons-scale-factor 0.9)
;;   (all-the-icons-faicon-scale-factor 0.9)
;;   (all-the-icons-wicon-scale-factor 0.9)
;;   (all-the-icons-octicon-scale-factor 0.9)
;;   (all-the-icons-fileicon-scale-factor 0.9)
;;   (all-the-icons-material-scale-factor 0.9)
;;   (all-the-icons-alltheicon-scale-factor 0.9)
;;   (all-the-icons-color-icons t))

(use-package all-the-icons-dired
  :if (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p) (not (featurep 'dirvish)))
  :commands (all-the-icons-dired--refresh-advice)
  :hook
  (dired-mode-hook
    .
    (lambda ()
      (unless (file-remote-p default-directory)
        (all-the-icons-dired-mode 1))))
  :custom (all-the-icons-dired-monochrome nil)
  :diminish)

;; Display icons for all buffers in ibuffer
(use-package all-the-icons-ibuffer
  :when (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p))
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode)
  :custom (all-the-icons-ibuffer-icon-size 0.8))

;; Icons for minibuffer completion (e.g., `find-file-at-point')
(use-package all-the-icons-completion
  :when (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p))
  :commands all-the-icons-completion-mode
  :init (all-the-icons-completion-mode 1)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup))

(use-package nerd-icons
  :straight (:host github :repo "rainstormstudio/nerd-icons.el")
  :when (eq sb/icons-provider 'nerd-icons)
  :custom (nerd-icons-color-icons nil))

(use-package nerd-icons-completion
  :straight (:host github :repo "rainstormstudio/nerd-icons-completion")
  :when (eq sb/icons-provider 'nerd-icons)
  :init (nerd-icons-completion-mode 1)
  :hook (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :straight (:host github :repo "rainstormstudio/nerd-icons-dired")
  :hook (dired-mode-hook . nerd-icons-dired-mode)
  :diminish)

(use-package nerd-icons-ibuffer
  :when (eq sb/icons-provider 'nerd-icons)
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode)
  :custom (nerd-icons-ibuffer-icon-size 1.2))

;; Decrease minibuffer font size
;; https://stackoverflow.com/questions/7869429/altering-the-font-size-for-the-emacs-minibuffer-separately-from-default-emacs
(progn
  (defun sb/decrease-minibuffer-font ()
    "Customize minibuffer font."
    (set (make-local-variable 'face-remapping-alist) '((default :height 0.90))))

  (add-hook 'minibuffer-setup-hook #'sb/decrease-minibuffer-font))

;; Changing height of the echo area is jarring, but limiting the height makes it difficult to see
;; useful information.

;; (setq resize-mini-windows nil
;;       max-mini-window-height 5)

(use-package beacon ; Highlight the cursor position after the window scrolls
  :hook (emacs-startup-hook . beacon-mode)
  :diminish)

(when (display-graphic-p)
  ;; Show dividers on the right of each window, more prominent than the default
  (add-hook 'emacs-startup-hook #'window-divider-mode)

  ;; Copying text from the TUI includes the line numbers, which is a nuisance.
  (global-display-line-numbers-mode 1)

  ;; Default is 8 pixels, fringes do not work on the TUI. Having a fringe on the RHS seems
  ;; pointless.
  (fringe-mode '(10 . 0))

  ;; Cursor customizations do not work with TUI Emacs because the cursor style then is controlled by
  ;; the terminal application.

  (setq-default cursor-type 'box)
  (set-cursor-color "#ffffff") ; Set cursor color to white
  ;; Use a blinking bar for the cursor style to help identify it easily.
  (blink-cursor-mode 1))

;; vertical - Split the selected window into two windows (e.g., `split-window-below'), one above the
;; other.
(when (eq sb/window-split 'vertical)
  (setq
    split-width-threshold nil
    split-height-threshold 0))

;; horizontal - Split the selected window into two side-by-side windows (e.g.,
;; `split-window-right').
(when (eq sb/window-split 'horizontal)
  (setq
    split-height-threshold nil
    split-width-threshold 0))

(use-package hl-line
  :commands hl-line-highlight
  :hook (emacs-startup-hook . global-hl-line-mode))

;; The value of font height is in 1/10pt, so 100 implies 10pt. Font preferences will be ignored when
;; we use TUI Emacs. Then, the terminal font setting will be used.

(when (display-graphic-p)
  (cond
    ((string= (system-name) "inspiron-7572")
      (set-face-attribute 'default nil :font "MesloLGS NF" :height 170)
      (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 130)
      (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 130 :weight 'light)
      (set-face-attribute 'mode-line nil :height 110)
      (set-face-attribute 'mode-line-inactive nil :height 110))

    ((string= (system-name) "DESKTOP-4T8O69V")
      (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 140)
      (set-face-attribute 'mode-line nil :height 100)
      (set-face-attribute 'mode-line-inactive nil :height 100))

    ((string= (system-name) "dell-7506")
      (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 150)
      (set-face-attribute 'mode-line nil :height 120)
      (set-face-attribute 'mode-line-inactive nil :height 120))

    ((string= (system-name) "swarnendu-Dell-XPS-L502X")
      (set-face-attribute 'default nil :font "MesloLGS NF" :height 150)
      (set-face-attribute 'mode-line nil :height 110)
      (set-face-attribute 'mode-line-inactive nil :height 110))

    ((string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
      (set-face-attribute 'default nil :font "MesloLGS NF" :height 160)
      (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'light :height 140)
      (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 140 :weight 'light)
      (set-face-attribute 'mode-line nil :height 110)
      (set-face-attribute 'mode-line-inactive nil :height 110))))

(when (daemonp)
  (cond
    ((string= (system-name) "inspiron-7572")
      (add-to-list 'default-frame-alist '(font . "MesloLGSNF-17")))

    ((string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
      (add-to-list 'default-frame-alist '(font . "MesloLGSNF-17")))))

;; TODO: This package is probably causing the jumping behavior with `corfu-terminal-mode'.
(use-package olivetti
  :hook
  ((text-mode-hook prog-mode-hook) . olivetti-mode) ; `emacs-startup-hook' does not work
  :custom (olivetti-body-width 108)
  :diminish)

(use-package centaur-tabs
  :if (eq sb/tab-bar-handler 'centaur-tabs)
  :hook (emacs-startup-hook . centaur-tabs-mode)
  :bind*
  (("M-<right>" . centaur-tabs-forward-tab)
    ("M-<left>" . centaur-tabs-backward-tab)
    ("M-\"" . centaur-tabs-ace-jump))
  :custom
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "•") ; Unicode Bullet (0x2022)
  (centaur-tabs-set-close-button nil "I do not use the mouse")
  (centaur-tabs-show-new-tab-button nil "I do not use the mouse")
  (centaur-tabs-enable-ido-completion nil)
  ;; Other styles like "wave" are not rendered on the terminal, and also does not work well with
  ;; many themes
  (centaur-tabs-style "bar")
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-set-icons (display-graphic-p) "Icons may not be rendered in all terminals")
  (centaur-tabs-height 18)
  :config
  (with-eval-after-load "all-the-icons"
    (setq
      centaur-tabs-set-icons t
      ;; Gray out icons for inactive tabs
      centaur-tabs-gray-out-icons t))

  ;; Unlike `awesome-tab', the icons do not blend well with all themes.

  ;; (let ((themes '("doom-one"
  ;;                 "doom-nord"
  ;;                 "doom-molokai")))
  ;;   (progn
  ;;     (if (-contains? themes (symbol-name sb/theme))
  ;;         (setq centaur-tabs-set-icons t)
  ;;       (setq centaur-tabs-set-icons nil))))

  (centaur-tabs-headline-match)

  (when (fboundp 'projectile-mode)
    (centaur-tabs-group-by-projectile-project)))

(use-package awesome-tab
  :preface
  (defun sb/awesome-tab-buffer-groups ()
    "`awesome-tab-buffer-groups' control buffers' group rules.
  Group awesome-tab with mode if buffer is derived from
  `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode'
  `magit-mode'. All buffer name start with * will group to
  \"Emacs\". Other buffer group by `awesome-tab-get-group-name'
  with project name."
    (list
      (cond
        (
          (or (string-equal "*" (substring (buffer-name) 0 1))
            (memq
              major-mode
              '
              (magit-process-mode
                magit-status-mode
                magit-diff-mode
                magit-log-mode
                magit-file-mode
                magit-blob-mode
                magit-blame-mode)))
          "Emacs")
        (t
          (awesome-tab-get-group-name (current-buffer))))))
  :straight (:host github :repo "manateelazycat/awesome-tab")
  :if (eq sb/tab-bar-handler 'awesome-tab)
  :hook (emacs-startup-hook . awesome-tab-mode)
  :bind
  (("M-<right>" . awesome-tab-forward-tab)
    ("M-<left>" . awesome-tab-backward-tab)
    ("M-]" . awesome-tab-ace-jump))
  :custom-face
  (awesome-tab-selected-face ((t (:inherit default :height 1.0))))
  (awesome-tab-unselected-face ((t (:inherit default :height 0.8))))
  :custom
  (awesome-tab-label-fixed-length 14)
  (awesome-tab-cycle-scope 'tabs)
  :config
  ;; The variable is declared with a `defvar', so modifying it with `:custom' will not work.
  (setq awesome-tab-buffer-groups-function #'sb/awesome-tab-buffer-groups))

(provide 'init-ui)

;;; init-ui.el ends here
