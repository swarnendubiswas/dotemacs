;;; init-ui.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary: Configure UI and visual elements like icons and font size.

;;; Code:

(defvar sb/modeline-theme)
(defvar sb/window-split)
(defvar sb/minibuffer-completion)
(defvar sb/theme)

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

;; (use-package all-the-icons-dired
;;   :if (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p) (not (featurep 'dirvish)))
;;   :commands (all-the-icons-dired--refresh-advice)
;;   :hook
;;   (dired-mode-hook
;;     .
;;     (lambda ()
;;       (unless (file-remote-p default-directory)
;;         (all-the-icons-dired-mode 1))))
;;   :custom (all-the-icons-dired-monochrome nil)
;;   :diminish)

;; ;; Display icons for all buffers in ibuffer
;; (use-package all-the-icons-ibuffer
;;   :when (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p))
;;   :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode)
;;   :custom (all-the-icons-ibuffer-icon-size 0.8))

;; ;; Icons for minibuffer completion (e.g., `find-file-at-point')
;; (use-package all-the-icons-completion
;;   :when (and (eq sb/icons-provider 'all-the-icons) (display-graphic-p))
;;   :commands all-the-icons-completion-mode
;;   :init (all-the-icons-completion-mode 1)
;;   :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup))

(use-package nerd-icons
  :straight (:host github :repo "rainstormstudio/nerd-icons.el")
  ;; `nerd-icons-ivy-rich' depends on this package
  :when (or (eq sb/icons-provider 'nerd-icons) (eq sb/minibuffer-completion 'ivy))
  :custom
  (nerd-icons-color-icons nil)
  (nerd-icons-scale-factor 0.9))

(use-package nerd-icons-completion
  :straight (:host github :repo "rainstormstudio/nerd-icons-completion")
  :when (eq sb/icons-provider 'nerd-icons)
  :init (nerd-icons-completion-mode 1)
  :hook (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :straight (:host github :repo "rainstormstudio/nerd-icons-dired")
  :when (eq sb/icons-provider 'nerd-icons)
  :hook (dired-mode-hook . nerd-icons-dired-mode)
  :diminish)

(use-package nerd-icons-ibuffer
  :when (eq sb/icons-provider 'nerd-icons)
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode)
  :custom (nerd-icons-ibuffer-icon-size 1.0))

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

;; The color sometimes makes it difficult to distinguish text on terminals.

;; (use-package hl-line
;;   :commands hl-line-highlight
;;   :hook (emacs-startup-hook . global-hl-line-mode))

;; The value of font height is in 1/10pt, so 100 implies 10pt. Font preferences will be ignored when
;; we use TUI Emacs. Then, the terminal font setting will be used.

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
  (centaur-tabs-height 18)
  (centaur-tabs-set-icons nil)
  (centaur-tabs-icon-type 'nerd-icons)
  (centaur-tabs-gray-out-icons t "Gray out icons for inactive tabs")
  (centaur-tabs-show-count t "Helpful to identify tab overflows")
  :config
  ;; Unlike `awesome-tab', the icons do not blend well with all themes.

  ;; (let ((themes '("doom-one"
  ;;                 "doom-nord"
  ;;                 "doom-molokai")))
  ;;   (progn
  ;;     (if (-contains? themes (symbol-name sb/theme))
  ;;         (setq centaur-tabs-set-icons t)
  ;;       (setq centaur-tabs-set-icons nil))))

  ;; (centaur-tabs-headline-match)

  ;; Group tabs according to projectile's definition of projects
  (with-eval-after-load "projectile"
    (centaur-tabs-group-by-projectile-project)))

;; (use-package awesome-tab
;;   :preface
;;   (defun sb/awesome-tab-buffer-groups ()
;;     "`awesome-tab-buffer-groups' control buffers' group rules.
;;   Group awesome-tab with mode if buffer is derived from
;;   `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode'
;;   `magit-mode'. All buffer name start with * will group to
;;   \"Emacs\". Other buffer group by `awesome-tab-get-group-name'
;;   with project name."
;;     (list
;;       (cond
;;         (
;;           (or (string-equal "*" (substring (buffer-name) 0 1))
;;             (memq
;;               major-mode
;;               '
;;               (magit-process-mode
;;                 magit-status-mode
;;                 magit-diff-mode
;;                 magit-log-mode
;;                 magit-file-mode
;;                 magit-blob-mode
;;                 magit-blame-mode)))
;;           "Emacs")
;;         (t
;;           (awesome-tab-get-group-name (current-buffer))))))
;;   :straight (:host github :repo "manateelazycat/awesome-tab")
;;   :if (eq sb/tab-bar-handler 'awesome-tab)
;;   :hook (emacs-startup-hook . awesome-tab-mode)
;;   :bind
;;   (("M-<right>" . awesome-tab-forward-tab)
;;     ("M-<left>" . awesome-tab-backward-tab)
;;     ("M-]" . awesome-tab-ace-jump))
;;   :custom-face
;;   (awesome-tab-selected-face ((t (:inherit default :height 1.0))))
;;   (awesome-tab-unselected-face ((t (:inherit default :height 0.8))))
;;   :custom
;;   (awesome-tab-label-fixed-length 14)
;;   (awesome-tab-cycle-scope 'tabs)
;;   :config
;;   ;; The variable is declared with a `defvar', so modifying it with `:custom' will not work.
;;   (setq awesome-tab-buffer-groups-function #'sb/awesome-tab-buffer-groups))

(use-package modus-themes
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
  ;; (modus-themes-prompts '(intense bold gray background))
  ;; (modus-themes-org-blocks 'tinted-background)
  ;; (org-fontify-whole-block-delimiter-line nil)
  ;; (modus-themes-completions '((matches . (extrabold underline)) (selection . (semibold))))
  ;; (modus-themes-italic-constructs t)
  ;; (modus-themes-bold-constructs t)
  )

(when (and (eq sb/theme 'sb/customized) (display-graphic-p))
  (progn
    (set-foreground-color "#333333")
    (with-eval-after-load "hl-line"
      (set-face-attribute 'hl-line nil :background "light yellow"))
    (set-face-attribute 'region nil :background "gainsboro")))

;; Set `sb/theme' to `none' if you use this package

;; (use-package circadian
;;   :hook (emacs-startup-hook . circadian-setup)
;;   :custom (circadian-themes '((:sunrise . modus-vivendi) (:sunset . modus-vivendi))))

;; (use-package ef-themes
;;   :straight (:host github :repo "protesilaos/ef-themes")
;;   :if (or (eq sb/theme 'ef-trio-dark) (eq sb/theme 'ef-bio))
;;   :init
;;   (cond
;;     ((eq sb/theme 'ef-trio-dark)
;;       (load-theme 'ef-trio-dark t))
;;     ((eq sb/theme 'ef-bio)
;;       (load-theme 'ef-bio t))))

;; (use-package standard-themes
;;   :straight (:host github :repo "protesilaos/standard-themes")
;;   :if (or (eq sb/theme 'standard-light) (eq sb/theme 'standard-dark))
;;   :init
;;   (cond
;;     ((eq sb/theme 'standard-light)
;;       (load-theme 'standard-light t))
;;     ((eq sb/theme 'standard-dark)
;;       (load-theme 'standard-dark t))))

;; Python virtualenv information is not shown on the modeline. The package is not being actively
;; maintained.

(use-package powerline
  :preface
  (defun sb/powerline-raw (str &optional face pad)
    "Render STR as mode-line data using FACE and optionally PAD import.
PAD can be left (`l') or right (`r')."
    (when str
      (let*
        (
          (rendered-str (format-mode-line str))
          (padded-str
            (concat
              (when (and (> (length rendered-str) 0) (eq pad 'l))
                "")
              (if (listp str)
                rendered-str
                str)
              (when (and (> (length rendered-str) 0) (eq pad 'r))
                ""))))
        (if face
          (pl/add-text-property padded-str 'face face)
          padded-str))))

  ;; https://github.com/dgellow/config/blob/master/emacs.d/modules/01-style.el
  (defun sb/powerline-nano-theme ()
    "Setup a nano-like modeline"
    (interactive)
    (setq-default mode-line-format
      '
      ("%e"
        (:eval
          (let*
            (
              (active (powerline-selected-window-active))
              (face0
                (if active
                  'powerline-active0
                  'powerline-inactive0))
              (lhs
                (list
                  (powerline-raw
                    (concat
                      "GNU Emacs "
                      (number-to-string emacs-major-version)
                      "."
                      (number-to-string emacs-minor-version))
                    nil 'l)))
              (rhs
                (list
                  (when which-function-mode
                    (sb/powerline-raw which-func-format nil 'l))
                  (powerline-vc nil 'l)
                  (powerline-raw "")
                  (powerline-raw "%4l" nil 'l)
                  (powerline-raw ",")
                  (powerline-raw "%3c" nil 'r)
                  (if (buffer-modified-p)
                    (powerline-raw " ⠾" nil 'r)
                    (powerline-raw "  " nil 'r))))
              (center (list (powerline-raw "%b" nil 'r))))
            (concat
              (powerline-render lhs)
              (powerline-fill-center nil (/ (powerline-width center) 2.0))
              (powerline-render center)
              (powerline-fill nil (powerline-width rhs))
              (powerline-render rhs)))))))
  :if (eq sb/modeline-theme 'powerline)
  :commands powerline-default-theme
  :init
  (setq
    powerline-display-hud nil ; Visualization of the buffer position is not useful
    powerline-display-buffer-size nil
    powerline-display-mule-info nil ; File encoding information is not useful
    powerline-gui-use-vcs-glyph t
    powerline-height 20)

  (sb/powerline-nano-theme))

(use-package doom-modeline
  :if (eq sb/modeline-theme 'doom-modeline)
  :init
  (setq
    doom-modeline-buffer-encoding nil
    doom-modeline-checker-simple-format nil
    doom-modeline-indent-info nil
    doom-modeline-lsp t
    doom-modeline-minor-modes t
    doom-modeline-buffer-file-name-style 'file-name ; Reduce space on the modeline
    doom-modeline-unicode-fallback t)
  :hook (emacs-startup-hook . doom-modeline-mode))

;; (use-package awesome-tray ; Minimal modeline information
;;   :straight (:host github :repo "manateelazycat/awesome-tray")
;;   :if (eq sb/modeline-theme 'awesome-tray)
;;   :hook (emacs-startup-hook . awesome-tray-mode)
;;   :custom
;;   (awesome-tray-active-modules
;;     '("file-path" "buffer-name" "mode-name" "location" "belong" "flymake" "git" "hostname"))
;;   (awesome-tray-essential-modules '("file-path" "buffer-name" "location"))
;;   (awesome-tray-file-path-full-dirname-levels 2)
;;   (awesome-tray-evil-show-mode nil)
;;   (awesome-tray-meow-show-mode nil)
;;   (awesome-tray-mode-line-active-color "lavender")
;;   :custom-face
;;   (awesome-tray-default-face ((t (:inherit default :height 0.8))))
;;   (awesome-tray-module-awesome-tab-face ((t (:foreground "#b83059" :weight bold :height 0.8))))
;;   (awesome-tray-module-buffer-name-face ((t (:foreground "#cc7700" :weight bold :height 0.8))))
;;   (awesome-tray-module-date-face ((t (:foreground "#717175" :weight bold :height 0.8))))
;;   (awesome-tray-module-file-path-face ((t (:foreground "#5e8e2e" :weight normal :height 0.8))))
;;   (awesome-tray-module-git-face ((t (:foreground "#cc2444" :weight normal :height 0.8))))
;;   (awesome-tray-module-last-command-face ((t (:foreground "#0061cc" :weight bold :height 0.8))))
;;   (awesome-tray-module-location-face ((t (:foreground "#cc7700" :weight normal :height 0.8))))
;;   (awesome-tray-module-mode-name-face ((t (:foreground "#00a400" :weight bold :height 0.8))))
;;   (awesome-tray-module-parent-dir-face ((t (:foreground "#5e8e2e" :weight bold :height 0.8)))))

(defun sb/init-fonts-graphic ()
  (cond
    ((string= (system-name) "inspiron-7572")
      (progn
        ;; (set-face-attribute 'default nil :font "JetBrainsMono NF" :height 180)
        (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 170)
        (set-face-attribute 'mode-line nil :height 140)
        (set-face-attribute 'mode-line-inactive nil :height 140)))

    ((string= (system-name) "DESKTOP-4T8O69V") ; Inspiron 7572 Windows
      (progn
        (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 140)
        (set-face-attribute 'mode-line nil :height 100)
        (set-face-attribute 'mode-line-inactive nil :height 100)))

    ((string= (system-name) "dell-7506")
      (progn
        (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 150)
        (set-face-attribute 'mode-line nil :height 120)
        (set-face-attribute 'mode-line-inactive nil :height 120)))

    ((string= (system-name) "swarnendu-Dell-XPS-L502X")
      (progn
        (set-face-attribute 'default nil :font "MesloLGS NF" :height 150)
        (set-face-attribute 'mode-line nil :height 110)
        (set-face-attribute 'mode-line-inactive nil :height 110)))

    ((string= (system-name) "DESKTOP-LDLQMCO") ; CSE Desktop Windows
      (progn
        (set-face-attribute 'default nil :font "MesloLGS Nerd Font" :height 150)
        (set-face-attribute 'mode-line nil :height 110)
        (set-face-attribute 'mode-line-inactive nil :height 110)))

    ((string= (system-name) "cse-BM1AF-BP1AF-BM6AF")
      (progn
        (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 160)
        (set-face-attribute 'mode-line nil :height 120)
        (set-face-attribute 'mode-line-inactive nil :height 120)))))

(defun sb/init-fonts-daemon (frame)
  (cond
    ((string= (system-name) "inspiron-7572")
      (progn
        ;; (add-to-list 'default-frame-alist '(font . "MesloLGSNF-17"))
        (add-to-list 'default-frame-alist '(font . "JetBrainsMonoNF-17"))))))

(add-hook 'emacs-startup-hook #'sb/init-fonts-graphic)
(add-hook 'server-after-make-frame-functions #'sb/init-fonts-daemon 'append)

(use-package disable-mouse
  :hook (emacs-startup-hook . disable-mouse-global-mode)
  :diminish disable-mouse-global-mode)

;; TODO: This package is probably causing the jumping behavior with `corfu-terminal-mode'.
(use-package olivetti
  :hook
  ((text-mode-hook prog-mode-hook) . olivetti-mode) ; `emacs-startup-hook' does not work
  :custom (olivetti-body-width 108)
  :diminish)

(provide 'init-ui)

;;; init-ui.el ends here
