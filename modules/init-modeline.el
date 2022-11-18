;;; init-modeline.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/gui-theme)
(defvar sb/tui-theme)

;; The Python virtualenv information is not shown on the modeline. The package is not being actively
;; maintained.
(use-package powerline
  :preface
  ;; https://github.com/dgellow/config/blob/master/emacs.d/modules/01-style.el
  (defun sb/powerline-sb-theme ()
    "Setup a nano-like modeline"
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* ((active (powerline-selected-window-active))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (lhs (list
                                  (powerline-raw (concat "GNU Emacs "
                                                         (number-to-string
                                                          emacs-major-version)
                                                         "."
                                                         (number-to-string
                                                          emacs-minor-version))
                                                 nil 'l)))
                            (rhs (list
                                  (powerline-vc nil 'l)
                                  (powerline-raw "")
                                  (powerline-raw "%4l" nil 'l)
                                  (powerline-raw ",")
                                  (powerline-raw "%3c" nil 'r)
                                  (if (buffer-modified-p) (powerline-raw " ⠾" nil 'r)
                                    (powerline-raw "  " nil 'r))))
                            (center (list (powerline-raw "%b" nil 'r))))
                       (concat (powerline-render lhs)
                               (powerline-fill-center nil (/ (powerline-width center) 2.0))
                               (powerline-render center)
                               (powerline-fill nil (powerline-width rhs))
                               (powerline-render rhs)))))))
  :if (eq sb/modeline-theme 'powerline)
  :commands powerline-default-theme
  :init
  (setq powerline-display-hud nil ; Visualization of the position in the buffer is not useful
        powerline-display-buffer-size nil
        powerline-display-mule-info nil ; File encoding information is not useful
        powerline-gui-use-vcs-glyph t
        powerline-height 20)

  (sb/powerline-sb-theme))

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
  :straight
  (awesome-tray :type git :host github :repo "manateelazycat/awesome-tray")
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

(provide 'init-modeline)

;;; init-modeline.el ends here
