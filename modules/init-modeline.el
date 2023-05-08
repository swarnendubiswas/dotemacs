;;; init-modeline.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:
;;; utf-8; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/theme)

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
  (defun sb/powerline-sb-theme ()
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

  (sb/powerline-sb-theme))

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

(use-package awesome-tray ; Minimal modeline information
  :straight (:host github :repo "manateelazycat/awesome-tray")
  :if (eq sb/modeline-theme 'awesome-tray)
  :hook (emacs-startup-hook . awesome-tray-mode)
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

(provide 'init-modeline)

;;; init-modeline.el ends here
