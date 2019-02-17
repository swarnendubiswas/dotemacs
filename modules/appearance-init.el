;;; appearance-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Tweak Emacs appearance.

;;; Code:

(defvar dotemacs-theme)
(defvar dotemacs-use-ecb)
(defvar dotemacs-temp-directory)

(setq frame-title-format (list '(buffer-file-name "%f" "%b")) ; Better frame title
      indicate-empty-lines t
      custom-safe-themes t)

;; This is a buffer-local variable.
(setq-default indicate-buffer-boundaries 'right)

(use-package tool-bar
  :if (fboundp 'tool-bar-mode)
  :config
  ;; Maximize the vertical space
  (tool-bar-mode -1))

(use-package menu-bar
  :if (fboundp 'menu-bar-mode)
  :disabled t
  :config
  ;; One can learn many shortcuts from the menu bar entries.
  (menu-bar-mode 1))

(use-package tooltip
  :config (tooltip-mode -1))

(use-package scroll-bar
  :if (fboundp 'scroll-bar-mode)
  :config
  ;; Maximize the space for displaying the buffer.
  (scroll-bar-mode -1))

(use-package frame
  :config
  ;; ;; Start with Emacs window maximized:
  ;; ;; http://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
  ;; ;; Only the frame that Emacs creates at startup, but will not touch any subsequent frames you create.
  ;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
  ;; ;; It will maximize all frames: both the first one and any others you create. Options: fullheight, fullboth
  ;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Blinking cursor can be distracting
  (blink-cursor-mode -1))

(toggle-frame-maximized) ; Maximize Emacs on startup

;; Not the most useful information within Emacs
(use-package time ; Display the time and date in the mode line
  :disabled t
  :config
  (setq display-time-day-and-date t
        display-time-24hr-format nil
        display-time-default-load-average nil)
  (display-time))

;; linum-mode can slow down Emacs for large files:
;; http://blog.binchen.org/posts/turn-off-linum-mode-when-file-is-too-big.html
;; Display line numbers in the margin
(or (use-package linum
      :disabled t
      :config (global-linum-mode 1))

    (use-package nlinum ; Might improve performance with jit font locking.
      :ensure t
      :disabled t ;; Does not work with emacsclient, since the frame is created later.
      :config (global-nlinum-mode 1)))

(use-package hlinum ; Extension to linum-mode to highlight current line number in the margin
  :ensure t
  :disabled t
  :config (hlinum-activate))

(cond ((eq dotemacs-theme 'leuven) (use-package leuven-theme
                                     :ensure t
                                     :config (load-theme 'leuven t)))

      ((eq dotemacs-theme 'professional) (use-package professional-theme
                                           :ensure t
                                           :config (load-theme 'professional t)))

      ((eq dotemacs-theme 'eclipse) (use-package eclipse-theme
                                      :ensure t
                                      :config
                                      (load-theme 'eclipse t)
                                      (set-background-color "white")
                                      (set-face-attribute 'region nil
                                                          :background "LemonChiffon"
                                                          :foreground "black")
                                      (set-face-attribute 'mode-line nil
                                                          :background "grey88"
                                                          :foreground "black"
                                                          :box nil)
                                      ;; Org-mode customizations inspired from leuven theme
                                      (with-eval-after-load "org"
                                        (set-face-attribute 'org-level-1 nil
                                                            :height 1.2
                                                            :overline "#A7A7A7"
                                                            :foreground "#3C3C3C"
                                                            :background "#F5F5F5")
                                        (set-face-attribute 'org-level-2 nil
                                                            :height 1.1
                                                            :overline "#123555"
                                                            :foreground "#123555"
                                                            :background "#E5F4FB"))))

      ((eq dotemacs-theme 'spacemacs-light) (use-package spacemacs-common
                                              :ensure spacemacs-theme
                                              :config
                                              (load-theme 'spacemacs-light t)
                                              (add-to-list 'default-frame-alist '(background-color . "#fbf8ef"))))

      ((eq dotemacs-theme 'zenburn) (use-package zenburn
                                      :ensure t
                                      :config (load-theme 'zenburn t)))

      ((eq dotemacs-theme 'solarized-light) (use-package solarized-light-theme
                                              :ensure solarized-theme
                                              :config
                                              (load-theme 'solarized-light t)
                                              (setq solarized-distinct-fringe-background t)))

      ((eq dotemacs-theme 'solarized-dark) (use-package solarized-dark-theme
                                             :ensure solarized-theme
                                             :config (load-theme 'solarized-dark t)))

      ((eq dotemacs-theme 'tangotango) (use-package tangotango-theme
                                         :ensure t
                                         :config (load-theme 'tangotango t)))

      ((eq dotemacs-theme 'default) (progn
                                      (set-face-attribute 'region nil
                                                          :background "light sky blue"
                                                          :foreground "white"))))

(use-package tabbar
  :ensure t
  :preface
  (defun sb/tabbar-modification-state-change ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  (defun sb/tabbar-on-buffer-modification ()
    (set-buffer-modified-p t)
    (sb/tabbar-modification-state-change))

  (defun sb/tabbar-on-buffer-revert ()
    (set-buffer-modified-p nil)
    (sb/tabbar-modification-state-change))
  :init (tabbar-mode 1)
  :config
  (setq tabbar-use-images nil ; Speed up by not using images
        tabbar-auto-scroll-flag t
        tabbar-background-color nil
        tabbar-separator '(0.2))
  (setq tabbar-separator '(1)) ;; set tabbar-separator size to 1 pixel

  (add-hook 'after-save-hook #'sb/tabbar-modification-state-change)
  (add-hook 'first-change-hook #'sb/tabbar-on-buffer-modification)
  (add-hook 'after-revert-hook #'sb/tabbar-on-buffer-revert)

  ;; Add a buffer modification state indicator in the tab label, and place a space around the label to make it look less
  ;; crowded.
  (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
    (setq ad-return-value
          (if (and (buffer-modified-p (tabbar-tab-value tab))
                   (buffer-file-name (tabbar-tab-value tab)))
              (concat " * " (concat ad-return-value " "))
            (concat " " (concat ad-return-value " ")))))

  (if (eq dotemacs-theme 'spacemacs-light)
      (progn
        (set-face-attribute 'tabbar-unselected nil
                            :inherit 'tabbar-unselected
                            ;; :background "gray90"
                            ;; :height 0.9
                            )
        (set-face-attribute 'tabbar-selected nil
                            :inherit 'tabbar-default
                            :height 1
                            ;; :bold t
                            ;; :underline nil
                            )
        ;; (set-face-attribute 'tabbar-separator nil
        ;;                     :inherit 'tabbar-separator
        ;;                     :height 1.0)
        ;; (set-face-attribute 'tabbar-modified nil
        ;;                     :inherit 'tabbar-modified
        ;;                     ;; :foreground "red"
        ;;                     :height 0.9)
        ;; (set-face-attribute 'tabbar-selected-modified nil
        ;;                     :inherit 'tabbar-selected-modified
        ;;                     ;; :foreground "dark green"
        ;;                     :height 1.1
        ;;                     :bold t)
        )
    (progn
      ;; (set-face-attribute 'tabbar-default nil
      ;;                     :inherit nil
      ;;                     :height 0.9
      ;;                     :weight 'normal
      ;;                     :width 'normal
      ;;                     :slant 'normal
      ;;                     :underline nil
      ;;                     :strike-through nil
      ;;                     :stipple nil
      ;;                     :background "gray80"
      ;;                     :foreground "black"
      ;;                     ;; :box '(:line-width 2 :color "white" :style nil)
      ;;                     :box nil
      ;;                     ;; :family "Lucida Grande"
      ;;                     ;;:family "helvetica"
      ;;                     )

      ;; (set-face-attribute 'tabbar-default nil
      ;;                     :background "gray80")

      (set-face-attribute 'tabbar-selected nil
                          :inherit 'tabbar-default
                          :background "gray95"
                          :foreground "gray20"
                          :height 0.95
                          :box '(:line-width 3 :color "grey95" :style nil))

      ;; (set-face-attribute 'tabbar-selected nil
      ;;                 :inherit 'tabbar-default
      ;;                 :background "#f2f2f6"
      ;;                 :foreground "black"
      ;;                 ;; :box '(:line-width 1 :color "black" :style pressed-button)
      ;;                 :height 1.2
      ;;                 :bold t
      ;;                 :underline nil)

      ;; (set-face-attribute 'tabbar-unselected nil
      ;;                     :inherit 'tabbar-default
      ;;                     :background "gray80"
      ;;                     :box '(:line-width 3 :color "grey80" :style nil))

      ;; (set-face-attribute 'tabbar-unselected nil
      ;;                     :background "gray88"
      ;;                     :foreground "gray30"
      ;;                     :height 0.9)

      ;; (set-face-attribute 'tabbar-button nil
      ;;                     :inherit 'tabbar-default
      ;;                     :box nil)

      ;; (set-face-attribute 'tabbar-separator nil
      ;;                     :background "grey50"
      ;;                     :foreground "grey50"
      ;;                     :height 1.0)

      ;; (set-face-attribute 'tabbar-separator nil
      ;;                     :height 1.0)

      ;; (set-face-attribute 'tabbar-highlight nil
      ;;                     :underline t
      ;;                     :background "lemon chiffon")

      ;; (set-face-attribute 'tabbar-button nil
      ;;                     ;; :box '(:line-width 1 :color "gray72" :style released-button)
      ;;                     )

      ;; (set-face-attribute 'tabbar-modified nil
      ;;                     :background "gray88"
      ;;                     :foreground "red"
      ;;                     ;; :box '(:line-width 1 :color "black" :style sunken)
      ;;                     )

      ;; (set-face-attribute 'tabbar-selected-modified nil
      ;;                     :background "#f2f2f6"
      ;;                     :foreground "dark green"
      ;;                     ;; :box '(:line-width 1 :color "black" :style sunken)
      ;;                     :box '(:style pressed-button)
      ;;                     :height 1
      ;;                     :bold t
      ;;                     :underline nil)

      ))

  :bind (:map tabbar-mode-map
              ("M-<left>" . tabbar-backward-tab)
              ("M-<right>" . tabbar-forward-tab)))

;; ;; Set font face independent of the color theme, value is in 1/10pt, so 100 will give you 10pt.
;; (if (eq system-type 'windows-nt)
;;     (set-face-attribute 'default nil
;;                         :family "Consolas"
;;                         :height 120)
;;   (progn
;;     (cond ((string-equal (system-name) "consensus.ices.utexas.edu") (set-face-attribute
;;                                                                      'default nil
;;                                                                      :height 135))
;;           ((string-equal (system-name) "swarnendu") (set-face-attribute 'default nil
;;                                                                         ;; :family "Fira Code"
;;                                                                         :height 160))
;;           (t (set-face-attribute 'default nil
;;                                  ;; :family "Droid Sans Mono"
;;                                  :height 120)))))

;; (set-default-font "monaco")
;; (set-default-font "-apple-Monaco-normal-normal-normal-*-16-*-*-*-*-0-iso10646-1")

;; (set-frame-font "hack" nil t)
;; (set-frame-font "Hack 13" nil t)
;; (set-frame-font "FiraCode")
(set-frame-font "DejaVu Sans Mono" nil t)
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 120)

(use-package minimap
  :ensure t
  :disabled t
  :diminish minimap-mode
  :config
  (setq minimap-window-location 'right
        minimap-minimum-width 10
        minimap-width-fraction 0.08
        minimap-update-delay 0.2
        minimap-automatically-delete-window nil)
  (add-to-list 'minimap-major-modes 'html-mode)
  (add-to-list 'minimap-major-modes 'text-mode)
  (add-to-list 'minimap-major-modes 'latex-mode)
  (minimap-mode 1))

;; https://stackoverflow.com/questions/27845980/how-do-i-remove-newline-symbols-inside-emacs-vertical-border
;; (setf (cdr (assq 'continuation fringe-indicator-alist))
;;       ;; '(nil nil) ;; no continuation indicators
;;       '(nil right-curly-arrow) ;; right indicator only
;;       ;; '(left-curly-arrow nil) ;; left indicator only
;;       ;; '(left-curly-arrow right-curly-arrow) ;; default
;;       )

;; https://stackoverflow.com/questions/3281581/how-to-word-wrap-in-emacs
(global-visual-line-mode 1)
(diminish 'visual-line-mode)

(provide 'appearance-init)

;;; appearance-init.el ends here
