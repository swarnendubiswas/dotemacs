;;; misc-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Miscellaneous package configurations.

;;; Code:

(use-package smooth-scrolling
  :ensure t)

(use-package achievements
  :disabled t
  :ensure t
  :diminish achievements-mode
  :config
  (setq achievements-idle-time 600) ; seconds
  (achievements-mode 1))

;; speed up emacs for large files
(use-package vlf
  :ensure t
  :defer 2
  :init
  ;; warn when opening files bigger than 50MB
  (setq large-file-warning-threshold (* 50 1024 1024))
  (use-package vlf-setup))

;; erase 'all' consecutive white space characters in a given direction
(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode 1))

(use-package fish-mode
  :disabled t
  :ensure t)

;; move text with M-<up> and M-<down> like eclipse
(use-package move-text
  :ensure t
  :init (move-text-default-bindings))

(use-package duplicate-thing
  :ensure t
  :bind* ("C-c C-d" . duplicate-thing))

(use-package discover-my-major
  :ensure t
  :bind* ("C-h C-m" . discover-my-major))

(use-package manage-minor-mode
  :ensure t
  :defer t)

(use-package jgraph-mode
  :ensure t
  :defer t)

(use-package graphviz-dot-mode
  :ensure t
  :defer t)

(use-package goto-last-change
  :ensure t
  :pin melpa
  ;;:load-path "lisp/" ; prefer melpa
  :bind* ("C-x C-\\" . goto-last-change))

(use-package bug-hunter
  :ensure t
  :defer t)

(use-package pos-tip
  :ensure t
  ;;:init
  ;;(setq pos-tip-background-color "#4e4e4e")
  ;;(setq pos-tip-foreground-color "#5fafd7")
  )

(use-package popwin
  :ensure t
  :config
  (setq popwin:popup-window-height 20)
  (popwin-mode 1)
  (setq display-buffer-function 'popwin:display-buffer)
  (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
  (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)
  ;; M-x dired-jump-other-window
  (push '(dired-mode :position top) popwin:special-display-config)
  ;; M-!
  (push "*Shell Command Output*" popwin:special-display-config)
  ;; M-x compile
  (push '(compilation-mode :noselect t) popwin:special-display-config)
  (push '("^\*compilation*$" :regexp t) popwin:special-display-config)
  ;; *svn info*
  (push '("*svn-info*" :noselect t) popwin:special-display-config)
  (push '(svn-info-mode :noselect t) popwin:special-display-config)
  (push '(svn-status-mode :noselect t) popwin:special-display-config)
  ;; *Paradox report-bugs
  (push '("*Paradox Report*" :noselect t) popwin:special-display-config))

(use-package pabbrev
  :disabled t
  :ensure t
  :diminish pabbrev-mode
  :init (global-pabbrev-mode 1))

(use-package golden-ratio
  :disabled t
  :ensure t
  :diminish golden-ratio-mode
  :preface
  ;; http://tuhdo.github.io/helm-intro.html
  (defun dotemacs/helm-alive-p ()
    (if (boundp 'helm-alive-p)
        (symbol-value 'helm-alive-p)))
  :init
  (add-to-list 'golden-ratio-inhibit-functions #'dotemacs/helm-alive-p)
  (golden-ratio-mode 1)
  (setq golden-ratio-auto-scale t
        ;; https://truongtx.me/2014/11/15/auto-resize-windows-by-golden-ratio-in-emacs/
        split-width-threshold nil))

;; Edit file with sudo
(use-package sudo-edit
  :ensure t
  :defer t
  :bind ("M-s e" . sudo-edit))

;; hide "Auto-saving...done" messages
;; http://emacs.stackexchange.com/questions/12556/disabling-the-auto-saving-done-message
(defun my-auto-save-wrapper (save-fn &rest args)
  (apply save-fn '(t)))

(advice-add 'do-auto-save :around #'my-auto-save-wrapper)

(use-package ssh-file-modes
  :ensure t)

;; Expand region by semantic units
(use-package expand-region
  :ensure t
  :bind* ("C-=" . er/expand-region)
  :config
  (use-package change-inner
    :ensure t
    :bind* (("M-i" . change-inner)
            ("M-o" . change-outer))))

(provide 'misc-init)

;;; misc-init.el ends here
