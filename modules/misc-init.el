;;; misc-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Miscellaneous package configurations.

;;; Code:

(setq scroll-margin 0 ; Drag the point along while scrolling
      scroll-conservatively 1000 ; Never recenter the screen while scrolling
      scroll-error-top-bottom t) ; Move to beg/end of buffer before signalling an error

(use-package smooth-scrolling
  :ensure t
  :disabled t)

(use-package achievements
  :ensure t
  :disabled t
  :diminish achievements-mode
  :init
  (setq achievements-idle-time 600) ; seconds
  (achievements-mode 1))

;; speed up Emacs for large files
(use-package vlf
  :ensure t
  :defer 2
  :config
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
  :defer t
  :bind ("C-c d m" . manage-minor-mode))

(use-package jgraph-mode
  :ensure t
  :defer t)

(use-package graphviz-dot-mode
  :ensure t
  :defer t)

(use-package goto-last-change
  :ensure t
  :pin melpa
  :bind* ("C-x C-\\" . goto-last-change))

(use-package bug-hunter
  :ensure t
  :defer t)

(use-package pos-tip
  :ensure t
  :defer t)

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
(use-package popwin
  :ensure t
  :defer 2
  :config
  (popwin-mode 1)
  (setq popwin:popup-window-height 20
        display-buffer-function 'popwin:display-buffer)
  (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
  (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)
  ;; M-x dired-jump-other-window
  (push '(dired-mode :position top) popwin:special-display-config)
  ;; M-x compile
  ;; (push '(compilation-mode :noselect t) popwin:special-display-config)
  (push '("^\*compilation*$" :regexp t) popwin:special-display-config)
  ;; (add-to-list 'popwin:special-display-config '("*compilation*" :noselect t))
  (add-to-list 'popwin:special-display-config '("*Compile-Log*" :noselect t))
  (push '(svn-info-mode :noselect t) popwin:special-display-config)
  (push '(svn-status-mode) popwin:special-display-config)
  (push '(manage-minor-mode :noselect t) popwin:special-display-config)
  ;; (push '(help-mode :noselect t) popwin:special-display-config)
  (push '("*Help*" :regexp t) popwin:special-display-config)
  ;; (add-to-list 'popwin:special-display-config '("*Help*"))
  ;; *Paradox report-bugs
  (push '("*Paradox Report*" :regexp t :noselect t) popwin:special-display-config)
  (add-to-list 'popwin:special-display-config '("*Completions*" :noselect t))
  (add-to-list 'popwin:special-display-config '("*Occur*" :noselect t))
  (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  (add-to-list 'popwin:special-display-config '("*Remember*" :stick t))
  (add-to-list 'popwin:special-display-config '("*Org Agenda*"))
  (add-to-list 'popwin:special-display-config '("*sdic*" :noselect t))
  (add-to-list 'popwin:special-display-config '("*Apropos*"))
  (add-to-list 'popwin:special-display-config '("*Warnings*"))
  (add-to-list 'popwin:special-display-config '(" *auto-async-byte-compile*" :noselect t)))

(use-package pabbrev
  :ensure t
  :disabled t
  :diminish pabbrev-mode
  :init (global-pabbrev-mode 1))

(use-package golden-ratio
  :ensure t
  :disabled t
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
  :bind ("M-s e" . sudo-edit))

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
