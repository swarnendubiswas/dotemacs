;;; misc-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Miscellaneous package configurations.

;;; Code:

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

(use-package vlf ; speed up Emacs for large files
  :ensure t
  :config
  (setq large-file-warning-threshold (* 50 1024 1024)) ; warn when opening files bigger than 50MB
  (use-package vlf-setup))

(use-package hungry-delete ; erase 'all' consecutive white space characters in a given direction
  :ensure t
  :diminish hungry-delete-mode
  :init (global-hungry-delete-mode 1))

(use-package move-text ; move text with M-<up> and M-<down> like eclipse
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
  :bind ("C-c d m" . manage-minor-mode))

(use-package jgraph-mode
  :ensure t
  :defer t)

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :config (setq graphviz-dot-indent-width 4))

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

;; http://stackoverflow.com/questions/13242165/emacs-auto-complete-popup-menu-broken
(use-package popup
  :ensure nil
  :defer t
  :config (setq popup-use-optimized-column-computation nil))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
(use-package popwin
  :ensure t
  :config
  (setq popwin:popup-window-height 20)
  (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
  (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)
  ;; M-x dired-jump-other-window
  (push '(dired-mode :position top) popwin:special-display-config)
  ;; M-x compile
  (push '(compilation-mode :noselect t) popwin:special-display-config)
  (add-to-list 'popwin:special-display-config '("*Compile-Log*" :noselect t))
  (push '(svn-info-mode :noselect t) popwin:special-display-config)
  (push '(svn-status-mode) popwin:special-display-config)
  (push '("^\*svn-.+\*$" :regexp t) popwin:special-display-config)
  (push '(manage-minor-mode :noselect t) popwin:special-display-config)
  ;; (push '(help-mode :noselect t) popwin:special-display-config)
  (push '("*Help*" :regexp t) popwin:special-display-config)
  (push '("*Paradox Report*" :regexp t :noselect t) popwin:special-display-config)
  (add-to-list 'popwin:special-display-config '("*Completions*" :noselect t))
  (add-to-list 'popwin:special-display-config '("*Occur*" :noselect t))
  (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  (add-to-list 'popwin:special-display-config '("*Remember*" :stick t))
  (add-to-list 'popwin:special-display-config '("*Org Agenda*"))
  (add-to-list 'popwin:special-display-config '("*sdic*" :noselect t))
  (add-to-list 'popwin:special-display-config '("*Apropos*"))
  (add-to-list 'popwin:special-display-config '("*Warnings*"))
  (add-to-list 'popwin:special-display-config '(" *auto-async-byte-compile*" :noselect t))
  (popwin-mode 1))

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

(use-package sudo-edit ; Edit file with sudo
  :ensure t
  :bind ("M-s e" . sudo-edit))

(use-package ssh-file-modes
  :ensure t
  :defer t)

(use-package expand-region ; Expand region by semantic units
  :ensure t
  :bind* ("C-=" . er/expand-region)
  :config
  (use-package change-inner
    :ensure t
    :bind* (("M-i" . change-inner)
            ("M-o" . change-outer))))

(use-package expand-line
  :ensure t
  :defines expand-line-mode
  :init (expand-line-mode 1))

(use-package smart-mark
  :ensure t
  :disabled t
  :init (smart-mark-mode 1))

(use-package undo-tree ; Visualize with C-x u
  :ensure t
  :defer t
  :config
  (setq undo-tree-mode-lighter ""
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-auto-save-history nil
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode)

;; avoid Emacs querying "active processes exist; kill them and exit anyway?", since we are creating an inferior python
;; process and aspell
(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(use-package ignoramus
  :ensure t
  :init (ignoramus-setup))

(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))

(provide 'misc-init)

;;; misc-init.el ends here
