;;; misc-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Miscellaneous package configurations.

;;; Code:

(use-package smooth-scrolling
  :ensure t
  :disabled t ;; This repositions the cursor to the middle on scrolling if the search string is at the boundaries.
  :config (smooth-scrolling-mode 1))

(use-package achievements
  :ensure t
  :disabled t
  :diminish achievements-mode
  :config
  (setq achievements-idle-time 600)
  (achievements-mode 1))

(use-package vlf ; Speed up Emacs for large files
  :ensure t
  :config
  (setq large-file-warning-threshold (* 50 1024 1024)) ; Warn when opening files bigger than 50MB
  (use-package vlf-setup))

(use-package hungry-delete ; Erase 'all' consecutive white space characters in a given direction
  :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode 1))

;; https://github.com/emacsmirror/emacswiki.org/blob/master/move-text.el
;; https://github.com/emacsmirror/emacswiki.org/commit/301bf7df6e48a6b3227adb13d749f3753c9dc7dc
(use-package move-text ; Move text with M-<up> and M-<down> like Eclipse
  :ensure t
  :config (move-text-default-bindings))

(use-package duplicate-thing
  :ensure t
  :bind ("C-c C-d" . duplicate-thing))

(use-package discover-my-major ; Discover key bindings and their meaning for the current Emacs major mode
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package manage-minor-mode ; Manage your minor-mode on the dedicated interface buffer
  :ensure t
  :bind ("C-c d m" . manage-minor-mode))

(use-package jgraph-mode
  :ensure t
  :defer t)

(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :config (setq graphviz-dot-indent-width 4))

(use-package gnuplot
  :ensure t
  :mode ("\\.gp\\'" . gnuplot-mode)
  :interpreter ("gnuplot" . gnuplot-mode)
  :defer t)

(use-package goto-last-change
  :ensure t
  :bind ("C-x C-\\" . goto-last-change))

(use-package bug-hunter
  :ensure t
  :defer t)

(use-package pos-tip
  :ensure t
  :defer t
  :config
  ;; Keep frame maximized
  (when (eq system-type 'windows-nt)
    (pos-tip-w32-max-width-height t)))

;; http://stackoverflow.com/questions/13242165/emacs-auto-complete-popup-menu-broken
(use-package popup
  :ensure t
  :config (setq popup-use-optimized-column-computation nil))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
(use-package popwin
  :ensure t
  :config
  (setq popwin:popup-window-height 20
        popwin:close-popup-window-timer-interval 0.5)

  ;; Disable this if we are opening helm buffers on the right
  ;; (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
  ;; (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

  ;; Helm buffers include the "help" string
  ;; (push '("*Help*" :regexp t) popwin:special-display-config)

  ;; M-x dired-jump-other-window
  (push '(dired-mode :position top) popwin:special-display-config)
  ;; M-x compile
  (push '(compilation-mode :noselect t) popwin:special-display-config)
  (add-to-list 'popwin:special-display-config '("*Compile-Log*" :noselect t))
  (push '(svn-info-mode :noselect t) popwin:special-display-config)
  (push '(svn-status-mode) popwin:special-display-config)
  (push '("^\*svn-.+\*$" :regexp t) popwin:special-display-config)
  (push '(manage-minor-mode :noselect t) popwin:special-display-config)
  (push '(help-mode :noselect t) popwin:special-display-config)
  (push '("*Paradox Report*" :regexp t :noselect t) popwin:special-display-config)
  (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
  (push '("*Kill Ring*") popwin:special-display-config) ; Browse Kill Ring

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

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :preface
  ;; http://tuhdo.github.io/helm-intro.html
  (defun dotemacs/helm-alive-p ()
    (if (boundp 'helm-alive-p)
        (symbol-value 'helm-alive-p)))
  :config
  (add-to-list 'golden-ratio-inhibit-functions #'dotemacs/helm-alive-p)
  (setq golden-ratio-auto-scale t
        ;; https://truongtx.me/2014/11/15/auto-resize-windows-by-golden-ratio-in-emacs/
        split-width-threshold nil)
  (golden-ratio-mode 1))

(use-package sudo-edit ; Edit file with sudo
  :ensure t
  :bind ("M-s e" . sudo-edit))

(use-package expand-region ; Expand region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region)
  :config
  (use-package change-inner
    :ensure t
    :disabled t
    :bind (("M-i" . change-inner)
           ("M-o" . change-outer))))

;; Restore point with "C-g" after marking a region
(use-package smart-mark
  :ensure t
  :config (smart-mark-mode 1))

(use-package undo-tree ; Visualize with C-x u
  :ensure t
  :disabled t
  :config
  (setq undo-tree-mode-lighter ""
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-auto-save-history nil
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode)

(use-package ignoramus ; Ignore backups, build files, et al. in Emacs
  :ensure t
  :if (bound-and-true-p dotemacs-use-ignoramus-p)
  :config
  (dolist (ext '(".log" ".out" ".toc" "-pkg.el" ".idx" ".fls"))
    (add-to-list 'ignoramus-file-basename-endings ext))
  (dolist (dir '("auto"))
    (add-to-list 'ignoramus-file-basename-exact-names dir))
  (ignoramus-setup))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf$" . pdf-view-mode)
  :if (unless (string-equal system-name "rain.cse.ohio-state.edu"))
  :config
  (setq-default pdf-view-display-size 'fit-page) ; fit page by default
  (setq pdf-view-resize-factor 1.10)
  (pdf-tools-install))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :ensure t)

(use-package browse-kill-ring
  :ensure t
  :if (or (eq dotemacs-selection 'ido) (eq dotemacs-selection 'none))
  :config
  (setq browse-kill-ring-highlight-current-entry t
        browse-kill-ring-highlight-inserted-item t
        browse-kill-ring-show-preview t
        browse-kill-ring-display-duplicates t)
  (use-package browse-kill-ring+
    :ensure t)
  :bind ("M-y" . browse-kill-ring))

(provide 'misc-init)

;;; misc-init.el ends here
