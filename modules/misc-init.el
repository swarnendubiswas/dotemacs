;;; misc-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Miscellaneous package configurations.

;;; Code:

(defvar dotemacs-temp-directory)

(use-package help+
  :ensure t)

(use-package help-fns+ ; Improved help commands
  :ensure t
  :commands (describe-buffer describe-command describe-file describe-keymap))

(use-package help-mode+
  :ensure t)

(use-package info+
  :ensure t)

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
  :init (move-text-default-bindings))

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
  :interpreter ("gnuplot" . gnuplot-mode))

(use-package goto-last-change
  :ensure t
  :bind ("C-x C-\\" . goto-last-change))

(use-package bug-hunter
  :ensure t
  :defer t)

(use-package pos-tip
  :ensure t
  :defer t)

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
  (push '("*Selection Ring:") popwin:special-display-config) ; Selection Ring
  (push '("*ag search*") popwin:special-display-config) ; Silver searcher

  (add-to-list 'popwin:special-display-config '("*Completions*" :noselect t))
  (add-to-list 'popwin:special-display-config '("*Occur*" :noselect t))
  (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  (add-to-list 'popwin:special-display-config '("*Apropos*"))
  (add-to-list 'popwin:special-display-config '("*Warnings*"))

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
  :bind (("M-s e" . sudo-edit)
         ("M-s c" . sudo-edit-current-file)))

(use-package expand-region ; Expand region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package change-inner ; This might be useful for programming modes
  :ensure t
  :disabled t
  :after expand-region
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package expand-line
  :ensure t
  :defines expand-line-mode
  :bind ("M-i" . turn-on-expand-line-mode))

(use-package smart-mark ; Restore point with "C-g" after marking a region
  :ensure t
  :config (smart-mark-mode 1))

(use-package undo-tree ; Visualize with C-x u
  :ensure t
  :config
  (setq undo-tree-mode-lighter ""
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-auto-save-history nil
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode 1)
  (unbind-key "C-/" undo-tree-map)
  :diminish undo-tree-mode)

(use-package ignoramus ; Ignore backups, build files, et al.
  :ensure t
  :if (bound-and-true-p dotemacs-use-ignoramus-p)
  :config
  (dolist (ext '(".cb"
                 ".cb2"
                 ".dvi"
                 ".fls"
                 ".idx"
                 ".log"
                 ".o"
                 ".out"
                 ".pdf"
                 "-pkg.el"
                 ".rel"
                 ".rip"
                 ".toc"))
    (add-to-list 'ignoramus-file-basename-endings ext))
  (dolist (filenames '("GPATH"
                       "GRTAGS"
                       "GSYMS"
                       "GTAGS"
                       "TAGS"))
    (add-to-list 'ignoramus-file-basename-exact-names filenames))
  (add-to-list 'ignoramus-file-basename-regexps "\\`\\.")
  (dolist (dir '("\\`\\."
                 "auto"))
    (add-to-list 'ignoramus-file-basename-exact-names dir))
  (ignoramus-setup))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :ensure t
  :preface
  ;; https://www.masteringemacs.org/article/iedit-interactive-multi-occurrence-editing-in-your-buffer
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
        (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (if iedit-mode
              (iedit-done)
            ;; `current-word' can of course be replaced by other functions.
            (narrow-to-defun)
            (iedit-start (current-word) (point-min) (point-max)))))))
  :init (bind-key* "C-." #'iedit-mode))

(use-package session
  :ensure t
  :init
  (add-hook 'after-init-hook #'session-initialize)
  (setq session-save-file (concat dotemacs-temp-directory "session")))

(use-package persistent-scratch
  :ensure t
  :config
  (setq persistent-scratch-save-file (concat dotemacs-temp-directory "persistent-scratch"))
  ;; Enable both autosave and restore on startup
  (ignore-errors (persistent-scratch-setup-default)))

(use-package immortal-scratch
  :ensure t
  :config (immortal-scratch-mode 1))

(use-package crux
  :ensure t
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  :bind (("C-c o" . crux-open-with)
         ("C-c i" . crux-ispell-word-then-abbrev)
         ("C-c C-r" . crux-recentf-find-file)))

(provide 'misc-init)

;;; misc-init.el ends here
