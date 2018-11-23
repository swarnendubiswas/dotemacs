;;; misc-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-
;;; Commentary:
;; Miscellaneous package configurations.

;;; Code:

(defvar dotemacs-temp-directory)
(defvar dotemacs-selection)

(use-package helpful
  :ensure t
  :bind
  (("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h f" . helpful-function)))

;; M-x vlf <PATH-TO-FILE>
(use-package vlf ; Speed up Emacs for large files
  :ensure t
  :config (setq large-file-warning-threshold (* 50 1024 1024)) ; Warn when opening files bigger than 50MB
  (use-package vlf-setup))

(use-package hungry-delete ; Erase 'all' consecutive white space characters in a given direction
  :ensure t
  :diminish hungry-delete-mode
  :init (global-hungry-delete-mode 1))

(use-package move-text ; Move text with M-<up> and M-<down> like Eclipse
  :ensure t
  :init (move-text-default-bindings))

(use-package duplicate-thing
  :ensure t
  :bind* ("C-c C-d" . duplicate-thing))

(use-package discover-my-major ; Discover key bindings and their meaning for the current Emacs major mode
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

(use-package manage-minor-mode ; Manage your minor-mode on the dedicated interface buffer
  :ensure t
  :bind ("C-c d m" . manage-minor-mode))

(use-package jgraph-mode
  :ensure t
  :mode ("\\.jgr\\'" . jgraph-mode))

(use-package graphviz-dot-mode
  :ensure t
  :mode "\\.dot\\'"
  :config (setq graphviz-dot-indent-width 4))

(use-package gnuplot
  :ensure t
  :mode ("\\.gp\\'" . gnuplot-mode)
  :interpreter ("gnuplot" . gnuplot-mode))

(use-package goto-last-change
  :ensure t
  :bind ("C-x C-\\" . goto-last-change))

(use-package bug-hunter
  :ensure t)

;; http://stackoverflow.com/questions/13242165/emacs-auto-complete-popup-menu-broken
(use-package popup
  :ensure t
  :disabled t
  :config (setq popup-use-optimized-column-computation nil))

;; https://git.framasoft.org/distopico/distopico-dotemacs/blob/master/emacs/modes/conf-popwin.el
;; https://github.com/dakrone/eos/blob/master/eos-core.org
(use-package popwin
  :ensure t
  ;; popwin does not support ecb or neotree. Only direx seems to be supported. Too bad, would loved to have both popwin and ecb enabled.
  ;; https://github.com/m2ym/popwin-el/issues/9
  :if (not (bound-and-true-p dotemacs-use-ecb))
  :demand t
  :disabled t
  :config
  (popwin-mode 1)
  (defvar popwin:special-display-config-backup popwin:special-display-config)
  (setq popwin:popup-window-height 20
        popwin:close-popup-window-timer-interval 0.5)

  ;; Helm buffers include the "help" string
  (push '("*Help*" :noselect t) popwin:special-display-config)

  (push '(dired-mode :position top) popwin:special-display-config)
  (push '(compilation-mode :noselect t) popwin:special-display-config)
  (push '("*Compile-Log*" :noselect t) popwin:special-display-config)
  (push '(svn-info-mode :noselect t) popwin:special-display-config)
  (push '(svn-status-mode) popwin:special-display-config)
  (push '("^\*svn-.+\*$" :regexp t) popwin:special-display-config)
  (push '("*manage-minor-mode*" :noselect t) popwin:special-display-config)
  (push '("*Paradox Report*" :regexp t :noselect t) popwin:special-display-config)
  (push '("*undo-tree\*" :width 0.3 :position right) popwin:special-display-config)
  (push '("*Kill Ring*" :noselect nil) popwin:special-display-config)
  (push '("*Selection Ring:") popwin:special-display-config)
  (push '("*ag search*" :noselect nil) popwin:special-display-config)
  (push '("*ggtags-global*" :stick t :noselect nil :height 30) popwin:special-display-config)
  (push '("*Flycheck errors*" :noselect nil) popwin:special-display-config)
  (push '("*ripgrep-search*" :noselect nil) popwin:special-display-config)
  (push '("^\*magit:.+\*$" :noselect nil) popwin:special-display-config)
  (push '("*xref*" :noselect nil) popwin:special-display-config)
  (push '("*helpful\*" :noselect nil) popwin:special-display-config)

  (add-to-list 'popwin:special-display-config '("*Completions*" :stick t :noselect t))
  (add-to-list 'popwin:special-display-config '("*Occur*" :noselect nil))
  (add-to-list 'popwin:special-display-config '("*Backtrace*"))
  (add-to-list 'popwin:special-display-config '("*Apropos*"))
  (add-to-list 'popwin:special-display-config '("*Warnings*")))

(use-package sudo-edit ; Edit file with sudo
  :ensure t
  :bind ("M-s e" . sudo-edit))

(use-package expand-region ; Expand region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package change-inner ; This might be useful for programming modes
  :ensure t
  :disabled t
  :after expand-region)

(use-package expand-line
  :ensure t
  :disabled t
  :bind ("M-i" . turn-on-expand-line-mode))

(use-package smart-mark ; Restore point with "C-g" after marking a region
  :ensure t
  :config (smart-mark-mode 1))

(use-package ignoramus ; Ignore backups, build files, et al.
  :ensure t
  :if (bound-and-true-p dotemacs-use-ignoramus-p)
  :config
  (dolist (ext '(".cb"
                 ".cb2"
                 ".dvi"
                 ".fls"
                 ".idx"
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
                       "TAGS"
                       "__init__.py"))
    (add-to-list 'ignoramus-file-basename-exact-names filenames))
  (add-to-list 'ignoramus-file-basename-regexps "\\`\\.")
  (dolist (dir '("\\`\\."
                 "__pycache__"
                 "auto"))
    (add-to-list 'ignoramus-file-basename-exact-names dir))
  (ignoramus-setup))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :ensure t
  :bind* ("C-." . iedit-mode))

(use-package persistent-scratch
  :ensure t
  :config
  (setq persistent-scratch-save-file (concat dotemacs-temp-directory "persistent-scratch"))
  ;; Enable both autosave and restore on startup
  (ignore-errors (persistent-scratch-setup-default)))

(use-package immortal-scratch
  :ensure t
  :disabled t
  :config (immortal-scratch-mode 1))

(use-package crux
  :ensure t
  :bind ("C-c i" . crux-ispell-word-then-abbrev))

(use-package elf-mode
  :ensure t)

(use-package apt-sources-list
  :ensure t
  :mode ("\\.list\\'" . apt-sources-list-mode))

(use-package amx
  :ensure t
  :disabled t
  :commands (amx amx-mode)
  :config
  (setq amx-save-file (concat dotemacs-temp-directory "amx-items"))
  (amx-mode 1)
  :bind*
  (([remap execute-extended-command] . amx)
   ("<f1>" . amx)))

(use-package rainbow-delimiters
  :ensure t
  ;; :init
  ;; (dolist (hook '(text-mode-hook prog-mode-hook))
  ;;   (add-hook hook #'rainbow-delimiters-mode))
  :hook ((text-mode prog-mode) . rainbow-delimiters-mode))

(use-package ssh-config-mode
  :ensure t
  :mode ("/\\.ssh/config\\'" . ssh-config-mode)
  :mode ("/sshd?_config\\'" . ssh-config-mode)
  :mode ("/known_hosts\\'" . ssh-known-hosts-mode)
  :mode ("/authorized_keys2?\\'" . ssh-authorized-keys-mode)
  :config (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

(use-package browse-kill-ring
  :ensure t
  :if (eq dotemacs-selection 'none)
  :commands browse-kill-ring
  :config
  (require 'popwin-browse-kill-ring)
  (setq browse-kill-ring-highlight-current-entry t
        browse-kill-ring-highlight-inserted-item t
        browse-kill-ring-show-preview t
        browse-kill-ring-display-duplicates t)
  ;; Binds "M-y" to browse-kill-ring
  (browse-kill-ring-default-keybindings)
  (use-package browse-kill-ring+
    :ensure t))

(use-package super-save
  :ensure t
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode 1))

(provide 'misc-init)

;;; misc-init.el ends here
