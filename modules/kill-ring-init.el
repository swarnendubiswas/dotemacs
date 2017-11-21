;;; kill-ring-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure browsing the kill ring.

;;; Code:

(defvar dotemacs-selection)

(use-package simple
  :config
  (setq kill-ring-max 200
        kill-do-not-save-duplicates t
        set-mark-command-repeat-pop t))

(use-package browse-kill-ring
  :ensure t
  :if (or (eq dotemacs-selection 'none) (eq dotemacs-selection 'ido))
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

(provide 'kill-ring-init)

;;; kill-ring-init.el ends here
