;;; kill-ring-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure browsing the kill ring.

;;; Code:

(use-package simple
  :config
  (setq kill-ring-max 200
        kill-do-not-save-duplicates t))

(or (use-package helm-ring
      :if (eq dotemacs-selection 'helm)
      :bind ([remap yank-pop] . helm-show-kill-ring)
      :config (helm-push-mark-mode 1))

    (use-package browse-kill-ring
      :ensure t
      :if (or (eq dotemacs-selection 'none) (eq dotemacs-selection 'ido))
      :config
      (setq browse-kill-ring-highlight-current-entry t
            browse-kill-ring-highlight-inserted-item t
            browse-kill-ring-show-preview t
            browse-kill-ring-display-duplicates t)
      ;; Binds "M-y" to browse-kill-ring
      (browse-kill-ring-default-keybindings)
      (use-package browse-kill-ring+
        :ensure t)))

(provide 'kill-ring-init)

;;; kill-ring-init.el ends here
