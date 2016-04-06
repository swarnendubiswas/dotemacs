;;; kill-ring-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure browsing the kill ring.

;;; Code:

(or (use-package helm-ring
      :bind ([remap yank-pop] . helm-show-kill-ring)
      :config (helm-push-mark-mode 1))

    (use-package browse-kill-ring
      :ensure t
      :disabled t
      :config
      (setq browse-kill-ring-highlight-current-entry t
            browse-kill-ring-highlight-inserted-item t
            browse-kill-ring-show-preview t
            browse-kill-ring-display-duplicates t)
      (browse-kill-ring-default-keybindings)
      (use-package browse-kill-ring+
        :ensure t)
      :bind ("M-y" . browse-kill-ring)))

(provide 'kill-ring-init)

;;; kill-ring-init.el ends here
