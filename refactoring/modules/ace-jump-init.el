;;; ace-jump-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup ace jump modes.

;;; Code:

(use-package ace-jump-mode
             :ensure t
             :disabled t
             :init (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
             )

(use-package ace-jump-buffer
             :ensure t
             :defer t
             :config (
                      )
             )

(use-package ace-isearch
             :ensure t
             :defer t
             :disabled t
             :init (global-ace-isearch-mode 1)
             )

(provide 'ace-jump-init)

;;; ace-jump-init.el ends here
