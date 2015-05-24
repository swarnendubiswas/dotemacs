;;; fci-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup fci.

;;; Code:

(use-package fill-column-indicator
  :disabled t
  :ensure t
  :preface
  (defun dotemacs--auto-fci-mode (&optional unused)
    (if (> (frame-width) 120)
        (fci-mode 1)
      (fci-mode 0)))

  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (fci-mode 1)))

  :config
  (setq-default fci-rule-column 120)
  (setq fci-handle-truncate-lines nil
        fci-rule-width 1
        fci-rule-color "grey40")
  ;;(add-hook 'text-mode-hook #'fci-mode)
  ;;(add-hook 'prog-mode-hook #'fci-mode)
  ;;(add-hook 'after-change-major-mode-hook #'dotemacs--auto-fci-mode)
  ;;(add-hook 'window-size-change-functions #'dotemacs--auto-fci-mode)
  
  (global-fci-mode 1))

(provide 'fci-init)

;;; fci-init.el ends here
