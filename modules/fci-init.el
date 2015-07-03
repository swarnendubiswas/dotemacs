;;; fci-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup fci.

;;; Code:

(use-package fill-column-indicator
  :ensure t
  :if (bound-and-true-p dotemacs--fci-p)
  :preface
  (defun dotemacs--auto-fci-mode (&optional unused)
    (if (> frame-width 120)
        (fci-mode 1)
      (fci-mode 0)))

  (define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (fci-mode 1)))

  :init
  (global-fci-mode 1)
  ;;(add-hook 'text-mode-hook #'fci-mode)
  ;;(add-hook 'prog-mode-hook #'fci-mode)
  ;;(add-hook 'after-change-major-mode-hook #'dotemacs--auto-fci-mode)
  ;;(add-hook 'window-size-change-functions #'dotemacs--auto-fci-mode)

  :config
  (setq-default fci-rule-column 120)
  (setq fci-handle-truncate-lines nil
        fci-rule-width 1
        fci-rule-color "grey40"))

(provide 'fci-init)

;;; fci-init.el ends here
