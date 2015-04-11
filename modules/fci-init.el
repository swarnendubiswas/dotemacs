;;; fci-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup fci.

;;; Code:

(use-package fill-column-indicator
  :ensure t
  :defer 5
  :config
  (setq-default fci-rule-column 120)
  (setq fci-handle-truncate-lines nil
        fci-rule-width 1
        fci-rule-color "grey40")
  (add-hook 'text-mode-hook #'fci-mode)
  (add-hook 'LaTeX-mode-hook #'fci-mode)
  (add-hook 'org-mode-hook #'fci-mode)
  (add-hook 'prog-mode-hook #'fci-mode)
  (defun auto-fci-mode (&optional unused)
    (if (> (frame-width) 120)
        (fci-mode 1)
      (fci-mode 0)))
  (add-hook 'after-change-major-mode-hook 'auto-fci-mode)
  (add-hook 'window-size-change-functions 'auto-fci-mode)
  ;;(define-globalized-minor-mode
  ;;  global-fci-mode fci-mode (lambda () (fci-mode 1)))
  ;;(global-fci-mode 1)
  )

(provide 'fci-init)

;;; fci-init.el ends here
