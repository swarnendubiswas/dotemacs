;;; imenu-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup imenu.

;;; Code:

(defvar dotemacs-selection)

(use-package imenu
  :config
  (setq imenu-auto-rescan t)
  (use-package imenu-anywhere
    :ensure t)
  (use-package popup-imenu
    :ensure t)
  :bind ("C-c C-j" . imenu-anywhere))

(use-package imenu-list
  :ensure t
  :after imenu
  :config
  (setq imenu-list-auto-resize nil)
  (if (string-equal (system-name) "sbiswas-Dell-System-XPS-L502X")
      (setq imenu-list-size 0.12)
    (setq imenu-list-size 0.10))
  ;; (add-hook 'python-mode-hook #'imenu-list-minor-mode)
  ;; (add-hook 'c-mode-common-hook #'imenu-list-minor-mode)
  )

(provide 'imenu-init)

;;; imenu-init.el ends here
