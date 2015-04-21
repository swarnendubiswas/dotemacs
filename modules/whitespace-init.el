;;; whitespace-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Configure whitespace.

;;; Code:

;; (use-package simple
;;   :config (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package whitespace
  :disabled t
  :defer 5
  :diminish global-whitespace-mode
  :config
  (setq-default indicate-empty-lines nil ; show empty lines after buffer end
                show-trailing-whitespace t
                ;;whitespace-style '(face empty spaces tabs newline space-mark tab-mark newline-mark lines-tail trailing)
                whitespace-style '(tabs newline space-mark tab-mark newline-mark))
  ;;(set-face-attribute 'whitespace-line nil :background "red1" :foreground "yellow" :weight 'bold)
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :defer 5
  :diminish whitespace-cleanup-mode
  :config (add-hook 'before-save-hook #'whitespace-cleanup-mode))

(provide 'whitespace-init)

;;; whitespace-init.el ends here
