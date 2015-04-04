;;; whitespace-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Configure whitespace.

;;; Code:

(use-package whitespace
  :diminish global-whitespace-mode
  :disabled t
  :defer 5
  :config
  (setq-default indicate-empty-lines nil ; show empty lines after buffer end
                show-trailing-whitespace t
                whitespace-style '(tabs newline space-mark tab-mark newline-mark))
  (global-whitespace-mode 1))

;;(setq whitespace-style '(face empty spaces tabs newline space-mark tab-mark newline-mark lines-tail trailing))
;;(set-face-attribute 'whitespace-line nil :background "red1" :foreground "yellow" :weight 'bold)

(use-package whitespace-cleanup-mode
  :ensure t
  ;;:disabled t
  :defer 10
  :diminish whitespace-cleanup-mode
  :config
  ;;(add-hook 'before-save-hook #'whitespace-cleanup-mode)
  (global-whitespace-cleanup-mode 1))

(provide 'whitespace-init)

;;; whitespace-init.el ends here
