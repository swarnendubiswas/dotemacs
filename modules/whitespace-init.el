;;; whitespace-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure whitespace.

;;; Code:

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package whitespace
  :disabled t
  :diminish global-whitespace-mode
  :init
  (setq-default indicate-empty-lines nil ; show empty lines after buffer end
                show-trailing-whitespace t
                ;;whitespace-style '(face empty spaces tabs newline space-mark tab-mark newline-mark lines-tail trailing)
                whitespace-style '(tabs newline tab-mark newline-mark))
  ;;(set-face-attribute 'whitespace-line nil :background "red1" :foreground "yellow" :weight 'bold)
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init
  ;; (dolist (hook '(prog-mode-hook))
  ;;   (add-hook hook #'whitespace-cleanup-mode))
  (whitespace-cleanup-mode 1))

(provide 'whitespace-init)

;;; whitespace-init.el ends here
