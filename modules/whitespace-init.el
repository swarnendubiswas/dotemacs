;;; whitespace-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Configure whitespace

;;; Code:

(use-package whitespace
  :diminish global-whitespace-mode
  :init
  (setq-default indicate-empty-lines nil ; show empty lines after buffer end
                show-trailing-whitespace t
                ;;(setq whitespace-style '(face empty spaces tabs newline space-mark tab-mark newline-mark lines-tail trailing))
                whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))
  :config (global-whitespace-mode 1))

;;(setq whitespace-style '(face empty spaces tabs newline space-mark tab-mark newline-mark lines-tail trailing))

;;(set-face-attribute 'whitespace-line nil
;;                    :background "red1"
;;                    :foreground "yellow"
;;                    :weight 'bold)
;;(global-whitespace-mode 1)

(use-package whitespace-cleanup-mode
  :ensure t
  :defer t
  :config
  ;; (add-hook 'before-save-hook 'whitespace-cleanup-mode)
  (global-whitespace-cleanup-mode 1))

(provide 'whitespace-init)

;;; whitespace-init.el ends here
