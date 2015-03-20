;;; whitespace-init.el --- Part of emacs initialization

;;; Commentary:
;; Configure whitespace

;;; Code:

(setq-default indicate-empty-lines nil ; show empty lines after buffer end
              ;;show-trailing-whitespace t
              whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

;; (add-hook 'before-save-hook 'whitespace-cleanup-mode)
;;(setq whitespace-style '(face empty spaces tabs newline space-mark tab-mark newline-mark lines-tail trailing))

;;(set-face-attribute 'whitespace-line nil
;;                    :background "red1"
;;                    :foreground "yellow"
;;                    :weight 'bold)
;;(global-whitespace-mode 1)

(use-package whitespace-cleanup-mode
  :ensure t
  :disabled t)

;;; whitespace-init.el ends here
