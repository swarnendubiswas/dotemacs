;;; whitespace-init.el --- Part of emacs initialization

;;; Commentary:
;; Configure whitespace

;;; Code:

(use-package whitespace
  :diminish global-whitespace-mode
  :init (global-whitespace-mode 1)
  :config
  (setq-default indicate-empty-lines nil ; show empty lines after buffer end
                ;;show-trailing-whitespace t
                ;;(setq whitespace-style '(face empty spaces tabs newline space-mark tab-mark newline-mark lines-tail trailing))
                whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark)))

;;(setq whitespace-style '(face empty spaces tabs newline space-mark tab-mark newline-mark lines-tail trailing))

;;(set-face-attribute 'whitespace-line nil
;;                    :background "red1"
;;                    :foreground "yellow"
;;                    :weight 'bold)
;;(global-whitespace-mode 1)

(use-package whitespace-cleanup-mode
  :ensure t
  :disabled t
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup-mode))

;;; whitespace-init.el ends here