(use-package fill-column-indicator
             :ensure t
             :defer t
             :config
             (progn
               (setq-default fci-rule-column 120)
               (setq fci-handle-truncate-lines nil
                     ;; fci-rule-width 1
                     )
               )
             )

;; fci
;;(define-globalized-minor-mode
;;  global-fci-mode fci-mode (lambda () (fci-mode 1)))
;;(global-fci-mode 1)
;; (defun auto-fci-mode (&optional unused)
;;   (if (> (frame-width) 120)
;;       (fci-mode 1)
;;     (fci-mode 0))
;;   )
;;(add-hook 'after-change-major-mode-hook 'auto-fci-mode)
;;(add-hook 'window-size-change-functions 'auto-fci-mode)