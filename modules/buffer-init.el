;;; buffer-init.el --- Part of emacs initialization

;;; Commentary:
;; Buffer configurations

;;; Code:

(use-package ibuffer
  :init
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1)))
  :config
  (progn
    (defalias 'list-buffers 'ibuffer) ; turn on ibuffer by default
    (setq ibuffer-expert t
          ibuffer-shrink-to-minimum-size t
          ibuffer-always-show-last-buffer nil
          ibuffer-default-sorting-mode 'recency ; 'major-mode
          ibuffer-sorting-mode 'recency
          ibuffer-use-header-line t)
    (use-package ibuffer-tramp
      :ensure t
      :defer t))
  :bind ("C-x C-b" . ibuffer))

;;(global-set-key (kbd "C-x C-b") 'ibuffer) ; use ibuffer for buffer list

(provide 'buffer-init)

;;; buffer-init.el ends here