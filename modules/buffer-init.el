;;; buffer-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Buffer configurations.

;;; Code:

(use-package ibuffer
  :defer t
  :init (defalias 'list-buffers 'ibuffer) ; turn on ibuffer by default
  :config
  (progn
    (setq ibuffer-expert t
          ;;ibuffer-shrink-to-minimum-size t
          ibuffer-always-show-last-buffer nil
          ;;ibuffer-default-sorting-mode 'recency ; 'major-mode
          ;;ibuffer-sorting-mode 'recency
          ibuffer-use-header-line t)
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-auto-mode 1)))
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-do-sort-by-recency)))
    ;; (add-hook 'ibuffer-mode-hook
    ;;           (lambda ()
    ;;             (ibuffer-recompile-formats -1)))
    ;;(global-set-key (kbd "C-x C-b") 'ibuffer) ; use ibuffer for buffer list
    (use-package ibuffer-vc
      :ensure t
      :defer t))
  :bind ("C-x C-b" . ibuffer))

(provide 'buffer-init)

;;; buffer-init.el ends here
