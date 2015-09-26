;;; anzu-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Show number of searches in the mode line.

;;; Code:

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (global-anzu-mode 1)
  (setq anzu-search-threshold 10000
        anzu-minimum-input-length 2)
  (unless (eq dotemacs-theme 'leuven)
    (set-face-attribute 'anzu-mode-line nil :foreground "blue" :weight 'light)))

(provide 'anzu-init)

;;; anzu-init.el ends here
