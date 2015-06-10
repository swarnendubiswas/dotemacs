;;; anzu-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Show number of searches in the mode line.

;;; Code:

(use-package anzu
  :disabled t ; since we are using swiper
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode 1)
  (set-face-attribute 'anzu-mode-line nil :foreground "blue" :weight 'bold))

(provide 'anzu-init)

;;; anzu-init.el ends here
