;;; anzu-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Show number of searches in the mode line.

;;; Code:

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (global-anzu-mode 1)
  (set-face-attribute 'anzu-mode-line nil :foreground "blue" :weight 'bold))

(provide 'anzu-init)

;;; anzu-init.el ends here
