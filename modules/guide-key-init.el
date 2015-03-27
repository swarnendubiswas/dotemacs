;;; guide-key-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup guide key

;;; Code:

(use-package guide-key
  :ensure t
  :commands guide-key-mode
  :defer 10
  :diminish guide-key-mode
  :init (guide-key-mode 1)
  :config
  (setq guide-key/guide-key-sequence t
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom))

(use-package guide-key-tip
  :ensure t
  :defer t
  :config (setq guide-key-tip/enabled t))

(provide 'guide-key-init)

;;; guide-key-init.el ends here
