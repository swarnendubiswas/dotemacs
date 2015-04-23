;;; guide-key-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup guide key.

;;; Code:

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence t
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom
        guide-key/idle-delay 0.5)
  (guide-key-mode 1))

(use-package guide-key-tip
  :disabled t
  :ensure t
  :config (setq guide-key-tip/enabled t))

(provide 'guide-key-init)

;;; guide-key-init.el ends here
