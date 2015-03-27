;;; guide-key-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup guide key

;;; Code:

(use-package guide-key
  :ensure t
  :commands guide-key-mode
  :defer 10
  :diminish guide-key-mode
  :config
  (setq guide-key/guide-key-sequence t
        guide-key/recursive-key-sequence-flag t
        guide-key/popup-window-position 'bottom)
  (guide-key-mode 1))

(use-package guide-key-tip
  :ensure t
  :defer t
  :config (setq guide-key-tip/enabled t))

(provide 'guide-key-init)

;;; guide-key-init.el ends here
