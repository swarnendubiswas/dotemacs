;;; guide-key-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup guide key

;;; Code:

(use-package guide-key
             :ensure t
             :defer t
  :diminish guide-key-mode
             :config (setq guide-key/guide-key-sequence t
                           guide-key/recursive-key-sequence-flag t
                guide-key/popup-window-position 'bottom)
  :init (guide-key-mode 1))

(use-package guide-key-tip
             :ensure t
             :defer t
  :config (setq guide-key-tip/enabled t))

(provide 'guide-key-init)

;;; guide-key-init.el ends here
