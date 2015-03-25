;;; text-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup text mode.

;;; Code:

;;(add-hook 'text-mode-hook 'turn-on-auto-fill)
(setq comment-auto-fill-only-comments t)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook #'fci-mode)

(provide 'text-init)

;;; text-init.el ends here
