;;; text-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup text mode.

;;; Code:

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(provide 'text-init)

;;; text-init.el ends here
