;;; c-init.el --- Part of emacs initialization

;;; Commentary:
;; C/C++ programming mode specific.

;;; Code:

(use-package cc-mode
  :defer t
  :config 
  (setq c-default-style "cc-mode"
        c-basic-offset 2))

(provide 'c-init)

;;; c-init.el ends here
