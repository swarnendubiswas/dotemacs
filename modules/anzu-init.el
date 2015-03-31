;;; anzu-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Anzu: show number of searches in the mode line

;;; Code:

(use-package anzu
  :ensure t
  :defer 10
  :diminish anzu-mode
  :config (global-anzu-mode 1))

(provide 'anzu-init)

;;; anzu-init.el ends here
