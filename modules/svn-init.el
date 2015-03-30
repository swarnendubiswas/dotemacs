;;; svn-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup svn

;;; Code:

(use-package psvn
  :ensure t
  :defer t
  :init
  (setq svn-status-verbose nil
        svn-status-display-full-path t
        svn-status-auto-revert-buffers t
        svn-status-use-ido-completion t)
  :config
  (add-hook 'LaTeX-mode-hook #'svn-status))

(provide 'svn-init)

;;; svn-init.el ends here
