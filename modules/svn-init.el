;;; svn-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup svn

;;; Code:

(use-package psvn
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'svn-status)
  :config
  (setq svn-status-verbose nil
        svn-status-display-full-path t
        svn-status-auto-revert-buffers t
        svn-status-use-ido-completion t))

(provide 'svn-init)

;;; svn-init.el ends here
