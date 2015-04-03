;;; svn-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup svn.

;;; Code:

(use-package psvn
  :ensure t
  :defer 10
  :config
  (setq svn-status-verbose nil
        svn-status-display-full-path t
        svn-status-auto-revert-buffers t
        svn-status-use-ido-completion t)
  ;; I use svn and emacs mostly for LaTeX files.
  (add-hook 'LaTeX-mode-hook #'svn-status))

(provide 'svn-init)

;;; svn-init.el ends here
