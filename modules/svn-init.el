;;; svn-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup svn.

;;; Code:

;; SB: It seems we cannot use psvn to commit multiple files at once.
(use-package psvn
  :ensure t
  :config
  (setq svn-status-verbose nil
        svn-status-display-full-path t
        svn-status-auto-revert-buffers t
        svn-status-use-ido-completion t)
  ;; SB: I use svn and emacs mostly for LaTeX files.
  (add-hook 'text-mode-hook #'svn-status)
  (add-hook 'prog-mode-hook #'svn-status))

(provide 'svn-init)

;;; svn-init.el ends here
