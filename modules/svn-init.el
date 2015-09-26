;;; svn-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup svn.

;;; Code:

(use-package psvn
  :ensure t
  :functions svn-status-update-modeline
  :bind ("C-c d s" . svn-status)
  :config
  (setq svn-status-verbose nil
        svn-status-hide-unknown nil
        svn-status-hide-unmodified t
        svn-status-display-full-path t
        svn-status-auto-revert-buffers t
        svn-status-use-ido-completion t))

(provide 'svn-init)

;;; svn-init.el ends here
