;;; svn-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup svn.

;;; Code:

;; SB: FIXME: These packages seems to mess up tags and indentation

(use-package psvn
  :ensure t
  :defer t
  :bind ("C-c d s" . svn-status)
  :config
  (setq svn-status-verbose nil
        svn-status-hide-unknown nil
        svn-status-hide-unmodified t
        svn-status-display-full-path t
        svn-status-auto-revert-buffers t
        svn-status-use-ido-completion t)

  (use-package helm-ls-svn
    :ensure t))

(provide 'svn-init)

;;; svn-init.el ends here
