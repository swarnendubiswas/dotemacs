;;; svn-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup svn.

;;; Code:

;; SB: FIXME: These packages seems to mess up tags and indentation

(or (use-package psvn
      ;;:disabled t
      :ensure t
      :init
      (setq svn-status-verbose nil
            svn-status-hide-unknown nil
            svn-status-hide-unmodified t
            svn-status-display-full-path t
            svn-status-auto-revert-buffers t
            svn-status-use-ido-completion t)
      (add-hook 'LaTex-mode-hook #'svn-status)
      (add-hook 'prog-mode-hook #'svn-status))

    (use-package dsvn
      :disabled t
      :ensure t
      :config
      (add-hook 'LaTex-mode-hook #'svn-status)
      (add-hook 'prog-mode-hook #'svn-status)))

(use-package helm-ls-svn
  :disabled t
  :ensure t)

(provide 'svn-init)

;;; svn-init.el ends here
