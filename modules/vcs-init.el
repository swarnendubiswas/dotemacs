;;; vcs-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup git and svn.

;;; Code:

(defvar dotemacs-temp-directory)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init
  (setq transient-levels-file (concat dotemacs-temp-directory "transient/levels.el")
        transient-values-file (concat dotemacs-temp-directory "transient/values.el")
        transient-history-file (concat dotemacs-temp-directory "transient/history.el")
        magit-save-repository-buffers t
        magit-completing-read-function 'ivy-completing-read)
  :config
  (setq magit-post-display-buffer-hook
        #'(lambda ()
            (when (derived-mode-p 'magit-status-mode)
              (delete-other-windows)))))

(use-package magit-popup
  :after magit)

(use-package gitignore-mode
  :ensure t)

(use-package gitattributes-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

;; (use-package magit-svn
;;   :defer t)

(use-package psvn
  :load-path "extras"
  :bind ("C-c d s" . svn-status)
  :config
  (setq svn-status-verbose nil
        svn-status-hide-unknown nil
        svn-status-hide-unmodified t
        svn-status-display-full-path t
        svn-status-auto-revert-buffers t))

(provide 'vcs-init)

;;; vcs-init.el ends here
