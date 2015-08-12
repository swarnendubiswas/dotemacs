;;; git-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup git.

;;; Code:

;; https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
(use-package magit
  :ensure t
  :if (not (string-equal system-name "rain.cse.ohio-state.edu")) ; magit needs git >=1.9.4
  :diminish magit-auto-revert-mode
  :commands magit-status
  :config
  (setq magit-auto-revert-mode nil
        magit-item-highlight-face 'bold
        magit-last-seen-setup-instructions "1.4.0")

  (use-package gitconfig-mode ; Git configuration mode
    :ensure t
    :defer t)

  (use-package gitignore-mode ; .gitignore mode
    :ensure t
    :defer t)

  (use-package gitattributes-mode ; Git attributes mode
    :ensure t
    :defer t)

  (use-package helm-ls-git
    :ensure t
    :defer t)

  (use-package magit-svn
    :ensure t
    :defer t))

(provide 'git-init)

;;; git-init.el ends here
