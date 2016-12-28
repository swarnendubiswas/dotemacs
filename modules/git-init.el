;;; git-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup git.

;;; Code:

;; https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el

;; Magit needs git >=1.9.4
(use-package magit
  :functions magit-status
  :config
  (setq magit-auto-revert-mode nil
        magit-item-highlight-face 'bold
        magit-completing-read-function 'ivy-completing-read)

  (use-package magit-popup)

  (use-package gitconfig-mode ; Git configuration mode
    :defer t)

  (use-package gitignore-mode ; .gitignore mode
    :defer t)

  (use-package gitattributes-mode ; Git attributes mode
    :defer t)

  (use-package magit-svn
    :defer t))

(provide 'git-init)

;;; git-init.el ends here
