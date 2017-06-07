;;; git-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup git.

;;; Code:

(defvar dotemacs-selection)

;; https://github.com/itsjeyd/.emacs.d/blob/emacs24/init.el
(use-package magit
  :ensure t
  :config
  (setq magit-auto-revert-mode nil
        magit-item-highlight-face 'bold)
  (when (eq dotemacs-selection 'ivy)
    (setq magit-completing-read-function 'ivy-completing-read))

  (use-package magit-popup)

  (use-package gitconfig-mode
    :ensure t
    :defer t)

  (use-package gitignore-mode
    :ensure t
    :defer t)

  (use-package gitattributes-mode
    :ensure t
    :defer t)

  (use-package magit-svn
    :defer t))

(provide 'git-init)

;;; git-init.el ends here
