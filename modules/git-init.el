;;; git-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup git.

;;; Code:

(use-package magit
  :ensure t
  :defer t
  :diminish magit-auto-revert-mode
  :config
  (setq magit-auto-revert-mode nil
        magit-item-highlight-face 'bold
        magit-last-seen-setup-instructions "1.4.0"))

;; Git commit message mode
(use-package git-commit-mode
  :ensure t
  :defer t)

;; Git configuration mode
(use-package gitconfig-mode
  :ensure t
  :defer t)

;; .gitignore mode
(use-package gitignore-mode
  :ensure t
  :defer t)

;; Git attributes mode
(use-package gitattributes-mode
  :ensure t
  :defer t)

;; Mode for git rebase -i
(use-package git-rebase-mode
  :ensure t
  :defer t)

(provide 'git-init)

;;; git-init.el ends here
