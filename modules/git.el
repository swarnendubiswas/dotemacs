;;; git.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup git.

;;; Code:

(defvar dotemacs-selection)

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-save-repository-buffers t)
  (setq magit-post-display-buffer-hook
        #'(lambda ()
            (when (derived-mode-p 'magit-status-mode)
              (delete-other-windows))))

  (cond ((eq dotemacs-selection 'ido)  (setq magit-completing-read-function 'magit-ido-completing-read))
        ((eq dotemacs-selection 'ivy)  (setq magit-completing-read-function 'ivy-completing-read)))


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

;; https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html#The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate
(setq vc-handled-backends nil)

(provide 'git)

;;; git.el ends here
