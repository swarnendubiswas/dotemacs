;;; anzu-init.el --- Part of emacs initialization

;;; Commentary:
;; Anzu: show number of searches in the mode line

;;; Code:

(use-package anzu
             :ensure t
             :defer t
             :diminish anzu-mode
             :config (global-anzu-mode 1))

(provide 'anzu-init)

;;; anzu-init.el ends here
