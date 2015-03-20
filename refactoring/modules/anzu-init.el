;;; anzu-init.el --- Part of emacs initialization

;;; Commentary:
;; Anzu: show number of searches in the mode line

;;; Code:

(use-package anzu-mode
             :ensure t
             :defer t
             :diminish anzu-mode
             :idle (global-anzu-mode 1)
             )

;;; anzu-init.el ends here
