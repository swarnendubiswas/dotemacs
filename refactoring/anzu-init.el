;;; anzu-init.el --- Part of emacs initialization

;;; Commentary:
;; Anzu: show number of searches in the mode line

;;; Code:

(use-package anzu-mode
             :ensure t
             :defer t
             :idle (global-anzu-mode 1)
             )
(global-anzu-mode 1)

;;; misc-init.el ends here
