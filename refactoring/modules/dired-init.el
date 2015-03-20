;;; dired-init.el --- Part of emacs initialization

;;; Commentary:
;; Dired configurations

;;; Code:

;; C-x C-j opens dired with the cursor right on the file you're editing, otherwise
;; you can use C-x d, or 'M-x dired'
(require 'dired) ; needed for dired-mode-map
               (setq dired-auto-revert-buffer t ; revert each dired buffer automatically when you visit it
                     dired-recursive-deletes 'always ; single prompt for all n directories
      dired-recursive-copies 'always)
               (setq-default diredp-hide-details-initially-flag nil)

  ;; ((add-hook 'dired-load-hook ; dired-load-hook
  ;;         (lambda ()
  ;;           (load "dired-x"))))


;; (autoload 'dired-jump "dired-x"
;;   "Jump to dired buffer corresponding to current buffer."
;;   'interactive)
;; (setq dired-bind-jump t)

(use-package dired-x
             :commands (dired-jump)
             :config (setq dired-bind-jump t))


(use-package direx
             :ensure t
             :defer t)

(use-package dired-efap
             :ensure t
             :defer t)

(use-package dired+
             :ensure t
             :defer t)

(use-package dired-details
             :ensure t
             :defer t)

(use-package dired-details+
             :ensure t
             :defer t)

(use-package dired-rainbow
             :ensure t
             :defer t)

(use-package dired-hacks-utils
             :ensure t
             :defer t)

(provide 'dired-init)

;;; dired-init ends here
