;;; dired-init.el --- Part of emacs initialization

;;; Commentary:
;; Dired configurations

;;; Code:

;;(require 'dired) ; needed for dired-mode-map

;; use "C-x d", or "M-x dired"
(use-package dired
  :config
  (setq dired-auto-revert-buffer t ; revert each dired buffer automatically when you visit it
        dired-recursive-deletes 'always ; single prompt for all n directories
        dired-recursive-copies 'always
        ;;delete-by-moving-to-trash t
        dired-listing-switches "-ABhl --si --group-directories-first")
        ;;dired-listing-switches "-ABhltc --si --group-directories-first")
  (setq-default diredp-hide-details-initially-flag nil))

;; Jump to dired buffer corresponding to current buffer.
(use-package dired-x
  :commands (dired-jump)
  :config (setq dired-bind-jump t)
  :bind ("C-x C-j" . dired-jump)) ;; C-x C-j opens dired with the cursor right on the file you're editing

(use-package direx
  :ensure t
  :defer t)

(use-package dired-efap
  :ensure t
  :disabled t)

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

(use-package nav
  :disabled t
  :load-path "~/.emacs.d/lisp/emacs-nav-49/"
  :config
  (nav-mode)
  ;;(nav-disable-overeager-window-splitting)
  :bind ("<f6>" . nav-toggle))

(provide 'dired-init)

;;; dired-init ends here
