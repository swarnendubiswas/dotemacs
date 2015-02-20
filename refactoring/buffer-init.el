;;; buffer-init.el --- Part of emacs initialization

;;; Commentary:
;; Buffer configurations

;;; Code:

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)))
(defalias 'list-buffers 'ibuffer) ; turn on ibuffer by default
(setq ibuffer-expert t
      ibuffer-shrink-to-minimum-size t
      ibuffer-always-show-last-buffer nil
      ibuffer-default-sorting-mode 'recency ; 'major-mode
      ibuffer-sorting-mode 'recency
      ibuffer-use-header-line t)

;; C-x C-j opens dired with the cursor right on the file you're editing, otherwise
;; you can use C-x d, or 'M-x dired'
(require 'dired) ; needed for dired-mode-map
(add-hook 'dired-load-hook ; dired-load-hook
          (lambda ()
            (load "dired-x")))
(autoload 'dired-jump "dired-x"
  "Jump to dired buffer corresponding to current buffer."
  'interactive)
(setq dired-bind-jump t)

(use-package dired+
             :ensure t
             :defer t
             )

(use-package dired-details
             :ensure t
             :defer t
             )

(use-package dired-details+
             :ensure t
             :defer t
             )

(use-package dired-rainbow
             :ensure t
             :defer t
             )

(use-package dired-hacks-utils
             :ensure t
             :defer t
             )

(provide 'buffer-init)

;;; buffer-init.el ends here
