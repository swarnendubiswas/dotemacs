;;; company-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Initialize company mode for auto completion.

;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :init (global-company-mode 1)
  :config
  (setq company-dabbrev-downcase nil ; turn off auto downcasing of things
        company-show-numbers t
        company-minimum-prefix-length 2
        ;; Invert the navigation direction if the completion popup is displayed on top (happens near the bottom of
        ;; windows).
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t))
  
(use-package company-auctex
  :ensure t
  :defer t
  :config (company-auctex-init))

(use-package company-statistics
  :ensure t
  :defer t
  :init (company-statistics-mode 1))

(use-package company-math
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          ;; Add backends for math characters
          (add-to-list 'company-backends 'company-math-symbols-unicode)
          (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(provide 'company-init)

;;; company-init.el ends here
