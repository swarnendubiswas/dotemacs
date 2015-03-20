;;; company-init.el --- Part of emacs initialization

;;; Commentary:
;; Initialize company mode for auto completion.

;;; Code:

(use-package company
             :ensure t
             :diminish company-mode
             :config (setq company-dabbrev-downcase nil ; turn off auto downcasing of things
                           company-show-numbers t
                           company-minimum-prefix-length 2
                           ;; invert the navigation direction if the completion popup is displayed on top
                           ;; (happens near the bottom of windows)
                company-tooltip-flip-when-above t)
  :init (global-company-mode 1))

(use-package company-auctex
             :ensure t
             :defer t
             :config (company-auctex-init))

(use-package company-statistics
             :ensure t
             :defer t
	     :disabled t
             :config (company-statistics-mode 1))

(use-package company-math
             :ensure t
             :defer t)

(use-package company-quickhelp
             :ensure t
             :defer t)

(provide 'company-init)

;;; company-init.el ends here
