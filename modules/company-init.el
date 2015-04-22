;;; company-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Initialize company mode for auto completion.

;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :defer 2
  :config
  (setq company-dabbrev-downcase nil ; turn off auto downcasing of things
        company-show-numbers t ; show quick-access numbers for the first ten candidates
        company-minimum-prefix-length 2
        ;; Invert the navigation direction if the completion popup is displayed on top (happens near the bottom of
        ;; windows).
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t)
  ;; http://emacs.stackexchange.com/questions/3654/filename-completion-using-company-mode
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-backends 'company-dabbrev)
  (add-to-list 'company-backends 'company-dabbrev-code)
  (global-company-mode 1))

(use-package company-auctex
  :ensure t
  :config
  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda ()
  ;;             (company-auctex-init)))
  (with-eval-after-load 'company
    (company-auctex-init)))

(use-package company-statistics
  :ensure t
  :config
  (eval-after-load 'company
    (company-statistics-mode 1)))

(use-package company-math
  :ensure t
  :config (with-eval-after-load 'company
            ;; Add backends globally
            (add-to-list 'company-backends 'company-math-symbols-unicode)
            (add-to-list 'company-backends 'company-math-symbols-latex)
            (add-to-list 'company-backends 'company-latex-commands)))

(use-package company-quickhelp
  :ensure t
  :config
  ;;(add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  (eval-after-load 'company
    (company-quickhelp-mode 1)))

(use-package company-c-headers
  :ensure t
  :config (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-c-headers)))

(provide 'company-init)

;;; company-init.el ends here
