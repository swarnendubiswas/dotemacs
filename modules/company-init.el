;;; company-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Initialize company mode for auto completion.

;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-dabbrev-downcase nil ; turn off auto downcasing of things
        company-dabbrev-ignore-case nil
        company-show-numbers t ; show quick-access numbers for the first ten candidates
        company-minimum-prefix-length 3
        ;; Invert the navigation direction if the completion popup is displayed on top (happens near the bottom of
        ;; windows).
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t)

  ;; http://emacs.stackexchange.com/questions/3654/filename-completion-using-company-mode
  (add-to-list 'company-backends 'company-files)
  (add-to-list 'company-backends 'company-capf)
  
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (global-company-mode 1)

  (use-package company-dabbrev
    :config 
    ;; (add-to-list 'company-backends 'company-dabbrev)
    )

  (use-package company-dabbrev-code
    :config
    (setq company-dabbrev-code-ignore-case nil
          company-dabbrev-code-everywhere t)
    ;; (add-to-list 'company-backends 'company-dabbrev-code)
    )
  
  (use-package helm-company
    :if (eq dotemacs-helm-or-ido 'helm)
    :ensure t)
  
  (use-package company-web
    :ensure t
    :config
    (add-to-list 'company-backends 'company-web-html)
    (add-to-list 'company-backends 'company-web-jade)
    (add-to-list 'company-backends 'company-web-slim))
  
  (use-package company-auctex
    :ensure t
    :config
    ;; (add-hook 'LaTeX-mode-hook
    ;;           (lambda ()
    ;;             (company-auctex-init)))
    (company-auctex-init))
  
  (use-package company-statistics
    :ensure t
    :config (company-statistics-mode 1))
  
  (use-package company-math
    :ensure t
    :config
    ;; Add backends globally
    (add-to-list 'company-backends 'company-math-symbols-unicode)
    (add-to-list 'company-backends 'company-math-symbols-latex)
    (add-to-list 'company-backends 'company-latex-commands))
  
  (use-package company-quickhelp
    :ensure t
    :config
    ;;(add-hook 'global-company-mode-hook #'company-quickhelp-mode)
    (company-quickhelp-mode 1))
  
  (use-package company-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-c-headers)
    (cond ((string-equal system-name "XXX")
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.4.4/"))
          ((string-equal system-name "YYY")
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9")))))

(provide 'company-init)

;;; company-init.el ends here
