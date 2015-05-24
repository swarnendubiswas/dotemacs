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
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-idle-delay 0.3
        ;; start autocompletion only after typing
        company-begin-commands '(self-insert-command))

  ;; http://emacs.stackexchange.com/questions/3654/filename-completion-using-company-mode
  (add-to-list 'company-backends #'company-files)
  (add-to-list 'company-backends #'company-capf)

  ;; enabling this seems to disable the company popup
  ;;(add-to-list 'company-backends 'company-gtags)
  ;;(setq company-backends (delete 'company-semantic company-backends))
  ;;(setq company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))

  (global-company-mode 1)

  ;; https://github.com/company-mode/company-mode/issues/180
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

  (use-package company-dabbrev
    :disabled t
    :config (add-to-list 'company-backends #'company-dabbrev))

  ;; enabling this seems to disable the company popup
  (use-package company-dabbrev-code
    :disabled t
    :config
    (setq company-dabbrev-code-ignore-case nil
          company-dabbrev-code-everywhere t)
    (add-to-list 'company-backends #'company-dabbrev-code))
  
  (use-package helm-company
    :ensure t)
  
  (use-package company-web
    :ensure t
    :preface
    (defun company-web--setup ()
      (setq-local company-backends
                  (append '(company-web-html company-web-jade company-web-slim)
                          company-backends)))
    :config (add-hook 'web-mode-hook #'company-web--setup))

  (use-package company-statistics
    :ensure t
    :config (company-statistics-mode 1))

  ;; https://github.com/vspinu/company-math
  (use-package company-math
    :ensure t
    :preface
    (defun company-math--setup ()
      (setq-local company-backends
                  (append '(company-math-symbols-latex company-math-symbols-unicode company-latex-commands)
                          company-backends)))
    :config (add-hook 'LaTeX-mode-hook #'company-math--setup))

  (use-package company-quickhelp
    :ensure t
    :config (company-quickhelp-mode 1)))

(provide 'company-init)

;;; company-init.el ends here
