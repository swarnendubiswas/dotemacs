;;; company-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize company mode for auto completion.

;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (setq company-global-modes t ; Turn on company-mode for all major modes
        company-show-numbers t ; Quick-access numbers for the first ten candidates
        company-minimum-prefix-length 3
        company-tooltip-flip-when-above t ; Invert the navigation direction if the completion popup is displayed on top
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-selection-wrap-around t
        company-dabbrev-downcase 'case-replace
        company-dabbrev-ignore-case 'keep-prefix
        company-dabbrev-code-ignore-case t
        ;; Offer completions in comments and strings
        company-dabbrev-code-everywhere t)
  (global-company-mode 1)

  ;; https://github.com/company-mode/company-mode/issues/180
  (when (bound-and-true-p dotemacs-use-fci-p)
    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)))

(use-package company-dict ; FIXME: yasnippet support disables company popup
  :ensure t
  :after company
  :config
  (setq company-dict-dir (concat dotemacs-temp-directory "dict/")
        company-dict-enable-fuzzy t
        company-dict-enable-yasnippet nil)
  (add-to-list 'company-backends 'company-dict))

(use-package company-flx
  :ensure t
  :after company
  :config
  (setq company-flx-limit 50)
  (company-flx-mode 1))

(use-package company-statistics
  :ensure t
  :after company
  :config
  (setq company-statistics-file (concat dotemacs-temp-directory "company-statistics-cache.el"))
  (company-statistics-mode 1))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (setq company-quickhelp-delay 0.2
        company-quickhelp-max-lines 60)
  (company-quickhelp-mode 1)
  (unbind-key "M-h" company-quickhelp-mode-map)
  (bind-key "M-h" #'mark-paragraph))

(provide 'company-init)

;;; company-init.el ends here
