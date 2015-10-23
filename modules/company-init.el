;;; company-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize company mode for auto completion.

;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode 1)
  (setq company-global-modes t
        company-show-numbers t ; show quick-access numbers for the first ten candidates
        company-minimum-prefix-length 2
        ;; Invert the navigation direction if the completion popup is displayed on top (happens near the bottom of
        ;; windows).
        company-tooltip-flip-when-above t
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        ;; start autocompletion only after typing
        ;; company-begin-commands '(self-insert-command)
        company-idle-delay 0.3
        company-selection-wrap-around t
        company-selection-changed t
        company-require-match nil)

  (use-package company-keywords
    :init (add-to-list 'company-backends #'company-keywords))

  (use-package company-dabbrev
    :init
    (setq company-dabbrev-downcase nil ; turn off auto downcasing of things
          company-dabbrev-ignore-case nil))

  (use-package company-dabbrev-code
    :init
    (setq company-dabbrev-code-ignore-case nil
          company-dabbrev-code-everywhere t))

  ;; https://github.com/company-mode/company-mode/issues/180
  (when (bound-and-true-p dotemacs-fci-p)
    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))

  (use-package company-emoji
    :ensure t
    :defer t
    :config (add-hook 'text-mode-hook 'company-emoji-init))

  (use-package company-try-hard
    :ensure t
    :init (bind-key "C-z" #'company-try-hard company-active-map))

  :config
  (use-package company-flx
    :ensure t
    :disabled t
    :config
    (setq company-flx-limit 50)
    (company-flx-mode 1)))

(use-package company-statistics
  :ensure t
  :defer 2
  :config
  (company-statistics-mode 1)
  (setq company-statistics-file (concat dotemacs-temp-directory "company-statistics-cache.el")))

(use-package company-quickhelp
  :ensure t
  :defer 2
  :config
  (company-quickhelp-mode 1)
  (setq company-quickhelp-delay 0.5
        company-quickhelp-max-lines 60)
  (unbind-key "M-h" global-map)
  (bind-key* "M-h" #'mark-paragraph))

(provide 'company-init)

;;; company-init.el ends here
