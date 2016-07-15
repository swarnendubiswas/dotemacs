;;; company-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Initialize company mode for auto completion.

;;; Code:

(use-package company
  :ensure t
  :diminish company-mode
  :init (global-company-mode 1)
  :config
  (setq company-global-modes t ; Turn on company-mode for all major modes
        company-show-numbers t ; Quick-access numbers for the first ten candidates
        company-minimum-prefix-length 3
        company-tooltip-flip-when-above t ; Invert the navigation direction if the completion popup is displayed on top
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-selection-wrap-around t
        company-dabbrev-downcase nil ; Do not downcase the returned candidates
        company-dabbrev-code-everywhere t ; Offer completions in comments and strings
        ;; https://github.com/company-mode/company-mode/wiki/Switching-from-AC
        company-begin-commands '(self-insert-command)
        company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                            company-preview-frontend
                            company-echo-metadata-frontend))

  ;; https://github.com/company-mode/company-mode/issues/87
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

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
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))

  (cond ((eq dotemacs-selection 'ivy) (progn
                                        (with-eval-after-load "counsel"
                                          (bind-key [remap complete-symbol] #'counsel-company company-mode-map)
                                          (bind-key [remap completion-at-point] #'counsel-company company-mode-map)
                                          (bind-key "C-:" #'counsel-company company-mode-map)
                                          (bind-key "C-:" #'counsel-company company-active-map))))
        ((eq dotemacs-selection 'helm) (progn
                                         (with-eval-after-load "helm-company"
                                           (bind-key [remap complete-symbol] #'helm-company company-mode-map)
                                           (bind-key [remap completion-at-point] #'helm-company company-mode-map)
                                           (bind-key "C-:" #'helm-company company-mode-map)
                                           (bind-key "C-:" #'helm-company company-active-map)))))
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

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
  (unbind-key "M-h" company-quickhelp-mode-map))

(provide 'company-init)

;;; company-init.el ends here
