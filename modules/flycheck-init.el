;;; flycheck-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

;; flycheck requires gcc 4.8 at least
(use-package flycheck
  :ensure t
  :disabled t
  :if (not (string-equal system-name "rain.cse.ohio-state.edu"))
  :diminish flycheck-mode
  :init (add-hook 'prog-mode-hook #'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-completion-system 'ido
        flycheck-standard-error-navigation nil
        flycheck-check-syntax-automatically '(save mode-enabled))

  :config
  (use-package flycheck-tip
    :ensure t
    :disabled t)

  ;; Show flycheck messages in popups
  (use-package flycheck-pos-tip
    :ensure t
    :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

  (use-package flycheck-color-mode-line
    :ensure t
    :disabled t
    :config (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

  ;; Check package conventions with flycheck
  (use-package flycheck-package
    :ensure t
    :disabled t
    :init (flycheck-package-setup))

  (use-package helm-flycheck
    :ensure t
    :disabled t)

  ;; https://github.com/flycheck/flycheck-google-cpplint
  ;; Add Google C++ Style checker. By default, syntax checked by Clang and Cppcheck (Windows?). Also, need to setup cpplint.py.
  (use-package flycheck-google-cpplint
    :ensure t
    :if (eq system-type 'gnu/linux)
    :config
    (flycheck-add-next-checker 'c/c++-clang '(t . c/c++-googlelint) t)
    (setq flycheck-googlelint-linelength 'dotemacs-fill-column
          flycheck-googlelint-filter "-whitespace/line_length")))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
