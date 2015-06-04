;;; flycheck-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

;; flycheck requires gcc 4.8 at least
(use-package flycheck
  :disabled t
  :ensure t
  ;;:if (not (string-equal system-name "XXX"))
  :diminish flycheck-mode
  :init (add-hook 'prog-mode-hook #'global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-completion-system 'ido
        flycheck-standard-error-navigation nil
        flycheck-check-syntax-automatically '(save mode-enabled))
  
  (use-package flycheck-tip
    :disabled t
    :ensure t)

  ;; Show flycheck messages in popups
  (use-package flycheck-pos-tip
    :disabled t
    :ensure t
    :init (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

  (use-package flycheck-color-mode-line
    :disabled t
    :ensure t
    :config (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

  ;; Check package conventions with flycheck
  (use-package flycheck-package
    :disabled t
    :ensure t
    :init (flycheck-package-setup))

  (use-package helm-flycheck
    :disabled t
    :ensure t)

  ;; https://github.com/flycheck/flycheck-google-cpplint
  ;; Add Google C++ Style checker. By default, syntax checked by Clang and Cppcheck (Windows?). Also, need to setup cpplint.py.
  (use-package flycheck-google-cpplint
    :disabled t
    :ensure t
    :config
    (flycheck-add-next-checker 'c/c++-clang '(t . c/c++-googlelint) t)
    (setq flycheck-googlelint-linelength 120
          flycheck-googlelint-filter "-whitespace/line_length")))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
