;;; flycheck-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure flycheck.

;;; Code:

(defvar dotemacs-mode-line-theme)

;; Install these packages to help with syntax checking:
;; sudo apt install libxml2-utils chktex shellcheck ruby-dev tidy
;; pip install --update proselint pylint Sphinx --user
;; sudo npm i -g elsint js-yaml less jsonlint
;; sudo npm i -g stylelint --save-dev
;; sudo gem install scss_lint mdl

(use-package flycheck
  :ensure t
  ;; :hook (prog-mode . flycheck-mode)
  :init (global-flycheck-mode 1)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
        flycheck-display-errors-delay 0.5
        ;; Faster than the default
        flycheck-highlighting-mode 'lines
        flycheck-check-syntax-automatically '(save idle-change idle-buffer-switch)
        flycheck-pylintrc "/home/swarnendu/.config/pylintrc")
  (setq-local flycheck-python-pylint-executable "python3")
  (setq-default flycheck-disabled-checkers '(tex-lacheck python-flake8))

  (add-hook 'python-mode-hook
            (lambda ()
              (setq flycheck-checker 'python-pylint)))

  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c++11")
              (setq flycheck-gcc-language-standard "c++11")))

  (when (eq dotemacs-mode-line-theme 'spaceline)
    (setq flycheck-mode-line nil)))

(use-package avy-flycheck
  :ensure t
  :ensure avy
  :after avy flycheck
  :config
  ;; Binds avy-flycheck-goto-error to C-c ! g
  (avy-flycheck-setup))

(use-package flycheck-popup-tip ; Show error messages in popups
  :ensure t
  :disabled t
  :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package flycheck-pos-tip
  :ensure t
  :disabled t
  :hook (flycheck-mode . flycheck-pos-tip-mode))

(use-package flycheck-inline
  :ensure t
  :hook (flycheck-mode . flycheck-inline-mode))

(provide 'flycheck-init)

;;; flycheck-init.el ends here
