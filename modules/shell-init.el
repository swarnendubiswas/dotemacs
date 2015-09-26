;;; shell-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup shell, eshell, terminal emulation. This module is not to configure editing of shell scripts.

;;; Code:

(use-package shell
  :disabled t
  :config
  (use-package comint
    :init
    (setq comint-scroll-to-bottom-on-input t   ;; always insert at the bottom
          ;; no duplicates in command history
          comint-input-ignoredups t))

  (use-package shell-command
    :ensure t
    :init (shell-command-completion-mode 1))

  ;; The following setup is from readline-complete package at
  ;; https://github.com/monsanto/readline-complete.el/blob/master/readline-complete.el
  ;; set up shell (not eshell) mode
  (setq explicit-shell-file-name "fish"
        explicit-bash-args '("-c" "export EMACS=; stty echo; fish")
        comint-process-echoes t)

  (use-package readline-complete
    :ensure t
    :functions ac-modes ac-rlc-setup-sources
    :config
    (when (fboundp 'company-mode)
      (push 'company-readline company-backends)
      (add-hook 'rlc-no-readline-hook
                (lambda ()
                  (company-mode -1))))
    (when (fboundp 'auto-complete-mode)
      (add-to-list 'ac-modes 'shell-mode)
      (add-hook 'shell-mode-hook #'ac-rlc-setup-sources)))

  (use-package bash-completion
    :ensure t
    :init (bash-completion-setup))

  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

  (use-package term
    :config
    (use-package term+
      :ensure t)))

(provide 'shell-init)

;;; shell-init.el ends here
