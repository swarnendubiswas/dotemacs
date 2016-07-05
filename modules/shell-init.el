;;; shell-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup shell, eshell, terminal emulation.  This module is not to configure editing of shell scripts.

;;; Code:

(use-package shell
  :disabled t
  :config
  (use-package comint
    :config
    (setq comint-scroll-to-bottom-on-input t   ;; Always insert at the bottom
          ;; No duplicates in command history
          comint-input-ignoredups t))

  (use-package shell-command
    :ensure t
    :config (shell-command-completion-mode 1))

  ;; The following setup is from readline-complete package at
  ;; https://github.com/monsanto/readline-complete.el/blob/master/readline-complete.el
  ;; set up shell (not eshell) mode
  (setq explicit-shell-file-name "fish"
        explicit-bash-args '("-c" "export EMACS=; stty echo; fish")
        comint-process-echoes t)

  (use-package readline-complete
    :ensure t
    :functions (ac-modes ac-rlc-setup-sources)
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
    :config (bash-completion-setup))

  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on))

(use-package term
  :disabled t
  :config
  (use-package term+
    :ensure t))

;; Avoid Emacs querying "active processes exist; kill them and exit anyway?", since we are creating an inferior python
;; process and aspell
(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(provide 'shell-init)

;;; shell-init.el ends here
