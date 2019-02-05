;;; shell-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup shell, eshell, terminal emulation.  This module is not to configure editing of shell scripts.

;;; Code:

(defvar dotemacs-completion-in-buffer)

(use-package shell
  :config
  (use-package comint
    :config
    (setq comint-scroll-to-bottom-on-input t ; Always insert at the bottom
          ;; No duplicates in command history
          comint-input-ignoredups t))

  (use-package shell-command
    :ensure t
    :config (shell-command-completion-mode 1))

  ;; Set up shell (not eshell) mode: https://github.com/monsanto/readline-complete.el/blob/master/readline-complete.el
  ;; https://stackoverflow.com/questions/37409085/how-to-define-a-default-shell-for-emacs
  (setq-default explicit-shell-file-name "/bin/bash"
                shell-file-name explicit-shell-file-name
                explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
                comint-process-echoes t)
  (setenv "ESHELL" shell-file-name)

  (use-package readline-complete
    :ensure t
    :config
    (when (bound-and-true-p dotemacs-completion-in-buffer)
      (push 'company-readline company-backends)
      (add-hook 'rlc-no-readline-hook
                (lambda ()
                  (company-mode -1)))))

  (use-package bash-completion
    :ensure t
    :config (bash-completion-setup))

  ;; http://www.joshstaiger.org/archives/2005/07/fixing_garbage.html
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on))

;; Avoid Emacs querying "active processes exist; kill them and exit anyway?", since we are creating an inferior python
;; process and aspell
(add-hook 'comint-exec-hook
          (lambda ()
            (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

(use-package fish-completion
  :ensure t
  :if (when (executable-find "fish"))
  :config (global-fish-completion-mode))

(provide 'shell-init)

;;; shell-init.el ends here
