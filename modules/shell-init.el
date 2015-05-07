;;; shell-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup shell (not eshell) mode.

;;; Code:

;; (defun setup-shell-mode ()
;;   "Personal preferences for shell mode."
;;   (interactive)
;;   (setq sh-basic-offset 4
;;         sh-indentation 4))
;; (add-hook 'sh-mode-hook 'setup-shell-mode)

(use-package sh-script
  :defer t
  :config
  (setq sh-basic-offset 4
        sh-indent-comment t
        sh-indentation 4)
  (bind-key "C-c C-d" 'duplicate-thing shell-mode-map))

(use-package comint
  :config
  (setq comint-scroll-to-bottom-on-input t   ;; always insert at the bottom
        ;; no duplicates in command history
        comint-input-ignoredups t))

(use-package shell-command
  :config (shell-command-completion-mode 1))

;; The following setup is from readline-complete package at
;; https://github.com/monsanto/readline-complete.el/blob/master/readline-complete.el
;; set up shell (not eshell) mode
(setq explicit-shell-file-name "fish"
      explicit-bash-args '("-c" "export EMACS=; stty echo; fish")
      comint-process-echoes t)

(use-package readline-complete
  :ensure t
  :defer t
  :config
  (when (fboundp 'company-mode)
    (push 'company-readline company-backends)
    (add-hook 'rlc-no-readline-hook
              (lambda ()
                (company-mode -1))))
  (when (fboundp 'auto-complete-mode)
    (add-to-list 'ac-modes 'shell-mode)
    (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)))

(use-package bash-completion
  :ensure t
  :defer t
  :config (bash-completion-setup))

;;(add-hook 'sh-set-shell-hook 'flymake-shell-load) ;; flymake syntax-check for shell scripts
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)

(provide 'shell-init)

;;; shell-init.el ends here
