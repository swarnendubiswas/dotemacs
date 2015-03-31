;;; shell-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup shell (not eshell) mode.

;;; Code:

(use-package readline-complete
  :ensure t
  :defer t)

(use-package bash-completion
  :ensure t
  :defer t
  :config (bash-completion-setup))

;; set up shell (not eshell) mode
(setq explicit-shell-file-name "fish"
      ;;explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
      sh-basic-offset 4
      sh-indent-comment t
      comint-process-echoes t)

;; setup auto-completion framework
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))
;;(add-hook 'sh-set-shell-hook 'flymake-shell-load) ;; flymake syntax-check for shell scripts
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'shell-init)

;;; shell-init.el ends here
