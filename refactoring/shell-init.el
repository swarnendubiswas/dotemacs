;;; shell-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup shell (not eshell) mode.

;;; Code:

(setq explicit-shell-file-name "fish"
      explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
      comint-process-echoes t
      )

;; setup auto-completion framework
(push 'company-readline company-backends)

(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))
(add-hook 'sh-set-shell-hook 'flymake-shell-load) ;; flymake syntax-check for shell scripts
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'shell-init)

;;; shell-init.el ends here
