;; set up shell (not eshell) mode
(setq explicit-shell-file-name "fish"
      explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
      )
(setq comint-process-echoes t)
;; setup auto-completion framework
(push 'company-readline company-backends)
(add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))
(add-hook 'sh-set-shell-hook 'flymake-shell-load) ;; flymake syntax-check for shell scripts
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
