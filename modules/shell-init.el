;;; shell-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

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
        sh-indentation 4))

(use-package readline-complete
  :ensure t
  :defer t
  :config
  (push 'company-readline company-backends)
  (add-hook 'rlc-no-readline-hook
            (lambda ()
              (company-mode -1))))

(use-package bash-completion
  :ensure t
  :defer t
  :config (bash-completion-setup))

;; set up shell (not eshell) mode
(setq explicit-shell-file-name "fish"
      ;;explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
      comint-process-echoes t)

;;(add-hook 'sh-set-shell-hook 'flymake-shell-load) ;; flymake syntax-check for shell scripts
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'shell-init)

;;; shell-init.el ends here
