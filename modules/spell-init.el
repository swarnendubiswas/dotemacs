;;; spell-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup spell check.

;;; Code:

(use-package flyspell
  :defer 5
  :config
  (setq-default ispell-program-name "/usr/bin/aspell")
  ;; speed up aspell: ultra | fast | normal | bad-spellers
  (setq ispell-extra-args '("--sug-mode=normal"))
  ;;(bound-and-true-p 'flyspell-mode)
  (add-hook 'find-file-hooks 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :diminish flyspell-mode
  :bind
  ("C-c i f" . flyspell-mode)
  ("C-c i b" . flyspell-buffer))

;; (eval-after-load "flyspell"
;;   '(diminish 'flyspell-mode))

(provide 'spell-init)

;;; spell-init.el ends here
