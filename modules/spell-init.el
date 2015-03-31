;;; spell-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup spell check.

;;; Code:

(use-package flyspell
  :init
  (setq-default ispell-program-name "/usr/bin/aspell")
  ;; speed up aspell: ultra | fast | normal
  (setq ispell-extra-args '("--sug-mode=normal"))
  :config
  (add-hook 'find-file-hooks 'turn-on-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  :diminish flyspell-mode
  :bind
  ("C-c i f" . flyspell-mode)
  ("C-c i b" . flyspell-buffer))

;; (eval-after-load "flyspell"
;;   '(diminish 'flyspell-mode))

(provide 'spell-init)

;;; spell-init.el ends here
