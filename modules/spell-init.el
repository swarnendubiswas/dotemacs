;;; spell-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup spell check.

;;; Code:

(use-package flyspell
  :init (add-hook 'find-file-hooks 'turn-on-flyspell)
  :config
  (setq-default ispell-program-name "/usr/bin/aspell")
  ;; speed up aspell: ultra | fast | normal
  (setq ispell-extra-args '("--sug-mode=normal"))
  :diminish flyspell-mode
  :bind
  ("C-c i f" . flyspell-mode)
  ("C-c i b" . flyspell-buffer))

;; (eval-after-load "flyspell"
;;   '(diminish 'flyspell-mode))

(provide 'spell-init)

;;; spell-init.el ends here
