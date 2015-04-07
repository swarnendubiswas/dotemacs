;;; spell-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Setup spell check.

;;; Code:

(use-package flyspell
  :defer 5
  :config
  (setq-default ispell-program-name "/usr/bin/aspell")
  ;; speed up aspell: ultra | fast | normal | bad-spellers
  (setq ispell-extra-args '("--sug-mode=normal" "--lang=en_US"))
  ;;(bound-and-true-p 'flyspell-mode)
  (add-hook 'find-file-hooks 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (setq flyspell-sort-corrections t
        flyspell-check-region-doublons t
        flyspell-issue-message-flag nil)
  :diminish flyspell-mode
  :bind
  ("C-c i f" . flyspell-mode)
  ("C-c i b" . flyspell-buffer))

;; (eval-after-load "flyspell"
;;   '(diminish 'flyspell-mode))

(provide 'spell-init)

;;; spell-init.el ends here
