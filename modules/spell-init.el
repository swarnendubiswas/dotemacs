;;; spell-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup spell check.

;;; Code:

(use-package flyspell
  :init 
  ;;(add-hook 'find-file-hooks 'turn-on-flyspell)
  :config
  (setq-default ispell-program-name "/usr/bin/aspell")
  ;; speed up aspell: ultra | fast | normal
  (setq ispell-extra-args '("--sug-mode=normal"))
  :diminish flyspell-mode
  :bind
  ("C-c i f" . flyspell-mode)
  ("C-c i b" . flyspell-buffer))

;; (global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
;; (global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)

;; (eval-after-load "flyspell"
;;   '(diminish 'flyspell-mode))

(provide 'spell-init)

;;; spell-init.el ends here
