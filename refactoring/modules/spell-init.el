;;; spell-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup spell check.

;;; Code:

(add-hook 'find-file-hooks 'turn-on-flyspell) 
(setq-default ispell-program-name "/usr/bin/aspell")
;; speed up aspell: ultra | fast | normal
(setq ispell-extra-args '("--sug-mode=normal"))

(provide 'spell-init)

;;; spell-init.el ends here
