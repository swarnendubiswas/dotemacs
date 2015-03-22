;;; abbrev-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup abbrev

;;; Code:

(use-package abbrev
  :disabled t
  :commands abbrev-mode
  :diminish abbrev-mode
  :init
  (add-hook 'text-mode-hook 'abbrev-mode)
  :config
  (setq-default abbrev-file-name "~/.emacs.d/abbrev_defs") 
  (setq save-abbrevs nil ; do not ask to save new abbrevs when quitting
        dabbrev-case-replace nil) ; preserve case when expanding
  ;;(quietly-read-abbrev-file)
  )

(provide 'abbrev-init)

;;; abbrev-init.el ends here
