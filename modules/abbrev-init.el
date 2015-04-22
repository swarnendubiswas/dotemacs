;;; abbrev-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup abbrev.

;;; Code:

(use-package abbrev
  :disabled t
  :diminish abbrev-mode
  :config
  (setq-default abbrev-file-name (concat emacs-tmp-directory "abbrev_defs"))
  (setq save-abbrevs nil ; do not ask to save new abbrevs when quitting
        dabbrev-case-replace nil) ; preserve case when expanding
  ;;(quietly-read-abbrev-file)
  (add-hook 'text-mode-hook #'abbrev-mode)
  (add-hook 'LaTeX-mode-hook #'abbrev-mode))

(provide 'abbrev-init)

;;; abbrev-init.el ends here
