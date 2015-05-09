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
        ;; preserve case when expanding
        dabbrev-case-replace nil)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  ;; enable in all modes derived from text-mode
  (add-hook 'text-mode-hook #'abbrev-mode))

(provide 'abbrev-init)

;;; abbrev-init.el ends here
