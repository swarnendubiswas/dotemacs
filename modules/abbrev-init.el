;;; abbrev-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup abbrev.

;;; Code:

(use-package abbrev
  :disabled t
  :diminish abbrev-mode
  :config
  (setq-default abbrev-file-name (concat dotemacs--temp-directory "abbrev_defs"))
  (setq save-abbrevs 'silently ; do not ask to save new abbrevs when quitting
        ;; preserve case when expanding
        dabbrev-case-replace nil)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))
  ;; enable in all modes derived from text-mode
  (add-hook 'text-mode-hook #'abbrev-mode)
  (add-hook 'prog-mode-hook
            (lambda ()
              (abbrev-mode -1))))

(provide 'abbrev-init)

;;; abbrev-init.el ends here
