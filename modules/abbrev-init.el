;;; abbrev-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup abbrev mode, useful for auto-correction.

;;; Code:

;; M-x edit-abbrev
;; C-x a i g inverse-add-global-abbrev
;; C-x a i l inverse-add-mode-abbrev
;; M-x write-abbrev-file
(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq-default abbrev-file-name (concat dotemacs-extras-directory "abbrev_defs"))
  (add-hook 'text-mode-hook #'abbrev-mode) ; Need this in :init to work
  :config
  (setq save-abbrevs 'silently) ; Do not ask to save new abbrevs when quitting
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(provide 'abbrev-init)

;;; abbrev-init.el ends here
