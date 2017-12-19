;;; abbrev-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup abbrev mode, useful for auto-correction.

;;; Code:

(defvar dotemacs-extras-directory)

(use-package abbrev
  :diminish abbrev-mode
  :init
  (setq abbrev-file-name (concat dotemacs-extras-directory "abbrev_defs"))
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'abbrev-mode ))
  :config
  ;; Do not ask to save new abbrevs when quitting
  (setq save-abbrevs 'silently))

(provide 'abbrev-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; M-x edit-abbrev                     ;;
;; ;; C-x a i g inverse-add-global-abbrev ;;
;; ;; C-x a i l inverse-add-mode-abbrev   ;;
;; ;; M-x write-abbrev-file               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; abbrev-init.el ends here
