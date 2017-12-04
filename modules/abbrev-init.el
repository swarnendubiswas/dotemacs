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
  (setq save-abbrevs 'silently) ; Do not ask to save new abbrevs when quitting
  ;; (if (file-exists-p abbrev-file-name)
  ;;     (quietly-read-abbrev-file))
  )

(provide 'abbrev-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; M-x edit-abbrev                     ;;
;; ;; C-x a i g inverse-add-global-abbrev ;;
;; ;; C-x a i l inverse-add-mode-abbrev   ;;
;; ;; M-x write-abbrev-file               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; abbrev-init.el ends here
