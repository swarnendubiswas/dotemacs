;;; defaults-init.el --- Part of emacs initialization

;;; Commentary:
;; Setup and tweak emacs defaults.

;;; Code:

(provide 'defaults-init)

;; startup
(setq inhibit-default-init t ; disable loading of "default.el" at startup
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'text-mode ; *scratch* is in Lisp interaction mode by default, use text mode instead
      ) 
(setq-default major-mode 'text-mode)


;; better frame titles
;;(setq frame-title-format (concat  "%b - emacs@" (system-name)))
(setq frame-title-format
      (list '(buffer-file-name "%f" "%b") " -- " "GNU Emacs " emacs-version "@" system-name))


(setq require-final-newline t ; always end a file with a newline
      sentence-end-double-space nil)
(fset 'yes-or-no-p 'y-or-n-p) ; type "y"/"n" instead of "yes"/"no"
(set-face-attribute 'default nil :height 110) ; set font size, value is in 1/10pt, so 100 will give you 10pt

;; we need to paste something from another program, but sometimes we do real paste after some kill
;; action, that will erase the clipboard, so we need to save it to kill ring.
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard t) ; enable use of system clipboard across emacs and other applications


;; backup
(setq make-backup-files nil ; stop making backup ~ files
      backup-inhibited t) ; disable backup for a per-file basis, not to be used by major modes


;; auto revert
(global-auto-revert-mode 1) ; auto-refresh all buffers, does not work for remote files
(setq-default auto-revert-interval 5 ; default is 5 s
              auto-revert-verbose nil
              global-auto-revert-non-file-buffers t ; auto-refresh dired buffers
              ) 


(delete-selection-mode 1) ; typing with the mark active will overwrite the marked region
(transient-mark-mode 1) ; enable visual feedback on selections, default since v23
(global-hungry-delete-mode 1) ; erase 'all' consecutive white space characters in a given direction

(provide 'defaults-init)

;;; defaults-init.el ends here
