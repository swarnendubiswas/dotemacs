;;; recentf-init.el --- Part of emacs initialization

;;; Commentary:
;; Track recently-accessed files.

;;; Code:

(setq recentf-max-menu-items 15 ; show in recent menu
      recentf-max-saved-items 50 ; keep track of last xx files
      recentf-auto-cleanup 'never
      recentf-exclude '("/tmp/") ; "/ssh:"
      recentf-filename-handlers '(abbreviate-file-name)) ; save file names relative to my current home directory
(recentf-mode 1)

(provide 'recentf-init)

;;; recentf-init.el ends here
