;;; recentf-init.el --- Part of emacs initialization

;;; Commentary:
;; Track recently-accessed files.

;;; Code:

(use-package recentf
  ;;:commands (recentf-mode recentf-add-file recentf-apply-filename-handlers)
  :init
  (recentf-mode 1)
  :config
  (setq recentf-max-menu-items 15 ; show in recent menu
        recentf-max-saved-items 50 ; keep track of last xx files
        recentf-auto-cleanup 'never
        recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads\\.el\\'" "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies"
                          "/tmp/")) ; "/ssh:"
  ;;(add-to-list 'recentf-filename-handlers 'abbreviate-file-name) ; save file names relative to my current home directory
  (use-package recentf-ext
    :ensure t))

(provide 'recentf-init)

;;; recentf-init.el ends here
