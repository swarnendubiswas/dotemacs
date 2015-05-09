;;; recentf-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Track recently-accessed files.

;;; Code:

(use-package recentf
  :init (recentf-mode 1)
  :config
  (setq recentf-max-menu-items 15 ; show in recent menu
        recentf-max-saved-items 50 ; keep track of last xx files
        recentf-auto-cleanup 'mode
        recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads.el\\'"
                          "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies" "[/\\]tmp/.*"
                          ".*/recentf\\'" "~$" "/.autosaves/" ".*-loaddefs.el") ; "/ssh:"
        recentf-save-file (concat dotemacs-temp-directory "recentf"))
  (add-to-list 'recentf-exclude "[/\\]tmp/recentf-marker\\'")
  ;; clean up recent files on startup, since otherwise the exclude list is not always respected
  ;;(recentf-auto-cleanup)

  ;; save file names relative to my current home directory
  ;;(add-to-list 'recentf-filename-handlers 'abbreviate-file-name)
  ;;(global-set-key [f8] 'recentf-open-files)

  ;; ;; Periodically (600 s) save recently opened files, in case emacs crashes
  ;; (eval-after-load 'recentf
  ;;   '(run-with-timer 0 (* 600 60) 'recentf-save-list))
  
  ;; Add buffers to recent list.
  (use-package recentf-ext
    :ensure t)
  
  ;; Useful for synchronizing updates in case more than one emacs window is open.
  (use-package sync-recentf
    :disabled t
    :load-path "lisp/"
    :config (recentf-auto-cleanup))
  
  :bind ("<f8>" . recentf-open-files))

(provide 'recentf-init)

;;; recentf-init.el ends here
