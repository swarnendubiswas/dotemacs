;;; recentf-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Track recently-accessed files.

;;; Code:

(use-package recentf
  :init
  (setq recentf-save-file (concat dotemacs-temp-directory "recentf") ; set this first so that recentf can load content from this
        recentf-max-menu-items 15 ; show in recent menu
        recentf-max-saved-items 50 ; keep track of last xx files
        recentf-auto-cleanup 45 ; clean up after Emacs has been idle for certain number of seconds
        ;; check regex with re-builder
        recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads.el\\'"
                          "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies" "[/\\]tmp/.*"
                          ".*/recentf\\'" "~$" "/.autosaves/" ".*-loaddefs.el" "/TAGS$"
                          "[/\\]tmp/recentf-marker\\'" "/company-statistics-cache.el$")) ; "/ssh:"
  (recentf-mode 1)
  ;; clean up recent files on startup, since otherwise the exclude list is not always respected
  (recentf-cleanup)

  (add-to-list 'recentf-used-hooks
               '(dired-after-readin-hook recentf-track-opened-file))

  ;; ;; Periodically (600 s) save recently opened files, in case emacs crashes
  ;; (run-with-timer 0 (* 600 60) 'recentf-save-list)

  (use-package recentf-ext ; Add directories to recent list.
    :ensure t)

  ;; Useful for synchronizing updates in case more than one emacs window is open. If more than one window is open, the
  ;; last window to close overwrites the recentf list.
  (use-package sync-recentf
    :ensure t
    :pin melpa
    :init (recentf-auto-cleanup))

  (setq initial-buffer-choice 'recentf-open-files)
  :bind ("<f8>" . recentf-open-files))

(provide 'recentf-init)

;;; recentf-init.el ends here
