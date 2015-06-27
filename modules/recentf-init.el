;;; recentf-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Track recently-accessed files.

;;; Code:

(use-package recentf
  :init
  ;; set this first so that recentf can load content from this
  (setq recentf-save-file (concat dotemacs--temp-directory "recentf")
        recentf-max-menu-items 15 ; show in recent menu
        recentf-max-saved-items 50 ; keep track of last xx files
        ;; clean up after Emacs has been idle for certain number of seconds
        recentf-auto-cleanup 45
        ;; check regex with re-builder
        recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads.el\\'"
                          "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies" "[/\\]tmp/.*"
                          ".*/recentf\\'" "~$" "/.autosaves/" ".*-loaddefs.el" "/TAGS$"
                          "[/\\]tmp/recentf-marker\\'" "/company-statistics-cache.el$")) ; "/ssh:"

  (recentf-mode 1)

  ;; clean up recent files on startup, since otherwise the exclude list is not always respected
  (recentf-cleanup)

  :config
  (add-to-list 'recentf-used-hooks
               '(dired-after-readin-hook recentf-track-opened-file))

  ;; clean up recent files on startup, since otherwise the exclude list is not always respected
  ;;(add-hook 'after-init-hook #'recentf-auto-cleanup)

  ;; save file names relative to my current home directory
  ;;(add-to-list 'recentf-filename-handlers 'abbreviate-file-name)

  ;; ;; Periodically (600 s) save recently opened files, in case emacs crashes
  ;; (with-eval-after-load "recentf"
  ;;   (run-with-timer 0 (* 600 60) 'recentf-save-list))

  ;;(add-hook 'kill-emacs-hook #'recentf-save-list)

  ;; Add buffers to recent list.
  (use-package recentf-ext
    :ensure t)

  ;; Useful for synchronizing updates in case more than one emacs window is open. If more than one window is open, the
  ;; last window to close overwrites the recentf list.
  (use-package sync-recentf
    :ensure t
    :pin melpa
    ;;:load-path "lisp/"
    :init (recentf-auto-cleanup))

  :bind ("<f8>" . recentf-open-files))

(provide 'recentf-init)

;;; recentf-init.el ends here
