;;; recentf-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Track recently-accessed files.

;;; Code:

(use-package recentf
  :commands (recentf-mode) ;recentf-add-file recentf-apply-filename-handlers recentf-open-files
  :config
  (setq recentf-max-menu-items 15 ; show in recent menu
        recentf-max-saved-items 50 ; keep track of last xx files
        recentf-auto-cleanup 'never
        recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads\\.el\\'"
                          "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies" "/tmp/")) ; "/ssh:"
  ;; save file names relative to my current home directory
  ;;(add-to-list 'recentf-filename-handlers 'abbreviate-file-name)
  (recentf-mode 1)
  ;; Periodically (10 s) save recently opened files, in case emacs crashes
  (eval-after-load 'recentf
    '(run-with-timer 0 (* 10 60) 'recentf-save-list))
  ;; Add buffers to recent list.
  (use-package recentf-ext
    :ensure t)
  :bind ("<f8>" . recentf-open-files))

;;(global-set-key [f8] 'recentf-open-files)

(provide 'recentf-init)

;;; recentf-init.el ends here
