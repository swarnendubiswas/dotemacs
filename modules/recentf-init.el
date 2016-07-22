;;; recentf-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Track recently-accessed files.

;;; Code:

(defvar dotemacs-temp-directory)
(defvar dotemacs-selection)

(use-package recentf
  :init
  (setq recentf-save-file (concat dotemacs-temp-directory "recentf") ; Set this first so that recentf can load content from this
        recentf-max-menu-items 15 ; Show in recent menu
        recentf-max-saved-items 200 ; Keep track of last xx files
        ;;recentf-auto-cleanup 300
        ;; Check regex with re-builder
        recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads.el\\'"
                          "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies" "[/\\]tmp/.*"
                          ".*/recentf\\'" "~$" "/.autosaves/" ".*-loaddefs.el" "/TAGS$"
                          ;;"[/\\]tmp/sync-recentf-marker\\'"
                          ;; "/ssh:"
                          "/company-statistics-cache.el$"))
  (recentf-mode 1) ; This is needed in :init for the keybinding to work
  :config
  (add-to-list 'recentf-used-hooks
               '(dired-after-readin-hook recentf-track-opened-file))
  (when (eq dotemacs-selection 'none)
    (bind-key "<f9>" #'recentf-open-files)))

(use-package recentf-ext ; Add directories to recent list
  :ensure t
  :after recentf)

(provide 'recentf-init)

;;; recentf-init.el ends here
