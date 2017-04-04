;;; recentf-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Track recently-accessed files.

;;; Code:

(defvar dotemacs-temp-directory)
(defvar dotemacs-selection)

;; Adding directories to the list of recent files decreases the number of entries of recent files. Therefore, we use a
;; different command/keybinding to lookup recent directories.

(use-package recentf
  :init
  (setq-default recentf-save-file (concat dotemacs-temp-directory "recentf") ; Set this first so that recentf can load content
                                        ; from this
                recentf-max-menu-items 10 ; Show in recent menu
                recentf-max-saved-items 200
                ;; Disable this so that recentf does not attempt to stat remote files:
                ;; https://www.emacswiki.org/emacs/RecentFiles
                recentf-auto-cleanup 'never
                recentf-menu-filter 'recentf-sort-descending
                ;; Check regex with re-builder
                recentf-exclude '("[/\\]\\.elpa/" "[/\\]\\.ido\\.last\\'" "[/\\]\\.git/" ".*\\.gz\\'" ".*-autoloads.el\\'"
                                  "[/\\]archive-contents\\'" "[/\\]\\.loaddefs\\.el\\'" "url/cookies" "[/\\]tmp/.*"
                                  ".*/recentf\\'" "~$" "/.autosaves/" ".*-loaddefs.el" "/TAGS$" "/ssh:" "/sudo:"
                                  "/company-statistics-cache.el$"))
  (recentf-mode 1) ; This is needed in :init for the keybinding to work
  :config
  (add-to-list 'recentf-used-hooks
               '(dired-after-readin-hook recentf-track-opened-file))
  (when (eq dotemacs-selection 'none)
    (bind-key "<f9>" #'recentf-open-files))
  (run-at-time nil (* 10 60) 'recentf-save-list))

;; Hide the "wrote to recentf" message, which can be irritating.
(defun dotemacs-recentf-save-list (orig-fun &rest args)
  "Hide messages appearing in ORIG-FUN."
  (let ((inhibit-message t))
    (apply orig-fun args)))

(advice-add 'recentf-save-list :around #'dotemacs-recentf-save-list)

(provide 'recentf-init)

;;; recentf-init.el ends here
