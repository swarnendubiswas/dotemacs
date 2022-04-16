;;; lang-org.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package org
  :init
  (setq org-directory "~/Documents/Org/"
        org-note-directory (concat org-directory "notes/")
        org-log-directory (concat org-directory "notes/logs/")
        org-inbox-file (concat org-directory "inbox.org")
        org-project-file (concat org-directory "projects.org")
        org-default-notes-file org-inbox-file
        org-modules '(ol-docview ol-info org-id org-habit))

  (advice-add 'server-execute :before
              (defun enable-org-protocol (&rest r)
                (unless (featurep 'org-protocol) (require 'org-protocol))))

  (autoload 'org-super-agenda "org-agenda")
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n!)" "HOLD(h!)" "|" "DONE(d)" "CXLD(c)")))

  (setq org-columns-default-format "%40ITEM %1PRIORITY %20TAGS %6Effort(EFFORT){:} %8CLOCKSUM"
        org-cycle-open-archived-trees t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-global-properties '(("STYLE_ALL" . "habit")
                                ("Effort_ALL" . "0:10 0:15 0:30 0:45 1:00 2:00 3:00 5:00"))
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-image-actual-width '(0.7)
        org-imenu-depth 3
        org-log-done 'time
        org-log-into-drawer t
        org-log-redeadline nil
        org-log-reschedule nil
        org-pretty-entities t
        org-preview-latex-image-directory (no-littering-expand-var-file-name "ltximg/")
        org-read-date-prefer-future nil
        org-startup-folded t
        org-startup-indented t
        org-startup-with-inline-images t
        org-track-ordered-property-with-tag t
        org-use-property-inheritance t
        org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{}
        org-yank-adjusted-subtrees t)

  (add-hook 'org-mode-hook
            (defun init-org-mode ()
              "Stuff to do when opening `org-mode' files."
              (setq truncate-lines nil)
              ;; disable <> auto pairing in electric-pair-mode for org-mode
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c) (if (char-equal c ?<) t
                                    (,electric-pair-inhibit-predicate c))))

              (setq imenu-create-index-function #'org-imenu-get-tree)))

  (defun open-org-inbox-file ()
    "Open `org-inbox-file'"
    (interactive)
    (find-file org-inbox-file))

  (defalias 'open-org-default-notes-file #'open-org-inbox-file
    "Open `org-default-notes-file'")

  (defun open-org-project-file ()
    "Open `org-project-file'"
    (interactive)
    (find-file org-project-file))

  (unless (fboundp 'find-lisp-find-files)
    (autoload #'find-lisp-find-files "find-lisp"))

  (defun org-note-files ()
    "Get the list of `org-mode' file in `org-note-directory'."
    (find-lisp-find-files org-note-directory "\.org$"))

  (defun org-log-files ()
    "Get the list of `org-mode' file in `org-log-directory'."
    (find-lisp-find-files org-log-directory "\.org$"))

          (defun org-inherited-priority (s)
            (cond
             ;; Priority cookie in this heading
             ((string-match org-priority-regexp s)
              (* 1000 (- org-priority-lowest
                         (org-priority-to-value (match-string 2 s)))))
             ;; No priority cookie, but already at highest level
             ((not (org-up-heading-safe))
              (* 1000 (- org-priority-lowest org-priority-default)))
             ;; Look for the parent's priority
             (t (org-inherited-priority (org-get-heading))))))

  (use-package org-attach
    :defer t
    :commands (org-attach-follow org-attach-complete-link)
    :init
    (org-link-set-parameters "attachment"
                             :follow #'org-attach-follow
                             :complete #'org-attach-complete-link)
    :config
    (setq org-attach-archive-delete 'query
          org-attach-id-dir (concat org-directory "attach/")
          org-attach-method 'mv
          org-attach-store-link-p 'file))

(provide 'lang-org)
;;; lang-org.el ends here
