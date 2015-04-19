;;; search-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

(use-package isearch
  :defer t
  :config
  (setq search-highlight t) ; highlight incremental search
  :diminish isearch-mode)

(use-package replace
  :defer t
  :config
  (setq query-replace-highlight t)) ; highlight during query

(setq case-fold-search t) ; make search ignore case

(provide 'search-init)

;;; search-init.el ends here
