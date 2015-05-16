;;; search-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

(use-package isearch
  :defer t
  :config
  (setq search-highlight t) ; highlight incremental search
  (use-package isearch+
    :ensure t)
  (use-package isearch-dabbrev
    :ensure t
    :config (bind-key "<tab>" 'isearch-dabbrev-expand isearch-mode-map))
  (use-package isearch-symbol-at-point
    :ensure t)
  :diminish isearch-mode)

(use-package replace
  :defer t
  :config
  (setq query-replace-highlight t) ; highlight during query
  (use-package replace+
    :ensure t))

(setq case-fold-search t ; make search ignore case
      grep-highlight-matches t
      grep-scroll-output t) 

(provide 'search-init)

;;; search-init.el ends here
