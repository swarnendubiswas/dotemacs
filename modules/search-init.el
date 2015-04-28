;;; search-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

(use-package isearch
  :defer t
  :config
  (setq search-highlight t) ; highlight incremental search
  (use-package isearch+
    :ensure t
    :config
    ;; (eval-after-load "isearch"
    ;;   '(require 'isearch+))
    )
  (use-package isearch-dabbrev
    :ensure t
    :config
    ;; (eval-after-load "isearch"
    ;;   '(progn
    ;;      (require 'isearch-dabbrev)
    ;;      (define-key isearch-mode-map (kbd "<tab>") 'isearch-dabbrev-expand)))
    (bind-key "<tab>" 'isearch-dabbrev-expand isearch-mode))
  (use-package isearch-symbol-at-point
    :ensure t)
  :diminish isearch-mode)

(use-package replace
  :defer t
  :config
  (setq query-replace-highlight t) ; highlight during query
  (use-package replace+
    :ensure t
    :config
    ;; (eval-after-load "replace"
    ;;   '(progn (require 'replace+)))
    ))

(setq case-fold-search t) ; make search ignore case

(provide 'search-init)

;;; search-init.el ends here
