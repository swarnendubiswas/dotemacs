;;; search-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

(use-package isearch
  :defer t
  :config
  (setq search-highlight t ; highlight incremental search
        isearch-allow-scroll t)
  (use-package isearch+
    :ensure t)
  (use-package isearch-dabbrev
    :ensure t
    :config (bind-key "<tab>" 'isearch-dabbrev-expand isearch-mode-map))
  (use-package isearch-symbol-at-point
    :ensure t)
  :diminish isearch-mode)

(unbind-key "C-s") ; isearch-forward-regexp
(bind-key "C-f" #'isearch-forward-regexp)
(bind-key "C-f" #'isearch-repeat-forward isearch-mode-map)

(use-package replace
  :defer t
  :config
  (setq query-replace-highlight t) ; highlight during query
  (use-package replace+
    :ensure t))

(setq case-fold-search t ; make search ignore case
      grep-highlight-matches t
      grep-scroll-output t)

(use-package swiper ; performs poorly if there are a large number of matches
  :ensure t
  :config
  (setq swiper-min-highlight 3) ; be less noisy
  :bind ("C-c s s" . swiper))

(use-package swiper-helm
  :ensure t
  :bind ("C-c s h" . swiper-helm))

(use-package color-moccur
  :ensure t
  :functions (isearch-moccur isearch-all)
  :bind ("C-c s o" . moccur)
  :config
  (use-package moccur
    :config
    (use-package moccur-edit
      :ensure t))
  (bind-keys
   :map isearch-mode-map
   ("C-c s i" . isearch-moccur)
   ("C-c s m" . isearch-moccur-all)))

(use-package loccur
  :ensure t
  :functions loccur-mode
  :defer t
  :config (loccur-mode 1)
  :diminish loccur-mode)

(use-package ag
  :ensure t
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t)
  (use-package helm-ag
    :ensure t
    :if (bound-and-true-p dotemacs-use-helm-p)
    :config
    (setq helm-ag-fuzzy-match t
          helm-ag-insert-at-point 'symbol
          helm-ag-source-type 'file-line))
  :bind (("C-c s d" . ag-dired)
         ("C-c s f" . ag-files)
         ("C-c s a" . helm-ag)))

(use-package find-file-in-project
  :ensure t
  :config (setq ffip-prefer-ido-mode t))

(provide 'search-init)

;;; search-init.el ends here
