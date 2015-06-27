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

;; this package now provides ivy-mode
(use-package swiper
  :disabled t ;; performs poorly if there are a large number of matches
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ;; be less noisy
        swiper-min-highlight 3)
  ;;(bind-key "<f6>" 'ivy-resume ivy-mode-map)
  (use-package swiper-helm
    :ensure t)
  :diminish ivy-mode
  :bind* (("C-f" . swiper-helm)
          ("C-r" . swiper-helm)))

(use-package color-moccur
  :commands (isearch-moccur isearch-all)
  :bind ("M-s O" . moccur)
  :init
  (bind-key "M-o" 'isearch-moccur isearch-mode-map)
  (bind-key "M-O" 'isearch-moccur-all isearch-mode-map)
  :config
  (use-package moccur-edit))

(use-package loccur
  :ensure t
  :defer t
  :config (loccur-mode 1)
  :diminish loccur-mode)

(provide 'search-init)

;;; search-init.el ends here
