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

(use-package replace
  :defer t
  :config
  (setq query-replace-highlight t) ; highlight during query
  (use-package replace+
    :ensure t))

(setq case-fold-search t ; make search ignore case
      grep-highlight-matches t
      grep-scroll-output t)

;; performs poorly if there are a large number of matches
(use-package swiper
  :ensure t
  :defer t
  :config
  (use-package ivy
    :init (ivy-mode 1)
    :diminish ivy-mode)
  (setq ivy-use-virtual-buffers t
        ;; be less noisy
        swiper-min-highlight 3)
  (bind-key "C-c u" 'ivy-resume ivy-mode-map)
  (use-package swiper-helm
    :ensure t)
  :bind* (("C-c f" . swiper)
          ("C-c r" . swiper-helm)))

(use-package color-moccur
  :ensure t
  :defer t
  :commands (isearch-moccur isearch-all)
  :bind ("M-s O" . moccur)
  :config
  (bind-keys
   :map isearch-mode-map
   ("M-o" . isearch-moccur)
   ("M-O" . isearch-moccur-all))
  (use-package moccur-edit))

(use-package loccur
  :ensure t
  :commands loccur-mode
  :defer t
  :config (loccur-mode 1)
  :diminish loccur-mode)

(provide 'search-init)

;;; search-init.el ends here
