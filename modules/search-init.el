;;; search-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

(defvar dotemacs-selection)

(setq case-fold-search t) ; Make search ignore case

;; Use "C-'" in isearch-mode-map to use avy-isearch to select one of the currently visible isearch candidates.
(use-package isearch
  :commands (isearch-forward isearch-forward-regexp isearch-repeat-forward)
  :preface
  ;; http://endlessparentheses.com/leave-the-cursor-at-start-of-match-after-isearch.html?source=rss
  (defun dotemacs--isearch-exit-other-end ()
    "Exit isearch, at the opposite end of the string."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
  :config
  (setq search-highlight t ; Highlight incremental search
        isearch-allow-scroll t)
  (use-package isearch+
    :ensure t
    :diminish isearch-mode)
  (use-package isearch-dabbrev
    :ensure t
    :bind (:map isearch-mode-map
                ("<tab>" . isearch-dabbrev-expand)))
  (use-package isearch-symbol-at-point
    :ensure t)
  :bind (("C-s" . nil) ; isearch-forward-regexp
         ("C-f" . isearch-forward-regexp)
         :map isearch-mode-map
         ("C-s" . nil) ; isearch-repeat-forward
         ("C-f" . isearch-repeat-forward)
         ("C-<return>" . isearch-exit-other-end)))

(use-package replace+
  :ensure t
  :after replace)

(use-package swiper ; Performs poorly if there are a large number of matches
  :ensure t
  :bind ("<f4>" . swiper)
  :config
  (setq swiper-use-visual-line t
        swiper-action-recenter t))

(use-package swiper-helm
  :ensure t
  :after swiper helm
  :if (eq dotemacs-selection 'helm)
  :bind ("<f4>" . swiper-helm))

(use-package ag
  :ensure t
  :config
  (setq ag-reuse-buffers t
        ag-reuse-window t
        ag-highlight-search t))

(use-package helm-ag
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :after ag helm
  :bind ("C-c s a" . helm-ag)
  :config
  (setq helm-ag-fuzzy-match t
        helm-ag-insert-at-point 'symbol
        helm-ag-source-type 'file-line))

(use-package wgrep-ag ; Edit the *ag* buffer with wgrep-change-to-wgrep-mode
  :ensure t
  :after ag
  :config
  (use-package wgrep
    :ensure t
    :config (setq wgrep-auto-save-buffer t))
  (add-hook 'ag-mode-hook #'wgrep-ag-setup))

(use-package ido-occur
  :ensure t
  :if (eq dotemacs-selection 'ido)
  :bind (("C-c s o" . ido-occur)
         ("C-c s O" . ido-occur-at-point)))

(use-package ace-isearch
  :ensure t
  :ensure avy
  :ensure ace-jump-mode
  :diminish  ace-isearch-mode
  :config (global-ace-isearch-mode 1))

(use-package ripgrep
  :ensure t
  :bind ("C-c s r" . ripgrep-regexp))

(provide 'search-init)

;;; search-init.el ends here
