;;; search-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-mode-line-theme)
(defvar dotemacs-theme)

(setq case-fold-search t) ; Make search ignore case

;; Use "C-'" in isearch-mode-map to use avy-isearch to select one of the currently visible isearch candidates.
(use-package isearch
  :commands (isearch-forward isearch-forward-regexp isearch-repeat-forward)
  :preface
  ;; http://endlessparentheses.com/leave-the-cursor-at-start-of-match-after-isearch.html?source=rss
  (defun sb/isearch-exit-other-end ()
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
  (use-package isearch-symbol-at-point
    :ensure t)
  :bind (("C-s" . nil) ; isearch-forward-regexp
         ("C-f" . isearch-forward-regexp)
         :map isearch-mode-map
         ("C-s" . nil) ; isearch-repeat-forward
         ("C-f" . isearch-repeat-forward)
         ("C-<return>" . sb/isearch-exit-other-end)))

(use-package isearch-dabbrev
  :ensure t
  :bind (:map isearch-mode-map
              ("<tab>" . isearch-dabbrev-expand)))

(use-package replace+
  :ensure t
  :after replace)

(use-package anzu
  :ensure t
  :after isearch
  :diminish anzu-mode
  :config
  (setq anzu-search-threshold 10000
        anzu-minimum-input-length 2)
  (when (eq dotemacs-mode-line-theme 'spaceline)
    (setq anzu-cons-mode-line-p nil))
  (unless (eq dotemacs-theme 'leuven)
    (set-face-attribute 'anzu-mode-line nil
                        :foreground "blue"
                        :weight 'light))
  (global-anzu-mode 1))

(use-package swiper ; Performs poorly if there are a large number of matches
  :ensure t
  :bind ("<f4>" . swiper)
  :config
  (setq swiper-use-visual-line t
        swiper-action-recenter t))

(use-package ag
  :ensure t
  :defer t
  :config
  (setq ag-reuse-buffers t
        ag-reuse-window t
        ag-highlight-search t))

(use-package wgrep
  :ensure t
  :defer t
  :config (setq wgrep-auto-save-buffer t))

(use-package wgrep-ag ; Edit the *ag* buffer with wgrep-change-to-wgrep-mode
  :ensure t
  :after ag
  :config (add-hook 'ag-mode-hook #'wgrep-ag-setup))

(use-package ido-occur
  :ensure t
  :if (eq dotemacs-selection 'ido)
  :bind (("C-c s o" . ido-occur)
         ("C-c s O" . ido-occur-at-point)))

(use-package ace-isearch
  :ensure t
  :ensure avy
  :ensure ace-jump-mode
  :diminish ace-isearch-mode
  :defer t
  :config (global-ace-isearch-mode 1))

(use-package ripgrep
  :ensure t
  :bind ("C-c s r" . ripgrep-regexp))

(provide 'search)

;;; search-init.el ends here
