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
    :disabled t
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
  :config
  (setq swiper-use-visual-line t
        swiper-action-recenter t)
  (when (eq dotemacs-selection 'none)
    (bind-key "<f4>" #'swiper)))

(use-package wgrep
  :ensure t
  :config (setq wgrep-auto-save-buffer t))

(use-package ripgrep
  :ensure t
  :bind ("C-c s r" . ripgrep-regexp))

(provide 'search-init)

;;; search-init.el ends here
