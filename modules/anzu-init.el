;;; anzu-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Show number of searches in the mode line.

;;; Code:

(defvar dotemacs-mode-line-theme)
(defvar dotemacs-theme)

(use-package anzu
  :ensure t
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

(provide 'anzu-init)

;;; anzu-init.el ends here
