;;; search-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

(setq case-fold-search t) ; Make search ignore case

(use-package isearch
  :commands (isearch-forward isearch-forward-regexp isearch-repeat-forward)
  :preface
  ;; https://www.reddit.com/r/emacs/comments/3yxk2x/flexible_isearch_without_a_package/
  (defun dotemacs--isearch-fuzzy ()
    (interactive)
    (let ((search-whitespace-regexp ".*?"))
      (call-interactively 'isearch-forward)))
  :init
  (unbind-key "C-s") ; isearch-forward-regexp
  (setq search-highlight t ; highlight incremental search
        isearch-allow-scroll t)
  (use-package isearch+
    :ensure t)
  (use-package isearch-dabbrev
    :ensure t
    :bind (:map isearch-mode-map
                ("<tab>" . isearch-dabbrev-expand)))
  (use-package isearch-symbol-at-point
    :ensure t)
  :diminish isearch-mode
  :bind (("C-f" . isearch-forward-regexp)
         :map isearch-mode-map
         ("C-f" . isearch-repeat-forward)))

(use-package replace
  :defer t
  :config
  (setq query-replace-highlight t) ; Highlight during query
  (use-package replace+
    :ensure t))

(use-package swiper ; Performs poorly if there are a large number of matches
  :ensure t
  :config
  (setq swiper-min-highlight 3 ; Be less noisy
        swiper-use-visual-line t)
  :bind ("C-c s s" . swiper))

(use-package swiper-helm
  :ensure t
  :if (eq dotemacs-selection 'helm)
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
  :bind (("C-c s d" . ag-dired)
         ("C-c s f" . ag-files)))

(use-package helm-ag
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :bind ("C-c s a" . helm-ag)
  :config
  (setq helm-ag-fuzzy-match t
        helm-ag-insert-at-point 'symbol
        helm-ag-source-type 'file-line))

(when (eq dotemacs-selection 'ivy)
  (bind-key "C-c s c" #'counsel-ag))

(use-package find-file-in-project
  :ensure t
  :config (setq ffip-prefer-ido-mode t))

(use-package grep
  :bind
  (("C-c s g" . grep)
   ("C-c s r" . rgrep)
   ("C-c s l" . lgrep))
  :init
  (setq grep-highlight-matches t
        grep-scroll-output t
        grep-find-ignored-files '(".#*" "*~" "*.blg" "*.bbl" "*.elc" "*.lof" "*.idx" "*.lot" "*.toc" "*.aux"
                                  "*.pyc" "*.pyo" "*.pdf"))
  (use-package grep+
    :ensure t)
  (add-to-list 'grep-find-ignored-directories "auto")
  (add-to-list 'grep-find-ignored-directories ".cache")
  (add-to-list 'grep-find-ignored-directories "__pycache__"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-f M-n    swiper thing-at-point    Get the occurrences of the current symbol in the current file.    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'search-init)

;;; search-init.el ends here
