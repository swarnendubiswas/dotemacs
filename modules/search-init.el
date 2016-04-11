;;; search-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-f M-n    swiper thing-at-point    Get the occurrences of the current symbol in the current file.    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq case-fold-search t) ; Make search ignore case

;; Use "C-'" in isearch-mode-map to use avy-isearch to select one of the currently visible isearch candidates.
(use-package isearch
  :commands (isearch-forward isearch-forward-regexp isearch-repeat-forward)
  :preface
  ;; https://www.reddit.com/r/emacs/comments/3yxk2x/flexible_isearch_without_a_package/
  (defun dotemacs--isearch-fuzzy ()
    (interactive)
    (let ((search-whitespace-regexp ".*?"))
      (call-interactively 'isearch-forward)))

  ;; http://endlessparentheses.com/leave-the-cursor-at-start-of-match-after-isearch.html?source=rss
  (defun dotemacs--isearch-exit-other-end ()
    "Exit isearch, at the opposite end of the string."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
  :init
  (unbind-key "C-s") ; isearch-forward-regexp
  (setq search-highlight t ; Highlight incremental search
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
         ("C-f" . isearch-repeat-forward)
         ("C-<return>" . isearch-exit-other-end)))

(use-package replace
  :defer t
  :config
  (setq query-replace-highlight t) ; Highlight during query
  (use-package replace+
    :ensure t))

(use-package swiper ; Performs poorly if there are a large number of matches
  :ensure t
  :bind ("C-c s" . swiper))

(use-package swiper-helm
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :bind ("C-c s h" . swiper-helm))

(use-package color-moccur
  :ensure t
  :disabled t
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
  :disabled t
  :config (loccur-mode 1)
  :diminish loccur-mode)

;; Move between results by pressing n and p
;; Visit the file by pressing <return> or clicking
;; Run the search again by pressing g
;; Close the buffer with q
;; Kill the buffer with k
(use-package ag
  :ensure t
  ;; :bind (("C-c s d" . ag-dired)
  ;;        ("C-c s f" . ag-files))
  :config
  (setq ag-reuse-buffers t
        ag-reuse-window t
        ag-highlight-search t))

(use-package helm-ag
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :bind ("C-c a" . helm-ag)
  :config
  (setq helm-ag-fuzzy-match t
        helm-ag-insert-at-point 'symbol
        helm-ag-source-type 'file-line))

(when (eq dotemacs-selection 'ivy)
  (bind-key "C-c a" #'counsel-ag)
  ;; Shows only the first 200 results, use "C-c C-o" to save all the matches to a buffer.
  (bind-key "C-c g" #'counsel-git-grep))

(use-package find-file-in-project
  :ensure t
  :config (setq ffip-prefer-ido-mode t))

(use-package grep
  ;; :bind
  ;; (("C-c s g" . grep)
  ;;  ("C-c s r" . rgrep)
  ;;  ("C-c s l" . lgrep))
  :init
  (setq grep-highlight-matches t
        grep-scroll-output t
        grep-find-ignored-files '(".#*"
                                  "*~"
                                  "*.aux"
                                  "*.blg"
                                  "*.bbl"
                                  "*.elc"
                                  "*.lof"
                                  "*.idx"
                                  "*.lot"
                                  "*.toc"
                                  "*.pyc"
                                  "*.pyo"
                                  "*.pdf"))
  (use-package grep+
    :ensure t)
  (add-to-list 'grep-find-ignored-directories "auto")
  (add-to-list 'grep-find-ignored-directories ".cache")
  (add-to-list 'grep-find-ignored-directories "__pycache__"))

(provide 'search-init)

;;; search-init.el ends here
