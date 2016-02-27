;;; search-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

(setq case-fold-search t) ; Make search ignore case

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
;; (bind-key "C-f" #'isearch-forward-regexp)
;; (bind-key "C-f" #'isearch-repeat-forward isearch-mode-map)

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
  :bind (("C-c s s" . swiper)
         ("C-f" . swiper)
         ("C-r" . swiper)))

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
    :bind ("C-c s a" . helm-ag)
    :config
    (setq helm-ag-fuzzy-match t
          helm-ag-insert-at-point 'symbol
          helm-ag-source-type 'file-line))
  :bind (("C-c s d" . ag-dired)
         ("C-c s f" . ag-files)))

(use-package find-file-in-project
  :ensure t
  :config (setq ffip-prefer-ido-mode t))

(use-package grep
  :bind
  (("C-c s g" . grep)
   ("C-c s r" . rgrep)
   ("C-c s l" . lgrep))
  :config
  (setq grep-highlight-matches t
        grep-scroll-output t)
  (use-package grep+
    :ensure t)
  (add-to-list 'grep-find-ignored-directories ".cache")
  (add-to-list 'grep-find-ignored-directories "__pycache__"))

(use-package helm-grep
  :init
  ;; http://stackoverflow.com/questions/28316688/how-to-bind-helm-do-grep-1-to-a-key-in-emacs
  (global-set-key [f12]
                  (lambda ()
                    (interactive)
                    (let ((current-prefix-arg 't))
                      (call-interactively 'helm-do-grep)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-f M-n    swiper thing-at-point    Get the occurrences of the current symbol in the current file.    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'search-init)

;;; search-init.el ends here
