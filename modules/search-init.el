;;; search-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure search and replace.

;;; Code:

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
  (unbind-key "C-s" isearch-mode-map) ; isearch-repeat-forward
  (setq search-highlight t ; Highlight incremental search
        isearch-allow-scroll t)
  :config
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

(use-package replace+
  :ensure t
  :after replace)

(use-package swiper ; Performs poorly if there are a large number of matches
  :ensure t
  :preface
  ;; For certain files with long lines, the results in the swiper buffer is truncated to the right. These wrapper
  ;; methods are to get around that problem.
  (defun dotemacs-swiper-with-visual-line-mode ()
    "Long lines are truncated at the right without visual line"
    (interactive)
    (visual-line-mode 1)
    (swiper)
    (visual-line-mode -1))

  (defun dotemacs-swiper-all-with-visual-line-mode ()
    "Long lines are truncated at the right without visual line"
    (interactive)
    (visual-line-mode 1)
    (swiper-all)
    (visual-line-mode -1))
  :bind (("C-c s" . dotemacs-swiper-with-visual-line-mode)
         ("C-c S" . dotemacs-swiper-all-with-visual-line-mode)
         ("C-c r" . ivy-resume))
  :config
  (setq swiper-use-visual-line t
        swiper-action-recenter t)
  (when (not (eq dotemacs-selection 'ivy))
    (setq ivy-height 20)))

(use-package swiper-helm
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :bind ("<f4>" . swiper-helm))

;; Move between results by pressing n and p
;; Visit the file by pressing <return> or clicking
;; Run the search again by pressing g
;; Close the buffer with q
;; Kill the buffer with k
;; C-h m inside a results buffer will show all the keybindings available to you.
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
  (bind-key "C-c g" #'counsel-git-grep)
  (setq counsel-grep-swiper-limit 1000000) ; Number of characters in the buffer
  (bind-key "C-c o" #'counsel-grep-or-swiper)
  (bind-key "<f4>" #'counsel-grep-or-swiper))

;; Edit the *ag* buffer with wgrep-change-to-wgrep-mode
(use-package wgrep
  :ensure t)

;; Uses ivy by default for completing reads, if ivy is installed.
(use-package find-file-in-project
  :ensure t
  :disabled t
  :config (setq ffip-prefer-ido-mode t))

(use-package grep
  :disabled t
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

(use-package swoop
  :ensure t
  :config (setq swoop-use-target-magnifier t
                swoop-use-target-magnifier-size 1.2))

;; "C-c C-e" to go into edit mode
(use-package helm-swoop
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :bind
  (("C-c h s" . helm-swoop)
   ("C-c h /" . helm-multi-swoop))
  :config
  (setq helm-multi-swoop-edit-save t ; Save buffer when helm-multi-swoop-edit complete
        helm-swoop-speed-or-color nil
        helm-swoop-split-direction #'split-window-vertically
        helm-swoop-split-with-multiple-windows nil
        helm-swoop-move-to-line-cycle t ; go to the opposite side of line from the end or beginning of line
        helm-swoop-use-line-number-face t))


(provide 'search-init)

;;; search-init.el ends here
