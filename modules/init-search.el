;;; init-search.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; Use "C-'" in `isearch-mode-map' to use `avy-isearch' to select one of the currently visible
;; `isearch' candidates.
(use-package isearch
  :straight nil
  :commands (isearch-forward-regexp isearch-repeat-forward isearch-occur)
  :custom
  (search-highlight t "Highlight incremental search")
  (isearch-lazy-highlight t)
  (isearch-lazy-count t)
  :bind
  ;; Change the bindings for `isearch-forward-regexp' and `isearch-repeat-forward'
  (("C-s"     . nil)
   ("C-M-f"   . nil) ; Was bound to `isearch-forward-regexp', but we use it for `sp-forward-sexp'
   ("C-f"     . isearch-forward-regexp)
   :map isearch-mode-map
   ("C-s"     . nil)
   ("C-f"     . isearch-repeat-forward)
   ("C-c C-o" . isearch-occur)))

(use-package isearch-symbol-at-point ; Auto populate `isearch' with the symbol at point
  :straight t
  :after isearch
  :commands (isearch-forward-symbol ; "M-s _"
             isearch-symbol-at-point
             isearch-forward-symbol-at-point ; "M-s ."
             isearch-backward-symbol-at-point))

(use-package anzu
  :straight t
  :diminish anzu-mode
  :commands global-anzu-mode
  :init
  (setq anzu-search-threshold     10000
        anzu-minimum-input-length 2)
  (global-anzu-mode 1)
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)))

(use-package swiper
  :straight t
  :if (eq sb/minibuffer-completion 'ivy)
  :commands (swiper swiper-isearch)
  :custom (swiper-action-recenter t))

(progn
  (with-eval-after-load "grep"
    (defvar grep-highlight-matches)
    (defvar grep-scroll-output)
    (defvar grep-find-ignored-directories)

    (setq grep-command           "grep -irHn "
          grep-highlight-matches t
          grep-scroll-output     t)

    (dolist (dirs '(".cache" "node_modules" "vendor" ".clangd"))
      (add-to-list 'grep-find-ignored-directories dirs))))

;; When the "*grep*" buffer is huge, `wgrep-change-to-wgrep-mode' might freeze Emacs for several
;; minutes.
(use-package wgrep ; Writable grep
  :straight t
  :bind
  (:map grep-mode-map ; These keybindings are also defined in `wgrep-mode-map'
        ("C-x C-p" . wgrep-change-to-wgrep-mode)
        ("C-x C-s" . wgrep-finish-edit)
        ("C-x C-k" . wgrep-abort-changes)
        ("C-x C-q" . wgrep-exit))
  :custom (wgrep-auto-save-buffer t))

;; Use "S" to change the search term, "D" to change the search directory, "g" to rerun the search,
;; and "o" to view the result in another window.
(use-package deadgrep
  :straight t
  :bind ("C-c s d" . deadgrep))

(provide 'init-search)

;;; init-search.el ends here
