;;; init-search.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)

;; Use "C-'" in `isearch-mode-map' to use `avy-isearch' to select one of the currently visible
;; `isearch' candidates.
(use-package isearch
  :straight (:type built-in)
  :commands
  (isearch-forward-regexp isearch-repeat-forward isearch-occur)
  :bind
  ;; Change the bindings for `isearch-forward-regexp' and `isearch-repeat-forward'
  (("C-s"     . nil)
   ("C-M-f"   . nil) ; Was bound to `isearch-forward-regexp', but we use it for `sp-forward-sexp'
   ("C-f"     . isearch-forward-regexp)
   ("C-r"     . isearch-backward-regexp)
   :map isearch-mode-map
   ("C-s"     . nil)
   ("C-f"     . isearch-repeat-forward)
   ("C-c C-o" . isearch-occur)
   ("C-'" . avy-isearch))
  :custom
  (search-highlight t "Highlight incremental search")
  (isearch-lazy-highlight t)
  (isearch-lazy-count t))

(use-package isearch-symbol-at-point ; Auto populate `isearch' with the symbol at point
  :after isearch
  :commands
  (isearch-forward-symbol ; "M-s _"
   isearch-forward-symbol-at-point ; "M-s ."
   isearch-backward-symbol-at-point)
  :bind
  ("C-c s p" . isearch-symbol-at-point))

(use-package anzu
  :commands
  (global-anzu-mode)
  :init
  (setq anzu-search-threshold     10000
        anzu-minimum-input-length 2)
  (global-anzu-mode 1)
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :diminish anzu-mode)

(use-package swiper
  :if (eq sb/minibuffer-completion 'ivy)
  :commands
  (swiper swiper-isearch)
  :custom
  (swiper-action-recenter t))

(with-eval-after-load "grep"
  (defvar grep-highlight-matches)
  (defvar grep-scroll-output)
  (defvar grep-find-ignored-directories)

  (setq grep-command           "grep -irHn "
        grep-highlight-matches t
        grep-scroll-output     t)

  (dolist (dirs '(".cache" "node_modules" "vendor" ".clangd"))
    (add-to-list 'grep-find-ignored-directories dirs)))

;; When the "*grep*" buffer is huge, `wgrep-change-to-wgrep-mode' might freeze Emacs for several
;; minutes.
(use-package wgrep ; Writable grep
  :bind
  (:map grep-mode-map ; These keybindings are also defined in `wgrep-mode-map'
        ("C-x C-p" . wgrep-change-to-wgrep-mode)
        ("C-x C-s" . wgrep-finish-edit)
        ("C-x C-k" . wgrep-abort-changes)
        ("C-x C-q" . wgrep-exit))
  :custom
  (wgrep-auto-save-buffer t))

;; Use "S" to change the search term, "D" to change the search directory, "g" to rerun the search,
;; "o" to view the result in another window, "n" and "p" to move between results buttons, "M-n" and
;; "M-p" to move between file headers, and "C-c C-k" to stop a running search.
(use-package deadgrep
  :bind
  ("C-c s d" . deadgrep))

;; (use-package affe
;;   :if (eq sb/minibuffer-completion 'vertico)
;;   :commands (affe-grep affe-find consult--customize-put)
;;   :config
;;   (consult-customize affe-grep :preview-key (kbd "M-.")))

;; `avy-setup-default' will bind `avy-isearch' to `C-'' in `isearch-mode-map', so that you can
;; select one of the currently visible `isearch' candidates using `avy'.
(use-package avy
  :commands avy-setup-default
  :bind
  (("M-b"   . avy-goto-word-1)
   ("C-'"   . avy-goto-char-timer)
   ("C-/"   . avy-goto-line))
  :custom
  ;; Option "pre" is distracting because of all the movement while highlighting selection keys. This
  ;; causes the eyes to lose focus.
  (avy-style 'de-bruijn)
  (avy-background t))

(progn
  (defvar reb-re-syntax)

  (setq reb-re-syntax 'string))

;; Package `visual-regexp' provides an alternate version of `query-replace' which highlights matches
;; and replacements as you type.
(use-package visual-regexp
  :commands
  (vr/replace vr/mark)
  :bind
  ([remap query-replace] . vr/query-replace))

(provide 'init-search)

;;; init-search.el ends here
