;;; init-whitespace.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(setq delete-trailing-lines t)

;; (use-package whitespace
;;   :commands (global-whitespace-mode whitespace-buffer
;;                                     whitespace-cleanup whitespace-turn-off)
;;   :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
;;   :hook
;;   (markdown-mode-hook
;;    . (lambda ()
;;        (setq show-trailing-whitespace t
;;              whitespace-style
;;              '(face ; Visualize using faces
;;                ;; tabs
;;                ;; spaces
;;                trailing ; Trailing whitespace
;;                ;; newline
;;                ;; tab-mark ; Mark any tabs
;;                ;; empty ; Empty lines at beginning or end of buffer
;;                ;; lines ; Lines that extend beyond `whitespace-line-column'
;;                ;; indentation ; Wrong kind of indentation (tab when spaces and vice versa)
;;                ;; space-mark
;;                ;; space-before-tab ; Mixture of space and tab on the same line
;;                ;; space-after-tab ; Mixture of space and tab on the same line
;;                ;; empty
;;                ;; newline-mark
;;                missing-newline-at-eof))
;;        (whitespace-mode 1)))
;;   :custom
;;   (whitespace-line-column sb/fill-column))

;; Call `whitespace-cleanup' only if the initial buffer was clean. This mode works on the entire
;; file unlike `ws-butler'. To enable the mode for an entire project, set `whitespace-cleanup-mode'
;; to `t' in the `.dir-locals.el' file.
(use-package whitespace-cleanup-mode
  :defines whitespace-cleanup-mode-ignore-modes
  :diminish
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  :config
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)
  :custom
  (whitespace-cleanup-mode-preserve-point t)
  (whitespace-cleanup-mode-only-if-initially-clean t))

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :diminish
  :hook (prog-mode-hook . ws-butler-mode))

;; "M-x delete-trailing-whitespace" deletes trailing lines. This is different from
;; `whitespace-cleanup-mode' since this is unconditional
(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(provide 'init-whitespace)

;;; init-whitespace.el ends here
