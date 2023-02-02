;;; init-whitespace.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:
;;; utf-8; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(setq delete-trailing-lines t)

;; "M-x delete-trailing-whitespace" deletes trailing lines. This is different from
;; `whitespace-cleanup-mode' since this is unconditional

(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (add-hook 'write-file-functions #'delete-trailing-whitespace)
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(use-package whitespace
  :commands
  (global-whitespace-mode whitespace-buffer
                          whitespace-cleanup
                          whitespace-turn-off)
  :hook
  (markdown-mode-hook . (lambda ()
                          (setq show-trailing-whitespace t
                                whitespace-style
                                '(face ; Visualize using faces
                                  ;; tabs
                                  ;; spaces
                                  trailing ; Trailing whitespace
                                  ;; newline
                                  ;; tab-mark ; Mark any tabs
                                  ;; empty ; Empty lines at beginning or end of buffer
                                  ;; lines ; Lines that extend beyond `whitespace-line-column'
                                  ;; Wrong kind of indentation (tab when spaces and vice versa)
                                  ;; indentation
                                  ;; space-mark
                                  ;; space-before-tab ; Mixture of space and tab on the same line
                                  ;; space-after-tab ; Mixture of space and tab on the same line
                                  ;; empty
                                  ;; newline-mark
                                  missing-newline-at-eof))
                          (whitespace-mode 1)))
  :custom
  (whitespace-line-column sb/fill-column)
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode))

;; Call `whitespace-cleanup' only if the initial buffer was clean. This mode works on the entire
;; file unlike `ws-butler'. To enable the mode for an entire project, set `whitespace-cleanup-mode'
;; to `t' in the `.dir-locals.el' file.

(use-package whitespace-cleanup-mode
  :defines whitespace-cleanup-mode-ignore-modes
  :commands
  (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  :custom
  (whitespace-cleanup-mode-preserve-point t)
  (whitespace-cleanup-mode-only-if-initially-clean t)
  :config
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)
  :diminish)

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :hook
  (prog-mode-hook . ws-butler-mode)
  :diminish)

(provide 'init-whitespace)

;;; init-whitespace.el ends here
