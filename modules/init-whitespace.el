;;; init-whitespace.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(when nil
  (progn
    (declare-function whitespace-buffer "whitespace")
    (declare-function whitespace-turn-off "whitespace")

    (unless (fboundp 'whitespace-mode)
      (autoload #'whitespace-mode "whitespace" nil t))
    (unless (fboundp 'global-whitespace-mode)
      (autoload #'global-whitespace-mode "whitespace" nil t))
    (unless (fboundp 'whitespace-buffer)
      (autoload #'whitespace-buffer "whitespace" nil t))
    (unless (fboundp 'whitespace-cleanup)
      (autoload #'whitespace-cleanup "whitespace" nil t))
    (unless (fboundp 'whitespace-turn-off)
      (autoload #'whitespace-turn-off "whitespace" nil t))

    (add-hook 'markdown-mode-hook #'whitespace-mode)

    (with-eval-after-load "whitespace"
      (defvar whitespace-line-column)
      (defvar whitespace-style)

      (setq show-trailing-whitespace t
            whitespace-line-column sb/fill-column
            whitespace-style '(face ; Visualize using faces
                               lines-tail
                               trailing ; Trailing whitespace
                               ;; tab-mark ; Mark any tabs
                               ;; empty ; Empty lines at beginning or end of buffer
                               ;; lines ; Lines that extend beyond `whitespace-line-column'
                               ;; indentation ; Wrong indentation (tab when spaces and vice versa)
                               ;; space-before-tab ; Mixture of space and tab on the same line
                               ;; space-after-tab ; Mixture of space and tab on the same line
                               ))

      (diminish 'global-whitespace-mode)
      (diminish 'whitespace-mode)
      (diminish 'whitespace-newline-mode))))

;; Call `whitespace-cleanup' only if the initial buffer was clean. This mode works on the entire
;; file unlike `ws-butler'. To enable the mode for an entire project, set `whitespace-cleanup-mode'
;; to `t' in the `.dir-locals.el' file.
(use-package whitespace-cleanup-mode
  :defines whitespace-cleanup-mode-ignore-modes
  :diminish
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  ;; :hook (prog-mode-hook . whitespace-cleanup-mode)
  :config
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)
  :custom
  (whitespace-cleanup-mode-preserve-point t)
  (whitespace-cleanup-mode-only-if-initially-clean t))

;; Unobtrusively trim extraneous white-space *ONLY* in lines edited
(use-package ws-butler
  :commands ws-butler-mode
  :diminish
  :hook (prog-mode-hook . ws-butler-mode))

;; This is different from `whitespace-cleanup-mode' since this is unconditional
(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (setq delete-trailing-lines t) ; "M-x delete-trailing-whitespace" deletes trailing lines
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

(provide 'init-whitespace)

;;; init-whitespace.el ends here
