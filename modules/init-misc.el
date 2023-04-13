;;; init-misc.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar which-key-use-C-h-commands)

(use-package transient
  :commands transient-bind-q-to-quit
  :custom
  (transient-semantic-coloring t)
  :config
  ;; Allow using `q' to quit out of popups, in addition to `C-g'
  (transient-bind-q-to-quit))

;; The built-in `describe-function' includes both functions and macros. `helpful-function' is
;; functions only, so we use `helpful-callable' as a drop-in replacement.
(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable) ; "C-h f"
   ([remap describe-variable] . helpful-variable) ; "C-h v"
   ([remap describe-symbol]   . helpful-symbol) ; "C-h o"
   ([remap describe-key]      . helpful-key) ; "C-h k"
   ("C-h c"                   . helpful-command)
   ("C-h p"                   . helpful-at-point)
   :map helpful-mode-map
   ("q"                       . helpful-kill-buffers)))

;; Erase all consecutive white space characters in a given direction
(use-package hungry-delete
  :hook
  ((minibuffer-setup-hook . (lambda ()
                              (hungry-delete-mode -1)))
   (emacs-startup-hook . global-hungry-delete-mode))
  :diminish)

(use-package move-text ; Move lines with "M-<up>" and "M-<down>"
  :bind
  (("M-<down>" . move-text-down)
   ("M-<up>"   . move-text-up)))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind
  (("C-h C-m" . discover-my-major)
   ("C-h M-m" . discover-my-mode)))

(use-package expand-region ; Expand region by semantic units
  :bind
  (("C-="   . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package expand-line
  :bind
  ("M-i" . turn-on-expand-line-mode)
  :diminish)

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  :hook
  (emacs-startup-hook . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :commands
  (whole-line-or-region-local-mode)
  :hook
  (emacs-startup-hook . whole-line-or-region-global-mode)
  :diminish whole-line-or-region-local-mode)

(use-package goto-last-change
  :bind
  ("C-x C-\\" . goto-last-change))

;; The real beginning and end of buffers (i.e., `point-min' and `point-max') are accessible by
;; pressing the keys "M-<" and "M->" keys again.
(use-package beginend
  :hook
  (emacs-startup-hook . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))

(use-package vundo
  :straight (:host github :repo "casouri/vundo")
  :if sb/EMACS28+
  :bind
  (([remap undo] . vundo)
   ("C-z"        . vundo)
   :map vundo-mode-map
   ("C-a"        . vundo-stem-root)
   ("C-e"        . vundo-stem-end)
   ;; These are for horizontal movements.
   ("C-f"        . vundo-forward)
   ("C-b"        . vundo-backward)
   ;; These are for vertical movements.
   ("C-n"        . vundo-next)
   ("C-p"        . vundo-previous)))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :bind*
  ("C-." . iedit-mode))

(use-package hl-todo
  :hook
  (emacs-startup-hook . global-hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces (append '(("LATER"    . "#d0bf8f")
                                        ("ISSUE"    . "#ff8c00")
                                        ("DEBUG"    . "#ff8c00")
                                        ("TEST"     . "tomato")
                                        ("WARNING"  . "#cc0000")
                                        ("BEWARE"   . "#aa0000")
                                        ("REFACTOR" . "#cc9393"))
                                      hl-todo-keyword-faces)))

(use-package highlight-numbers
  :hook
  ((prog-mode-hook yaml-mode-hook conf-mode-hook css-mode-hook
                   html-mode-hook) . highlight-numbers-mode))

(use-package page-break-lines ; Display ugly "^L" page breaks as tidy horizontal lines
  :commands
  (page-break-lines-mode)
  :hook
  (emacs-startup-hook . global-page-break-lines-mode)
  :diminish)

;; ;; https://emacs.stackexchange.com/questions/19686/how-to-use-pdf-tools-pdf-view-mode-in-emacs
;; ;; Use `isearch', `swiper' will not work
;; (use-package pdf-tools
;;   :if (display-graphic-p)
;;   :defines pdf-annot-activate-created-annotations
;;   :commands
;;   (pdf-tools-install pdf-loader-install pdf-view-mode
;;                      pdf-annot-delete
;;                      pdf-annot-add-highlight-markup-annotation
;;                      pdf-annot-add-text-annotation)
;;   :mode ("\\.pdf\\'" . pdf-view-mode)
;;   ;; Register an autoloaded command for `pdf-view-mode', defer loading of `pdf-tools', and run
;;   ;; `pdf-view-mode' if the beginning of a buffer matches the string "%PDF".
;;   :magic ("%PDF" . pdf-view-mode)
;;   :bind
;;   (:map pdf-view-mode-map
;;         ("j" . pdf-view-next-line-or-next-page)
;;         ("k" . pdf-view-previous-line-or-previous-page)
;;         ("n" . pdf-view-next-page-command)
;;         ("p" . pdf-view-previous-page-command)
;;         ("a" . pdf-view-first-page)
;;         ("e" . pdf-view-last-page)
;;         ("l" . pdf-view-goto-page)
;;         ("P" . pdf-view-fit-page-to-window)
;;         ("W" . pdf-view-fit-width-to-window)
;;         ("H" . pdf-view-fit-height-to-window)
;;         ("+" . pdf-view-enlarge)
;;         ("-" . pdf-view-shrink)
;;         ("r" . pdf-view-revert-buffer)
;;         ("d" . pdf-annot-delete)
;;         ("h" . pdf-annot-add-highlight-markup-annotation)
;;         ("t" . pdf-annot-add-text-annotation)
;;         ("M" . pdf-view-midnight-minor-mode))
;;   :custom
;;   (pdf-annot-activate-created-annotations t  "Automatically annotate highlights")
;;   (pdf-view-resize-factor 1.1 "Fine-grained zoom factor of 10%")
;;   :config
;;   (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install :no-query)'

;;   (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable

;;   ;; We do not enable `pdf-view-themed-minor-mode' since it can change plot colors
;;   (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes))

;; ;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.
;; (use-package saveplace-pdf-view
;;   :after (pdf-tools saveplace)
;;   :demand t)

(use-package wc-mode
  :commands
  (wc-mode))

;; Gets the definition of word or phrase at point from https://wordnik.com/
(use-package define-word
  :commands
  (define-word define-word-at-point))

(use-package esup
  :if (bound-and-true-p sb/debug-init-file)
  :commands
  (esup))

(use-package bug-hunter
  :commands
  (bug-hunter-init-file bug-hunter-file))

;; `amx-major-mode-commands' limits to commands that are relevant to the current major mode,
;; `amx-show-unbound-commands' shows frequently used commands that have no key bindings.

(use-package amx
  :after counsel
  :commands
  (execute-extended-command-for-buffer)
  :hook
  (emacs-startup-hook . amx-mode)
  :bind
  (("M-x"  . amx)
   ("<f1>" . amx)))

;; Save a bookmark with `bookmark-set' ("C-x r m"). To revisit that bookmark, use `bookmark-jump'
;; ("C-x r b") or `bookmark-bmenu-list' ("C-x r l"). Rename the bookmarked location in
;; `bookmark-bmenu-mode' with `R'.

(use-package bm
  :preface
  (defun sb/bm-setup ()
    "Wrapper function to help call with a timer."
    ;; `kill-buffer-hook' is not called when Emacs is killed
    (add-hook 'kill-emacs-hook (lambda ()
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
    (add-hook 'after-save-hook        #'bm-buffer-save)
    (add-hook 'kill-buffer-hook       #'bm-buffer-save)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
    (add-hook 'after-revert-hook      #'bm-buffer-restore)
    (add-hook 'find-file-hook         #'bm-buffer-restore)
    (add-hook 'emacs-startup-hook     #'bm-repository-load))
  :commands
  (bm-buffer-save-all bm-repository-save)
  :init
  ;; Must be set before `bm' is loaded
  (setq bm-restore-repository-on-load t
        bm-verbosity-level 1
        bm-modeline-display-total t)
  :hook
  ((kill-emacs-hook . (lambda ()
                        (bm-buffer-save-all)
                        (bm-repository-save)))
   (after-save-hook . bm-buffer-save)
   (kill-buffer-hook . bm-buffer-save)
   (vc-before-checkin-hook . bm-buffer-save)
   (after-revert-hook . bm-buffer-restore)
   (find-file-hook . bm-buffer-restore)
   (emacs-startup-hook . bm-repository-load))
  :bind
  (("C-<f1>" . bm-toggle)
   ("C-<f2>" . bm-next)
   ("C-<f3>" . bm-previous))
  :config
  ;; Save bookmarks
  (setq-default bm-buffer-persistence t))

(use-package crux
  :bind
  (("C-c d i" . crux-ispell-word-then-abbrev)
   ("<f12>"   . crux-kill-other-buffers)
   ("C-c d s" . crux-sudo-edit))
  :bind* ("C-c C-d" . crux-duplicate-current-line-or-region))

(use-package rainbow-mode
  :hook
  ((css-mode-hook html-mode-hook web-mode-hook help-mode-hook) . rainbow-mode)
  :diminish)

(use-package volatile-highlights
  :hook
  (emacs-startup-hook . volatile-highlights-mode)
  :diminish volatile-highlights-mode)

(use-package unfill
  :commands
  (unfill-region unfill-paragraph unfill-toggle))

(use-package info-colors ; Better looking info pages
  :hook
  (Info-selection-hook . info-colors-fontify-node))

(use-package xclip
  :if (or (executable-find "xclip") (executable-find "xsel"))
  :hook (emacs-startup-hook . xclip-mode))

(use-package fix-word
  :bind
  (("M-u" . fix-word-upcase)
   ("M-l" . fix-word-downcase)
   ("M-c" . fix-word-capitalize)))

(use-package string-inflection
  :bind
  (:map prog-mode-map
        ("C-c C-u" . string-inflection-all-cycle)))

(use-package gcmh ; Allow GC to happen after a period of idle time
  :commands gcmh-idle-garbage-collect
  :hook
  (emacs-startup-hook . gcmh-mode)
  :diminish)

(use-package kill-file-path
  :straight (:host github :repo "chyla/kill-file-path")
  :commands
  (kill-file-path-basename kill-file-path-basename-without-extension kill-file-path-dirname
                           kill-file-path))

(use-package change-inner
  :commands (change-inner change-outer yank-inner yank-outer))

(provide 'init-misc)

;;; init-misc.el ends here
