;;; init-misc.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar which-key-use-C-h-commands)

(use-package transient
  :demand t
  :config
  ;; Allow using `q' to quit out of popups, in addition to `C-g'
  (transient-bind-q-to-quit))

;; The built-in `describe-function' includes both functions and macros. `helpful-function' is
;; functions only, so we use `helpful-callable' as a drop-in replacement.
(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-symbol]   . helpful-symbol)
   ([remap describe-key]      . helpful-key)
   ;; ("C-h v" . helpful-variable)
   ;; ("C-h k" . helpful-key)
   ;; ("C-h f" . helpful-callable)
   ("C-h c" . helpful-command)
   ("C-h p" . helpful-at-point)
   ;; ("C-h o" . helpful-symbol)
   :map helpful-mode-map
   ("q"     . helpful-kill-buffers)))

;; Erase all consecutive white space characters in a given direction
(use-package hungry-delete
  :diminish
  :hook
  ((minibuffer-setup-hook . (lambda ()
                              (hungry-delete-mode -1)))
   (after-init-hook . global-hungry-delete-mode)))

(use-package move-text ; Move lines with "M-<up>" and "M-<down>"
  :commands (move-text-up move-text-down move-text-default-bindings)
  :init (move-text-default-bindings))

(use-package duplicate-thing
  :bind ("C-c C-d" . duplicate-thing))

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind
  (("C-h C-m" . discover-my-major)
   ("C-h M-m" . discover-my-mode)))

;; Manage minor-mode on the dedicated interface buffer
(use-package manage-minor-mode
  :commands manage-minor-mode)

(use-package expand-region ; Expand region by semantic units
  :bind
  (("C-="   . er/expand-region)
   ("C-M-=" . er/contract-region)))

(use-package expand-line
  :diminish
  :bind ("M-i" . turn-on-expand-line-mode))

;; Restore point to the initial location with "C-g" after marking a region
(use-package smart-mark
  :hook (after-init-hook . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :commands (whole-line-or-region-local-mode)
  :diminish whole-line-or-region-local-mode
  :hook (after-init-hook . whole-line-or-region-global-mode))

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

;; The real beginning and end of buffers (i.e., `point-min' and `point-max') are accessible by
;; pressing the keys "M-<" and "M->" keys again.
(use-package beginend
  :hook (after-init-hook . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))

;; The package has many bugs, and it has never worked well for me. I am trying out `vundo'.
(use-package undo-tree
  :defines undo-tree-map
  :commands (global-undo-tree-mode undo-tree-redo)
  :diminish
  :disabled t
  :config
  (setq undo-tree-auto-save-history              t
        undo-tree-visualizer-diff                t
        undo-tree-visualizer-relative-timestamps t
        undo-tree-visualizer-timestamps          t)
  (unbind-key "C-/" undo-tree-map)
  :hook (find-file-hook . undo-tree-mode)
  :bind
  (([remap undo] . undo-tree-undo)
   ([remap redo] . undo-tree-redo)
   ("C-z"   . undo-tree-undo)
   ("C-x u" . undo-tree-visualize)))

(use-package vundo
  :if sb/EMACS28+
  :straight (vundo :type git :host github :repo "casouri/vundo")
  :bind
  (([remap undo] . vundo)
   ("C-z" . vundo)
   :map vundo-mode-map
   ("C-a" . vundo-stem-root)
   ("C-e" . vundo-stem-end)
   ;; These are for horizontal movements.
   ("C-f" . vundo-forward)
   ("C-b" . vundo-backward)
   ;; These are for vertical movements.
   ("C-n" . vundo-next)
   ("C-p" . vundo-previous)))

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :bind* ("C-." . iedit-mode))

(use-package hl-todo
  :hook (after-init-hook . global-hl-todo-mode)
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
  :hook ((prog-mode-hook yaml-mode-hook conf-mode-hook
                         css-mode-hook html-mode-hook) . highlight-numbers-mode))

(use-package page-break-lines ; Display ugly "^L" page breaks as tidy horizontal lines
  :diminish
  :commands (page-break-lines-mode)
  :hook (after-init-hook . global-page-break-lines-mode))

;; First mark the word, then add more cursors. Use `mc/edit-lines' to add a cursor to each line in
;; an active region that spans multiple lines.
(use-package multiple-cursors
  :bind
  (("C-<"     . mc/mark-previous-like-this)
   ("C->"     . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package doc-view
  :custom
  (doc-view-continuous t)
  (doc-view-resolution 120))

;; https://emacs.stackexchange.com/questions/19686/how-to-use-pdf-tools-pdf-view-mode-in-emacs
;; Use `isearch', `swiper' will not work
(use-package pdf-tools
  :if (display-graphic-p)
  :defines pdf-annot-activate-created-annotations
  :commands (pdf-tools-install pdf-loader-install pdf-view-mode
                               pdf-annot-delete pdf-annot-add-highlight-markup-annotation
                               pdf-annot-add-text-annotation)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  ;; Register an autoloaded command for `pdf-view-mode', defer loading of `pdf-tools', and run
  ;; `pdf-view-mode' if the beginning of a buffer matches the string "%PDF".
  :magic ("%PDF" . pdf-view-mode)
  :custom
  (pdf-annot-activate-created-annotations t  "Automatically annotate highlights")
  (pdf-view-resize-factor 1.1 "Fine-grained zoom factor of 10%")
  :config
  (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install :no-query)'

  (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable

  ;; We do not enable `pdf-view-themed-minor-mode' since it can change plot colors
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  :bind
  (:map pdf-view-mode-map
        ("j"  . pdf-view-next-line-or-next-page)
        ("k"  . pdf-view-previous-line-or-previous-page)
        ("n"  . pdf-view-next-page-command)
        ("p"  . pdf-view-previous-page-command)
        ("a"  . pdf-view-first-page)
        ("e"  . pdf-view-last-page)
        ("l"  . pdf-view-goto-page)
        ("P"  . pdf-view-fit-page-to-window)
        ("W"  . pdf-view-fit-width-to-window)
        ("H"  . pdf-view-fit-height-to-window)
        ("+"  . pdf-view-enlarge)
        ("-"  . pdf-view-shrink)
        ("r"  . pdf-view-revert-buffer)
        ("d"  . pdf-annot-delete)
        ("h"  . pdf-annot-add-highlight-markup-annotation)
        ("t"  . pdf-annot-add-text-annotation)
        ("M"  . pdf-view-midnight-minor-mode)))

;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.
(use-package saveplace-pdf-view
  :after (pdf-tools saveplace)
  :demand t)

;; ;; LATER: Check for continuous scroll support with `pdf-tools'
;; (use-package image-roll
;;   :straight (image-roll :type git :host github
;;                         :repo "dalanicolai/image-roll.el"))

(use-package logview
  :commands logview-mode)

(use-package wc-mode
  :commands wc-mode)

;; Gets the definition of word or phrase at point from https://wordnik.com/
(use-package define-word
  :commands (define-word define-word-at-point))

(use-package number-separator
  :straight (number-separator :type git :host github
                              :repo "legalnonsense/number-separator.el")
  :commands number-separator-mode
  :custom
  (number-separator ",")
  (number-separator-interval 3)
  (number-separator-ignore-threshold 4)
  (number-separator-decimal-char ".")
  :diminish)

(use-package eldoc
  :straight (:type built-in)
  :if (symbol-value 'sb/IS-LINUX)
  :diminish
  :hook (prog-mode-hook . turn-on-eldoc-mode)
  :config
  ;; The variable-height minibuffer and extra eldoc buffers are distracting. This variable limits
  ;; ElDoc messages to one line. This prevents the echo area from resizing itself unexpectedly when
  ;; point is on a variable with a multiline docstring, which is distracting, but then it cuts of
  ;; useful information.
  ;; (setq eldoc-echo-area-use-multiline-p nil)

  ;; Allow eldoc to trigger after completions
  (with-eval-after-load "company"
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort)))

;; `eldoc-box-hover-at-point-mode' blocks the view because it shows up at point.

;; (use-package eldoc-box
;;   :commands (eldoc-box-hover-at-point-mode)
;;   :hook (eldoc-mode-hook . eldoc-box-hover-mode)
;;   :custom
;;   (eldoc-box-clear-with-C-g t)
;;   (eldoc-box-fringe-use-same-bg nil)
;;   :diminish eldoc-box-hover-mode eldoc-box-hover-at-point-mode)

(use-package esup
  :commands esup
  :if (bound-and-true-p sb/debug-init-file))

(use-package bug-hunter
  :if (bound-and-true-p sb/debug-init-file)
  :commands (bug-hunter-init-file bug-hunter-file))

(use-package explain-pause-mode
  :straight (explain-pause-mode :type git :host github
                                :repo "lastquestion/explain-pause-mode")
  :if (bound-and-true-p sb/debug-init-file)
  :commands (explain-pause-mode explain-pause-top)
  :diminish)

;; `amx-major-mode-commands' limits to commands that are relevant to the current major mode
;; `amx-show-unbound-commands' shows frequently used commands that have no key bindings
(use-package amx
  :commands execute-extended-command-for-buffer
  :hook (after-init-hook . amx-mode)
  :bind
  (("M-x"  . execute-extended-command) ; We need this if we use `vertico' and `consult'
   ("<f1>" . execute-extended-command))
  :custom
  (amx-auto-update-interval 10 "Update the command list every n minutes")
  (amx-history-length 15))

;; This package adds a "C-'" binding to the Ivy minibuffer that uses Avy
(use-package ivy-avy
  :after ivy
  :bind
  (:map ivy-minibuffer-map
        ("C-'"   . ivy-avy)))

(use-package bm
  :commands (bm-buffer-save-all bm-repository-save bm-toggle bm-next bm-previous
                                bm-repository-load bm-buffer-save bm-buffer-restore)
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
    (add-hook 'after-init-hook        #'bm-repository-load))
  :init
  ;; Must be set before `bm' is loaded
  (setq bm-restore-repository-on-load t
        bm-verbosity-level 1
        bm-modeline-display-total t)
  :hook (after-init-hook . sb/bm-setup)
  :config
  (setq-default bm-buffer-persistence t) ; Save bookmarks
  :bind
  (("C-<f1>" . bm-toggle)
   ("C-<f2>" . bm-next)
   ("C-<f3>" . bm-previous)))

(use-package crux
  :bind
  (("C-c d i" . crux-ispell-word-then-abbrev)
   ("<f12>"   . crux-kill-other-buffers)
   ("C-c d s" . crux-sudo-edit)
   ("C-a"     . crux-move-beginning-of-line)))

;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(setenv "SHELL" shell-file-name) ; Recommended to connect with Bash

;; `vterm' provides better performance than `eshell', `shell', and `(ansi-)term'. The advantage of
;; the later modules are they are built-in to Emacs. The package requires shell-side configuration.
;; Check https://github.com/akermu/emacs-libvterm.
(use-package vterm
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  (vterm-term-environment-variable "xterm-24bit")
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
              (buffer-face-mode t))))

(use-package vterm-toggle
  :bind ("C-`" . vterm-toggle))

(use-package rainbow-mode
  :hook ((css-mode-hook html-mode-hook web-mode-hook help-mode-hook) . rainbow-mode))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :hook (after-init-hook . volatile-highlights-mode))

;; Use Emacsclient as the $EDITOR of child processes
(use-package with-editor
  :hook (after-init-hook . shell-command-with-editor-mode))

(use-package unfill
  :commands (unfill-region unfill-paragraph unfill-toggle))

;; Better looking info pages
(use-package info-colors
  :hook (Info-selection-hook . info-colors-fontify-node))

;; Highlight and allow to open http links in strings and comments in buffers.
(use-package goto-addr
  :hook ((prog-mode-hook . goto-address-prog-mode)
         (text-mode-hook . goto-address-mode)))

(use-package xclip
  :init (xclip-mode 1))

(use-package fix-word
  :bind
  (("M-u" . fix-word-upcase)
   ("M-l" . fix-word-downcase)
   ("M-c" . fix-word-capitalize)))

(use-package string-inflection
  :bind
  (:map prog-mode-map
        ("C-c C-u" . string-inflection-all-cycle)))

(provide 'init-misc)

;;; init-misc.el ends here
