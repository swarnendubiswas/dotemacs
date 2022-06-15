;;; init-misc.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar which-key-use-C-h-commands)

(use-package transient
  :demand t)

;; The built-in `describe-function' includes both functions and macros. `helpful-function' is
;; functions only, so we use `helpful-callable' as a drop-in replacement.
(use-package helpful
  :bind
  (("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h f" . helpful-callable)
   ("C-h c" . helpful-command)
   ("C-h p" . helpful-at-point)
   ("C-h o" . helpful-symbol)
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
  :magic ("%PDF" . pdf-view-mode)
  :hook (after-init-hook . (lambda ()
                             (require 'pdf-tools nil t)))
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
        ("C-s" . isearch-forward)
        ("C-S-s" . isearch-backward)
        ("="  . pdf-view-fit-page-to-window)
        ("r"  . pdf-view-revert-buffer)
        ("d"   . pdf-annot-delete)
        ("h"   . pdf-annot-add-highlight-markup-annotation)
        ("t"   . pdf-annot-add-text-annotation)
        ("M"   . pdf-view-midnight-minor-mode)))

;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.
(use-package saveplace-pdf-view
  :after (pdf-tools saveplace)
  :demand t)

;; LATER: Check for continuous scroll support with `pdf-tools'
(use-package image-roll
  :straight (image-roll :type git :host github
                        :repo "dalanicolai/image-roll.el"))

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
(use-package eldoc-box
  :commands (eldoc-box-hover-at-point-mode)
  :hook (eldoc-mode-hook . eldoc-box-hover-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-fringe-use-same-bg nil)
  :diminish eldoc-box-hover-mode eldoc-box-hover-at-point-mode)

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

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `after-save-hook' and leads to auto-formatters being invoked more frequently.
(use-package super-save
  :defines (super-save-remote-files super-save-triggers super-save-hook-triggers)
  :diminish
  :hook (after-init-hook . super-save-mode)
  :custom
  (super-save-remote-files nil "Ignore remote files, can cause Emacs to hang")
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

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

(progn
  (defvar reb-re-syntax)

  (setq reb-re-syntax 'string))

(use-package visual-regexp
  :commands (vr/replace vr/mark)
  :bind ([remap query-replace] . vr/query-replace))

(use-package rainbow-mode
  :hook ((css-mode-hook html-mode-hook web-mode-hook help-mode-hook) . rainbow-mode))

;; Provide context-dependent actions similar to a content menu
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :after vertico
  :defines vertico-map
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command
        which-key-use-C-h-commands nil)
  :bind
  (([remap describe-bindings] . embark-bindings)
   :map vertico-map
   ("C-l" . embark-act)
   ("C-," . embark-dwim)
   ("C-c C-l" . embark-export)))

(use-package embark-consult
  :after (embark consult))

;; Enriches the completion display with annotations, e.g., documentation strings or file information
(use-package marginalia
  :after vertico
  :init (marginalia-mode 1)
  :config
  ;; Add project-buffer annotator.
  (add-to-list 'marginalia-annotator-registry
               '(project-buffer marginalia-annotate-project-buffer))
  (with-eval-after-load "projectile"
    (add-to-list 'marginalia-command-categories
                 '(projectile-switch-project . file))
    (add-to-list 'marginalia-command-categories
                 '(projectile-switch-open-project . file))
    (add-to-list 'marginalia-command-categories
                 '(projectile-find-file . project-file))
    (add-to-list 'marginalia-command-categories
                 '(projectile-recentf . project-file))
    (add-to-list 'marginalia-command-categories
                 '(projectile-display-buffer . project-buffer))
    (add-to-list 'marginalia-command-categories
                 '(projectile-switch-to-buffer . project-buffer))))

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
  :unless (display-graphic-p)
  :init (xclip-mode 1))

(provide 'init-misc)

;;; init-misc.el ends here
