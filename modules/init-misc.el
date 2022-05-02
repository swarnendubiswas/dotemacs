;;; init-misc.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)

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
  :commands (hungry-delete-mode global-hungry-delete-mode)
  :diminish
  :hook
  ((minibuffer-setup-hook . (lambda ()
                              (hungry-delete-mode -1)))
   (after-init-hook . global-hungry-delete-mode)))

(use-package move-text ; Move lines with "M-<up>" and "M-<down>"
  :commands (move-text-up move-text-down move-text-default-bindings)
  :init (move-text-default-bindings))

(use-package duplicate-thing
  :bind* ("C-c C-d" . duplicate-thing))

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
  ;; :init (run-with-idle-timer 3 nil #'smart-mark-mode)
  :hook (after-init-hook . smart-mark-mode))

;; Operate on the current line if no region is active
(use-package whole-line-or-region
  :commands (whole-line-or-region-local-mode whole-line-or-region-global-mode)
  :diminish (whole-line-or-region-local-mode)
  ;; :init (run-with-idle-timer 3 nil #'whole-line-or-region-global-mode)
  :hook (after-init-hook . whole-line-or-region-global-mode))

(use-package goto-last-change
  :bind ("C-x C-\\" . goto-last-change))

;; The real beginning and end of buffers (i.e., `point-min' and `point-max') are accessible by
;; pressing the keys "M-<" and "M->" keys again.
(use-package beginend
  ;; :init (run-with-idle-timer 3 nil #'beginend-global-mode)
  :hook (after-init-hook . beginend-global-mode)
  :config
  (dolist (mode (cons 'beginend-global-mode (mapcar #'cdr beginend-modes)))
    (diminish mode)))

(use-package undo-tree
  :defines undo-tree-map
  :commands (global-undo-tree-mode undo-tree-redo)
  :diminish
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

(use-package iedit ; Edit multiple regions in the same way simultaneously
  :bind* ("C-." . iedit-mode))

;; Avoid the "Overwrite old session file (not loaded)?" warning by loading the `session' package
(use-package session
  :disabled t
  :commands (session-initialize)
  :hook (after-init-hook . session-initialize))

(use-package hl-todo
  :commands global-hl-todo-mode
  ;; :init (run-with-idle-timer 3 nil #'global-hl-todo-mode)
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
  :commands highlight-numbers-mode
  :hook ((prog-mode-hook yaml-mode-hook conf-mode-hook
                         css-mode-hook html-mode-hook) . highlight-numbers-mode))

(use-package page-break-lines ; Display ugly "^L" page breaks as tidy horizontal lines
  :diminish
  :commands (global-page-break-lines-mode page-break-lines-mode)
  ;; :init (run-with-idle-timer 3 nil #'global-page-break-lines-mode)
  :hook (after-init-hook . global-page-break-lines-mode))

;; First mark the word, then add more cursors. Use `mc/edit-lines' to add a cursor to each line in
;; an active region that spans multiple lines.
(use-package multiple-cursors
  :bind
  (("C-<"     . mc/mark-previous-like-this)
   ("C->"     . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

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
  ;; :init (run-with-idle-timer 3 nil #'require 'pdf-tools nil t) ; Expensive to load
  :hook (after-init-hook . (lambda ()
                             (require 'pdf-tools nil t)))
  :config
  (pdf-loader-install) ; Expected to be faster than `(pdf-tools-install :no-query)'

  (setq-default pdf-view-display-size 'fit-width) ; Buffer-local variable

  (setq pdf-annot-activate-created-annotations t  ; Automatically annotate highlights
        pdf-view-resize-factor 1.1) ; Fine-grained zoom factor of 10%

  ;; We do not enable `pdf-view-themed-minor-mode' since it can change plot colors
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  :bind
  (:map pdf-view-mode-map
        ("C-s" . isearch-forward)
        ("d"   . pdf-annot-delete)
        ("h"   . pdf-annot-add-highlight-markup-annotation)
        ("t"   . pdf-annot-add-text-annotation)
        ("M"   . pdf-view-midnight-minor-mode)))

;; Support `pdf-view-mode' and `doc-view-mode' buffers in `save-place-mode'.
(use-package saveplace-pdf-view
  :after (pdf-tools saveplace)
  :demand t)

(use-package logview
  :commands logview-mode)

(use-package wc-mode
  :commands wc-mode)

;; Gets the definition of word or phrase at point from https://wordnik.com/
(use-package define-word
  :commands (define-word define-word-at-point))

(when nil
  (progn
    (eval-when-compile
      (if (bound-and-true-p sb/disable-package.el)
          (use-package number-separator
            :straight (number-separator :type git :host github
                                        :repo "legalnonsense/number-separator.el"))
        (use-package number-separator
          :ensure nil
          :load-path "extras")))

    (declare-function number-separator-mode "number-separator")

    (unless (fboundp 'number-separator-mode)
      (autoload #'number-separator-mode "number-separator" nil t))

    (with-eval-after-load "number-separator"
      (defvar number-separator)
      (defvar number-separator-interval)
      (defvar number-separator-ignore-threshold)
      (defvar number-separator-decimal-char)

      (setq number-separator ","
            number-separator-interval 3
            number-separator-ignore-threshold 4
            number-separator-decimal-char ".")

      (diminish 'number-sepator-mode))))

(when (symbol-value 'sb/IS-LINUX)
  (progn
    (unless (fboundp 'turn-on-eldoc-mode)
      (autoload #'turn-on-eldoc-mode "eldoc" nil t))

    (dolist (hook '(prog-mode-hook))
      (add-hook hook #'turn-on-eldoc-mode))

    (with-eval-after-load "eldoc"
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
                           'company-abort))

      (diminish 'eldoc-mode))))

;; `eldoc-box-hover-at-point-mode' blocks the view because it shows up at point.
(use-package eldoc-box
  :commands (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :hook (eldoc-mode-hook . eldoc-box-hover-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-fringe-use-same-bg nil)
  :diminish eldoc-box-hover-mode eldoc-box-hover-at-point-mode)

(use-package esup
  :commands esup
  :if (bound-and-true-p sb/debug-init-file))

(use-package bug-hunter
  :disabled t
  :if (bound-and-true-p sb/debug-init-file)
  :commands (bug-hunter-init-file bug-hunter-file))

(when (bound-and-true-p sb/debug-init-file)
  (eval-when-compile
    (if (bound-and-true-p sb/disable-package.el)
        (use-package explain-pause-mode
          :straight (explain-pause-mode :type git :host github
                                        :repo "lastquestion/explain-pause-mode"))
      (use-package explain-pause-mode
        :ensure nil
        :load-path "extras")))

  (declare-function explain-pause-mode "explain-pause-mode")
  (declare-function explain-pause-top "explain-pause-mode")

  (unless (fboundp 'explain-pause-mode)
    (autoload #'explain-pause-mode "explain-pause-mode" nil t))
  (unless (fboundp 'explain-pause-top)
    (autoload #'explain-pause-top "explain-pause-mode" nil t))

  (with-eval-after-load "explain-pause-mode"
    (diminish 'explain-pause-mode)))

(use-package ace-window
  :bind ([remap other-window] . ace-window))

;; "Shift + direction" arrows
(progn
  (unless (fboundp 'windmove-default-keybindings)
    (autoload #'windmove-default-keybindings "windmove" nil t))

  (windmove-default-keybindings)

  (with-eval-after-load "windmove"
    (defvar windmove-wrap-around)

    ;; Wrap around at edges
    (setq windmove-wrap-around t)))

;; Save buffers when Emacs loses focus. This causes additional saves which triggers the
;; `after-save-hook' and leads to auto-formatters being invoked more frequently. We do not need this
;; given that we have `auto-save-visited-mode' enabled.
(use-package super-save
  :defines (super-save-remote-files super-save-triggers)
  :commands super-save-mode
  :disabled t
  :diminish
  ;; :init (run-with-idle-timer 3 nil #'super-save-mode)
  :hook (after-init-hook . super-save-mode)
  :config
  (setq super-save-remote-files nil) ; Ignore remote files, can cause Emacs to hang
  (add-to-list 'super-save-triggers 'ace-window))

;; `amx-major-mode-commands' limits to commands that are relevant to the current major mode
;; `amx-show-unbound-commands' shows frequently used commands that have no key bindings
(use-package amx
  :commands amx-mode
  :hook (after-init-hook . amx-mode)
  :bind
  ;; We need this if we use `vertico' and `consult'
  (("M-x"  . execute-extended-command)
   ("<f1>" . execute-extended-command-for-buffer))
  :custom
  (amx-auto-update-interval 10 "Update the command list every n minutes"))

;; `avy-setup-default' will bind `avy-isearch' to `C-'' in `isearch-mode-map', so that you can
;; select one of the currently visible `isearch' candidates using `avy'.
(use-package avy
  :commands avy-setup-default
  :bind
  (("M-b"   . avy-goto-word-1)
   ("C-'"   . avy-goto-char-timer) ; Does not work with TUI, but works with Alacritty
   ("M-g c" . avy-goto-char-timer) ; TODO: Reuse the keybinding
   ("C-/"   . avy-goto-line) ; Does not work with TUI, but works with Alacritty
   ;; TODO: Reuse the keybinding
   ("M-g l" . avy-goto-line)))

(use-package ace-jump-buffer
  :bind ("C-b" . ace-jump-buffer)
  :config
  (setq ajb-max-window-height 30
        ajb-sort-function 'bs--sort-by-name))

;; This package adds a "C-'" binding to the Ivy minibuffer that uses Avy
(use-package ivy-avy
  :after ivy
  :bind
  (:map ivy-minibuffer-map
        ("C-'"   . ivy-avy) ; Does not work with TUI, but works with Alacritty
        ;; TODO: Reuse the keybinding
        ("M-g l" . ivy-avy)))

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
  (setq bm-restore-repository-on-load t)
  ;; We need to use a reasonable delay so that reading the saved bookmarks file does not affect
  ;; usability
  ;; (run-with-idle-timer 2 nil #'sb/bm-setup)
  :hook (after-init-hook . sb/bm-setup)
  :config (setq-default bm-buffer-persistence t)
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
  :config
  (setq vterm-always-compile-module t
        vterm-max-scrollback 5000
        vterm-term-environment-variable "xterm-24bit")

  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
              (buffer-face-mode t))))

(use-package vterm-toggle
  :commands vterm-toggle
  :bind ("C-`" . vterm-toggle))

;; This is different from `whitespace-cleanup-mode' since this is unconditional
(when (bound-and-true-p sb/delete-trailing-whitespace-p)
  (setq delete-trailing-lines t) ; "M-x delete-trailing-whitespace" deletes trailing lines
  (add-hook 'before-save-hook #'delete-trailing-whitespace))

;; Call `whitespace-cleanup' only if the initial buffer was clean. This mode works on the entire
;; file unlike `ws-butler'. To enable the mode for an entire project, set `whitespace-cleanup-mode'
;; to `t' in the `.dir-locals.el' file.
(use-package whitespace-cleanup-mode
  :disabled t
  :diminish
  :commands (global-whitespace-cleanup-mode whitespace-cleanup-mode)
  :config
  (add-to-list 'whitespace-cleanup-mode-ignore-modes 'markdown-mode)
  :custom
  (whitespace-cleanup-mode-preserve-point t))

(progn
  (defvar reb-re-syntax)

  (setq reb-re-syntax 'string))

(use-package visual-regexp
  :commands (vr/replace vr/query-replace vr/mark)
  :bind ([remap query-replace] . vr/query-replace))

;; (declare-function sb/sshlist "private")

;; (progn
;;   (defun sb/ivy-tramp ()
;;     "Invoke remote hosts with ivy and tramp."
;;     (interactive)
;;     (counsel-find-file (ivy-read "Remote Tramp targets: " (sb/sshlist))))

;;   (bind-key "C-c d t" #'sb/ivy-tramp))

(use-package counsel-tramp
  :if (eq sb/minibuffer-completion 'ivy)
  :bind ("C-c d t" . counsel-tramp))

(when (eq sb/minibuffer-completion 'vertico)
  (progn
    (eval-when-compile
      (if (bound-and-true-p sb/disable-package.el)
          (use-package consult-tramp
            :straight (consult-tramp :type git :host github :repo "Ladicle/consult-tramp"))
        (use-package consult-tramp
          :ensure nil
          :load-path "extras")))

    (declare-function consult-tramp "consult-tramp")

    (bind-keys :package consult-tramp
               ("C-c d t" . consult-tramp))))

;; TODO: SSH into Gcloud
;; https://gist.github.com/jackrusher/36c80a2fd6a8fe8ddf46bc7e408ae1f9
;; Make sure you have set your default project with:
;; "gcloud config set project <project-name>"
;; "C-x C-f /gcssh:compute-instance:/path/to/filename.clj"

;; LATER: Can we shorten long Tramp file names? This does not work with Tramp.
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/data/swarnendu/" . "/vindhya/data/swarnendu/"))
;; (add-to-list 'directory-abbrev-alist
;;              '("/ssh:swarnendu@vindhya.cse.iitk.ac.in:/home/swarnendu/" . "/vindhya/home/swarnendu/"))

(use-package rainbow-mode
  :commands rainbow-mode
  :hook ((css-mode-hook html-mode-hook web-mode-hook help-mode-hook) . rainbow-mode))

(use-package init-open-recentf
  :after recentf
  :demand t
  :disabled t
  :config (init-open-recentf))

(provide 'init-misc)

;;; init-misc.el ends here
