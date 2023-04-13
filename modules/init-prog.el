;;; init-languages.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:
;;; utf-8; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary: This file contains configurations related to `prog-mode'.

;;; Code:

(add-hook
  'prog-mode-hook
  (lambda ()
    (auto-fill-mode 1) ; Autofill comments

    ;; Native from Emacs 27+, disable in TUI since the line characters also get copied.
    (when (display-graphic-p)
      (display-fill-column-indicator-mode 1))))

(use-package which-func
  :hook (prog-mode-hook . which-func-mode)
  :custom
  (which-func-modes
    '
    (emacs-lisp-mode
      c-mode
      c++-mode
      perl-mode
      cperl-mode
      python-mode
      makefile-mode
      sh-mode
      java-mode)))

(use-package subword
  :straight (:type built-in)
  :hook (prog-mode-hook . subword-mode)
  :diminish)

;; (use-package outline ; Edit outlines
;;   ;; :hook
;;   ;; (prog-mode-hook . outline-minor-mode)
;;   :diminish outline-minor-mode)

;; Hide top-level code blocks. Enable code folding, which is useful for browsing large files. This
;; module is part of Emacs, and is better maintained than other alternatives like `origami'.

(use-package hideshow
  :straight (:type built-in)
  :commands (hs-hide-all hs-hide-initial-comment-block hs-show-all hs-show-block)
  :hook
  ;; Hideshow is not defined for `ini-mode'.
  ((python-mode-hook emacs-lisp-mode-hook java-mode-hook sh-mode-hook) . hs-minor-mode)
  :custom (hs-isearch-open t "Open all folds while searching")
  :diminish hs-minor-mode)

(use-package symbol-overlay ; Highlight symbol under point
  :commands (transient-define-prefix)
  :hook (prog-mode-hook . symbol-overlay-mode)
  :bind (("M-p" . symbol-overlay-jump-prev) ("M-n" . symbol-overlay-jump-next))
  :custom
  ;; Delay highlighting to allow for transient cursor placements
  (symbol-overlay-idle-time 2)
  :diminish)

(use-package highlight-escape-sequences
  :hook (prog-mode-hook . hes-mode))

(use-package compile
  :straight (:type built-in)
  :bind
  ;; "<f10>" and "<f11>" conflict with Gnome window manager keybindings
  (("<f10>" . compile) ("<f11>" . recompile))
  :custom
  (compilation-always-kill t "Kill a compilation process before starting a new one")
  (compilation-ask-about-save nil "Save all modified buffers without asking")
  ;; Automatically scroll the *Compilation* buffer as output appears, but stop at the first
  ;; error.
  (compilation-scroll-output 'first-error))

(use-package fancy-compilation
  :straight (:host codeberg :repo "ideasman42/emacs-fancy-compilation" :branch "main")
  :after compile
  :init (fancy-compilation-mode 1))

(use-package rainbow-delimiters
  :hook ((prog-mode-hook latex-mode-hook LaTeX-mode-hook org-src-mode-hook) . rainbow-delimiters-mode))

;; Tree-sitter provides advanced syntax highlighting features
(use-package tree-sitter
  :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode) (prog-mode-hook . global-tree-sitter-mode))
  :config
  (use-package tree-sitter-langs
    :demand t)
  :diminish tree-sitter-mode)

(use-package eldoc
  :straight (:type built-in)
  :hook (prog-mode-hook . turn-on-eldoc-mode)
  :custom (eldoc-area-prefer-doc-buffer t "Disable popups")
  ;; The variable-height minibuffer and extra eldoc buffers are distracting. We can limit ElDoc
  ;; messages to one line which prevents the echo area from resizing itself unexpectedly when point
  ;; is on a variable with a multiline docstring, but then it cuts of useful information.
  ;; (eldoc-echo-area-use-multiline-p nil)
  :config
  ;; Allow eldoc to trigger after completions
  (with-eval-after-load "company"
    (eldoc-add-command
      'company-complete-selection
      'company-complete-common
      'company-capf
      'company-abort))
  :diminish)

;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(setenv "SHELL" shell-file-name) ; Recommended to connect with Bash

;; TODO: Try https://github.com/jart/disaster

(use-package rmsbolt
  :commands rmsbolt-mode)

(provide 'init-prog)

;;; init-prog.el ends here
