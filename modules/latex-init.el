;;; latex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Anonymous-Functions.html
;; When defining a lambda expression that is to be used as an anonymous function, you can in principle use any method
;; to construct the list. But typically you should use the lambda macro, or the function special form, or the #' read
;; syntax which is a short-hand for using function. Quoting a lambda form means the anonymous function is not
;; byte-compiled. The following forms are all equivalent:
;; (lambda (x) (* x x))
;; (function (lambda (x) (* x x)))
;; #'(lambda (x) (* x x))

;;; Code:

(use-package tex
  :ensure auctex
  :defer t
  :functions (TeX-PDF-mode TeX-source-correlate-mode)
  :config
  (setq TeX-auto-save t ; enable parse on save, stores parsed information in an "auto" directory
        TeX-parse-self t ; Parse documents
        TeX-clean-confirm nil
        TeX-quote-after-quote nil ; allow original LaTeX quotes
        TeX-electric-sub-and-superscript t ; automatically insert braces in math mode
        TeX-default-mode 'LaTeX-mode
        TeX-force-default-mode t
        TeX-auto-untabify t ; remove all tabs before saving
        TeX-source-correlate-method 'synctex ;; Provide forward and inverse search with SyncTeX
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)
  (setq-default TeX-master nil) ; query for master file
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))

  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode) ;; compile files to pdf by default

  (use-package tex-site)

  (use-package tex-mode
    :functions (latex-mode latex-electric-env-pair-mode)
    :diminish latex-electric-env-pair-mode
    :config
    (setq latex-run-command "latexmk")
    (add-hook 'LaTeX-mode-hook
              (lambda()
                (latex-electric-env-pair-mode 1))))

  (use-package tex-buf
    :config (setq TeX-save-query nil))

  (use-package tex-fold
    :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

  (use-package latex
    :mode ("\\.tex\\'" . LaTeX-mode)
    :functions LaTeX-math-mode
    :config
    (setq LaTeX-syntactic-comments t)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq-default TeX-command-default "LatexMk")))

    ;; http://stackoverflow.com/questions/17777189/what-is-the-difference-of-tex-mode-and-latex-mode-and-latex-mode-in-emacs
    ;;(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))

    ;; (with-eval-after-load "LaTeX"
    ;;   (define-key LaTeX-mode-map (kbd "C-c C-d") nil))
    ;; (with-eval-after-load "LaTeX"
    ;;   (define-key LaTeX-mode-map (kbd "C-c C-d") 'duplicate-thing))
    ;; (bind-key "C-c C-d" 'duplicate-thing LaTeX-mode-map)

    ;; unset "C-c ;" since we want to bind it to 'comment-line
    (define-key LaTeX-mode-map (kbd "C-c ;") nil))

  (use-package latex-pretty-symbols
    :ensure t)

  ;; required by ac-math and company-math
  (use-package math-symbol-lists
    :ensure t)

  (when (eq dotemacs-completion 'auto-complete)
    (use-package auto-complete-auctex
      :ensure t)

    (use-package ac-tex-ref
      :load-path "packages"
      :config
      (setq ac-sources
            (append '(ac-source-tex-ref ac-source-tex-cite)
                    ac-sources)))

    (use-package ac-math
      :ensure t
      :preface
      (defun ac-latex-mode-setup ()
        (setq ac-sources
              (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands ac-source-auctex-labels)
                      ac-sources)))
      :config
      (add-to-list 'ac-modes 'latex-mode)
      (add-hook 'TeX-mode-hook #'ac-latex-mode-setup)
      (setq ac-math-unicode-in-math-p t)))

  (when (eq dotemacs-completion 'company)
    (use-package company-auctex
      :ensure t
      :config (company-auctex-init))

    (use-package company-math
      :ensure t
      :config
      (add-to-list 'company-backends
                   '(company-math-symbols-latex
                     company-latex-commands))))

  (use-package auctex-latexmk
    :ensure t
    :init
    (auctex-latexmk-setup)
    ;; this variable is buffer-local
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-command-default "LatexMk"))))

  (use-package latex-extra
    :ensure t
    :disabled t ; overrides a few useful keymap prefixes
    :config
    (latex/setup-keybinds)
    (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

  (use-package latex-preview-pane ; currently does not support multi-file parsing
    :ensure t
    :disabled t
    :config (latex-preview-pane-enable))

  (use-package latex-math-preview
    :load-path "packages")

  (use-package magic-latex-buffer
    :ensure t
    :disabled t
    :diminish magic-latex-buffer
    :init
    (add-hook 'LaTeX-mode-hook #'magic-latex-buffer)
    (setq magic-latex-enable-block-highlight nil
          magic-latex-enable-subscript nil
          magic-latex-enable-pretty-symbols nil
          magic-latex-enable-block-align nil
          magic-latex-enable-inline-image nil))

  (use-package bibtex
    :commands bibtex-mode
    :config
    (add-hook 'bibtex-mode-hook #'BibTeX-auto-store)
    (setq bibtex-maintain-sorted-entries t)
    (use-package bibtex-utils
      :ensure t))

  ;; "C-c b f" runs the command 'bib-find
  (use-package bib-cite
    :diminish bib-cite-minor-mode
    :config
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (bib-cite-minor-mode 1)))
    (setq bib-cite-use-reftex-view-crossref t))

  (use-package helm-bibtex
    :ensure t
    :if (bound-and-true-p dotemacs-use-helm-p)
    :init
    (use-package parsebib
      :ensure t)
    :bind ("C-c b x" . helm-bibtex)
    :config (setq helm-bibtex-bibliography '("/home/biswass/workspace/bib/plass-reformatted.bib")))

  (use-package reftex
    :diminish reftex-mode
    :init
    (setq reftex-plug-into-AUCTeX t
          reftex-insert-label-flags '(t t)
          reftex-cite-format 'abbrv
          reftex-save-parse-info t
          reftex-use-multiple-selection-buffers t
          reftex-enable-partial-scans t
          reftex-default-bibliography '("~/workspace/bib/plass.bib")
          reftex-idle-time 0.5)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex) ; for use with AUCTeX
    ;; for Emacs latex mode
    (add-hook 'lateX-mode-hook #'turn-on-reftex))

  (use-package tex-smart-umlauts
    :ensure t
    :init (add-hook 'LaTeX-mode-hook #'tex-smart-umlauts-decode))

  ;; https://github.com/expez/.emacs.d/blob/master/lisp/init-latex.el
  (defadvice LaTeX-insert-item (after remove-whitespace-first-item activate)
    "This advice is meant to fix the issue where an extra blank
line is naively added by `LaTeX-insert-item' when not already on
an item line."
    (check-item-entry)))

(provide 'latex-init)

;;; latex-init.el ends here
