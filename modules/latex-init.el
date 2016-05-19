;;; latex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

(use-package tex
  :ensure auctex
  :defer t
  :config
  (setq TeX-auto-save t ; Enable parse on save, stores parsed information in an "auto" directory
        TeX-parse-self t ; Parse documents
        TeX-clean-confirm nil
        TeX-quote-after-quote nil ; Allow original LaTeX quotes
        TeX-electric-sub-and-superscript t ; Automatically insert braces in math mode
        TeX-default-mode 'LaTeX-mode
        TeX-force-default-mode t
        ;; Remove all tabs before saving
        TeX-auto-untabify t)

  ;; Provide forward "C-c C-v" (TeX-view) and inverse (C-Mouse-1, Ctrl + "Left Click") search with SyncTeX
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server 'ask)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

  (setq-default TeX-master nil) ; Query for master file

  ;; Compile files to pdf by default, this is already the default from AUCTeX 11.88, but we want to be sure
  (TeX-global-PDF-mode 1)
  (add-to-list 'TeX-command-list
               '("PDFLaTeX" "%'pdflatex%(mode)%' %t" TeX-run-TeX nil t
                 (plain-tex-mode LaTeX-mode docTeX-mode)
                 :help "Run PDFLaTeX"))
  (add-to-list 'TeX-command-list
               '("View" "%V" TeX-run-discard nil t))

  ;; ;; Save all files before compilation, https://github.com/grettke/home/blob/master/.emacs.el
  ;; (defadvice TeX-command-master (before before-TeX-command-master activate)
  ;;   (progn
  ;;     (dotemacs-save-all-buffers)
  ;;     (dotemacs--tabbar-modification-state-change)
  ;;     (dotemacs--tabbar-on-buffer-modification)))
  ;;   ;; http://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode
  ;;   (add-hook 'LaTeX-mode-hook
  ;;             (lambda()
  ;;               (add-hook 'after-save-hook #'TeX-command-master nil 'make-it-local)))

  (when (>= emacs-major-version 25)
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode))

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

  (use-package latex
    :mode ("\\.tex\\'" . LaTeX-mode)
    :functions LaTeX-math-mode
    :config
    (setq LaTeX-syntactic-comments t)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)

    ;; http://stackoverflow.com/questions/17777189/what-is-the-difference-of-tex-mode-and-latex-mode-and-latex-mode-in-emacs
    ;;(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))

    ;; (with-eval-after-load "LaTeX"
    ;;   (define-key LaTeX-mode-map (kbd "C-c C-d") nil))
    ;; (with-eval-after-load "LaTeX"
    ;;   (define-key LaTeX-mode-map (kbd "C-c C-d") 'duplicate-thing))
    ;; (bind-key "C-c C-d" 'duplicate-thing LaTeX-mode-map)

    ;; Unset "C-c ;" since we want to bind it to 'comment-line
    (define-key LaTeX-mode-map (kbd "C-c ;") nil))

  (use-package latex-pretty-symbols
    :ensure t
    :disabled t)

  (use-package latex-unicode-math-mode
    :ensure t
    :disabled t
    :diminish latex-unicode-mode
    :config
    ;; This converts LaTeX to Unicode inside math environments.
    ;; (add-hook 'LaTeX-mode-hook 'latex-unicode-math-mode)
    ;; This converts LaTeX to Unicode everwhere, not only in math environments.
    (add-hook 'LaTeX-mode-hook 'latex-unicode-mode))

  ;; Required by ac-math and company-math
  (use-package math-symbol-lists
    :ensure t)

  (when (eq dotemacs-completion-in-buffer 'auto-complete)
    (use-package auto-complete-auctex
      :ensure t)

    (use-package ac-tex-ref
      :load-path "extras"
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

  (when (eq dotemacs-completion-in-buffer 'company)
    (use-package company-auctex
      :ensure t
      :config (company-auctex-init))

    (use-package company-math
      :ensure t
      :config
      (add-to-list 'company-backends
                   '(company-math-symbols-latex company-latex-commands))))

  (use-package auctex-latexmk
    :ensure t
    :config
    (auctex-latexmk-setup)
    ;; This variable is buffer-local
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-command-default "LatexMk"))))

  (use-package latex-extra
    :ensure t
    :disabled t ; Overrides a few useful keymap prefixes
    :config
    (latex/setup-keybinds)
    (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

  (use-package latex-preview-pane ; Currently does not support multi-file parsing
    :ensure t
    :disabled t
    :config (latex-preview-pane-enable))

  (use-package latex-math-preview
    :load-path "extras")

  (use-package magic-latex-buffer
    :ensure t
    :disabled t
    :diminish magic-latex-buffer
    :config
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

  (use-package bib-cite
    :diminish bib-cite-minor-mode
    :config
    (bib-cite-minor-mode 1)
    (setq bib-cite-use-reftex-view-crossref t)
    :bind
    (:map bib-cite-minor-mode-map
          ("C-c b" . nil) ; We use "C-c b" for comment-box
          ("C-c l a" . bib-apropos)
          ("C-c l b" . bib-make-bibliography)
          ("C-c l d" . bib-display)
          ("C-c l t" . bib-etags)
          ("C-c l f" . bib-find)
          ("C-c l n" . bib-find-next)
          ("C-c l h" . bib-highlight-mouse)))

  ;; http://joostkremers.github.io/ebib/ebib-manual.html#the-ebib-buffers
  (use-package ebib
    :ensure t
    :bind ("C-c l e" . ebib)
    :config
    (setq ebib-bibtex-dialect 'BibTeX
          ebib-uniquify-keys t
          ebib-index-display-fields '("title")
          ebib-file-associations '(("pdf" . "evince") ("ps" . "evince"))
          ebib-bib-search-dirs '("/home/biswass/workspace/bib")
          ebib-preload-bib-files '("/home/biswass/workspace/bib/plass-formatted.bib")))

  ;; C-c = reftex-toc
  ;; C-c [
  ;; C-c (
  ;; C-c )
  (use-package reftex
    :diminish reftex-mode
    :config
    (setq reftex-plug-into-AUCTeX t
          reftex-insert-label-flags '(t t)
          reftex-cite-format 'abbrv
          reftex-save-parse-info t
          reftex-use-multiple-selection-buffers t
          reftex-enable-partial-scans t
          reftex-default-bibliography '("~/workspace/bib/plass.bib")
          reftex-idle-time 0.5
          reftex-toc-follow-mode t)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex) ; Use with AUCTeX
    ;; Emacs latex mode
    (add-hook 'lateX-mode-hook #'turn-on-reftex))

  (use-package tex-smart-umlauts
    :ensure t
    :config (add-hook 'LaTeX-mode-hook #'tex-smart-umlauts-decode))

  ;; https://github.com/expez/.emacs.d/blob/master/lisp/init-latex.el
  (defadvice LaTeX-insert-item (after remove-whitespace-first-item activate)
    "This advice is meant to fix the issue where an extra blank
line is naively added by `LaTeX-insert-item' when not already on
an item line."
    (check-item-entry))

  :bind (:map LaTeX-mode-map
              ("C-c C-d" . nil)))

(use-package helm-bibtex
  :ensure t
  :disabled t
  :if (eq dotemacs-selection 'helm)
  :after tex
  :config
  (use-package parsebib
    :ensure t)
  (use-package bibtex-completion
  :config
  (setq bibtex-completion-bibliography '("/home/biswass/workspace/bib/plass-formatted.bib")
        bibtex-completion-cite-prompt-for-optional-arguments nil))
  :bind ("C-c l x" . helm-bibtex)
  :config (setq helm-bibtex-full-frame t))

(use-package ivy-bibtex
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :after tex
  :config
  (use-package parsebib
    :ensure t)
  (use-package bibtex-completion
  :config
  (setq bibtex-completion-bibliography '("/home/biswass/workspace/bib/plass-formatted.bib")
        bibtex-completion-cite-prompt-for-optional-arguments nil))
  :bind ("C-c l x" . ivy-bibtex))

(use-package outline
  :ensure t
  :after tex
  :diminish outline-minor-mode
  :init (add-hook 'LaTeX-mode-hook #'outline-minor-mode)
  :config
  (use-package outline-magic
    :ensure t
    :init
    (add-hook 'outline-mode-hook
              (lambda ()
                (require 'outline-cycle)))))

(provide 'latex-init)

;;; latex-init.el ends here
