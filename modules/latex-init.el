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
  (setq-default TeX-master nil) ; Query for master file

  ;; Provide forward "C-c C-v" (TeX-view) and inverse (C-Mouse-1, Ctrl + "Left Click") search with SyncTeX, not required
  ;; with the pdfsync LaTeX package
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server 't)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

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
    (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode)))

(use-package tex-mode
  :after tex
  :functions (latex-mode latex-electric-env-pair-mode)
  :diminish latex-electric-env-pair-mode
  :config
  (setq latex-run-command "latexmk")
  ;; This is not needed if we have auto-pairs enabled.
  (add-hook 'LaTeX-mode-hook
            (lambda()
              (latex-electric-env-pair-mode -1))))

(use-package tex-buf
  :config (setq TeX-save-query nil))

(use-package latex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :functions LaTeX-math-mode
  :config
  (setq LaTeX-syntactic-comments t
        ;; This is not needed if we have auto-pairs enabled.
        LaTeX-electric-left-right-brace nil)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)
  ;; https://github.com/expez/.emacs.d/blob/master/lisp/init-latex.el
  (defadvice LaTeX-insert-item (after remove-whitespace-first-item activate)
    "This advice is meant to fix the issue where an extra blank
line is naively added by `LaTeX-insert-item' when not already on
an item line."
    (check-item-entry))
  :bind (:map LaTeX-mode-map
              ;; Unset "C-c ;" since we want to bind it to 'comment-line
              ("C-c C-d" . nil)))

;; Required by ac-math and company-math
(use-package math-symbol-lists
  :ensure t
  :after tex)

(when (eq dotemacs-completion-in-buffer 'auto-complete)
  (use-package auto-complete-auctex
    :ensure t
    :after tex)

  (use-package ac-tex-ref
    :after tex
    :load-path "extras"
    :config
    (setq ac-sources
          (append '(ac-source-tex-ref ac-source-tex-cite)
                  ac-sources)))

  (use-package ac-math
    :ensure t
    :after tex
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
    :after tex
    :config (company-auctex-init))

  (use-package company-math
    :ensure t
    :after tex
    :config
    (add-to-list 'company-backends
                 '(company-math-symbols-latex company-latex-commands company-math-symbols-unicode))))

(use-package auctex-latexmk
  :ensure t
  :after tex
  :config
  (auctex-latexmk-setup)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              ;; This variable is buffer-local
              (setq TeX-command-default "LatexMk"))))

(use-package latex-math-preview
  :load-path "extras")

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

(use-package reftex
  :after tex
  :diminish reftex-mode
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-insert-label-flags '(t t)
        reftex-cite-format 'abbrv
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-enable-partial-scans t
        reftex-default-bibliography '("~/workspace/bib/plass-formatted.bib")
        reftex-idle-time 0.5
        reftex-toc-follow-mode t)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex) ; Use with AUCTeX
  ;; Emacs latex mode
  (add-hook 'lateX-mode-hook #'turn-on-reftex))

(use-package tex-smart-umlauts
  :ensure t
  :after tex
  :config (add-hook 'LaTeX-mode-hook #'tex-smart-umlauts-decode))

(use-package parsebib
  :ensure t
  :after tex)

(use-package bibtex-completion
  :after tex
  :config
  (setq bibtex-completion-bibliography '("/home/biswass/workspace/bib/plass-formatted.bib")
        bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-cite-default-as-initial-input t))

(use-package helm-bibtex
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :after tex
  :config
  :bind ("C-c l x" . helm-bibtex)
  :config
  (helm-delete-action-from-source "Insert BibTeX key" helm-source-bibtex)
  (helm-add-action-to-source "Insert BibTeX key" 'bibtex-completion-insert-key helm-source-bibtex 0)
  (setq helm-bibtex-full-frame t))

(use-package ivy-bibtex
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :after tex
  :config
  ;; https://github.com/tmalsburg/helm-bibtex/
  (defun ivy-bibtex (&optional arg)
    "Search BibTeX entries using ivy.

With a prefix ARG the cache is invalidated and the bibliography
reread."
    (interactive "P")
    (when arg
      (setq bibtex-completion-bibliography-hash ""))
    (bibtex-completion-init)
    (ivy-read "BibTeX Items: "
              (bibtex-completion-candidates 'ivy-bibtex-candidates-formatter)
              :caller 'ivy-bibtex
              :action 'bibtex-completion-insert-key))
  :bind ("C-c l x" . ivy-bibtex))

(use-package outline
  :ensure t
  :after tex
  :diminish outline-minor-mode
  :init (add-hook 'LaTeX-mode-hook
                  (lambda ()
                    (outline-minor-mode 1)))
  :config
  (use-package outline-magic
    :ensure t
    :init
    (add-hook 'outline-mode-hook
              (lambda ()
                (require 'outline-cycle)))))

(provide 'latex-init)

;;; latex-init.el ends here
