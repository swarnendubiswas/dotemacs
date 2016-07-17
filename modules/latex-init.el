;;; latex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

(defvar dotemacs-completion-in-buffer)
(defvar dotemacs-selection)
(defvar prettify-symbols-unprettify-at-point)
(defvar helm-bibtex-full-frame)

(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode))

(use-package tex-buf ; Requires tex and latex
  :config
  (setq TeX-auto-save t ; Enable parse on save, stores parsed information in an "auto" directory
        TeX-parse-self t ; Parse documents
        TeX-clean-confirm nil
        TeX-quote-after-quote nil ; Allow original LaTeX quotes
        TeX-electric-sub-and-superscript t ; Automatically insert braces in math mode
        TeX-auto-untabify t ; Remove all tabs before saving
        TeX-save-query nil
        LaTeX-syntactic-comments t)

  (setq-default TeX-master nil) ; Query for master file

  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)

  ;; Provide forward "C-c C-v" (TeX-view) and inverse (C-Mouse-1, Ctrl + "Left Click") search with SyncTeX
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server 'ask)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

  (add-to-list 'TeX-command-list
               '("PDFLaTeX" "%'pdflatex%(mode)%' %t" TeX-run-TeX nil t
                 (plain-tex-mode tex-mode TeX-mode LaTeX-mode TeX-latex-mode docTeX-mode)
                 :help "Run PDFLaTeX"))
  (add-to-list 'TeX-command-list
               '("View" "%V" TeX-run-discard nil t))

  (when (>= emacs-major-version 25)
    (setq prettify-symbols-unprettify-at-point 'right-edge)
    (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode))

  (unbind-key "C-c C-d" LaTeX-mode-map)

  ;; Unset "C-c ;" since we want to bind it to 'comment-line
  (unbind-key "C-c ;" LaTeX-mode-map))

(use-package tex-fold
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode
  :diminish latex-electric-env-pair-mode
  :init
  (setq latex-run-command "latexmk")
  (add-hook 'TeX-mode-hook
            (lambda()
              (latex-electric-env-pair-mode 1))))

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-command-default "LatexMk"))))

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
                 '(company-math-symbols-latex company-latex-commands company-math-symbols-unicode))))

;; ;; https://github.com/expez/.emacs.d/blob/master/lisp/init-latex.el
;; (defadvice LaTeX-insert-item (after remove-whitespace-first-item activate)
;;   "This advice is meant to fix the issue where an extra blank
;; line is naively added by `LaTeX-insert-item' when not already on
;; an item line."
;;   (check-item-entry))

(use-package bibtex
  :init (add-hook 'bibtex-mode-hook #'turn-on-auto-revert-mode)
  :config
  (setq bibtex-maintain-sorted-entries t)
  (use-package bibtex-utils
    :ensure t))

(use-package reftex
  :diminish reftex-mode
  :commands (reftex-citation)
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-insert-label-flags '(t t)
        reftex-cite-format 'abbrv
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-auto-update-selection-buffers t
        reftex-enable-partial-scans t
        reftex-allow-automatic-rescan t
        reftex-default-bibliography '("~/workspace/bib/plass.bib")
        reftex-idle-time 0.5
        reftex-toc-follow-mode t
        reftex-use-fonts t
        reftex-highlight-selection 'both)

  (use-package reftex-cite
    :preface
    ;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
    (defun get-bibtex-keys (file)
      (with-current-buffer (find-file-noselect file)
        (mapcar 'car (bibtex-parse-keys))))

    (defun find-bibliography-file ()
      "Try to find a bibliography file using RefTeX."
      ;; Returns a string with text properties (as expected by read-file-name) or empty string if no file can be found
      (interactive)
      (let ((bibfile-list nil))
        (condition-case nil
            (setq bibfile-list (reftex-get-bibfile-list))
          (error (ignore-errors
                   (setq bibfile-list (reftex-default-bibliography)))))
        (if bibfile-list
            (car bibfile-list) "")))

    (defun reftex-add-all-bibitems-from-bibtex ()
      (interactive)
      (mapc 'LaTeX-add-bibitems
            (apply 'append
                   (mapcar 'get-bibtex-keys (reftex-get-bibfile-list)))))
    :config (add-hook 'reftex-load-hook #'reftex-add-all-bibitems-from-bibtex)))

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

(use-package tex-smart-umlauts
  :ensure t
  :init (add-hook 'LaTeX-mode-hook #'tex-smart-umlauts-mode))

(use-package parsebib
  :ensure t)

(use-package helm-bibtex
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :bind ("C-c l x" . helm-bibtex)
  :config
  (use-package bibtex-completion
    :after tex
    :config
    (setq bibtex-completion-bibliography '("/home/biswass/workspace/bib/plass-formatted.bib")
          bibtex-completion-cite-prompt-for-optional-arguments nil
          bibtex-completion-cite-default-as-initial-input t))

  (helm-delete-action-from-source "Insert BibTeX key" helm-source-bibtex)
  (helm-add-action-to-source "Insert BibTeX key" 'bibtex-completion-insert-key helm-source-bibtex 0)
  (setq helm-bibtex-full-frame t))

(use-package ivy-bibtex
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :bind ("C-c l x" . ivy-bibtex)
  :config
  (use-package bibtex-completion
    :after tex
    :config
    (setq bibtex-completion-bibliography '("/home/biswass/workspace/bib/plass-formatted.bib")
          bibtex-completion-cite-prompt-for-optional-arguments nil
          bibtex-completion-cite-default-as-initial-input t))

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
              :action 'bibtex-completion-insert-key)))

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
