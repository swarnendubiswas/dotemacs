;;; latex-new-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-completion-in-buffer)

(defconst bibs-list '("~/plass-workspace/bib/plass-formatted.bib"
                      "~/iss-workspace/papers/approximate-bib/paper.bib"))

(use-package parsebib
  :ensure t)

(use-package bibtex-completion
  :if (or (eq dotemacs-selection 'helm) (eq dotemacs-selection 'ivy))
  :config
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-cite-default-as-initial-input t)
  (setq bibtex-completion-bibliography '("~/plass-workspace/bib/plass-formatted.bib"
                                         "~/iss-workspace/papers/approximate-bib/paper.bib")))

(use-package ivy-bibtex
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :bind ("C-c l x" . ivy-bibtex)
  :config (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key))

(use-package company-bibtex
  :ensure t
  :if (eq dotemacs-completion-in-buffer 'company)
  :init
  (add-to-list 'company-backends 'company-bibtex)
  (setq company-bibtex-bibliography '("~/plass-workspace/bib/plass-formatted.bib"
                                      "~/iss-workspace/papers/approximate-bib/paper.bib")))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-s") #'dotemacs-save-buffer-and-run-latexmk)))

(provide 'latex-new-init)

;;; latex-new-init.el ends here
