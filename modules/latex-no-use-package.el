;;; latex-no-use-package.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

(defvar dotemacs-completion-in-buffer)
(defvar dotemacs-selection)
(defvar prettify-symbols-unprettify-at-point)
(defvar helm-bibtex-full-frame)

(require 'tex)
(require 'tex-site)
(require 'tex-mode)
(require 'tex-buf)
(require 'tex-mode)
(require 'latex)

(load "auctex.el" nil t t)

(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

(setq TeX-auto-save t ; Enable parse on save, stores parsed information in an "auto" directory
      TeX-parse-self t ; Parse documents
      TeX-clean-confirm nil
      TeX-quote-after-quote nil ; Allow original LaTeX quotes
      TeX-electric-sub-and-superscript t ; Automatically insert braces in math mode
      ;; Remove all tabs before saving
      TeX-auto-untabify t)

;; Provide forward "C-c C-v" (TeX-view) and inverse (C-Mouse-1, Ctrl + "Left Click") search with SyncTeX
(setq TeX-source-correlate-method 'synctex
      TeX-source-correlate-mode t
      TeX-source-correlate-start-server 'ask)
(setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
(add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

(setq-default TeX-master nil) ; Query for master file

(add-to-list 'TeX-command-list
             '("PDFLaTeX" "%'pdflatex%(mode)%' %t" TeX-run-TeX nil t
               (plain-tex-mode LaTeX-mode docTeX-mode)
               :help "Run PDFLaTeX"))
(add-to-list 'TeX-command-list
             '("View" "%V" TeX-run-discard nil t))

(when (>= emacs-major-version 25)
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (add-hook 'LaTeX-mode-hook #'prettify-symbols-mode))

(setq latex-run-command "latexmk")
(add-hook 'LaTeX-mode-hook
          (lambda()
            (latex-electric-env-pair-mode 1)))

(setq TeX-save-query nil)

(add-hook 'TeX-mode-hook #'TeX-fold-mode)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (progn
              ((TeX-PDF-mode 1)
               (LaTeX-math-mode 1)
               (auto-fill-mode 1)))))

(setq LaTeX-syntactic-comments t)
;; Unset "C-c ;" since we want to bind it to 'comment-line
(unbind-key "C-c ;" LaTeX-mode-map)

(require 'math-symbol-lists)

(when (eq dotemacs-completion-in-buffer 'company)
  (require 'company-auctex)
  (company-auctex-init)

  (require 'company-math)
  (add-to-list 'company-backends
               '(company-math-symbols-latex
                 company-latex-commands
                 company-math-symbols-unicode)))

(require 'auctex-latexmk)
(auctex-latexmk-setup)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq TeX-command-default "LatexMk")))

(require 'bibtex)
(add-hook 'bibtex-mode-hook #'BibTeX-auto-store)
(setq bibtex-maintain-sorted-entries t)

(require 'bibtex-utils)

(require 'bib-cite)
(bib-cite-minor-mode 1)
(diminish 'bib-cite-minor-mode)
(setq bib-cite-use-reftex-view-crossref t)
(bind-keys :map bib-cite-minor-mode-map
           ("C-c b" . nil) ; We use "C-c b" for comment-box
           ("C-c l a" . bib-apropos)
           ("C-c l b" . bib-make-bibliography)
           ("C-c l d" . bib-display)
           ("C-c l t" . bib-etags)
           ("C-c l f" . bib-find)
           ("C-c l n" . bib-find-next)
           ("C-c l h" . bib-highlight-mouse))

(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(diminish 'reftex-mode)
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
      reftex-auto-view-crossref t
      reftex-auto-recenter-toc t
      reftex-use-fonts t
      reftex-highlight-selection 'both)
(add-hook 'LaTeX-mode-hook #'turn-on-reftex)

(require 'reftex-cite)

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

;; FIXME: This seems to work if we have latex-math-mode, auto-fill-function, and TeX-PDF-mode enabled.
(defun reftex-add-all-bibitems-from-bibtex ()
  (interactive)
  (message "reftex-add-all-bibitems-from-bibtex is getting called")
  (mapc 'LaTeX-add-bibitems
        (apply 'append
               (mapcar 'get-bibtex-keys (reftex-get-bibfile-list)))))

(add-hook 'reftex-mode-hook #'reftex-add-all-bibitems-from-bibtex)
(add-hook 'reftex-load-hook #'reftex-add-all-bibitems-from-bibtex)

(require 'tex-smart-umlauts)
(add-hook 'LaTeX-mode-hook #'tex-smart-umlauts-mode)

;; https://github.com/expez/.emacs.d/blob/master/lisp/init-latex.el
(defadvice LaTeX-insert-item (after remove-whitespace-first-item activate)
  "This advice is meant to fix the issue where an extra blank
line is naively added by `LaTeX-insert-item' when not already on
an item line."
  (check-item-entry))

(unbind-key "C-c C-d" LaTeX-mode-map)

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

(require 'outline)
(add-hook 'LaTeX-mode-hook #'outline-minor-mode)
(diminish 'outline-minor-mode)

(require 'outline-magic)
(add-hook 'outline-mode-hook
          (lambda ()
            (require 'outline-cycle)))

(provide 'latex-no-use-package)

;;; latex-no-use-package.el ends here
