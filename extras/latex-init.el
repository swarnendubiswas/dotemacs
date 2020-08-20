;;; latex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode. Auctex provides LaTeX-mode, which is an alias.

;;; Code:

;; Initialize auctex, auctex overrides the tex package
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode))

(with-eval-after-load 'tex-mode
  (setq TeX-auto-save t ; Enable parse on save, stores parsed information in an "auto" directory
        TeX-auto-untabify t ; Remove all tabs before saving
        TeX-clean-confirm nil
        TeX-electric-sub-and-superscript t ; Automatically insert braces in math mode
        TeX-parse-self t ; Parse documents
        TeX-PDF-mode t ;; use pdflatex
        TeX-quote-after-quote nil ; Allow original LaTeX quotes
        TeX-save-query nil
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        ;; don't start the emacs server when correlating sources
        TeX-source-correlate-start-server nil
        TeX-syntactic-comment t
        LaTeX-item-indent 0 ; Two spaces + Extra indentation
        LaTeX-syntactic-comments t
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  ;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill)

  (setq-default TeX-master nil) ; Query for master file

  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)

  ;; Disable "LaTeX-insert-item" in favor of imenu
  ;; (unbind-key "C-c C-d" LaTeX-mode-map)
  ;; Unset "C-c ;" since we want to bind it to 'comment-line
  ;; (unbind-key "C-c ;" LaTeX-mode-map)

  (use-package auctex-latexmk
    :ensure t
    :custom
    (auctex-latexmk-inherit-TeX-PDF-mode t "Pass the -pdf flag when TeX-PDF-mode is active")
    (TeX-command-default "LatexMk")
    :config (auctex-latexmk-setup)
    :hook (LaTeX-mode . (lambda()
                          (require 'auctex-latexmk))))

  (use-package company-auctex
    :ensure t
    :init (company-auctex-init))

  (use-package math-symbol-lists ; Required by ac-math and company-math
    :ensure t)

  (use-package company-math
    :ensure t
    :after company
    :init
    (add-to-list 'company-backends
                 '(company-math-symbols-latex company-latex-commands company-math-symbols-unicode)))

  (use-package company-reftex
    :ensure t
    :after company
    :init
    (add-to-list 'company-backends '(company-reftex-labels company-reftex-citations)))

  (use-package ivy-bibtex
    :ensure t
    :bind ("C-c x b" . ivy-bibtex)
    :config
    (use-package bibtex-completion
      :custom
      (bibtex-completion-cite-prompt-for-optional-arguments nil)
      (bibtex-completion-cite-default-as-initial-input t)
      (bibtex-completion-display-formats '((t . "${author:24} ${title:*} ${=key=:16} ${=type=:12}"))))
    :custom (ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

  (use-package bibtex
    :init (add-hook 'bibtex-mode-hook #'turn-on-auto-revert-mode)
    :config
    (setq bibtex-maintain-sorted-entries t)
    (use-package bibtex-utils
      :ensure t))

  (use-package reftex
    :bind (("C-c [" . reftex-citation)
           ("C-c )" . reftex-reference)
           ("C-c (" . reftex-label))
    ;; :preface
    ;; (defun get-bibtex-keys (file)
    ;;   (with-current-buffer (find-file-noselect file)
    ;;     (mapcar 'car (bibtex-parse-keys))))
    ;; (defun reftex-add-all-bibitems-from-bibtex ()
    ;;   (interactive)
    ;;   (mapc 'LaTeX-add-bibitems
    ;;         (apply 'append
    ;;                (mapcar 'get-bibtex-keys (reftex-get-bibfile-list)))))
    :custom
    (reftex-plug-into-AUCTeX t)
    (reftex-save-parse-info t)
    (reftex-use-multiple-selection-buffers t)
    (reftex-enable-partial-scans t)
    (reftex-toc-follow-mode t "Other buffer follows the point in toc buffer")
    (reftex-highlight-selection 'both))

  ;; (with-eval-after-load 'reftex
  ;;     (reftex-add-all-bibitems-from-bibtex))

  ;;  (add-hook 'reftex-toc-mode-hook #'reftex-toc-rescan)

  (setq bibtex-align-at-equal-sign t)

  ;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
  (use-package reftex-cite
    :after auctex
    :preface
    (defun get-bibtex-keys (file)
      (with-current-buffer (find-file-noselect file)
        (mapcar 'car (bibtex-parse-keys))))
    (defun reftex-add-all-bibitems-from-bibtex ()
      (interactive)
      (mapc 'LaTeX-add-bibitems
            (apply 'append
                   (mapcar 'get-bibtex-keys (reftex-get-bibfile-list)))))
    :config (reftex-add-all-bibitems-from-bibtex))

  ;; (use-package reftex-cite
  ;;   :preface
  ;;   ;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
  ;; (defun get-bibtex-keys (file)
  ;;   (with-current-buffer (find-file-noselect file)
  ;;     (mapcar 'car (bibtex-parse-keys))))
  ;;   (defun find-bibliography-file ()
  ;;     "Try to find a bibliography file using RefTeX."
  ;;     ;; Returns a string with text properties (as expected by read-file-name) or empty string if no file can be found
  ;;     (interactive)
  ;;     (let ((bibfile-list nil))
  ;;       (condition-case nil
  ;;           (setq bibfile-list (reftex-get-bibfile-list))
  ;;         (error (ignore-errors
  ;;                  (setq bibfile-list (reftex-default-bibliography)))))
  ;;       (if bibfile-list
  ;;           (car bibfile-list) "")))
  ;; (defun reftex-add-all-bibitems-from-bibtex ()
  ;;   (interactive)
  ;;   (mapc 'LaTeX-add-bibitems
  ;;         (apply 'append
  ;;                (mapcar 'get-bibtex-keys (reftex-get-bibfile-list)))))
  ;;   :init (add-hook 'reftex-load-hook #'reftex-add-all-bibitems-from-bibtex))

  (use-package bib-cite
    :diminish bib-cite-minor-mode
    :init  (add-hook 'LaTeX-mode-hook (lambda ()
                                        (bib-cite-minor-mode 1)))
    :config (setq bib-cite-use-reftex-view-crossref t)
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

  ;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
  (defun sb/save-buffer-and-run-latexmk ()
    "Save the current buffer and run LaTeXMk also."
    (interactive)
    (require 'tex-buf)
    (let ((process (TeX-active-process))) (if process (delete-process process)))
    (let ((TeX-save-query nil)) (TeX-save-document ""))
    (TeX-command-menu "LaTeXMk"))

  (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
    (add-hook hook
              (lambda ()
                (add-hook 'after-save-hook
                          (lambda ()
                            (sb/save-buffer-and-run-latexmk)) nil t))))

  (use-package company-bibtex
    :ensure t
    :init
    (add-to-list 'company-backends 'company-bibtex)
    ;; (add-to-list 'company-fuzzy--no-prefix-backends 'company-bibtex)
    )

  ;; (bind-key "C-x C-s" #'sb/save-buffer-and-run-latexmk LaTeX-mode-map)
  ;; (bind-key "C-x C-s" #'sb/save-buffer-and-run-latexmk latex-mode-map)
  )

(provide 'latex-init)

;;; latex-init.el ends here
