;;; init-latex.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp;
;;; coding:utf-8; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/minibuffer-completion)
(defvar sb/user-tmp-directory)

;; `lsp-latex' provides better support for the `texlab' server compared to `lsp-tex'. On the other
;; hand, `lsp-tex' supports `digestif'. `lsp-latex' does not require `auctex'. However, the server
;; performance is very poor, so I continue to prefer `auctex'.

(use-package lsp-latex
  :defines (lsp-latex-bibtex-formatter lsp-latex-latex-formatter
                                       lsp-latex-bibtex-formatter-line-length
                                       lsp-latex-chktex-on-open-and-save
                                       lsp-latex-build-on-save
                                       lsp-latex-build-is-continuous
                                       lsp-latex-build-args
                                       lsp-latex-diagnostics-delay)
  :hook
  (latex-mode-hook . (lambda()
                       (require 'lsp-latex)
                       (lsp-deferred)))
  :custom
  (lsp-latex-bibtex-formatter             "latexindent")
  (lsp-latex-latex-formatter              "latexindent")
  (lsp-latex-bibtex-formatter-line-length sb/fill-column)
  (lsp-latex-chktex-on-open-and-save      t)
  (lsp-latex-build-is-continuous          t)
  ;; Delay time in milliseconds before reporting diagnostics
  (lsp-latex-diagnostics-delay            2000)
  :config
  (add-to-list 'lsp-latex-build-args "-c")
  (add-to-list 'lsp-latex-build-args "-pvc")

  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection "texlab")
  ;;   :major-modes '(tex-mode latex-mode LaTeX-mode bibtex-mode)
  ;;   :remote? t
  ;;   :server-id 'texlab-r))
  )

;; Auctex provides enhanced versions of `tex-mode' and `latex-mode', which automatically replace the
;; vanilla ones. Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides
;; the tex package.

(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :defines (tex-fontify-script font-latex-fontify-script
                               font-latex-fontify-sectioning
                               TeX-syntactic-comment
                               TeX-save-query LaTeX-item-indent
                               LaTeX-syntactic-comments
                               LaTeX-fill-break-at-separators)
  :functions (TeX-active-process)
  :commands (TeX-active-process TeX-save-document tex-site
                                LaTeX-mode LaTeX-math-mode
                                TeX-PDF-mode
                                TeX-source-correlate-mode
                                TeX-active-process
                                TeX-command-menu
                                TeX-revert-document-buffer
                                TeX-master-file
                                TeX-next-error)
  :hook
  (((latex-mode-hook LaTeX-mode-hook) . LaTeX-math-mode)
   ((latex-mode-hook LaTeX-mode-hook) . TeX-PDF-mode) ; Use `pdflatex'
   ((latex-mode-hook LaTeX-mode-hook) . TeX-source-correlate-mode)
   (LaTeX-mode-hook . turn-on-auto-fill))
  :config
  (setq TeX-auto-save t ; Enable parse on save, stores parsed information in an `auto' directory
        TeX-auto-untabify t ; Remove all tabs before saving
        TeX-clean-confirm nil
        ;; Automatically insert braces after typing ^ and _ in math mode
        TeX-electric-sub-and-superscript t
        TeX-electric-math t ; Inserting $ completes the math mode and positions the cursor
        TeX-parse-self t ; Parse documents
        TeX-quote-after-quote nil ; Allow original LaTeX quotes
        TeX-save-query nil ; Save buffers automatically when compiling
        TeX-source-correlate-method 'synctex
        ;; Do not start the emacs server when correlating sources
        TeX-source-correlate-start-server t
        TeX-syntactic-comment t
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        LaTeX-item-indent 0 ; Indent lists by two spaces
        LaTeX-syntactic-comments t
        LaTeX-fill-break-at-separators nil ; Do not insert line-break at inline math
        tex-fontify-script nil ; Avoid raising of superscripts and lowering of subscripts
        ;; Avoid superscripts and subscripts from being displayed in a different font size
        font-latex-fontify-script nil
        ;; Avoid emphasizing section headers
        font-latex-fontify-sectioning 1.0)

  (setq-default TeX-master nil) ; Query for master file

  ;; Revert PDF buffer after TeX compilation has finished
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)

  (unbind-key "C-c ;" TeX-mode-map)
  (unbind-key "C-c C-d" TeX-mode-map)
  (bind-key "$" #'self-insert-command TeX-mode-map)

  :bind
  ("C-c x q" . TeX-insert-quote))

(use-package bibtex
  :straight (:type built-in)
  :hook
  ((bibtex-mode-hook . turn-on-auto-revert-mode)
   (bibtex-mode-hook . lsp-deferred))
  :custom
  (bibtex-align-at-equal-sign     t)
  (bibtex-maintain-sorted-entries t)
  (bibtex-comma-after-last-field  nil))

(use-package ivy-bibtex
  :if (eq sb/minibuffer-completion 'ivy)
  :defines (ivy-bibtex-default-action
            bibtex-completion-cite-default-as-initial-input
            bibtex-completion-cite-prompt-for-optional-arguments
            bibtex-completion-display-formats)
  :bind ("C-c x b" . ivy-bibtex)
  :custom (ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  :config
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

  (require 'bibtex-completion)

  (setq bibtex-completion-cite-default-as-initial-input t
        bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-display-formats
        '((t . "${author:24} ${title:*} ${=key=:16} ${=type=:12}"))))

;; Reftex is useful to view ToC even with LSP support
;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493

(use-package reftex
  :straight (:type built-in)
  :commands (reftex-get-bibfile-list bibtex-parse-keys
                                     reftex-mode
                                     reftex-toc-rescan
                                     reftex-toc-Rescan
                                     reftex-default-bibliography)
  :diminish
  :hook ((LaTeX-mode-hook latex-mode-hook) . reftex-mode)
  :bind
  (("C-c ["   . reftex-citation)
   ("C-c )"   . reftex-reference)
   ("C-c ("   . reftex-label)
   ("C-c ="   . reftex-toc))
  :preface
  (defun sb/get-bibtex-keys (file)
    (with-current-buffer (find-file-noselect file)
      (mapcar 'car (bibtex-parse-keys))))

  (defun sb/reftex-add-all-bibitems-from-bibtex ()
    (interactive)
    (mapc 'LaTeX-add-bibitems
          (apply 'append
                 (mapcar 'sb/get-bibtex-keys (reftex-get-bibfile-list)))))

  (defun sb/find-bibliography-file ()
    "Try to find a bibliography file using RefTeX.
      Returns a string with text properties (as expected by read-file-name) or
empty string if no file can be found"
    (interactive)
    (let ((bibfile-list nil))
      (condition-case nil
          (setq bibfile-list (reftex-get-bibfile-list))
        (error (ignore-errors
                 (setq bibfile-list (reftex-default-bibliography)))))
      (if bibfile-list
          (car bibfile-list) "")))

  (defun sb/reftex-try-add-all-bibitems-from-bibtex ()
    "Try to find a bibliography file using RefTex and parse the bib keys.
Ignore if no file is found."
    (interactive)
    (let ((bibfile-list nil))
      (condition-case nil
          (setq bibfile-list (reftex-get-bibfile-list))
        (error (ignore-errors
                 (setq bibfile-list (reftex-default-bibliography)))))
      ;; (message "%s" bibfile-list)
      (mapc 'LaTeX-add-bibitems
            (apply 'append
                   (mapcar 'sb/get-bibtex-keys bibfile-list)))))
  :custom
  (reftex-enable-partial-scans t)
  (reftex-highlight-selection 'both)
  (reftex-plug-into-AUCTeX t)
  (reftex-save-parse-info t "Save parse info to avoid reparsing every time a file is visited")
  (reftex-toc-follow-mode t "Other buffer follows the point in TOC buffer")
  ;; Make the toc display with a vertical split, since it is easy to read long lines
  (reftex-toc-split-windows-horizontally nil)
  (reftex-toc-split-windows-fraction 0.6 "Give TOC buffer more room")
  ;; (reftex-guess-label-type t "Try to guess the label type before prompting")
  (reftex-use-fonts t "Use nice fonts for TOC")
  ;; (reftex-revisit-to-follow t "Revisit files if necessary when browsing toc")
  ;; (reftex-auto-recenter-toc t "Center on the section currently being edited")
  ;; (reftex-use-multiple-selection-buffers t "Cache selection buffers for faster access")
  :config
  ;; (sb/reftex-try-add-all-bibitems-from-bibtex)
  ;; (add-hook 'reftex-load-hook #'sb/reftex-add-all-bibitems-from-bibtex)

  (with-eval-after-load "reftex-toc"
    (bind-keys :package reftex-toc
               :map reftex-toc-mode-map
               ("n" . reftex-toc-next)
               ("p" . reftex-toc-previous)
               ("r" . reftex-toc-rescan)
               ("R" . reftex-toc-Rescan)
               ("g" . revert-buffer)
               ("q" . reftex-toc-quit)
               ("z" . reftex-toc-jump)
               (">" . reftex-toc-demote)
               ("<" . reftex-toc-promote))

    ;; Rescan the entire document, not only the current file (`reftex-toc-rescan'), to be consistent
    ;; but this is expensive.
    (add-hook 'reftex-toc-mode-hook #'reftex-toc-rescan)))

(use-package bib-cite
  :straight auctex
  :diminish bib-cite-minor-mode
  :hook
  ((LaTeX-mode-hook latex-mode-hook) . (lambda()
                                         (bib-cite-minor-mode 1)))
  ;; :bind
  ;; (:map bib-cite-minor-mode-map
  ;;       ("C-c b"   . nil) ; We use `C-c b' for `comment-box'
  ;;       ("C-c l a" . bib-apropos)
  ;;       ("C-c l b" . bib-make-bibliography)
  ;;       ("C-c l d" . bib-display)
  ;;       ("C-c l t" . bib-etags)
  ;;       ("C-c l f" . bib-find)
  ;;       ("C-c l n" . bib-find-next))
  :custom
  (bib-cite-use-reftex-view-crossref t "Use RefTeX functions for finding bibliography files"))

;; TODO: https://github.com/tom-tan/auctex-latexmk/pull/40
(use-package auctex-latexmk
  :after tex-mode
  :straight (auctex-latexmk :type git :host github :repo "wang1zhen/auctex-latexmk")
  :demand t
  :commands (auctex-latexmk-setup auctex-latexmk)
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t "Pass the '-pdf' flag when `TeX-PDF-mode' is active")
  (TeX-command-default "LatexMk")
  :config
  (auctex-latexmk-setup))

;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
(declare-function TeX-active-process "tex.el")

(defun sb/save-buffer-and-run-latexmk ()
  "Save the current buffer and run LaTeXMk also."
  (interactive)
  (require 'tex)
  ;; (require 'tex-buf)
  ;; Kill any active compilation process
  (let ((process (TeX-active-process)))
    (if process (delete-process process)))
  (let ((TeX-save-query nil))
    (TeX-save-document ""))
  (TeX-command-menu "LaTeXMk"))

(declare-function TeX-run-TeX "tex")

(defun sb/latex-compile-open-pdf ()
  "Helper function to compile files.
Save the current buffer, run LaTeXMk, and switch to the PDF
after a successful compilation."
  (interactive)
  (let ((TeX-save-query nil)
        (process (TeX-active-process))
        (TeX-process-asynchronous nil)
        (master-file (TeX-master-file)))
    (if process (delete-process process))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk" "latexmk -pdf" master-file)
    (if (plist-get TeX-error-report-switches (intern master-file))
        (TeX-next-error t)
      (progn
        (minibuffer-message "LaTeXMk done")
        (when (display-graphic-p)
          (find-file (concat (file-name-directory (concat master-file ".tex"))
                             (concat master-file ".pdf"))))))))

;; (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
;;   (add-hook hook
;;             (lambda ()
;;               (add-hook 'after-save-hook
;;                         (lambda ()
;;                           (sb/save-buffer-and-run-latexmk)) nil t))))

;; (with-eval-after-load "tex-mode"
;;   (defvar latex-mode-map)
;;   (bind-key "C-x C-s" #'sb/latex-compile-open-pdf latex-mode-map))

(with-eval-after-load "latex"
  (defvar LaTeX-mode-map)

  ;; Disable `LaTeX-insert-item' in favor of `imenu'
  (unbind-key "C-c C-j" LaTeX-mode-map)

  ;; (unbind-key "C-c C-d" LaTeX-mode-map)
  ;; Unset "C-c ;" since we want to bind it to 'comment-line
  ;; (unbind-key "C-c ;" LaTeX-mode-map)

  (bind-key "C-c x q" #'TeX-insert-quote LaTeX-mode-map)
  (bind-key "C-x C-s" #'sb/latex-compile-open-pdf LaTeX-mode-map))

;; `math-preview' requires external nodejs program "math-preview". Make sure that "math-preview" is
;; in "$PATH".
(use-package math-preview
  :straight (math-preview :type git :host gitlab :repo "matsievskiysv/math-preview")
  :commands (math-preview-all math-preview-at-point math-preview-region)
  :custom
  (math-preview-command (expand-file-name "node_modules/.bin/math-preview"
                                          sb/user-tmp-directory)))

(provide 'init-latex)

;;; init-latex.el ends here
