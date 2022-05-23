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

;; (use-package lsp-latex
;;   :defines (lsp-latex-bibtex-formatter lsp-latex-latex-formatter
;;                                        lsp-latex-bibtex-formatter-line-length
;;                                        lsp-latex-chktex-on-open-and-save
;;                                        lsp-latex-build-on-save
;;                                        lsp-latex-build-is-continuous
;;                                        lsp-latex-build-args
;;                                        lsp-latex-diagnostics-delay)
;;   :hook
;;   (latex-mode-hook . (lambda()
;;                        (require 'lsp-latex)
;;                        (lsp-deferred)))
;;   :custom
;;   (lsp-latex-bibtex-formatter             "latexindent")
;;   (lsp-latex-latex-formatter              "latexindent")
;;   (lsp-latex-bibtex-formatter-line-length sb/fill-column)
;;   (lsp-latex-chktex-on-open-and-save      t)
;;   (lsp-latex-build-is-continuous          t)
;;   ;; Delay time in milliseconds before reporting diagnostics
;;   (lsp-latex-diagnostics-delay            2000)
;;   :config
;;   (add-to-list 'lsp-latex-build-args "-c")
;;   (add-to-list 'lsp-latex-build-args "-pvc")

;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-tramp-connection "texlab")
;;     :major-modes '(tex-mode latex-mode LaTeX-mode bibtex-mode)
;;     :remote? t
;;     :server-id 'texlab-r)))

;; Auctex provides enhanced versions of `tex-mode' and `latex-mode', which automatically replace the
;; vanilla ones. Auctex provides `LaTeX-mode', which is an alias to `latex-mode'. Auctex overrides
;; the tex package.

(progn
  (eval-when-compile
    (if (bound-and-true-p sb/disable-package.el)
        (use-package tex
          :straight auctex)
      (use-package tex
        :ensure auctex)))

  (declare-function LaTeX-math-mode "tex")
  (declare-function TeX-PDF-mode "tex")
  (declare-function TeX-source-correlate-mode "tex")
  (declare-function TeX-save-document "tex")
  (declare-function TeX-command-menu "tex")
  (declare-function TeX-revert-document-buffer "tex")

  (unless (fboundp 'tex-site)
    (autoload 'tex-site "tex-site.el" nil t))
  (unless (fboundp 'LaTeX-mode)
    (autoload #'LaTeX-mode "tex" nil t))
  (unless (fboundp 'LaTeX-math-mode)
    (autoload #'LaTeX-math-mode "tex" nil t))
  (unless (fboundp 'TeX-PDF-mode)
    (autoload #'TeX-PDF-mode "tex" nil t))
  (unless (fboundp 'TeX-source-correlate-mode)
    (autoload #'TeX-source-correlate-mode "tex" nil t))
  (unless (fboundp 'rainbow-delimiters-mode)
    (autoload #'rainbow-delimiters-mode "tex" nil t))
  (unless (fboundp 'TeX-active-process)
    (autoload #'TeX-active-process "tex" nil t))
  (unless (fboundp 'TeX-save-document)
    (autoload #'TeX-save-document "tex" nil t))
  (unless (fboundp 'TeX-command-menu)
    (autoload #'TeX-command-menu "tex" nil t))
  (unless (fboundp 'TeX-revert-document-buffer)
    (autoload #'TeX-revert-document-buffer "tex" nil t))

;;;###autoload
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

  (dolist (hook '(LaTex-mode-hook latex-mode-hook))
    (add-hook hook (lambda()
                     (LaTeX-math-mode)
                     (TeX-PDF-mode) ; Use `pdflatex'
                     (TeX-source-correlate-mode)
                     (turn-on-auto-fill))))

  (with-eval-after-load "tex"
    (defvar TeX-auto-save)
    (defvar TeX-auto-untabify)
    (defvar TeX-clean-confirm)
    (defvar TeX-electric-sub-and-superscript)
    (defvar TeX-parse-self)
    (defvar TeX-quote-after-quote)
    (defvar TeX-save-query)
    (defvar TeX-source-correlate-method)
    (defvar TeX-source-correlate-start-server)
    (defvar TeX-syntactic-comment)
    (defvar TeX-view-program-selection)
    (defvar TeX-view-program-list)
    (defvar LaTeX-item-indent)
    (defvar LaTeX-syntactic-comments)
    (defvar LaTeX-fill-break-at-separators)
    (defvar tex-fontify-script)
    (defvar font-latex-fontify-script)
    (defvar font-latex-fontify-sectioning)

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

    (unbind-key "C-c ;" TeX-mode-map)))


(progn
  (add-hook 'bibtex-mode-hook #'lsp-deferred)
  (add-hook 'bibtex-mode-hook #'turn-on-auto-revert-mode)

  (with-eval-after-load "bibtex"
    (defvar bibtex-align-at-equal-sign)
    (defvar bibtex-maintain-sorted-entries)

    (setq bibtex-align-at-equal-sign t
          bibtex-maintain-sorted-entries t)))

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

(progn
  (declare-function reftex-reference "reftex")
  (declare-function reftex-label "reftex")
  (declare-function reftex-get-bibfile-list "reftex")
  (declare-function reftex-default-bibliography "reftex")
  (declare-function bibtex-parse-keys "reftex")
  (declare-function bib-apropos "reftex")
  (declare-function bib-make-bibliography "reftex")
  (declare-function bib-display "reftex")
  (declare-function bib-etags "reftex")
  (declare-function bib-find "reftex")
  (declare-function bib-find-next "reftex")
  (declare-function bib-highlight-mouse "reftex")

  (unless (fboundp 'reftex-mode)
    (autoload #'reftex-mode "reftex" nil t))
  (unless (fboundp 'reftex-citation)
    (autoload #'reftex-citation "reftex" nil t))
  (unless (fboundp 'reftex-reference)
    (autoload #'reftex-reference "reftex" nil t))
  (unless (fboundp 'reftex-label)
    (autoload #'reftex-label "reftex" nil t))
  (unless (fboundp 'reftex-get-bibfile-list)
    (autoload #'reftex-get-bibfile-list "reftex" nil t))
  (unless (fboundp 'bibtex-parse-keys)
    (autoload #'bibtex-parse-keys "reftex" nil t))
  (unless (fboundp 'reftex-default-bibliography)
    (autoload #'reftex-default-bibliography "reftex" nil t))
  (unless (fboundp 'reftex-toc)
    (autoload #'reftex-toc "reftex" nil t))

  ;; http://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs/11660493#11660493
  (eval-and-compile
    (defun sb/get-bibtex-keys (file)
      (with-current-buffer
          (find-file-noselect file)
        (mapcar 'car
                (bibtex-parse-keys))))

    (defun sb/reftex-add-all-bibitems-from-bibtex nil
      (interactive)
      (mapc 'LaTeX-add-bibitems
            (apply 'append
                   (mapcar 'sb/get-bibtex-keys
                           (reftex-get-bibfile-list)))))

    (defun sb/find-bibliography-file nil
      "Try to find a bibliography file using RefTeX.
Returns a string with text properties (as expected by
read-file-name) or empty string if no file can be found"
      (interactive)
      (let
          ((bibfile-list nil))
        (condition-case nil
            (setq bibfile-list
                  (reftex-get-bibfile-list))
          (error
           (ignore-errors
             (setq bibfile-list
                   (reftex-default-bibliography)))))
        (if bibfile-list
            (car bibfile-list)
          "")))

    (defun sb/reftex-try-add-all-bibitems-from-bibtex nil
      "Try to find a bibliography file using RefTex and parse the bib keys.
Ignore if no file is found."
      (interactive)
      (let
          ((bibfile-list nil))
        (condition-case nil
            (setq bibfile-list
                  (reftex-get-bibfile-list))
          (error
           (ignore-errors
             (setq bibfile-list
                   (reftex-default-bibliography)))))
        (mapc 'LaTeX-add-bibitems
              (apply 'append
                     (mapcar 'sb/get-bibtex-keys bibfile-list))))))

  (dolist (hook '(LaTeX-mode-hook latex-mode-hook))
    (add-hook hook #'reftex-mode))

  (with-eval-after-load "reftex"
    (defvar reftex-enable-partial-scans)
    (defvar reftex-highlight-selection)
    (defvar reftex-plug-into-AUCTeX)
    (defvar reftex-save-parse-info)
    (defvar reftex-toc-follow-mode)
    (defvar reftex-use-multiple-selection-buffers)
    (defvar reftex-toc-split-windows-horizontally)
    (defvar reftex-toc-split-windows-fraction)
    (defvar reftex-guess-label-type)
    (defvar reftex-use-fonts)
    (defvar reftex-auto-recenter-toc)
    (defvar reftex-revisit-to-follow)

    (setq reftex-enable-partial-scans t
          reftex-highlight-selection 'both
          reftex-plug-into-AUCTeX t
          ;; Save parse info to avoid reparsing every time a file is visited
          reftex-save-parse-info t
          ;; reftex-toc-follow-mode t ; Other buffer follows the point in toc buffer
          ;; Cache selection buffers for faster access
          reftex-use-multiple-selection-buffers t
          ;; Make the toc display with a vertical split, since it is easy to read long lines
          reftex-toc-split-windows-horizontally nil
          ;; Adjust the fraction
          reftex-toc-split-windows-fraction 0.3
          ;; reftex-guess-label-type t ; Try to guess the label type before prompting
          ;; reftex-use-fonts t ; Use nice fonts for toc
          ;; reftex-revisit-to-follow t ; Revisit files if necessary when browsing toc
          ;; reftex-auto-recenter-toc t ; Center on the section currently being edited
          )

    ;; (sb/reftex-try-add-all-bibitems-from-bibtex)

    ;; (add-hook 'reftex-load-hook #'sb/reftex-add-all-bibitems-from-bibtex)
    ;; TODO: Rescan the entire document, not only the current file (`reftex-toc-rescan'), to be
    ;; consistent but this is expensive. We can use an idle timer.
    ;; (add-hook 'reftex-toc-mode-hook #'reftex-toc-rescan)
    ;; (add-hook 'reftex-toc-mode-hook #'reftex-toc-Rescan)

    (diminish 'reftex-mode))

  (bind-keys :package reftex
             ("C-c [" . reftex-citation)
             ("C-c )" . reftex-reference)
             ("C-c (" . reftex-label)
             ("C-c =" . reftex-toc)))

(progn
  (unless (fboundp 'bib-apropos)
    (autoload #'bib-apropos "bib-cite" nil t))
  (unless (fboundp 'bib-make-bibliography)
    (autoload #'bib-make-bibliography "bib-cite" nil t))
  (unless (fboundp 'bib-display)
    (autoload #'bib-display "bib-cite" nil t))
  (unless (fboundp 'bib-etags)
    (autoload #'bib-etags "bib-cite" nil t))
  (unless (fboundp 'bib-find)
    (autoload #'bib-find "bib-cite" nil t))
  (unless (fboundp 'bib-find-next)
    (autoload #'bib-find-next "bib-cite" nil t))
  (unless (fboundp 'bib-highlight-mouse)
    (autoload #'bib-highlight-mouse "bib-cite" nil t))
  (unless (fboundp 'bib-cite-minor-mode)
    (autoload #'bib-cite-minor-mode "bib-cite" nil t))

  (dolist (hook '(latex-mode-hook LaTex-mode-hook))
    (add-hook hook (lambda nil
                     (bib-cite-minor-mode 1))))

  (with-eval-after-load "bib-cite"
    (defvar bib-cite-use-reftex-view-crossref)
    (setq bib-cite-use-reftex-view-crossref t)

    (diminish 'bib-cite-minor-mode))

  (defvar bib-cite-minor-mode-map)
  (bind-keys :package bib-cite :map bib-cite-minor-mode-map
             ("C-c b") ; We use `C-c b' for `comment-box'
             ("C-c l a" . bib-apropos)
             ("C-c l b" . bib-make-bibliography)
             ("C-c l d" . bib-display)
             ("C-c l t" . bib-etags)
             ("C-c l f" . bib-find)
             ("C-c l n" . bib-find-next)
             ("C-c l h" . bib-highlight-mouse)))

;; TODO: https://github.com/tom-tan/auctex-latexmk/pull/40
;; We can disable this once `lsp-latex-build' works well
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

(with-eval-after-load "tex-mode"
  (defvar latex-mode-map)
  (bind-key "C-x C-s" #'sb/latex-compile-open-pdf latex-mode-map))

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
(progn
  (eval-when-compile
    (if (bound-and-true-p sb/disable-package.el)
        (use-package math-preview
          :straight (math-preview :type git :host gitlab :repo "matsievskiysv/math-preview"))
      (use-package math-preview
        :ensure nil
        :load-path "extras")))

  (declare-function math-preview-at-point "math-preview")
  (declare-function math-preview-region "math-preview")

  (unless (fboundp 'math-preview-all)
    (autoload #'math-preview-all "math-preview" nil t))
  (unless (fboundp 'math-preview-at-point)
    (autoload #'math-preview-at-point "math-preview" nil t))
  (unless (fboundp 'math-preview-region)
    (autoload #'math-preview-region "math-preview" nil t))

  (with-eval-after-load "math-preview"
    (setq math-preview-command (expand-file-name "node_modules/.bin/math-preview"
                                                 sb/user-tmp-directory))))

(provide 'init-latex)

;;; init-latex.el ends here
