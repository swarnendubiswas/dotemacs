;;; latex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

;; AUCTeX's LaTeX mode is called LaTeX-mode, while latex-mode is the Emacs default.

(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

(use-package tex-site ; Initialize auctex
  :ensure auctex ; once installed, auctex overrides the tex package
  :mode ("\\.tex\\'" . LaTeX-mode))

(use-package tex-mode
  :config
  (setq TeX-auto-save t ; Enable parse on save, stores parsed information in an "auto" directory
        TeX-parse-self t ; Parse documents
        TeX-clean-confirm nil
        TeX-quote-after-quote nil ; Allow original LaTeX quotes
        TeX-electric-sub-and-superscript t ; Automatically insert braces in math mode
        TeX-auto-untabify t ; Remove all tabs before saving
        TeX-save-query nil
        LaTeX-syntactic-comments t)

  ;; (setq-default TeX-master nil) ; Query for master file

  ;; Provide forward "C-c C-v" (TeX-view) and inverse (C-Mouse-1, Ctrl + "Left Click") search with SyncTeX
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server 'ask)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

  ;; (add-to-list 'TeX-command-list
  ;;              '("PDFLaTeX" "%'pdflatex%(mode)%' %t" TeX-run-TeX nil t
  ;;                (plain-tex-mode tex-mode TeX-mode LaTeX-mode TeX-latex-mode docTeX-mode)
  ;;                :help "Run PDFLaTeX"))
  ;; (add-to-list 'TeX-command-list
  ;;              '("View" "%V" TeX-run-discard nil t))

  ;; (unbind-key "C-c C-d" LaTeX-mode-map)
  ;; ;; Unset "C-c ;" since we want to bind it to 'comment-line
  ;; (unbind-key "C-c ;" LaTeX-mode-map)

  ;; :bind
  ;; ;; Disable "LaTeX-insert-item" in favor of imenu
  ;; ("C-c C-j" . nil)

  )

(setq font-latex-fontify-script nil)

;; prettify-symbol-mode is distracting while editing, and is buffer-local.
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (global-prettify-symbols-mode -1)
            (prettify-symbols-mode -1)))

(use-package auctex-latexmk
  :ensure t
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-command-default "LaTeXMk"))))

(use-package company-auctex
  :ensure t
  :config (company-auctex-init))

(use-package company-math
  :ensure t
  :ensure math-symbol-lists ; Required by ac-math and company-math
  :config
  (add-to-list 'company-backends
               '(company-math-symbols-latex company-latex-commands company-math-symbols-unicode)))

(use-package tex-smart-umlauts
  :ensure t
  :hook LaTeX-mode-hook)

(use-package tex-mode
  :diminish latex-electric-env-pair-mode
  :init
  (setq latex-run-command "latexmk")
  (add-hook 'TeX-mode-hook
            (lambda()
              (latex-electric-env-pair-mode 1))))

(use-package reftex
  :diminish reftex-mode
  :commands (reftex-citation)
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-insert-label-flags '(t t)
        reftex-cite-format 'abbrv
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-auto-update-selection-buffers t
        reftex-enable-partial-scans t
        reftex-allow-automatic-rescan t
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

(use-package parsebib
  :ensure t)

(use-package bibtex
  :init (add-hook 'bibtex-mode-hook #'turn-on-auto-revert-mode)
  :defer t
  :config
  (setq bibtex-maintain-sorted-entries t)
  (use-package bibtex-utils
    :ensure t))

(use-package bib-cite
  :disabled t
  :diminish bib-cite-minor-mode
  :init  (add-hook 'LaTeX-mode-hook #'bib-cite-minor-mode)
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

(use-package parsebib
  :ensure t)

(use-package bibtex-utils
  :ensure t)

(use-package bibtex-completion
  :config
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-cite-default-as-initial-input t
        bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:10}"))))

(use-package ivy-bibtex
  :ensure t
  :bind ("C-c l x" . ivy-bibtex)
  :config (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package company-bibtex
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-bibtex))

;; ;; https://rtime.felk.cvut.cz/~sojka/blog/compile-on-save/
;; ;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (string= major-mode "latex-mode")
;;               (TeX-command-menu "LaTeXMk")
;;               (revert-buffer :ignore-auto :noconfirm)
;;               (find-alternate-file (current-buffer)))))

;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (string= major-mode "latex-mode")
;;               (let ((process (TeX-active-process))) (if process (delete-process process)))
;;               (revert-buffer :ignore-auto :noconfirm)
;;               (find-alternate-file (current-buffer))
;;               (TeX-command-menu "LaTeXMk"))))

;; (defun run-latexmk ()
;;   (when (string= major-mode "latex-mode")
;;     (let ((TeX-save-query nil)
;;           (TeX-process-asynchronous t)
;;           (master-file (TeX-master-file)))
;;       (TeX-save-document "")
;;       (TeX-run-TeX "LaTexmk"
;;                    (TeX-command-expand "latexmk -pdf %t" 'TeX-master-file)
;;                    master-file)
;;       (if (plist-get TeX-error-report-switches (intern master-file))
;;           (TeX-next-error t)
;;         (minibuffer-message "LaTeXMk done")))))
;; (add-hook 'after-save-hook #'run-latexmk)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-s") #'sb/save-buffer-and-run-latexmk)))

;; ;; https://github.com/expez/.emacs.d/blob/master/lisp/init-latex.el
;; (defadvice LaTeX-insert-item (after remove-whitespace-first-item activate)
;;   "This advice is meant to fix the issue where an extra blank
;; line is naively added by `LaTeX-insert-item' when not already on
;; an item line."
;;   (check-item-entry))

(defun sb/company-LaTeX-backends ()
  "Add backends for LaTeX completion in company mode."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((;; Generic backends
           company-dabbrev
           company-files
           company-keywords
           company-capf
           company-dict
           ;; company-gtags
           ;; LaTeX specific backends
           company-bibtex
           company-auctex
           company-math-symbols-latex
           company-latex-commands
           company-math-symbols-unicode))))
(add-hook 'LaTeX-mode-hook #'sb/company-LaTeX-backends)

(require 'smartparens-latex)

(provide 'latex-init)

;;; latex-new-init.el ends here
