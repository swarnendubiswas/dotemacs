  ;;; latex-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode. Auctex provides LaTeX-mode, which is an alias.

;;; Code:

;; Initialize auctex, auctex overrides the tex package
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode))

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

;; Disable "LaTeX-insert-item" in favor of imenu
(unbind-key "C-c C-d" LaTeX-mode-map)
;; Unset "C-c ;" since we want to bind it to 'comment-line
(unbind-key "C-c ;" LaTeX-mode-map)

(use-package auctex-latexmk
  :ensure t
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  (TeX-command-default "LaTeXMk")
  :config (auctex-latexmk-setup)
  :hook (LaTeX-mode-hook . (lambda()
                             (require 'auctex-latexmk))))

(use-package math-symbol-lists ; Required by ac-math and company-math
  :ensure t)

(use-package company-auctex
  :ensure t
  :config (company-auctex-init))

;; (use-package company-math
;;   :ensure t
;;   :disabled t
;;   :init
;;   (add-to-list 'company-backends
;;                '(company-math-symbols-latex company-latex-commands company-math-symbols-unicode)))

;; (use-package bibtex
;;   :init (add-hook 'bibtex-mode-hook #'turn-on-auto-revert-mode)
;;   :defer t
;;   :config
;;   (setq bibtex-maintain-sorted-entries t)
;;   (use-package bibtex-utils
;;     :ensure t))

(use-package reftex
  :diminish
  :commands (reftex-citation)
  :init
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
        reftex-use-external-file-finders t
        reftex-highlight-selection 'both)
  ;; (add-hook 'LaTeX-mode-hook #'reftex-mode)
  (add-hook 'latex-mode-hook #'reftex-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex)   ; Emacs latex mode

  )

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
  :init (add-hook 'reftex-load-hook #'reftex-add-all-bibitems-from-bibtex))

(use-package bib-cite
  :defer t
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

;; (use-package tex-smart-umlauts
;;   :ensure t
;;   :init (add-hook 'LaTeX-mode-hook #'tex-smart-umlauts-mode))

(use-package parsebib
  :ensure t)

(use-package company-bibtex
  :ensure t
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

;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-x C-s") #'dotemacs-save-buffer-and-run-latexmk)))

;; (defun dotemacs--company-LaTeX-backends ()
;;   "Add backends for LaTeX completion in company mode."
;;   (make-local-variable 'company-backends)
;;   (setq company-backends
;;         '((;; Generic backends
;;            company-auctex
;;            company-capf
;;            company-files
;;            company-keywords
;;            ;; LaTeX specific backends
;;            company-bibtex
;;            company-math-symbols-latex
;;            company-latex-commands
;;            company-math-symbols-unicode))))
;; (add-hook 'LaTeX-mode-hook #'dotemacs--company-LaTeX-backends)

(use-package company-auctex
  :after (company latex))

(use-package ivy-bibtex
  :ensure t
  :bind ("C-c x b" . ivy-bibtex)
  :config
  (use-package bibtex-completion
    :custom
    (bibtex-completion-cite-prompt-for-optional-arguments nil)
    (bibtex-completion-cite-default-as-initial-input t)
    (bibtex-completion-display-formats '((t . "${author:24} ${title:*} ${=key=:10} ${=type=:10}"))))
  :custom (ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

;; ;; http://tex.stackexchange.com/questions/64897/automatically-run-latex-command-after-saving-tex-file-in-emacs
;; (defun sb/save-buffer-and-run-latexmk ()
;;   "Save the current buffer and run LaTeXMk also."
;;   (interactive)
;;   (require 'tex-buf)
;;   (let ((process (TeX-active-process))) (if process (delete-process process)))
;;   (let ((TeX-save-query nil)) (TeX-save-document ""))
;;   (TeX-command-menu "LaTeXMk"))
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c x c") #'sb/save-buffer-and-run-latexmk)))
;; (add-hook 'latex-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c x c") #'sb/save-buffer-and-run-latexmk)))

(provide 'latex-init)

;;; latex-init.el ends here
