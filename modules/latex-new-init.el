;;; latex-new-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-completion-in-buffer)

(use-package tex-site ; Initialize auctex
  :ensure auctex ; once installed, auctex overrides the tex package
  :mode ("\\.tex\\'" . LaTeX-mode))

(use-package tex-mode
  :bind
  ;; Disable "LaTeX-insert-item" in favor of imenu
  ("C-c C-j" . nil))

;; prettify-symbol-mode is distracting while editing, and is buffer-local.
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (global-prettify-symbols-mode -1)
            (prettify-symbols-mode -1)))

(use-package auctex-latexmk
  :ensure t
  :defer t
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-command-default "LaTeXMk"))))

;; Required by ac-math and company-math
(use-package math-symbol-lists
  :ensure t
  :defer t)

(when (eq dotemacs-completion-in-buffer 'company)
  (use-package company-auctex
    :ensure t
    :defer t
    :config (company-auctex-init))

  (use-package company-math
    :ensure t
    :defer t
    :config
    (add-to-list 'company-backends
                 '(company-math-symbols-latex company-latex-commands company-math-symbols-unicode))))

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
  :ensure t
  :defer t)

(use-package bibtex-completion
  :if (or (eq dotemacs-selection 'helm) (eq dotemacs-selection 'ivy))
  :defer t
  :config
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-cite-default-as-initial-input t))

(use-package helm-bibtex
  :ensure t
  :if (eq dotemacs-selection 'helm)
  :bind ("C-c l x" . helm-bibtex)
  :config
  (helm-delete-action-from-source "Insert BibTeX key" helm-source-bibtex)
  (helm-add-action-to-source "Insert BibTeX key" 'bibtex-completion-insert-key helm-source-bibtex 0)
  (setq helm-bibtex-full-frame t))

(use-package ivy-bibtex
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :bind ("C-c l x" . ivy-bibtex)
  :config (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key))

(use-package company-bibtex
  :ensure t
  :defer t
  :if (eq dotemacs-completion-in-buffer 'company)
  :init (add-to-list 'company-backends 'company-bibtex))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-s") #'dotemacs-save-buffer-and-run-latexmk)))

(provide 'latex-new-init)

;;; latex-new-init.el ends here
