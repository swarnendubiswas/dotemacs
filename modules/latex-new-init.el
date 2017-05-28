;;; latex-new-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-completion-in-buffer)

(defconst bibs-list '("~/plass-workspace/bib/plass-formatted.bib"
                      "~/iss-workspace/papers/approximate-bib/paper.bib"))

;; prettify-symbol-mode is distracting while editing, and is buffer-local.
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (global-prettify-symbols-mode -1)
            (prettify-symbols-mode -1)))

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
        reftex-highlight-selection 'both
        reftex-default-bibliography bibs-list)
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

(use-package bibtex-completion
  :if (or (eq dotemacs-selection 'helm) (eq dotemacs-selection 'ivy))
  :config
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-cite-default-as-initial-input t
        bibtex-completion-bibliography bibs-list))

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
  (setq company-bibtex-bibliography bibs-list))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-s") #'dotemacs-save-buffer-and-run-latexmk)))

(provide 'latex-new-init)

;;; latex-new-init.el ends here
