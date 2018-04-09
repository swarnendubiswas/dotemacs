;;; latex-new-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

(defvar dotemacs-selection)
(defvar dotemacs-completion-in-buffer)

;; NOTE: AUCTeX's LaTeX mode is called LaTeX-mode, while latex-mode is the Emacs default.

(use-package tex-site ; Initialize auctex
  :ensure auctex ; once installed, auctex overrides the tex package
  :mode ("\\.tex\\'" . LaTeX-mode))

(use-package tex-mode
  :config
  ;; Provide forward "C-c C-v" (TeX-view) and inverse (C-Mouse-1, Ctrl + "Left Click") search with SyncTeX
  (setq TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server 'ask)
  (setq TeX-view-program-list '(("Evince" "evince --page-index=%(outpage) %o")))
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)

  ;; :bind
  ;; ;; Disable "LaTeX-insert-item" in favor of imenu
  ;; ("C-c C-j" . nil)

  )

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

;; Required by ac-math and company-math
(use-package math-symbol-lists
  :ensure t)

(use-package company-auctex
  :ensure t
  :if (bound-and-true-p dotemacs-completion-in-buffer)
  :config (company-auctex-init))

(use-package company-math
  :ensure t
  :if (bound-and-true-p dotemacs-completion-in-buffer)
  :config
  (add-to-list 'company-backends
               '(company-math-symbols-latex company-latex-commands company-math-symbols-unicode)))

(use-package reftex
  :diminish reftex-mode
  :commands (reftex-citation)
  :hook (LaTeX-mode . reftex-mode)
  ;; :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
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

(use-package bibtex-utils
  :ensure t)

(use-package bibtex-completion
  :if (eq dotemacs-selection 'ivy)
  :config
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-cite-default-as-initial-input t
        bibtex-completion-display-formats '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:10}"))))

(use-package ivy-bibtex
  :ensure t
  :if (eq dotemacs-selection 'ivy)
  :bind ("C-c l x" . ivy-bibtex)
  :config (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

(use-package company-bibtex
  :ensure t
  :if (bound-and-true-p dotemacs-completion-in-buffer)
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

(provide 'latex-new-init)

;;; latex-new-init.el ends here
