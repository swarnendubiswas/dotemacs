;;; latex-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

;; http://stackoverflow.com/questions/17777189/what-is-the-difference-of-tex-mode-and-latex-mode-and-latex-mode-in-emacs
(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))

(use-package simple
  :config (add-hook 'LaTeX-mode-hook #'turn-on-auto-fill))

(use-package tex
  :ensure auctex
  :commands TeX-PDF-mode
  :config
  (setq TeX-auto-save t ; enable parse on save, stores parsed information in an "auto" directory
        TeX-parse-self t ; Parse documents
        TeX-electric-sub-and-superscript t ; automatically insert braces in math mode
        TeX-default-mode 'LaTeX-mode
        TeX-force-default-mode t
        TeX-auto-untabify t ; remove all tabs before saving
        TeX-source-correlate-method 'synctex ;; Provide forward and inverse search with SyncTeX
        TeX-source-correlate-mode t)
  (setq-default TeX-master nil) ; query for master file

  ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Anonymous-Functions.html
  ;; When defining a lambda expression that is to be used as an anonymous function, you can in principle use any method
  ;; to construct the list. But typically you should use the lambda macro, or the function special form, or the #' read
  ;; syntax which is a short-hand for using function. Quoting a lambda form means the anonymous function is not
  ;; byte-compiled. The following forms are all equivalent:
  ;; (lambda (x) (* x x))
  ;; (function (lambda (x) (* x x)))
  ;; #'(lambda (x) (* x x))

  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda ()
  ;;             (push
  ;;              '("Latexmk" "latexmk -pdf %s" TeX-run-background nil t
  ;;                :help "Run Latexmk on file")
  ;;              TeX-command-list)))

  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  ;; compile files to pdf by default
  (add-hook 'LaTeX-mode-hook #'TeX-PDF-mode))

(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode))

(use-package tex-mode
  :ensure auctex
  :commands latex-mode
  :config
  (setq latex-run-command "latexmk")
  (add-hook 'LaTeX-mode-hook
            (lambda()
              (latex-electric-env-pair-mode 1))))

(use-package latex
  :ensure auctex
  :config
  (setq LaTeX-syntactic-comments t)
  ;;(add-hook 'LaTeX-mode-hook #'latex-extra-mode)
  ;;(add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

  ;; (with-eval-after-load "LaTeX"
  ;;   (define-key LaTeX-mode-map (kbd "C-c C-d") nil))
  ;; (with-eval-after-load "LaTeX"
  ;;   (define-key LaTeX-mode-map (kbd "C-c C-d") 'duplicate-thing))
  ;;(bind-key "C-c C-d" 'duplicate-thing LaTeX-mode-map)

  ;; unset "C-c ;" since we want to bind it to 'comment-line
  (define-key LaTeX-mode-map (kbd "C-c ;") nil))

(use-package company-auctex
  :ensure t
  :if (eq dotemacs-completion 'company)
  :init
  ;; https://github.com/syl20bnr/spacemacs/blob/b89ce54df3b7df6284cae56f115db75a83721e17/contrib/auctex/packages.el
  ;; (with-eval-after-load "tex"
  ;;   (push 'company-auctex-labels company-backends-LaTeX-mode)
  ;;   (push 'company-auctex-bibs company-backends-LaTeX-mode)
  ;;   (push '(company-auctex-macros company-auctex-symbols company-auctex-environments)
  ;;         company-backends-LaTeX-mode))
  :config (company-auctex-init))

(use-package auctex-latexmk
  :ensure t
  :config
  (with-eval-after-load "LaTeX"
    (auctex-latexmk-setup)
    ;; this variable is buffer-local
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq TeX-command-default "LatexMk")))))

(use-package latex-extra
  :disabled t
  :ensure t
  :config (latex/setup-keybinds))

(use-package latex-pretty-symbols
  :disabled t
  :ensure t)

;; currently does not support multi-file parsing
(use-package latex-preview-pane
  :disabled t
  :ensure t
  :config (latex-preview-pane-enable))

(use-package latex-math-preview
  :disabled t
  :ensure t)

(use-package magic-latex-buffer
  :disabled t
  :ensure t
  :config (add-hook 'LaTeX-mode-hook #'magic-latex-buffer))

(use-package math-symbol-lists
  :ensure t)

(use-package bibtex
  :commands bibtex-mode
  :config
  (setq bibtex-maintain-sorted-entries t)
  (use-package bibtex-utils
    :ensure t))

(use-package reftex
  ;; :init
  ;; (autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
  ;; (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
  ;; (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
  :diminish reftex-mode
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-insert-label-flags '(t t)
        reftex-cite-format 'abbrv
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-enable-partial-scans t
        reftex-default-bibliography '("~/workspace/bib/plass.bib"))
  ;; (with-eval-after-load "reftex"
  ;;   (diminish 'reftex-mode))
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'reftex-mode))

(use-package ebib
  :disabled t
  :ensure t
  :bind ("C-c e" . ebib))

(use-package tex-smart-umlauts
  :ensure t)

(provide 'latex-init)

;;; latex-init.el ends here
