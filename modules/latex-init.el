;;; latex-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t ; enable parse on save, stores parsed information in an "auto" directory
        TeX-parse-self t ; Parse documents
        TeX-electric-sub-and-superscript t ; automatically insert braces in math mode
        TeX-default-mode 'latex-mode
        TeX-force-default-mode t 
        TeX-auto-untabify t
        TeX-source-correlate-method 'synctex ;; Provide forward and inverse search with SyncTeX
        TeX-source-correlate-mode t)
  (setq-default TeX-master nil ; query for master file
                TeX-command-default "LatexMk")
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; compile files to pdf by default
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))

(use-package tex-mode
  :ensure auctex
  :config
  (setq latex-run-command "latexmk"))

(use-package latex
  :ensure auctex
  :config
  (latex-electric-env-pair-mode 1)
  ;;(add-hook 'LaTeX-mode-hook 'latex-extra-mode)
  ;;(add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))

(eval-after-load 'LaTeX
  '(define-key LaTeX-mode-map (kbd "C-c C-d") nil))
(eval-after-load 'LaTeX
  '(define-key LaTeX-mode-map (kbd "C-c C-d") 'duplicate-thing))

(use-package auctex-latexmk
  :ensure t
  :config (with-eval-after-load 'latex
            (auctex-latexmk-setup)))

(use-package latex-extra
  :ensure t
  :disabled t
  :config
  (latex/setup-keybinds))

(use-package latex-pretty-symbols
  :ensure t
  :disabled t)

;; currently does not support multi-file parsing
(use-package latex-preview-pane
  :ensure t
  :disabled t
  :config (latex-preview-pane-enable))

(use-package latex-math-preview
  :ensure t
  :disabled t)

(use-package magic-latex-buffer
  :ensure t
  :disabled t
  :config (add-hook 'LaTeX-mode-hook 'magic-latex-buffer))

(use-package math-symbol-lists
  :ensure t)

(use-package bibtex-utils
  :ensure t
  :defer 10)

;; (autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
;; (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
;; (autoload 'reftex-citation "reftex-cite" "Make citation" nil)

(use-package reftex
  :diminish reftex-mode
  :config
  (setq reftex-plug-into-AUCTeX t
        reftex-insert-label-flags '(t t)
        reftex-cite-format 'abbrv
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-enable-partial-scans t)
  ;;(bound-and-true-p reftex-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'reftex-mode))

;; (eval-after-load "reftex"
;;   '(diminish 'reftex-mode))

(use-package ebib
  :ensure t
  :bind ("C-c e" . ebib))

(provide 'latex-init)

;;; latex-init.el ends here
