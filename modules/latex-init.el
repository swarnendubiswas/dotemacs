;;; latex-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Configure latex mode.

;;; Code:

(use-package auctex
  :ensure t
  :defer t)

(use-package auctex-latexmk
  :ensure t
  :defer t
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
  :config
  (latex-preview-pane-enable))

(use-package latex-math-preview
  :ensure t
  :disabled t)

(use-package magic-latex-buffer
  :ensure t
  :disabled t
  :config
  (add-hook 'LaTeX-mode-hook 'magic-latex-buffer))

(use-package math-symbol-lists
  :ensure t
  :defer t)

(use-package bibtex-utils
  :ensure t
  :defer t)

(add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-PDF-mode 1))) ; compile files to pdf by default

;;(add-hook 'LaTeX-mode-hook 'latex-extra-mode)
;;(add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(setq TeX-auto-save t ; enable parse on save, stores parsed information in an "auto" directory
      TeX-parse-self t ; Parse documents
      TeX-electric-sub-and-superscript t ; automatically insert braces in math mode
      TeX-force-default-mode t ; always use `TeX-default-mode', which defaults to `latex-mode'
      TeX-auto-untabify t
      latex-run-command "latexmk")

(setq-default TeX-master nil ; query for master file
              TeX-command-default "LatexMk")

;; (autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
;; (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
;; (autoload 'reftex-citation "reftex-cite" "Make citation" nil)

(use-package reftex
  :diminish reftex-mode
  :defer t
  :init
  (setq reftex-plug-into-AUCTeX t
        reftex-insert-label-flags '(t t)
        reftex-cite-format 'abbrv
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t
        reftex-enable-partial-scans t)
  :config (add-hook 'LaTeX-mode-hook #'turn-on-reftex))

;; (eval-after-load "reftex"
;;   '(diminish 'reftex-mode))

;; Provide forward and inverse search with SyncTeX
(setq TeX-source-correlate-method 'synctex
      TeX-source-correlate-mode t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(eval-after-load 'LaTeX
  '(define-key LaTeX-mode-map (kbd "C-c C-d") nil))
(eval-after-load 'LaTeX
  '(define-key LaTeX-mode-map (kbd "C-c C-d") 'duplicate-thing))

(use-package ebib
  :ensure t
  :bind ("C-c e" . ebib))

(provide 'latex-init)

;;; latex-init.el ends here
