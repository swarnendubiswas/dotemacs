;;; latex-init.el --- Part of emacs initialization

;;; Commentary:
;; Configure latex mode.

;;; Code:

(use-package auctex
  :ensure t
  :defer t)

(use-package auctex-latexmk
  :ensure t
  :defer t)

(use-package latex-extra
  :ensure t
  :disabled t
  :config
  (latex/setup-keybinds))

(use-package latex-pretty-symbols
  :ensure t
  :disabled t)

(use-package latex-preview-pane
  :ensure t
  :disabled t)

(use-package latex-math-preview
  :ensure t
  :disabled t)

(use-package magic-latex-buffer
  :ensure t
  :disabled t)

(use-package math-symbol-lists
  :ensure t
  :defer t)

(use-package bibtex-utils
  :ensure t
  :defer t)

(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)

;;(add-hook 'LaTeX-mode-hook 'latex-extra-mode)
;;(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'reftex-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
;;(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
(add-hook 'LaTeX-mode-hook #'writegood-mode)
(add-hook 'LaTeX-mode-hook #'abbrev-mode)
;;(add-hook 'LaTeX-mode-hook (lambda () (yas-reload-all)))
;;(add-hook 'LaTeX-mode-hook '(lambda () (yas-minor-mode)))
(add-hook 'LaTeX-mode-hook #'fci-mode)
(add-hook 'LaTeX-mode-hook (lambda () (TeX-PDF-mode 1))) ; compile files to pdf by default
;;(add-hook 'LaTeX-mode-hook #'auto-highlight-symbol-mode) ; highlight symbol at point

(setq TeX-auto-save t ; enable parse on save, stores parsed information in an "auto" directory
      TeX-parse-self t ; enable parse on load
      TeX-electric-sub-and-superscript t ; automatically insert braces in math mode
      TeX-force-default-mode t ; always use `TeX-default-mode', which defaults to `latex-mode'
      TeX-auto-untabify t
      latex-run-command "latexmk")
(setq-default TeX-master nil ; query for master file
              TeX-command-default "LatexMk")

(auctex-latexmk-setup) ; add support for latexmk

(setq reftex-plug-into-AUCTeX t
      reftex-cite-format 'abbrv
      reftex-save-parse-info t
      reftex-use-multiple-selection-buffers t
      reftex-enable-partial-scans t)

(eval-after-load "reftex"
  '(diminish 'reftex-mode))

;;(latex-preview-pane-enable) ; current does not support multi-file parsing

(setq TeX-source-correlate-method 'synctex)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(provide 'latex-init)

;;; latex-init.el ends here
