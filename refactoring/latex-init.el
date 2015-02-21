;;; latex-init.el --- Part of emacs initialization

;;; Commentary:
;; Configure latex mode.

;;; Code:

(use-package auctex
             :ensure t
             :defer t
             )

(use-package latex-extra
             :ensure t
             :defer t
             )

(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook #'reftex-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'LaTeX-mode-hook 'magic-latex-buffer)
(add-hook 'LaTeX-mode-hook #'writegood-mode)
(add-hook 'LaTeX-mode-hook #'abbrev-mode)
(add-hook 'LaTeX-mode-hook (lambda () (yas-reload-all)))
(add-hook 'LaTeX-mode-hook '(lambda () (yas-minor-mode)))
(add-hook 'LaTeX-mode-hook #'fci-mode)
(add-hook 'LaTeX-mode-hook #'TeX-PDF-mode) ; compile files to pdf by default
(add-hook 'LaTeX-mode-hook #'auto-highlight-symbol-mode) ; highlight symbol at point

(setq TeX-auto-save t ; enable parse on save, stores parsed information in an "auto" directory
      TeX-parse-self t ; enable parse on load
      TeX-electric-sub-and-superscript t ; automatically insert braces in math mode
      TeX-force-default-mode t ; always use `TeX-default-mode', which defaults to `latex-mode'
      reftex-plug-into-AUCTeX t
      )
(setq-default TeX-master nil) ; query for master file

(auctex-latexmk-setup) ; add support for latexmk

(provide 'latex-init)

;;; latex-init.el ends here
