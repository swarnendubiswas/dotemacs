;;; lsp-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup LSP.

;;; Code:

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook (prog-mode . lsp)
  :config
  (setq lsp-auto-guess-root t
        lsp-enable-snippet t
        lsp-enable-completion-at-point t
        lsp-enable-xref t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init (add-hook 'lsp-mode-hook #'lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet t
        company-lsp-enable-recompletion t))

(use-package lsp-java
  :ensure t
  :after lsp
  :init
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-inhibit-message t))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java
  :after lsp-java)

(use-package lsp-java-treemacs
  :after (treemacs))

(use-package lsp-clangd
  :ensure t
  :after lsp
  :config
  (add-hook 'c-mode-hook #'lsp-clangd-c-enable)
  (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
  (setq lsp-clangd-executable "/usr/bin/clangd-7"))

(use-package lsp-html
  :ensure t
  :after lsp
  :config (add-hook 'html-mode-hook #'lsp-html-enable))

(provide 'lsp-init)

;;; lsp-init.el ends here
