;;; lsp-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup LSP.

;;; Code:

;; npm i -g [--unsafe-perm] bash-language-server vscode-html-languageserver-bin typescript-language-server typescript vscode-css-languageserver-bin
;; pip install python-language-server[all] --user

(use-package lsp-mode
  :commands lsp
  :hook (prog-mode . lsp))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-java :ensure t :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java
  :after (lsp-java))

(use-package lsp-java-treemacs
  :after (treemacs))

(provide 'lsp-init)

;;; lsp-init.el ends here
