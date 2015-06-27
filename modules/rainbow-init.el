;;; rainbow-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Make emacs more colorful.

;;; Code:

(use-package rainbow-mode
  :disabled t
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-identifiers
  :disabled t
  :ensure t
  :init (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(use-package rainbow-delimiters
  :ensure t
  :init (rainbow-delimiters-mode 1))

(use-package rainbow-blocks
  :disabled t
  :ensure t
  :config (rainbow-blocks-mode 1)
  :diminish rainbow-blocks-mode)

(provide 'rainbow-init)

;;; rainbow-init.el ends here
