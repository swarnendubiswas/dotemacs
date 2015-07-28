;;; rainbow-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Make Emacs more colorful.

;;; Code:

(use-package rainbow-mode
  :ensure t
  :disabled t
  :init (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-identifiers
  :ensure t
  :disabled t
  :init (add-hook 'prog-mode-hook #'rainbow-identifiers-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package rainbow-blocks
  :ensure t
  :disabled t
  :init (rainbow-blocks-mode 1)
  :diminish rainbow-blocks-mode)

(provide 'rainbow-init)

;;; rainbow-init.el ends here
