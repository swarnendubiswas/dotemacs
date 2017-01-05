;;; rainbow-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Make Emacs more colorful.

;;; Code:

(use-package rainbow-mode
  :ensure t
  :disabled t
  :config (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(provide 'rainbow-init)

;;; rainbow-init.el ends here
