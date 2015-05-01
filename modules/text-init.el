;;; text-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Setup text mode.

;;; Code:

;; text-mode is a basic mode for LaTeX-mode and org-mode, and so these hooks will also get run for all modes derived
;; from a basic mode such as text-mode.
(use-package simple
  :config (add-hook 'text-mode-hook #'turn-on-auto-fill))

;; identify weasel words, passive voice, and duplicate words
(use-package writegood-mode
  :ensure t
  :bind ("C-c g" . writegood-mode)
  :diminish writegood-mode
  :init (add-hook 'text-mode-hook #'writegood-mode))

(use-package artbollocks-mode
  :disabled t
  :ensure t
  :diminish artbollocks-mode
  :config (add-hook 'text-mode-hook #'artbollocks-mode))

(provide 'text-init)

;;; text-init.el ends here
