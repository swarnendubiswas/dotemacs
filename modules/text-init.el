;;; text-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup text mode.

;;; Code:

;; text-mode is a basic mode for LaTeX-mode and org-mode, and so any hooks defined here will also get run for all modes
;; derived from a basic mode such as text-mode.

(add-hook 'text-mode-hook #'turn-on-auto-fill)

(or (use-package writegood-mode ; identify weasel words, passive voice, and duplicate words
      :ensure t
      :defer t
      ;;:bind* ("C-c g" . writegood-mode)
      :commands writegood-mode
      :diminish writegood-mode
      :init (add-hook 'text-mode-hook #'writegood-mode))

    (use-package artbollocks-mode
      :ensure t
      :disabled t
      :commands artbollocks-mode
      :diminish artbollocks-mode
      :init (add-hook 'text-mode-hook #'artbollocks-mode)))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode))
  :config
  ;;(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (use-package markdown-mode+
    :ensure t))

(use-package csv-mode
  :ensure t
  :defer t
  :config
  (use-package csv-nav
    :ensure t))

(provide 'text-init)

;;; text-init.el ends here
