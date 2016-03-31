;;; highlight-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure sentence, line, word highlighting.

;;; Code:

(use-package hl-line ; highlight current line
  :ensure t
  :if (bound-and-true-p display-graphic-p)
  :init
  ;; (global-hl-line-mode 1)
  (setq hl-line-sticky-flag nil) ; Highlight the line only in the active window
  (use-package hl-line+ ; highlight only when idle
    :ensure t
    :init (toggle-hl-line-when-idle 1)))

(use-package hlinum ; extension to linum-mode to highlight current line number in the margin
  :ensure t
  :init (hlinum-activate))

(use-package hilit-chg
  :defer t
  :config (highlight-changes-mode 1))

(use-package highlight-numbers
  :ensure t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol ; Highlight symbol under point
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  ;; Navigate occurrences of the symbol under point with M-n and M-p
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  :config
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t)
  :diminish highlight-symbol-mode)

(use-package fic-mode ; Highlight certain words
  :ensure t
  :diminish fic-mode
  :init
  (add-hook 'text-mode-hook #'fic-mode)
  (add-hook 'prog-mode-hook #'fic-mode))

(provide 'highlight-init)

;;; highlight-init.el ends here
