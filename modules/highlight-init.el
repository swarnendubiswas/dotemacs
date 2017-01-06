;;; highlight-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure sentence, line, word highlighting.

;;; Code:

(defvar dotemacs-theme)

(use-package hl-line ; Highlight current line
  :ensure t
  :config
  (setq hl-line-sticky-flag nil)
  (global-hl-line-mode 1)
  (unless (eq dotemacs-theme 'solarized-dark)
    (set-face-attribute 'hl-line nil
                        :background "old lace")))

(use-package hl-line+ ; Highlight only when idle
  :ensure t
  :disabled t
  :after hl-line
  :config
  (global-hl-line-mode -1)
  (toggle-hl-line-when-idle 1))

(use-package hlinum ; Extension to linum-mode to highlight current line number in the margin
  :ensure t
  :init (hlinum-activate))

(use-package hilit-chg
  :disabled t
  :diminish highlight-changes-mode
  :config (highlight-changes-mode 1))

(use-package highlight-numbers
  :ensure t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol ; Highlight symbol under point
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  ;; Navigate occurrences of the symbol under point with M-n and M-p
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t)
  :diminish highlight-symbol-mode)

(use-package fic-mode ; Highlight certain words
  :ensure t
  :diminish fic-mode
  :init
  (add-hook 'text-mode-hook #'fic-mode)
  (add-hook 'prog-mode-hook #'fic-mode)
  :config
  (add-to-list 'fic-highlighted-words '"XXX"))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config (add-hook 'text-mode-hook #'volatile-highlights-mode))

(use-package highlight-tail
  :ensure t
  :disabled t
  :diminish highlight-tail-mode
  :config (highlight-tail-mode 1))

(use-package beacon ; Highlight cursor position in buffer after scrolling
  :ensure t
  :disabled t
  :config (beacon-mode 1)
  :diminish beacon-mode)

(provide 'highlight-init)

;;; highlight-init.el ends here
