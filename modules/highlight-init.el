;;; highlight-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure sentence, line, word highlighting.

;;; Code:

;; highlight current line
(use-package hl-line
  :ensure t
  :if (display-graphic-p)
  :init
  ;; (global-hl-line-mode 1)

  ;; highlight only when idle
  (use-package hl-line+
    :ensure t
    :init (toggle-hl-line-when-idle 1)))

;; extension to linum-mode to highlight current line number in the margin
(use-package hlinum
  :ensure t
  :init (hlinum-activate))

(use-package hilit-chg
  :disabled t
  :init (highlight-changes-mode 1)
  :bind ("M-o C" . highlight-changes-mode))

(use-package highlight-numbers
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; Navigate occurrences of the symbol under point with M-n and M-p, and highlight symbol occurrences
(use-package highlight-symbol
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  (setq highlight-symbol-idle-delay 0.5
        highlight-symbol-on-navigation-p t)
  :diminish highlight-symbol-mode)

;; highlight certain words

(or (use-package fixme-mode
      :ensure t
      :init (fixme-mode 1))

    (use-package fic-mode
      :disabled t
      :ensure t
      :diminish fic-mode
      :init
      (add-hook 'text-mode-hook #'fic-mode)
      (add-hook 'prog-mode-hook #'fic-mode))

    (use-package fic-ext-mode
      :disabled t
      :ensure t
      :diminish fic-ext-mode
      :preface
      (defun add-something-to-mode-hooks (mode-list something)
        "helper function to add a callback to multiple hooks"
        (dolist (mode mode-list)
          (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
      :init (fic-ext-mode 1)
      :config
      ;;(add-something-to-mode-hooks '(prog text) #'fic-ext-mode)
      ))

(provide 'highlight-init)

;;; highlight-init.el ends here
