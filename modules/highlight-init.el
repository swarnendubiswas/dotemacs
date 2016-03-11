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
  :disabled t
  :init (highlight-changes-mode 1)
  :bind ("M-o C" . highlight-changes-mode))

(use-package highlight-numbers
  :ensure t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(or (use-package highlight-symbol
      :ensure t
      :init
      (add-hook 'prog-mode-hook #'highlight-symbol-mode)
      ;; Navigate occurrences of the symbol under point with M-n and M-p, and highlight symbol occurrences
      (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
      (setq highlight-symbol-idle-delay 0.5
            highlight-symbol-on-navigation-p t)
      :diminish highlight-symbol-mode)

    (use-package auto-highlight-symbol
      :ensure t
      :disabled t
      :init (global-auto-highlight-symbol-mode 1)
      :config
      (setq ahs-default-range 'ahs-range-whole-buffer)
      ;; M-<left>/<right> is overwritten by 'ahs-backward/forward, which is not useful
      (define-key auto-highlight-symbol-mode-map (kbd "M-<left>") nil)
      (define-key auto-highlight-symbol-mode-map (kbd "M-<right>") nil)
      (bind-keys
       :map auto-highlight-symbol-mode-map
       ("M-<" . ahs-backward)
       ("M->" . ahs-forward))))

;; highlight certain words

(or (use-package fixme-mode
      :ensure (bound-and-true-p dotemacs-use-marmalade-repo-p)
      :init (fixme-mode 1))

    (use-package fic-mode
      :ensure t
      :disabled t
      :diminish fic-mode
      :init
      (add-hook 'text-mode-hook #'fic-mode)
      (add-hook 'prog-mode-hook #'fic-mode))

    (use-package fic-ext-mode
      :ensure t
      :disabled t
      :diminish fic-ext-mode
      :preface
      (defun add-something-to-mode-hooks (mode-list something)
        "helper function to add a callback to multiple hooks"
        (dolist (mode mode-list)
          (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
      :init
      ;;(add-something-to-mode-hooks '(prog text) #'fic-ext-mode)
      (fic-ext-mode 1)))

(provide 'highlight-init)

;;; highlight-init.el ends here
