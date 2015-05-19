;;; highlight-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure sentence, line, word highlighting.

;;; Code:

;; highlight current line
(use-package hl-line
  :ensure t
  ;;:if (not (eq dotemacs-theme 'default))
  :config
  ;; (global-hl-line-mode 1)
  ;; highlight only when idle
  (use-package hl-line+
    :ensure t
    :config (toggle-hl-line-when-idle 1)))

;; extension to linum-mode to highlight current line number in the margin
(use-package hlinum
  :ensure t
  :config (hlinum-activate))

(use-package hilit-chg
  :disabled t
  :config (highlight-changes-mode 1) 
  :bind ("M-o C" . highlight-changes-mode))

(use-package highlight-numbers
  :ensure t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; https://github.com/nschum/highlight-symbol.el
;; Navigate occurrences of the symbol under point with M-n and M-p, and
;; highlight symbol occurrences
(use-package highlight-symbol
  :ensure t
  :init
  (progn (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
         (add-hook 'prog-mode-hook #'highlight-symbol-mode))
  :config
  (setq highlight-symbol-idle-delay 1.0
        highlight-symbol-on-navigation-p t)
  :diminish highlight-symbol-mode)

;; (use-package idle-highlight
;;   :ensure t
;;   :init (idle-highlight 1))

;; highlight all occurrences of word under the point
;; (use-package idle-highlight-mode
;;   :ensure t
;;   :init (add-hook 'prog-mode-hook
;;                   (lambda()
;;                     (idle-highlight-mode 1))))

;; highlight symbol at point
(use-package auto-highlight-symbol
  :ensure t
  :init
  (add-hook 'prog-mode-hook
            (lambda()
              (global-auto-highlight-symbol-mode 1))))

;; highlight certain words

(use-package fixme-mode
  :ensure t
  :init (fixme-mode 1))

(use-package fic-mode
  :ensure t
  :diminish fic-mode
  :init
  (progn
    ((add-hook 'text-mode-hook #'turn-on-fic-mode)
     (add-hook 'prog-mode-hook #'turn-on-fic-mode))))

(use-package fic-ext-mode
  :ensure t
  :diminish fic-ext-mode
  :preface
  (defun add-something-to-mode-hooks (mode-list something)
    "helper function to add a callback to multiple hooks"
    (dolist (mode mode-list)
      (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
  :init (fic-ext-mode 1)
  :config (add-something-to-mode-hooks '(prog text) 'fic-ext-mode))

(provide 'highlight-init)

;;; highlight-init.el ends here
