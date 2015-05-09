;;; highlight-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure sentence, line, word highlighting.

;;; Code:

(use-package hilit-chg
  :disabled t
  :config (highlight-changes-mode 1) 
  :bind ("M-o C" . highlight-changes-mode))

(use-package highlight-numbers
  :ensure t
  :config (highlight-numbers-mode 1))

(use-package highlight-symbol
  :disabled t
  :ensure t
  :config (highlight-symbol-mode 1)
  :diminish highlight-symbol-mode)

(use-package idle-highlight
  :disabled t
  :ensure t
  :config (idle-highlight))

;; highlight all occurrences of word under the point
(use-package idle-highlight-mode
  :disabled t
  :ensure t
  :config
  ;; (add-hook 'prog-mode-hook #'idle-highlight-mode)
  ;; (add-hook 'find-file-hook #'idle-highlight-mode)
  (idle-highlight-mode 1))

(use-package auto-highlight-symbol
  :disabled t
  :ensure t
  :config
  ;; (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode) ; highlight symbol at point
  (global-auto-highlight-symbol-mode 1))

;; highlight certain words

(use-package fixme-mode
  :ensure t
  :init (fixme-mode 1))

(use-package fic-mode
  :ensure t
  :diminish fic-mode
  :init
  (add-hook 'text-mode-hook #'turn-on-fic-mode)
  (add-hook 'prog-mode-hook #'turn-on-fic-mode))

(use-package fic-ext-mode
  :ensure t
  :preface
  (defun add-something-to-mode-hooks (mode-list something)
    "helper function to add a callback to multiple hooks"
    (dolist (mode mode-list)
      (add-hook (intern (concat (symbol-name mode) "-mode-hook")) something)))
  :init (fic-ext-mode 1)
  :config (add-something-to-mode-hooks '(prog text markdown) 'fic-ext-mode))

(provide 'highlight-init)

;;; highlight-init.el ends here
