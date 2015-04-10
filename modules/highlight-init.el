;;; highlight-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Configure sentence, line, word highlighting.

;;; Code:

(use-package highlight-numbers
  :ensure t
  :defer 5
  :config (highlight-numbers-mode 1))

(use-package highlight-symbol
  :ensure t
  :disabled t
  :config (highlight-symbol-mode 1)
  :diminish highlight-symbol-mode)

(use-package idle-highlight
  :ensure t
  :disabled t
  :config (idle-highlight))

(use-package idle-highlight-mode
  :ensure t
  :disabled t
  :config 
  ;;(add-hook 'prog-mode-hook 'idle-highlight-mode) ; highlight all occurrences of word under the point
  ;; (add-hook 'find-file-hook 'idle-highlight-mode)
  (idle-highlight-mode 1))

(use-package auto-highlight-symbol
  :ensure t
  :disabled t
  :config
  ;;(add-hook 'prog-mode-hook #'auto-highlight-symbol-mode) ; highlight symbol at point
  (global-auto-highlight-symbol-mode 1))

(provide 'highlight-init)

;;; highlight-init.el ends here
