;;; highlight-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; Configure sentence, line, word highlighting.

;;; Code:

(use-package highlight-numbers
  :ensure t
  :defer t
  :init
  ;;(add-hook 'prog-mode-hook #'highlight-numbers-mode)
  (highlight-numbers-mode 1))

(use-package highlight-symbol
  :ensure t
  :disabled t
  :init (highlight-symbol-mode 1)
  :diminish highlight-symbol-mode)

(use-package idle-highlight
  :ensure t
  :disabled t)

(use-package idle-highlight-mode
  :ensure t
  :disabled t)

(use-package auto-highlight-symbol
  :ensure t
  :disabled t
  :init (global-auto-highlight-symbol-mode 1))
  
(provide 'highlight-init)

;;; highlight-init.el ends here
