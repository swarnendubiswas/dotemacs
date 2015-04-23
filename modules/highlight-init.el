;;; highlight-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Configure sentence, line, word highlighting.

;;; Code:

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
(or (use-package fic-mode
      :ensure t
      :config (fic-mode 1)
      :diminish fic-mode)
    
    (use-package fixme-mode
      :ensure t
      :config (fixme-mode 1)))

(provide 'highlight-init)

;;; highlight-init.el ends here
