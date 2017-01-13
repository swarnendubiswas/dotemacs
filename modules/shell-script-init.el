;;; shell-script-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure editing shell scripts.

;;; Code:

(defvar dotemacs-completion-in-buffer)

(use-package sh-script ; Shell script mode
  :mode (("\\.zsh\\'" . sh-mode)
         ("\\bashrc\\'" . sh-mode))
  :config
  (setq sh-basic-offset 4
        sh-indent-comment t
        sh-indentation 4
        sh-indent-after-continuation 'always)
  (unbind-key "C-c C-d" sh-mode-map) ; Was bound to sh-cd-here

  (use-package company-shell
    :ensure t
    :if (eq dotemacs-completion-in-buffer 'company)
    :after company
    :config
    (setq company-shell-delete-duplicates t)
    (add-to-list 'company-backends 'company-shell)
    (add-to-list 'company-backends 'company-fish-shell))

  (use-package company-tern
    :ensure t
    :if (eq dotemacs-completion-in-buffer 'company)
    :after company
    :config (add-to-list 'company-backends 'company-tern)))

(use-package fish-mode
  :ensure t
  :mode ("\\.fish$" . fish-mode))

(provide 'shell-script-init)

;;; shell-script-init.el ends here
