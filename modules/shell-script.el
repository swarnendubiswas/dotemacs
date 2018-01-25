;;; shell-script.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

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
    :if (bound-and-true-p dotemacs-completion-in-buffer)
    :after company
    :config
    (setq company-shell-delete-duplicates t)
    (add-to-list 'company-backends 'company-shell)
    (add-to-list 'company-backends 'company-fish-shell)))

(use-package fish-mode
  :ensure t
  :mode ("\\.fish$" . fish-mode))

(defun sb/company-sh-backends ()
  "Add backends for C/C++ completion in company mode."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((;; Generic backends
           company-files
           company-keywords
           company-capf
           company-dabbrev
           company-dabbrev-code
           ;; Mode-specific
           company-shell
           company-fish-shell))))
(add-hook 'sh-mode-hook 'sb/company-sh-backends)

(provide 'shell-script)

;;; shell-script.el ends here
