;;; text-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup text mode.

;;; Code:

;; text-mode is a basic mode for LaTeX-mode and org-mode, and so any hooks defined here will also get run for all modes
;; derived from a basic mode such as text-mode.

(use-package writegood-mode ; Identify weasel words, passive voice, and duplicate words
  :ensure t
  :diminish writegood-mode
  :init (add-hook 'text-mode-hook #'writegood-mode))

(defun sb/company-text-backends ()
  "Add backends for text completion in company mode."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((;; Generic backends
           company-files
           company-keywords
           company-capf
           company-dict))))
(add-hook 'text-mode-hook #'sb/company-text-backends)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :diminish gfm-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :bind ("C-c C-d" . nil)
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-enable-math t
        markdown-make-gfm-checkboxes-buttons t
        markdown-command "pandoc -f markdown -s ")
  (use-package markdown-mode+
    :ensure t)
  (use-package pandoc
    :ensure t)
  (use-package pandoc-mode
    :ensure t
    :diminish pandoc-mode
    :config (add-hook 'markdown-mode-hook #'pandoc-mode)))

(use-package csv-mode
  :ensure t
  :defer t
  :config
  (use-package csv-nav
    :ensure t))

(use-package json-mode
  :ensure t
  :defer t
  :config (add-hook 'json-mode-hook #'flycheck-mode))

(provide 'text-init)

;;; text-init.el ends here
