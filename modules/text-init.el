;;; text-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup text mode.

;;; Code:

;;;;;;;;;;;;;;;;;;
;; TEXT EDITING ;;
;;;;;;;;;;;;;;;;;;

;; text-mode is a basic mode for LaTeX-mode and org-mode, and so any hooks defined here will also get run for all modes
;; derived from a basic mode such as text-mode.

(use-package writegood-mode ; Identify weasel words, passive voice, and duplicate words
  :ensure t
  :diminish writegood-mode
  :hook (text-mode . writegood-mode))

(defun sb/company-text-backends ()
  "Add backends for text completion in company mode."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((;; Generic backends
           company-files
           company-keywords
           company-capf
           company-dict
           company-dabbrev))))
(add-hook 'text-mode-hook #'sb/company-text-backends)

;;;;;;;;;;;;;;
;; MARKDOWN ;;
;;;;;;;;;;;;;;

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
        markdown-list-indent-width 2
        markdown-command "pandoc -f markdown -s "))

(use-package markdown-mode+
  :ensure t
  :after markdown-mode)

(use-package pandoc
  :ensure t
  :after markdown-mode)

(use-package pandoc-mode
  :ensure t
  :after markdown-mode
  :diminish pandoc-mode
  :hook (markdown-mode . pandoc-mode))

;;;;;;;;;
;; CSV ;;
;;;;;;;;;

(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'")

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'json-mode-beautify t t))))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(provide 'text-init)

;;; text-init.el ends here
