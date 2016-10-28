;;; text-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup text mode.

;;; Code:

;; text-mode is a basic mode for LaTeX-mode and org-mode, and so any hooks defined here will also get run for all modes
;; derived from a basic mode such as text-mode.

(defun dotemacs--text-mode-setup ()
  "Helper function for configuring text mode."
  (turn-off-hideshow))
(add-hook 'text-mode-hook #'dotemacs--text-mode-setup)

(use-package writegood-mode ; Identify weasel words, passive voice, and duplicate words
  :ensure t
  :functions writegood-mode
  :diminish writegood-mode
  :config (add-hook 'text-mode-hook #'writegood-mode))

(use-package langtool
  :ensure t
  :defer t
  :config
  (setq langtool-language-tool-jar (concat user-emacs-directory "packages/LanguageTool-3.3/languagetool-commandline.jar")
        langtool-java-classpath nil
        langtool-default-language "en-US"
        langtool-java-bin "/usr/bin/java"
        langtool-mother-tongue "en"))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-enable-math t
        markdown-command "pandoc -f markdown -s ")
  (add-hook 'markdown-mode-hook #'turn-on-auto-fill)
  (add-hook 'gfm-mode-hook #'turn-on-auto-fill)
  (use-package markdown-mode+
    :ensure t)
  (use-package pandoc-mode
    :ensure t
    :init (add-hook 'markdown-mode-hook #'pandoc-mode)))

(use-package csv-mode
  :ensure t
  :defer t
  :config
  (use-package csv-nav
    :ensure t))

(provide 'text-init)

;;; text-init.el ends here
