;;; text-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Setup text mode.

;;; Code:

;; text-mode is a basic mode for LaTeX-mode and org-mode, and so any hooks defined here will also get run for all modes
;; derived from a basic mode such as text-mode.

(add-hook 'text-mode-hook #'turn-off-auto-fill)

(or (use-package writegood-mode ; identify weasel words, passive voice, and duplicate words
      :ensure t
      :bind* ("C-c g" . writegood-mode)
      :functions writegood-mode
      :diminish writegood-mode
      :init (add-hook 'text-mode-hook #'writegood-mode))

    (use-package artbollocks-mode
      :ensure t
      :disabled t
      :commands artbollocks-mode
      :diminish artbollocks-mode
      :init (add-hook 'text-mode-hook #'artbollocks-mode)))

(use-package langtool
  :ensure t
  :defer t
  :config
  (setq langtool-language-tool-jar (concat user-emacs-directory "packages/LanguageTool-3.0/languagetool-commandline.jar")
        langtool-java-classpath nil
        langtool-default-language "en-US"
        langtool-java-bin "/usr/bin/java"
        langtool-mother-tongue "en"))

(use-package markdown-mode
  :ensure t
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode))
  :config
  (use-package markdown-mode+
    :ensure t))

(use-package csv-mode
  :ensure t
  :defer t
  :config
  (use-package csv-nav
    :ensure t))

(provide 'text-init)

;;; text-init.el ends here
