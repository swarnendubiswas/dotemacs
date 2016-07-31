;;; java-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Java programming mode.

;;; Code:

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2
                  c-set-style "java")))

(use-package ant)

(use-package autodisass-java-bytecode ; Can disassemble .class files from within jars as well
  :ensure t)

(use-package jdee
  :ensure t
  :defer t)

(use-package emacs-eclim
  :ensure t
  :init
  (use-package eclimd)
  (setq eclim-eclipse-dirs "/home/biswass/software/eclipse-neon-java/"
        eclim-executable "~/nonStandard/eclipse/eclim"
        eclim-default-workspace "/home/biswass/workspace")
  :config
  (global-eclim-mode)
  (use-package company-emacs-eclim
    :functions company-emacs-eclim-setup
    :config (company-emacs-eclim-setup)))

(provide 'java-init)

;;; java-init.el ends here
