;;; java-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Java programming mode.

;;; Code:

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2
                  c-set-style "java")))

;; Can disassemble .class files from within jars as well
(use-package autodisass-java-bytecode
  :ensure t)

(use-package jdee
  :ensure t
  :defer t)

(provide 'java-init)

;;; java-init.el ends here
