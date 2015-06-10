;;; java-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; Configure Java programming mode.

;;; Code:

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2
                  c-set-style "java")))

(use-package javap-mode
  :ensure t
  :defer t)

(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(provide 'java-init)

;;; java-init.el ends here
