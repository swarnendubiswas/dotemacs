;;; java-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

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

;;(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
(use-package jtags
  :ensure t
  :defer t
  :config
  (add-hook 'java-mode-hook 'jtags-mode))

(use-package javap-mode
  :ensure t
  :defer t)

(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(provide 'java-init)

;;; java-init.el ends here
