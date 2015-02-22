;;; latex-init.el --- Part of emacs initialization

;;; Commentary:
;; Configure latex mode.

;;; Code:

(use-package javap-mode
             :ensure t
             :defer t
             )

(use-package autodisass-java-bytecode
             :ensure t
             :defer t
             )

(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))
(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
(add-hook 'java-mode-hook 'jtags-mode)


