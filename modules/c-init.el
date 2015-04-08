;;; c-init.el --- Part of emacs initialization  -*- lexical-binding: t; -*- -*- no-byte-compile: t; -*-

;;; Commentary:
;; C/C++ programming mode specific.

;;; Code:

(use-package cc-mode
  :defer t
  :config
  (setq c-default-style "cc-mode"
        c-basic-offset 2))

(use-package cwarn
  :defer t
  :config
  (eval-after-load 'cc-mode
    '(global-cwarn-mode 1)))

;; Add Google C++ Style checker. In default, syntax checked by Clang and Cppcheck.
(use-package flycheck-google-cpplint
  :ensure t
  :defer t
  :config
  (eval-after-load 'cc-mode
    '(flycheck-add-next-checker 'c/c++-clang
                                'c/c++-googlelint 'append)))

(provide 'c-init)

;;; c-init.el ends here
