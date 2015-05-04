;;; cc-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; C/C++ programming mode specific.

;;; Code:

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "k&r")
                        (c++-mode . "stroustrup")
                        (other . "linux")))

(use-package cc-mode
  :defer t
  :config
  (setq c-set-style "cc-mode" ; options: bsd, linux, gnu
        c-basic-offset 2))

(use-package cwarn
  :defer t
  :config
  (eval-after-load 'cc-mode
    '(global-cwarn-mode 1)))

;; Add Google C++ Style checker. In default, syntax checked by Clang and Cppcheck.
(use-package flycheck-google-cpplint
  :disabled t
  :ensure t
  :defer t
  :config
  (eval-after-load 'cc-mode
    '(flycheck-add-next-checker 'c/c++-clang
                                'c/c++-googlelint 'append)))

(provide 'cc-init)

;;; cc-init.el ends here
