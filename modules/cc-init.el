;;; cc-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; C/C++ programming mode specific.

;;; Code:

;; Available C style: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Built_002din-Styles
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist
;;  and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style

(setq c-default-style '((java-mode . "java")
                        (c-mode . "k&r")
                        (c++-mode . "stroustrup")
                        (other . "gnu/linux")
                        (awk-mode . "awk")))

(use-package cc-mode
  :defer 5
  :init
  (setq c-set-style "cc-mode" ; options: bsd, linux, gnu
        c-basic-offset 2)

  :config
  (c-toggle-electric-state +1)
  (c-toggle-syntactic-indentation +1)
  (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

  (use-package cwarn
    :ensure t
    :defer 5
    :diminish cwarn-mode
    :config (global-cwarn-mode 1))

  (use-package hideif
    :defer 5
    :config
    (setq hide-ifdef-initially t)
    (hide-ifdef-mode 1))

  (use-package google-c-style
    :ensure t
    :defer 5
    :config
    (add-hook 'c-mode-common-hook #'google-set-c-style)
    (add-hook 'c-mode-common-hook #'google-make-newline-indent))

  (use-package ctypes
    :ensure t
    :config
    (setq ctypes-write-types-at-exit t)
    (ctypes-read-file nil nil t t)
    (ctypes-auto-parse-mode 1))

  (use-package function-args
    :ensure t
    :diminish function-args-mode
    :config
    (set-default 'semantic-case-fold t)
    ;; to include custom header locations
    ;;(semantic-add-system-include "/usr/local/boost_1_54_0/" 'c++-mode)
    (fa-config-default))

  ;; this is already the default, but I have this as a reminder.
  (bind-key "M-q" #'c-fill-paragraph c-mode-base-map))

;; http://emacs.stackexchange.com/questions/801/how-to-get-intelligent-auto-completion-in-c

;; http://tuhdo.github.io/c-ide.html
(with-eval-after-load "company"
  (setq company-backends (delete 'company-semantic company-backends)))

;; SB: TODO: Do I want this?
;; (with-eval-after-load 'cc-mode
;;   (define-key c-mode-map  [(tab)] 'company-complete)
;;   (define-key c++-mode-map  [(tab)] 'company-complete))

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends #'company-c-headers)

  (cond ((string-equal system-name "XXX")
         (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.4.4/")
         (add-to-list 'company-c-headers-path-system "~/workspace/intel-pintool/source/tools/PlassInstrumentation/lib/boost_1_58_0"))

        ((string-equal system-name "biswass-Dell-System-XPS-L502X")
         (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9")))

  (add-to-list 'company-clang-arguments "-I/home/biswass/workspace/intel-pintool/source/include")
  (add-to-list 'company-clang-arguments "-I/home/biswass/workspace/intel-pintool/source/tools/PlassInstrumentation/lib/boost_1_58_0"))

(use-package dep
  :disabled t
  :config
  (semantic-add-system-include "/usr/local/include")
  (semantic-add-system-include "~/linux/include"))

(provide 'cc-init)

;;; cc-init.el ends here
