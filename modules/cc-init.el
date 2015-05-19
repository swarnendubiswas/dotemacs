;;; cc-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; C/C++ programming mode specific.

;;; Code:

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
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
  :defer t
  :config
  (setq c-set-style "cc-mode" ; options: bsd, linux, gnu
        c-basic-offset 2)
  
  (use-package cwarn
    :defer t
    :config
    (eval-after-load 'cc-mode
      '(global-cwarn-mode 1))))

;; http://tuhdo.github.io/c-ide.html
(with-eval-after-load 'company 
  (setq company-backends (delete 'company-semantic company-backends))

  (with-eval-after-load 'cc-mode
    (define-key c-mode-map  [(tab)] 'company-complete)
    (define-key c++-mode-map  [(tab)] 'company-complete))
  
  (use-package company-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-c-headers)
    (cond ((string-equal system-name "rain.cse.ohio-state.edu")
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.4.4/")
           (add-to-list 'company-c-headers-path-system "~/workspace/intel-pintool/source/tools/PlassInstrumentation/lib/boost_1_58_0"))
          ((string-equal system-name "biswass-Dell-System-XPS-L502X")
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9"))))
  (add-to-list 'company-clang-arguments "-I/home/biswass/workspace/intel-pintool/source/include")
  (add-to-list 'company-clang-arguments "-I/home/biswass/workspace/intel-pintool/source/tools/PlassInstrumentation/lib/boost_1_58_0"))

(use-package dep
  :config
  (semantic-add-system-include "/usr/local/include")
  (semantic-add-system-include "~/linux/include"))

(provide 'cc-init)

;;; cc-init.el ends here