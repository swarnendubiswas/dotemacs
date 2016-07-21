;;; cc-init.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

;;; Commentary:
;; C/C++ programming mode specific.

;;; Code:

;; Available C style: https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Built_002din-Styles
;; gnu: The default style for GNU projects
;; k&r: What Kernighan and Ritchie, the authors of C used in their book
;; bsd: What BSD developers use, aka "Allman style" after Eric Allman.
;; whitesmith: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; stroustrup: What Stroustrup, the author of C++ used in his book
;; ellemtel: Popular C++ coding standards as defined by "Programming in C++, Rules and Recommendations," Erik Nyquist
;;  and Mats Henricson, Ellemtel
;; linux: What the Linux developers use for kernel development
;; python: What Python developers use for extension modules
;; java: The default style for java-mode (see below)
;; user: When you want to define your own style

(setq c-default-style '((java-mode . "java")
                        (c++-mode . "stroustrup")
                        (other . "gnu/linux")
                        (c-mode . "k&r")
                        (awk-mode . "awk")))

(use-package cc-mode
  :defer t
  :functions (c-toggle-electric-state c-toggle-syntactic-indentation c-fill-paragraph)
  :config
  (setq c-set-style "cc-mode" ; options: bsd, linux, gnu
        c-basic-offset 2
        c-auto-newline 1)
  (c-toggle-electric-state 1)
  (c-toggle-syntactic-indentation 1)
  (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)
  (add-hook 'c-mode-hook
            (lambda ()
              (abbrev-mode -1)))

  ;; Switch between header and implementation files
  (use-package find-file
    :ensure t
    :commands ff-find-other-file
    :bind ("C-c o" . ff-find-other-file))

  (use-package cwarn
    :ensure t
    :defer 2
    :diminish cwarn-mode
    :config (global-cwarn-mode 1))

  (use-package hideif
    :defer 2
    :diminish (hide-ifdef-mode hide-ifdef-hiding)
    :config
    (setq hide-ifdef-initially t)
    (add-hook 'c-mode-hook
              (lambda()
                (hide-ifdef-mode 1))))

  (use-package ctypes ;; FIXME: What is this for?
    :ensure t
    :disabled t
    :config
    (setq ctypes-write-types-at-exit t)
    (ctypes-read-file nil nil t t)
    (ctypes-auto-parse-mode 1))

  (use-package function-args ; this overrides M-u
    :ensure t
    :diminish function-args-mode
    :config
    (set-default 'semantic-case-fold t)
    ;; Include custom header locations
    (when (string-equal system-name "rain.cse.ohio-state.edu")
      (semantic-add-system-include "/usr/include/boost148/" 'c++-mode))
    (fa-config-default)
    (bind-key* "M-u" #'upcase-word))

  ;; This is already the default, but I have this as a reminder.
  (bind-key "M-q" #'c-fill-paragraph c-mode-base-map)

  ;; http://emacs.stackexchange.com/questions/801/how-to-get-intelligent-auto-completion-in-c

  ;; http://tuhdo.github.io/c-ide.html
  (with-eval-after-load "company"
    (setq company-backends (delete 'company-semantic company-backends)))

  (use-package company-c-headers
    :ensure t
    :if (eq dotemacs-completion-in-buffer 'company)
    :config
    (add-to-list 'company-backends #'company-c-headers)
    (add-to-list 'company-clang-arguments "-I/home/biswass/workspace/intel-pintool/source/include")
    (add-to-list 'company-clang-arguments "-I/home/biswass/workspace/intel-pintool/lib/boost_1_58_0/boost")
    (cond ((string-equal system-name "rain.cse.ohio-state.edu")
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.4.4/")
           (add-to-list 'company-c-headers-path-system "~/workspace/intel-pintool/lib/boost_1_58_0/boost"))

          ((string-equal system-name "biswass-Dell-System-XPS-L502X")
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9"))))

  (when (eq dotemacs-completion-in-buffer 'auto-complete)
    (with-eval-after-load "auto-complete"
      (add-to-list 'ac-sources 'ac-source-semantic)
      (add-to-list 'ac-sources 'ac-source-semantic-raw)
      (add-to-list 'ac-sources 'ac-source-gtags)))

  (use-package auto-complete-c-headers
    :ensure t
    :if (eq dotemacs-completion-in-buffer 'auto-complete)
    :config
    (add-hook 'c-mode-hook
              (lambda ()
                (add-to-list 'ac-sources 'ac-source-c-headers)
                (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))

  (use-package auto-complete-clang
    :ensure t
    :if (eq dotemacs-completion-in-buffer 'auto-complete))

  (use-package dep
    :config (semantic-add-system-include "/usr/include"))

  (use-package google-c-style ; Google's C/C++ style for c-mode
    :ensure t
    :config
    (add-hook 'c-mode-common-hook #'google-set-c-style)
    (add-hook 'c-mode-common-hook #'google-make-newline-indent))

  ;; Google C++ Style checker for Flycheck
  ;; Also, need to setup cpplint.py.
  (with-eval-after-load "flycheck"
    (use-package flycheck-google-cpplint
      :ensure t
      :if (eq system-type 'gnu/linux)
      :config
      (flycheck-add-next-checker 'c/c++-clang
                                 '(warning . c/c++-googlelint))
      (setq flycheck-googlelint-linelength 'dotemacs-fill-column
            flycheck-googlelint-filter "-whitespace/line_length"))))

(provide 'cc-init)

;;; cc-init.el ends here
