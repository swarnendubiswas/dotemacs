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

(defvar dotemacs-completion-in-buffer)

(setq c-default-style '((java-mode . "java")
                        (c++-mode . "stroustrup")
                        (other . "gnu/linux")
                        (c-mode . "k&r")
                        (awk-mode . "awk")))

(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)
  :config
  (setq c-set-style "cc-mode"
        c-basic-offset 2
        c-auto-newline 1)
  (c-toggle-electric-state 1)
  (c-toggle-syntactic-indentation 1)
  (add-hook 'c-mode-hook
            (lambda ()
              (abbrev-mode -1)))

  (use-package c-eldoc
    :ensure t
    :if (eq system-type 'gnu/linux)
    :init
    (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)
    (add-hook 'c++-mode-hook #'c-turn-on-eldoc-mode))

  (use-package cwarn
    :ensure t
    :diminish cwarn-mode
    :config (global-cwarn-mode 1))

  (use-package hideif
    :diminish (hide-ifdef-mode hide-ifdef-hiding)
    :disabled t
    :init
    (setq hide-ifdef-initially t)
    (add-hook 'c-mode-hook
              (lambda()
                (hide-ifdef-mode 1))))

  (use-package function-args
    :ensure t
    :diminish function-args-mode
    :init (function-args-mode)
    :config
    ;; Include custom header locations
    (semantic-add-system-include "/usr/include/boost" 'c++-mode)
    (when (string-equal (system-name) "consensus.ices.utexas.edu")
      (semantic-add-system-include "/home/sbiswas/intel-pintool/source/include/pin" 'c++-mode)
      (use-package semantic/bovine/c
        :config (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                             "/usr/lib/gcc/x86_64-linux-gnu/4.8.1/include/stddef.h")))
    (fa-config-default)
    ;; This overrides M-u
    ;; (bind-key* "M-u" #'upcase-word)
    :bind (:map function-args-mode-map
                ("M-u" . nil)
                ("C-c c s" . fa-show)
                ("C-c c u" . fa-idx-cycle-up)
                ("C-c c d" . fa-idx-cycle-down)
                ("C-c c b" . fa-jump-maybe)
                ("C-c c c" . moo-complete)
                ("C-M-k" . nil)
                ("C-c c k" . moo-jump-local)
                ("C-M-j" . nil)
                ("C-c c j" . moo-jump-directory)))

  ;; http://emacs.stackexchange.com/questions/801/how-to-get-intelligent-auto-completion-in-c

  ;; http://tuhdo.github.io/c-ide.html
  (with-eval-after-load "company"
    (setq company-backends (delete 'company-semantic company-backends)))

  (use-package company-c-headers
    :ensure t
    :if (eq dotemacs-completion-in-buffer 'company)
    :config
    (add-to-list 'company-backends #'company-c-headers)
    (add-to-list 'company-clang-arguments "-I/home/sbiswas/intel-pintool/source/include/pin")
    (add-to-list 'company-clang-arguments "-I/home/sbiswas/intel-pintool/lib/boost_1_58_0/boost")
    (add-to-list 'company-c-headers-path-system "/usr/include/")

    (cond ((string-equal (system-name) "consensus.ices.utexas.edu")
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8.5/")
           (add-to-list 'company-c-headers-path-system "/home/sbiswas/intel-pintool/lib/boost_1_58_0/boost")
           (add-to-list 'company-c-headers-path-system "/home/sbiswas/intel-pintool/source/include/pin"))

          ((string-equal (system-name) "sbiswas-Dell-System-XPS-L502X")
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/6/"))))

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

  (use-package google-c-style ; Google's C/C++ style for c-mode
    :ensure t
    :config
    (add-hook 'c-mode-common-hook #'google-set-c-style)
    (add-hook 'c-mode-common-hook #'google-make-newline-indent))

  ;; Google C++ Style checker for Flycheck, also need to setup cpplint.py.
  (use-package flycheck-google-cpplint
    :ensure t
    :after flycheck
    :if (eq system-type 'gnu/linux)
    :config
    (flycheck-add-next-checker 'c/c++-clang
                               '(warning . c/c++-googlelint))
    (setq flycheck-googlelint-linelength 'dotemacs-fill-column
          flycheck-googlelint-filter "-whitespace/line_length"))

  (unbind-key "C-M-a" c-mode-map)
  :bind (("C-c c a" . c-beginning-of-defun)
         ("C-c c e" . c-end-of-defun)
         :map c-mode-base-map
         ("M-q" . c-fill-paragraph)))

(use-package irony
  :ensure t
  :diminish irony-mode
  :commands irony-mode
  :defer t
  :preface
  ;; Replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  :init
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'c-mode-hook #'irony-mode)
  (add-hook 'objc-mode-hook #'irony-mode)
  :config
  (add-hook 'irony-mode-hook #'my-irony-mode-hook)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook #'flycheck-irony-setup)

  (use-package company-irony
    :ensure t
    :if (eq dotemacs-completion-in-buffer 'company)
    :commands company-irony-setup-begin-commands
    :init
    (use-package company-irony-c-headers
      :ensure t)
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
    :config
    (add-to-list 'company-backends '(company-irony-c-headers
                                     company-irony)))

  (use-package irony-eldoc
    :ensure t
    :commands irony-eldoc
    :config (add-hook 'irony-mode-hook #'irony-eldoc)))

(provide 'cc-init)

;;; cc-init.el ends here
