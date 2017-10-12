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


;; http://nilsdeppe.com/posts/emacs-c++-ide

(defvar dotemacs-completion-in-buffer)

(setq-default c-default-style '((java-mode . "java")
                                (c++-mode . "stroustrup")
                                (other . "gnu/linux")
                                (c-mode . "k&r")
                                (awk-mode . "awk")))

(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)
  :mode ("\\.c\\'" . c++-mode)
  :config
  (setq c-set-style "cc-mode"
        c-basic-offset 2
        c-auto-newline 1)
  (c-toggle-electric-state 1)
  (c-toggle-syntactic-indentation 1)
  (unbind-key "C-M-a" c-mode-map)
  :bind (:map c-mode-base-map
              ("C-c c a" . c-beginning-of-defun)
              ("C-c c e" . c-end-of-defun)
              ("M-q" . c-fill-paragraph)))

(use-package c-eldoc
  :ensure t
  :after eldoc
  :if (eq system-type 'gnu/linux)
  :init
  (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook #'c-turn-on-eldoc-mode))

(use-package function-args
  :ensure t
  :diminish function-args-mode
  ;; :init (function-args-mode)
  :config (fa-config-default)
  :bind (:map function-args-mode-map
              ("M-u" . nil)   ;; This overrides M-u
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
  :after (company cc-mode)
  :if (eq dotemacs-completion-in-buffer 'company)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/"))

(when (eq dotemacs-completion-in-buffer 'auto-complete)
  (with-eval-after-load "auto-complete"
    (add-to-list 'ac-sources 'ac-source-semantic)
    (add-to-list 'ac-sources 'ac-source-semantic-raw)
    (add-to-list 'ac-sources 'ac-source-gtags)))

(use-package auto-complete-c-headers
  :ensure t
  :after auto-complete
  :if (eq dotemacs-completion-in-buffer 'auto-complete)
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))

(use-package auto-complete-clang
  :ensure t
  :after auto-complete
  :if (eq dotemacs-completion-in-buffer 'auto-complete))

(use-package google-c-style ; Google's C/C++ style for c-mode
  :ensure t
  :defer t
  :config
  (add-hook 'c-mode-common-hook #'google-set-c-style)
  (add-hook 'c-mode-common-hook #'google-make-newline-indent))

(use-package flycheck-google-cpplint ; Google C++ Style checker for Flycheck, also need to setup cpplint.
  :defer t
  :after flycheck
  :load-path "extras"
  :if (eq system-type 'gnu/linux)
  :config
  (flycheck-add-next-checker 'c/c++-clang
                             '(warning . c/c++-googlelint))
  (setq flycheck-googlelint-linelength 'dotemacs-fill-column
        flycheck-googlelint-filter "-whitespace/line_length"))

  (use-package google-c-style ; Google's C/C++ style for c-mode
    :ensure t
    :config
    (add-hook 'c-mode-common-hook #'google-set-c-style)
    (add-hook 'c-mode-common-hook #'google-make-newline-indent))

;; Install irony-server on consensus: cmake -DLIBCLANG_INCLUDE_DIR=/workspace/sbiswas/software/llvm/clang+llvm-3.9.1-x86_64-linux-gnu-debian8/include -DLIBCLANG_LIBRARY=/usr/lib64/llvm/libclang.so -DCMAKE_INSTALL_PREFIX=/h2/sbiswas/.emacs.d/irony/ /h2/sbiswas/.emacs.d/elpa/irony-20170523.618/server && cmake --build . --use-stderr --config Release --target install
(use-package irony
  :ensure t
  :diminish irony-mode
  :defer t
  :preface
  ;; Replace the `completion-at-point' and `complete-symbol' bindings in irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  :init
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'c-mode-hook #'irony-mode)
  :config
  (add-hook 'irony-mode-hook #'my-irony-mode-hook)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook #'flycheck-irony-setup)

  (use-package company-irony
    :ensure t
    :ensure irony
    :if (eq dotemacs-completion-in-buffer 'company)
    :init
    (use-package company-irony-c-headers
      :ensure t)
    :config
    (add-to-list 'company-backends '(company-irony-c-headers
                                     company-irony
                                     company-gtags
                                     company-yasnippet
                                     company-clang)))

  (use-package flycheck-irony
    :ensure t
    :ensure irony
    :ensure flycheck
    :after flycheck
    :commands flycheck-irony-setup)

  (use-package irony-eldoc
    :ensure t
    :commands irony-eldoc
    :init (add-hook 'irony-mode-hook #'irony-eldoc)))

(use-package clang-format
  :ensure t
  :after cc-mode
  :init
  (setq clang-format-style-option "{BasedOnStyle: Google, IndentWidth: 4, ColumnLimit: 120}")
  (when (string-equal (system-name) "consensus.ices.utexas.edu")
    (setq clang-format-executable "/workspace/sbiswas/software/llvm/llvm.install/bin/clang-format")))

(use-package flycheck-clang-analyzer
  :ensure t
  :ensure flycheck
  :after flycheck
  :after cc-mode
  :config (flycheck-clang-analyzer-setup))

(use-package cuda-mode
  :ensure t
  :defer t)

(use-package opencl-mode
  :ensure t
  :defer t)

(provide 'cc-init)

;;; cc-init.el ends here
