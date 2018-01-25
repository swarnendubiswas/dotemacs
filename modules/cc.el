;;; cc.el --- Part of Emacs initialization  -*- lexical-binding: t; no-byte-compile: nil; -*-

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
(defvar dotemacs-temp-directory)

(setq-default c-default-style '((java-mode . "java")
                                (c++-mode . "stroustrup")
                                (other . "gnu/linux")
                                (c-mode . "k&r")
                                (awk-mode . "awk")))

(use-package cc-mode
  :defer t
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
  :after (eldoc cc-mode)
  :if (eq system-type 'gnu/linux)
  :init
  (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode)
  (add-hook 'c++-mode-hook #'c-turn-on-eldoc-mode))

(use-package function-args
  :ensure t
  :disabled t
  :after cc-mode
  :diminish function-args-mode
  :config (fa-config-default)
  :bind (:map function-args-mode-map
              ("C-M-k" . nil)
              ("C-M-j" . nil)
              :map c++-mode-map
              ("M-u" . nil)   ;; This overrides M-u
              ("C-c c s" . fa-show)
              ("C-c c b" . fa-jump)
              ("C-c c c" . moo-complete)
              ("C-c c l" . moo-jump-local)
              ("C-c c d" . moo-jump-directory)))

(use-package company-c-headers
  :ensure t
  :after (company cc-mode)
  :if (eq dotemacs-completion-in-buffer t)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (when (string-equal (system-name) "consensus.ices.utexas.edu")
    (dolist (paths '(
                     "/usr/include"
                     "/usr/include/boost"
                     "/usr/local/include"
                     "/usr/include/c++/4.8.5"
                     "/usr/include/c++/4.8.5/tr1"
                     "/usr/include/c++/4.8.5/x86_64-redhat-linux"
                     "/usr/lib/gcc/x86_64-redhat-linux/4.8.5/include"
                     ))
      (add-to-list 'company-c-headers-path-system paths))))

;; Install irony-server on consensus: cmake -DLIBCLANG_INCLUDE_DIR=/workspace/sbiswas/software/llvm/clang+llvm-3.9.1-x86_64-linux-gnu-debian8/include -DLIBCLANG_LIBRARY=/usr/lib64/llvm/libclang.so -DCMAKE_INSTALL_PREFIX=/h2/sbiswas/.emacs.d/irony/ /h2/sbiswas/.emacs.d/elpa/irony-20170523.618/server && cmake --build . --use-stderr --config Release --target install
(use-package irony
  :ensure t
  :diminish irony-mode
  :defer t
  :preface
  ;; Replace the `completion-at-point' and `complete-symbol' bindings in irony-mode's buffers by irony-mode's function
  (defun sb/irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
  :init
  (add-hook 'c++-mode-hook #'irony-mode)
  (add-hook 'c-mode-hook #'irony-mode)
  :config
  (setq irony-server-install-prefix (concat dotemacs-temp-directory "irony"))
  (add-hook 'irony-mode-hook #'sb/irony-mode-hook)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook #'flycheck-irony-setup)

  (use-package company-irony
    :ensure t
    :after company
    :if (bound-and-true-p dotemacs-completion-in-buffer)
    :init
    (use-package company-irony-c-headers
      :ensure t
      :after irony)
    :config
    ;; http://emacs.stackexchange.com/questions/801/how-to-get-intelligent-auto-completion-in-c
    ;; http://tuhdo.github.io/c-ide.html
    (setq company-backends (delete 'company-semantic company-backends))
    (add-to-list 'company-backends '(company-irony-c-headers
                                     company-irony
                                     ;; company-gtags
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
  (setq-default clang-format-style "{BasedOnStyle: LLVM, IndentWidth: 4, ColumnLimit: 120}")
  (when (string-equal (system-name) "consensus.ices.utexas.edu")
    (setq clang-format-executable "/workspace/sbiswas/software/llvm/llvm.install/bin/clang-format"))
  (when (string-equal (system-name) "sbiswas-Dell-System-XPS-L502X")
    (setq clang-format-executable "/usr/bin/clang-format-5.0")))

(use-package cuda-mode
  :ensure t
  :mode ("\\.cu\\'"	. cuda-mode))

(use-package opencl-mode
  :ensure t
  :defer t)

(defun sb/company-cc-backends ()
  "Add backends for C/C++ completion in company mode."
  (make-local-variable 'company-backends)
  (setq company-backends
        '((
           ;; C++ specific backends
           company-clang
           company-rtags
           company-irony
           company-c-headers
           company-irony-c-headers
           ;; Generic backends
           company-files
           company-keywords
           ;; company-dabbrev
           ;; company-dabbrev-code
           company-capf

           ;; company-semantic
           ;; company-gtags
           ))))
(add-hook 'c++-mode-hook #'sb/company-cc-backends)

(add-hook 'before-save-hook
          (lambda ()
            (when (string-equal major-mode "c++-mode")
              (clang-format-buffer))))

(provide 'cc)

;;; cc.el ends here
