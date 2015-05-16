;;; cc-init.el --- Part of emacs initialization  -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; C/C++ programming mode specific.

;;; Code:

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

(use-package semantic
  :config
  ;; (require 'semantic-ia)
  ;; (require 'semantic-loaddefs)
  ;; (require 'semanticdb)
  ;; SemanticDB files
  (setq semanticdb-default-save-directory (concat dotemacs-temp-directory "semanticdb"))
  (global-semanticdb-minor-mode 1)
  (global-semantic-highlight-func-mode 1)
  (semantic-mode 1))

(use-package idle
  :config
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-idle-completions-mode 1)
  (global-semantic-idle-breadcrumbs-mode 1))

(use-package mode
  :config
  (global-semantic-decoration-mode 1)
  ;; Enable SRecode (Template management) minor-mode.
  (global-srecode-minor-mode 1))

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
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.4.4/"))
          ((string-equal system-name "biswass-Dell-System-XPS-L502X")
           (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9"))))
  (add-to-list 'company-clang-arguments "-I/home/biswass/workspace/intel-pintool/source/include"))

(provide 'cc-init)

;;; cc-init.el ends here
