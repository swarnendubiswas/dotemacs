;;; init.el --- Emacs customization  -*- lexical-binding: t; no-byte-compile: nil; -*-
;; Swarnendu Biswas

;;; Commentary:

;; Notes: To evaluate an Sexp, just go to the end of the sexp and type "C-x C-e", instead of evaluating the whole buffer
;; Use C-M-x to evaluate the current top-level s-expression. Use M-: to evaluate any Emacs Lisp expression and print the
;; result.

;; Init file should not ideally contain calls to "load" or "require", since they cause eager loading and are expensive,
;; a cheaper alternative is to use "autoload".

;;; Code:

;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Anonymous-Functions.html
;; When defining a lambda expression that is to be used as an anonymous function, you can in principle use any method
;; to construct the list. But typically you should use the lambda macro, or the function special form, or the #' read
;; syntax which is a short-hand for using function. Quoting a lambda form means the anonymous function is not
;; byte-compiled. The following forms are all equivalent:
;; (lambda (x) (* x x))
;; (function (lambda (x) (* x x)))
;; #'(lambda (x) (* x x))

(setq debug-on-error t)

;; Personal modules for customizing Emacs initialization

(setq user-full-name "Swarnendu Biswas")

(add-to-list 'load-path (concat user-emacs-directory "modules/"))

(require 'config-init) ; Setup configuration variables
(require 'packages-init) ; Setup the package system

;; Now setup other modules

(require 'defaults-init)
(require 'appearance-init)
(require 'modeline-init)

;; Configure power packages
(require 'ibuffer-init)
(require 'dired-init)
(require 'search-init)
(require 'recentf-init)
(require 'company-init)
(require 'ivy-init)

;; Configure the more useful extensions
(require 'spell-init)
(require 'ace-modes-init)
(require 'indent-init)
(require 'parens-init)
(require 'projectile-init)
(require 'flycheck-init)

;; Setup helper packages
(require 'misc-init)
(require 'yasnippet-init)
(require 'whitespace-init)
(require 'highlight-init)
(require 'tramp-init)
(require 'imenu-init)
(require 'tags-init)

;; Configure individual major modes
(require 'text-init)
(require 'latex-init)
;; (require 'org-init)
(require 'prog-init)
(require 'c_cpp-init)
(require 'python-init)
;; (require 'java-init)
(require 'shell-script-init)
;; (require 'shell-init)
;; (require 'svn-init)
(require 'git-init)

(require 'functions-init)
(require 'keybindings-init) ; Generic keybindings, package-specific are usually in their own modules

;; Mark safe variables

(put 'company-clang-arguments 'safe-local-variable 'listp)
(put 'company-c-headers-path-user 'safe-local-variable 'listp)
(put 'reftex-default-bibliography 'safe-local-variable 'listp)
(put 'company-bibtex-bibliography 'safe-local-variable 'listp)
(put 'bibtex-completion-bibliography 'safe-local-variable 'listp)
(put 'flycheck-clang-include-path 'safe-local-variable 'listp)
(put 'flycheck-gcc-include-path 'safe-local-variable 'listp)

;;; init.el ends here
