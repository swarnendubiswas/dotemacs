;;; init.el --- Emacs customization  -*- lexical-binding: t; no-byte-compile: nil; -*-
;; Swarnendu Biswas

;;; Commentary:

;; Notes: To evaluate an Sexp, just go to the end of the sexp and type "C-x C-e", instead of evaluating the whole buffer
;; Use C-M-x to evaluate the current top-level s-expression.  Use M-: to evaluate any Emacs Lisp expression and print the
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

;; Personal modules for customizing Emacs initialization

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "modules/"))

(require 'config) ; Setup configuration variables
(require 'packages) ; Setup the package system

;; Now setup other modules

(require 'user)
(require 'defaults)
(require 'appearance)
(require 'mode-line)

;; Configure power packages
(require 'ibuffer)
(require 'dired)
(require 'search)
(require 'recentf)
(require 'company)
(cond ((eq dotemacs-selection 'ido)  (require 'ido))
      ((eq dotemacs-selection 'ivy)  (require 'ivy)))

;; Configure the more useful extensions
(require 'spell)
(require 'ace-modes)
(require 'indent)
(require 'parens)
(require 'abbrev)
(require 'kill-ring)

;; Setup helper packages
(require 'projectile)
(require 'misc)
(require 'yasnippet)
(require 'flycheck)
(require 'rainbow)
(require 'whitespace)
(require 'highlight)
(require 'tramp)
(require 'imenu)
(require 'tags)

;; Configure individual major modes
(require 'text)
;; (require 'latex)
(require 'latex-new)
(require 'org)
(require 'prog)
(require 'cc)
(require 'java)
(require 'python)
(require 'shell-script)
(require 'shell)
(require 'svn)
(require 'git)

(require 'functions)
(require 'keybindings) ; Generic keybindings, package-specific are usually in their own modules
(require 'server) ; Start the daemon/server

;; Mark safe variables

(put 'company-clang-arguments 'safe-local-variable 'listp)
(put 'company-c-headers-path-user 'safe-local-variable 'listp)
(put 'reftex-default-bibliograph 'safe-local-variable 'listp)
(put 'company-bibtex-bibliograph 'safe-local-variable 'listp)
(put 'bibtex-completion-bibliography 'safe-local-variable 'listp)

;;; init.el ends here
