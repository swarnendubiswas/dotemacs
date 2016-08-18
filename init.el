;;; init.el --- Emacs customization  -*- lexical-binding: t; no-byte-compile: nil; -*-
;; Swarnendu Biswas

;;; Commentary:

;; Notes: To evaluate an Sexp, just go to the end of the sexp and type "C-x C-e", instead of evaluating the whole buffer
;; Use C-M-x to evaluate the current top-level s-expression.  Use M-: to evaluate any Emacs Lisp expression and print the
;; result.

;; Init file should not ideally contain calls to "load" or "require", since they cause eager loading and are expensive,
;; a cheaper alternative is to use "autoload".

;; Interesting quotes (inspired from http://www.mygooglest.com/fni/dot-emacs.html):
;;
;;   "Show me your ~/.emacs and I will tell you who you are." -- Bogdan Maryniuk.
;;
;;   "People talk about getting used to a new editor, but over time, it is precisely the opposite that should happen -
;;    the editor should get used to us." -- Vivek Haldar in "New frontiers in text editing".
;;
;;   "Emacs is like a laser guided missile. It only has to be slightly mis-configured to ruin your whole day." -- Sean
;;    McGrath.
;;
;;   "While any text editor can save your files, only Emacs can save your soul." -- Per Abrahamsen.
;;
;;   "Emacs outshines all other editing software in approximately the same way that the noonday sun does the stars. It
;;    is not just bigger and brighter; it simply makes everything else vanish." -- Neal Stephenson, "In the Beginning
;;    was the Command Line".
;;
;;   "Nearly everybody is convinced that every style but their own is ugly and unreadable. Leave out the "but their own"
;;    and they're probably right..." -- Jerry Coffin (on indentation).
;;
;;   "The only real difficulties in programming are cache invalidation and naming things." -- Phil Karlton.
;;
;;   "Good code is its own best documentation. As you're about to add a comment, ask yourself, "How can I improve the
;;    code so that this comment isn't needed?" Improve the code and then document it to make it even clearer." -- Steve
;;    McConnell.
;;
;;   "What I don't understand is: why should you ever care how your editor looks, unless you're trying to win a
;;    screenshot competition? The primary factor in looking good should be the choice of a good font at a comfortable
;;    size, and a syntax coloring theme that you like. And that is not something specific to an editor. Editors like
;;    Emacs and vi have almost no UI! If Emacs is configured right, the only UI it has is the modeline and the
;;    minibuffer." -- Vivek Haldar in "New frontiers in text editing".
;;
;;   "Good code is like a good joke - it needs no explanation." -- Russ Olsen.
;;
;;   "As a poke at Emacs’ creeping featurism, vi advocates have been known to describe Emacs as “a great operating
;;    system, lacking only a decent editor”." -- Editor war (https://en.wikipedia.org/wiki/Editor_war).
;;
;;   "Emacs was a really great idea in the seventies and one of the frightening things about Emacs today is if you skip
;;    the last 20 years it is much the same." -- Don't use Emacs, says Java's father
;;    (http://www.computerworld.com.au/article/207799/don_t_use_emacs_says_java_father/).
;;
;;   "Bad programming is easy. Even Dummies can learn it in 21 days." -- Felleisen et al. in "How to Design Programs".
;;
;;   "A language that doesn't affect the way you think about programming, is not worth knowing." -- Alan Perlis.
;;
;;   "I swear you spend just as much time configuring Emacs as you do coding other stuff"
;;
;;   "Emacs saves you time when you work, and takes it back when you play with it" -- http://puntoblogspot.blogspot.com/
;;
;;   "Emacs isn't an editor. It's an interpreter that happens to ship with an editor program. It makes a lot more sense
;;   with this mindset." -- Wilfred Hughes.


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
(add-to-list 'load-path (concat user-emacs-directory "modules/"))

(require 'config-init) ; Setup configuration variables
(require 'packages-init) ; Setup the package system

;; Now setup other modules

(require 'user-init)
(require 'defaults-init)
(require 'appearance-init)
(require 'mode-line-init)

;; Configure power packages
(require 'ibuffer-init)
(require 'dired-init)
(require 'search-init)
(require 'recentf-init)
(if (eq dotemacs-completion-in-buffer 'company)
    (require 'company-init)
  (require 'auto-complete-init))
(require 'smex-init)
(cond ((eq dotemacs-selection 'helm) (require 'helm-init))
      ((eq dotemacs-selection 'ido)  (require 'ido-init))
      ((eq dotemacs-selection 'ivy)  (require 'ivy-init)))

;; Configure the more useful extensions
(require 'anzu-init)
(require 'flyspell-init)
(require 'ace-modes-init)
(require 'indent-init)
(require 'parens-init)
(require 'abbrev-init)
(require 'kill-ring-init)

;; Setup helper packages
(require 'projectile-init)
(require 'fci-init)
(require 'misc-init)
(require 'yasnippet-init)
(require 'flycheck-init)
(require 'rainbow-init)
(require 'whitespace-init)
(require 'highlight-init)

;; Configure individual major modes
(require 'text-init)
(require 'latex-init)
(require 'org-init)
(require 'prog-init)
(require 'cc-init)
(require 'java-init)
(require 'python-init)
(require 'shell-init)
(require 'svn-init)
(require 'git-init)

;; FIXME: For some reason, this needed to be loaded after svn-init module. Otherwise, the ggtags and helm-gtags modes are
;; not enabled on startup. I am not using this module currently.
(require 'tags-init)
(require 'custom-init)

(require 'keybindings-init) ; Generic keybindings, package-specific are usually in their own modules

(require 'server-init) ; Start the daemon/server

;;; init.el ends here
