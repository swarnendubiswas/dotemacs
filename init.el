;;; init.el --- Emacs customization  -*- lexical-binding: t; no-byte-compile: t; -*-
;; Swarnendu Biswas

;;; Commentary:

;; Notes: To evaluate an Sexp, just go to the end of the sexp and type "C-x C-e", instead of evaluating the whole buffer
;; Use C-M-x to evaluate the current top-level s-expression. Use M-: to evaluate any Emacs Lisp expression and print the
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


;;; Code:

;; customizing packages
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/")) ; already added by default
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp")) ; third-party packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules/")) ; personal modules for customizing emacs initialization

;; setup the packaging system
(require 'packages-init)

;; setup configuration variables
(require 'config-init)

;; now setup other modules

;; user, defaults, and appearance
(require 'user-init)
(require 'defaults-init)
(require 'appearance-init)
(require 'mode-line-init)

;; configure power packages
(require 'ibuffer-init)
(require 'dired-init)
(require 'search-init)
;; helm is configured to make use of recentf
(require 'recentf-init)

;;(if (bound-and-true-p use-company)
(if (eq dotemacs-completion 'company)
    (require 'company-init)
  (require 'auto-complete-init))

(require 'ido-init)
(require 'helm-init)
(require 'smex-init)

;; configure the more useful extensions
(require 'anzu-init)
(require 'flyspell-init)
(require 'projectile-init)

;; setup helper packages
(require 'abbrev-init)
(require 'ace-modes-init)
(require 'fci-init)
(require 'indent-init)
(require 'parens-init)
(require 'misc-init)
(require 'yasnippet-init)
(require 'flycheck-init)
(require 'guide-key-init)
(require 'rainbow-init)
(require 'undo-tree-init)
(require 'whitespace-init)
(require 'highlight-init)

;; configure individual major modes
(require 'text-init)
(require 'latex-init)
(require 'org-init)
(require 'prog-init)
(require 'cc-init)
(require 'java-init)
(require 'shell-init)

;; generic keybindings, package-specific are usually in their own modules
(require 'keybindings-init)

;; custom definitions
(require 'custom-init)

;; temporary modules
(require 'svn-init)
;; (require 'git-init)

(require 'cedet-init)
(require 'jdee-init)

;; for some reason, this needs to be loaded after svn-init module. Otherwise, the ggtags and helm-gtags modes are not
;; enabled on startup.
(require 'tags-init)

;; start the daemon/server
;; (require 'server-init)

;; start with the emacs window maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

;;; init.el ends here
