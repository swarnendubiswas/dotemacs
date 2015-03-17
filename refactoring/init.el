;;; init.el --- Emacs customization ;;; -*- lexical-binding: t; -*-
;; Swarnendu Biswas

;;; Commentary:

;; Notes: To evaluate an Sexp, just go to the end of the sexp and type "C-x C-e", instead of evaluating the whole buffer
;; Init file shouldn't ideally contain calls to "load" or "require", since they cause eager loading and are expensive, a
;; cheaper alternative is to use "autoload".

;; Interesting quotes (inspired from http://www.mygooglest.com/fni/dot-emacs.html):
;;
;;   "Show me your ~/.emacs and I will tell you who you are." -- Bogdan Maryniuk
;;
;;   "People talk about getting used to a new editor, but over time, it is precisely the opposite that should happen -
;;    the editor should get used to us." -- Vivek Haldar in "New frontiers in text editing".
;;
;;   "Emacs is like a laser guided missile. It only has to be slightly mis-configured to ruin your whole day." -- Sean
;;    McGrath
;;
;;   "Emacs outshines all other editing software in approximately the same way that the noonday sun does the stars. It
;;    is not just bigger and brighter; it simply makes everything else vanish." -- Neal Stephenson, "In the Beginning
;;    was the Command Line"
;;
;;   "Nearly everybody is convinced that every style but their own is ugly and unreadable. Leave out the "but their own"
;;    and they're probably right..." -- Jerry Coffin (on indentation)
;;
;;   "The only real difficulties in programming are cache invalidation and naming things." -- Phil Karlton
;;
;;   "Good code is its own best documentation. As you're about to add a comment, ask yourself, "How can I improve the
;;    code so that this comment isn't needed?" Improve the code and then document it to make it even clearer." -- Steve
;;    McConnell
;;
;;   "What I don't understand is: why should you ever care how your editor looks, unless you're trying to win a
;;    screenshot competition? The primary factor in looking good should be the choice of a good font at a comfortable
;;    size, and a syntax coloring theme that you like. And that is not something specific to an editor. Editors like
;;    Emacs and vi have almost no UI! If Emacs is configured right, the only UI it has is the modeline and the
;;    minibuffer." -- Vivek Haldar in "New frontiers in text editing".
;;
;;   "Good code is like a good joke - it needs no explanation." -- Russ Olsen

;;; Code:


;; customizing packages
;; FIXME: Why does this not work?
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'packages-init)



(require 'ido-init)
(require 'anzu-init)
(require 'abbrev-init)
(require 'ace-jump-init)
(require 'anzu-init)
(require 'appearance-init)
(require 'buffer-init)
(require 'c-init)
(require 'company-init)
(require 'custom-init)
(require 'defaults-init)
(require 'dired-init)
(require 'fci-init)
(require 'flycheck-init)
(require 'guide-key-init)
(require 'indent-init)
(require 'java-init)
(require 'keybindings-init)
(require 'latex-init)
(require 'misc-init)
(require 'mode-line-init)
(require 'org-init)
(require 'parens-init)
(require 'prog-init)
(require 'rainbow-init)
(require 'recent-init)
(require 'shell-init)
(require 'smex-init)
(require 'spell-init)
(require 'text-init)
(require 'undo-tree-init)
(require 'user-init)
(require 'yasnippet-init)

;; customize appearance

;; Package specific


;;; init.el ends here
