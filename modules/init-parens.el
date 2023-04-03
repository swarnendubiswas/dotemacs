;;; init-parens.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding: utf-8;
;;; no-byte-compile: t; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(use-package paren
  :straight (:type built-in)
  :hook
  (emacs-startup-hook . show-paren-mode)
  :custom
  (show-paren-style 'parenthesis) ; `mixed' may lead to performance problems
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;; Enable autopairing

;; (use-package elec-pair
;;   :straight (:type built-in)
;;   :disabled t
;;   :hook (emacs-startup-hook . electric-pair-mode)
;;   :config
;;   ;; https://emacs.stackexchange.com/questions/2538/how-to-define-additional-mode-specific-pairs-for-electric-pair-mode
;;   (defvar sb/markdown-pairs '((?` . ?`)) "Electric pairs for `markdown-mode'.")
;;   (defvar electric-pair-pairs)
;;   (defvar electric-pair-text-pairs)
;;   (defvar electric-pair-preserve-balance)

;;   (declare-function sb/add-markdown-pairs "init-parens")

;;   (defun sb/add-markdown-pairs ()
;;     "Add custom pairs to `markdown-mode'."
;;     (setq-local electric-pair-pairs (append electric-pair-pairs sb/markdown-pairs))
;;     (setq-local electric-pair-text-pairs electric-pair-pairs))

;;   (add-hook 'markdown-mode-hook #'sb/add-markdown-pairs)

;;   ;; Avoid balancing parentheses since they can be both irritating and slow
;;   (setq electric-pair-preserve-balance nil)

;;   ;; Disable pairs when entering minibuffer
;;   (add-hook 'minibuffer-setup-hook (lambda ()
;;                                      (electric-pair-mode -1)))
;;   ;; Re-enable pairs when existing minibuffer
;;   (add-hook 'minibuffer-exit-hook (lambda ()
;;                                     (electric-pair-mode 1))))

;; `sp-cheat-sheet' will show you all the commands available, with examples.
(use-package smartparens
  :commands
  (sp-pair sp-local-pair sp-raise-sexp sp-join-sexp sp-absorb-sexp
           sp-transpose-sexp sp-absort-sexp sp-copy-sexp
           sp-backward-kill-sexp sp-kill-sexp sp-change-inner
           sp-change-enclosing sp-convolute-sexp sp-emit-sexp
           sp-backward-down-sexp sp-backward-up-sexp
           sp-backward-slurp-sexp sp-backward-barf-sexp
           sp-forward-barf-sexp sp-forward-slurp-sexp sp-rewrap-sexp
           sp-unwrap-sexp sp-backward-unwrap-sexp sp-wrap-round
           sp-wrap-curly sp-wrap-square sp-split-sexp)
  :hook
  ((emacs-startup-hook . (lambda ()
                           (smartparens-global-mode 1)
                           (show-smartparens-global-mode 1)
                           (require 'smartparens-config))))
  :bind
  (("C-M-a" . sp-beginning-of-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-e" . sp-end-of-sexp) ; "f_oo bar" -> "foo bar_"
   ("C-M-u" . sp-up-sexp) ; "f_oo bar" -> "foo bar"_
   ("C-M-w" . sp-down-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-f" . sp-forward-sexp) ; "foo ba_r" -> "foo bar"_
   ("C-M-b" . sp-backward-sexp) ; "foo ba_r" -> "_foo bar"
   ("C-M-n" . sp-next-sexp) ; ))" -> ((foo) (bar))"
   ("C-M-p" . sp-previous-sexp) ; "(foo (b|ar baz))" -> "(foo| (bar baz))"
   ("C-S-b" . sp-backward-symbol) ; "foo bar| baz" -> "foo |bar baz"
   ("C-S-f" . sp-forward-symbol) ; "|foo bar baz" -> "foo| bar baz"
   ("C-M-k" . sp-splice-sexp) ; "(foo bar)" -> "foo bar"
   ;; "foo(2,3)" -> "foo[2,3]"
   ("C-M-r" . sp-rewrap-sexp))
  :custom
  (sp-show-pair-from-inside nil "show-parens is faster")
  (sp-highlight-pair-overlay nil "show-parens is faster")
  (sp-highlight-wrap-overlay nil "show-parens is faster")
  (sp-highlight-wrap-tag-overlay nil)
  :config
  ;; Introduces overhead to track parentheses pairs
  (smartparens-strict-mode -1)

  ;; Do not insert a parenthesis pair when the point is at the beginning of a word
  (sp-pair "("  nil :unless '(sp-point-before-word-p))
  (sp-pair "["  nil :unless '(sp-point-before-word-p))
  (sp-pair "{"  nil :unless '(sp-point-before-word-p))

  ;; Do not pair quotes unless they are free
  (sp-pair "\"" nil :unless '(sp-point-before-word-p sp-point-after-word-p))

  ;; Do not insert a "$" pair when the point is at the beginning or the end of a word
  (sp-local-pair 'latex-mode "$" nil :unless '(sp-point-before-word-p sp-point-after-word-p))

  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "/\*\*" "\*\*/"))

  (sp-with-modes 'java-mode
    (sp-local-pair "/\*\*" "\*\*/")
    (sp-local-pair "/\*" "\*/"))

  ;; Do not insert a "=" pair when the point is at the beginning or the end of a word
  (sp-with-modes 'org-mode
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p sp-point-before-word-p))
    (sp-local-pair "=" "="
                   :unless '(sp-point-after-word-p sp-point-before-word-p)
                   :post-handlers '(("[d1]" "SPC"))))
  :diminish)

(provide 'init-parens)

;;; init-parens.el ends here
