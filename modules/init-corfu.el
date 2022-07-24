;;; init-corfu.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

(defvar sb/capf)

;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
;; https://github.com/minad/corfu/wiki
(use-package corfu
  :preface
  (defun sb/corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun sb/corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-indexed
                              corfu-quick
                              corfu-history))
  :if (eq sb/capf 'corfu)
  :commands corfu--goto
  :hook
  (after-init-hook . global-corfu-mode)
  :bind
  (:map corfu-map
        ("[tab]" . corfu-next)
        ("C-n" . corfu-next)
        ("[backtab]" . corfu-previous)
        ("C-p" . corfu-previous)
        ("<escape>" . corfu-quit)
        ([remap move-beginning-of-line] . sb/corfu-beginning-of-prompt)
        ([remap move-end-of-line] . sb/corfu-end-of-prompt))
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0.1 "Recommended to not use zero for performance reasons")
  (corfu-auto-prefix 3)
  :config
  (unless (featurep 'corfu-doc)
    (setq corfu-echo-documentation t)))

(use-package corfu-info
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info))
  :after corfu
  :bind
  (:map corfu-map
        ("M-d" . corfu-info-documentation)
        ("M-l" . corfu-info-location)))

;; The indexed mode uses numeric prefix arguments, e.g., "C-0 RET" or "C-1 TAB".
(use-package corfu-indexed
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-indexed))
  :after corfu
  :commands corfu-indexed-mode
  :init (corfu-indexed-mode 1))

(use-package corfu-quick
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-quick))
  :after corfu
  :bind
  (:map corfu-map
        ("C-'" . corfu-quick-insert)))

(use-package corfu-history
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-history))
  :after (corfu savehist)
  :commands corfu-history-mode
  :init
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1))

(use-package corfu-doc
  :if (and (display-graphic-p) (eq sb/capf 'corfu))
  :hook
  (corfu-mode-hook . corfu-doc-mode)
  :bind
  (:map corfu-map
        ("M-p" . corfu-doc-scroll-down)
        ("M-n" . corfu-doc-scroll-up)
        ([remap corfu-info-documentation] . corfu-doc-toggle))
  :custom
  ;; Do not show documentation shown in both the echo area and in the `corfu-doc' popup
  (corfu-echo-documentation nil))

(use-package popon
  :straight (:type git :repo "https://codeberg.org/akib/emacs-popon.git")
  :if (and (eq sb/capf 'corfu) (not (display-graphic-p))))

(use-package corfu-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :if (and (eq sb/capf 'corfu) (not (display-graphic-p)))
  :hook
  (corfu-mode-hook . corfu-terminal-mode)
  :custom
  (corfu-terminal-position-right-margin 5))

(use-package corfu-doc-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
  :if (and (eq sb/capf 'corfu) (not (display-graphic-p)))
  :hook
  (corfu-mode-hook . corfu-doc-terminal-mode))

;; Here is a snippet to show how to support `company' backends with `cape'.
;; https://github.com/minad/cape/issues/20
;; (fset #'cape-path (cape-company-to-capf #'company-files))
;; (add-hook 'completion-at-point-functions #'cape-path)

;; https://kristofferbalintona.me/posts/cape/
(use-package cape
  :preface
  ;; https://kristofferbalintona.me/posts/202203130102/
  (defun sb/cape-capf-setup-lsp ()
    "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version. Also add `cape-file' and
`company-yasnippet' backends."
    (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
          (cape-capf-buster #'lsp-completion-at-point))
    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
  :after corfu
  :demand t
  :commands (cape-history ; Complete from Eshell, Comint or minibuffer history
             cape-file
             cape-keyword ; Complete programming language keyword
             cape-tex ; Complete unicode char from TeX command, e.g. \hbar.
             cape-abbrev ; Complete abbreviation at point
             cape-dict ; Complete word from dictionary at point
             cape-line ; Complete current line from other lines in buffer
             cape-symbol ; Elisp symbol
             cape-ispell ; Complete word at point with Ispell
             ;; Complete with Dabbrev at point
             cape-dabbrev
             cape-capf-buster
             cape-company-to-capf
             cape-super-capf
             sh-completion-at-point-function
             comint-completion-at-point
             citre-completion-at-point
             TeX--completion-at-point)
  :init
  ;; Initialize for all generic languages that are not specifically handled
  (add-to-list 'completion-at-point-functions #'cape-file 'append)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append)
  (add-to-list 'completion-at-point-functions #'cape-dict 'append)
  (add-to-list 'completion-at-point-functions #'cape-ispell 'append)
  :custom
  (cape-dabbrev-min-length 3)
  ;; Checking all other buffers for completetion ignoring the major mode seems to be expensive
  (cape-dabbrev-check-other-buffers nil)
  (cape-dict-file (expand-file-name "wordlist.5" sb/extras-directory))
  :config
  ;; https://github.com/minad/cape/issues/53
  ;; Override CAPFS for specific major modes

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list
                           (cape-super-capf #'citre-completion-at-point
                                            #'elisp-completion-at-point
                                            #'cape-file
                                            #'cape-symbol ; Elisp symbols
                                            #'cape-dabbrev
                                            #'cape-dict)))))

  (add-hook 'sh-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list
                           (cape-super-capf #'lsp-completion-at-point
                                            #'citre-completion-at-point
                                            #'sh-completion-at-point-function
                                            #'comint-completion-at-point
                                            #'cape-file
                                            #'cape-dabbrev
                                            #'cape-dict)))))

  (dolist (lsp-prog-modes '(c++-mode-hook java-mode-hook python-mode-hook))
    (add-hook lsp-prog-modes
              (lambda ()
                (setq-local completion-at-point-functions
                            (list
                             (cape-super-capf #'lsp-completion-at-point
                                              #'citre-completion-at-point
                                              #'tags-completion-at-point-function
                                              #'cape-file
                                              #'cape-keyword
                                              #'cape-dabbrev
                                              #'cape-dict))))))

  ;; (add-hook 'prog-mode-hook
  ;;           (lambda ()
  ;;             ;; (unless (derived-mode-p 'emacs-lisp-mode)
  ;;             (add-to-list 'completion-at-point-functions #'cape-file 'append)
  ;;             (add-to-list 'completion-at-point-functions #'cape-keyword 'append)
  ;;             (add-to-list 'completion-at-point-functions #'cape-history 'append)
  ;;             (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append)
  ;;             (add-to-list 'completion-at-point-functions #'cape-dict 'append)))

  (dolist (modes '(latex-mode-hook LaTeX-mode-hook))
    (add-hook modes
              (lambda ()
                (setq-local completion-at-point-functions
                            (list
                             (cape-super-capf #'lsp-completion-at-point
                                              #'citre-completion-at-point
                                              #'TeX--completion-at-point
                                              #'cape-file
                                              #'cape-tex
                                              #'cape-dabbrev
                                              #'cape-dict
                                              #'cape-ispell))))))

  ;; (add-hook 'LaTeX-mode-hook
  ;;           (lambda ()
  ;;             ;; (rquire 'company-auctex)
  ;;             ;; (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-auctex-bibs))
  ;;             ;; (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-auctex-labels))
  ;;             ;; (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-auctex-symbols))
  ;;             ;; (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-auctex-environments))
  ;;             ;; (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-auctex-macros))
  ;;             ;; (add-to-list 'completion-at-point-functions #'cape-file 'append)
  ;;             ;; (add-to-list 'completion-at-point-functions #'cape-keyword 'append)
  ;;             ;; (add-to-list 'completion-at-point-functions #'cape-tex 'append)
  ;;             ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append)
  ;;             ;; (add-to-list 'completion-at-point-functions #'cape-dict 'append)
  ;;             ))

  ;; (add-hook 'text-mode-hook
  ;;           (lambda ()
  ;;             (unless (or (derived-mode-p 'latex-mode) (derived-mode-p 'LaTeX-mode))
  ;;               (setq-local completion-at-point-functions
  ;;                           (list (cape-super-capf #'cape-file
  ;;                                                  #'cape-dabbrev
  ;;                                                  #'cape-dict))))))
  )

;; Provide icons for Corfu

;; (use-package kind-icon
;;   :after corfu
;;   :demand t
;;   :commands kind-icon-margin-formatter
;;   :if (display-graphic-p)
;;   :custom
;;   (kind-icon-face 'corfu-default)
;;   (kind-icon-default-face 'corfu-default) ; To compute blended backgrounds correctly
;;   ;; Prefer smaller icons and a more compact popup
;;   (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 0.6))
;;   (kind-icon-blend-background nil)
;;   (kind-icon-blend-frac 0.08)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; `all-the-icons' integration with `corfu'
;; https://github.com/lynnux/.emacs.d/blob/master/packages/corfu/corfu-icon.el
(use-package corfu-icon
  :straight nil
  :if (display-graphic-p)
  :after corfu
  :demand t
  :load-path "extras")

(provide 'init-corfu)

;;; init-corfu.el ends here
