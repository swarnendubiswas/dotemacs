;;; init-corfu.el --- Emacs customization -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8;
;;; no-byte-compile: nil; fill-column: 100 -*-

;; Swarnendu Biswas

;;; Commentary:

;;; Code:

;; https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
;; https://github.com/minad/corfu/wiki
(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-indexed
                              corfu-quick
                              corfu-info
                              corfu-history))
  :commands corfu--goto
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
  ;; :hook (prog-mode-hook . corfu-mode)
  :custom
  (corfu-cycle t "Enable cycling for `corfu-next/previous'")
  (corfu-auto t "Enable auto completion")
  (corfu-auto-delay 0.1 "Recommended to not use zero for performance reasons")
  (corfu-auto-prefix 2)
  (corfu-min-width 60)
  (corfu-max-width corfu-min-width "Always have the same width")
  (corfu-count 15)
  (corfu-preselect-first t)
  (corfu-separator ?\s) ; Use space
  (corfu-quit-no-match 'separator)
  :config
  (unless (featurep 'corfu-doc)
    (setq corfu-echo-documentation t))
  :bind
  (:map corfu-map
        ("[tab]" . corfu-next)
        ("C-n" . corfu-next)
        ("[backtab]" . corfu-previous)
        ("C-p" . corfu-previous)
        ("<escape>" . corfu-quit)
        ([remap move-beginning-of-line] . sb/corfu-beginning-of-prompt)
        ([remap move-end-of-line] . sb/corfu-end-of-prompt)))

(use-package corfu-info
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info))
  :after corfu
  :bind
  (:map corfu-map
        ("M-d" . corfu-info-documentation)
        ("M-l" . corfu-info-location)))

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
  :commands corfu-history-mode
  :after (corfu savehist)
  :init
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1))

(use-package corfu-doc
  :if (display-graphic-p)
  :hook (corfu-mode-hook . corfu-doc-mode)
  :bind
  (:map corfu-map
        ("M-p" . corfu-doc-scroll-down)
        ("M-n" . corfu-doc-scroll-up)
        ([remap corfu-info-documentation] . corfu-doc-toggle))
  :custom
  ;; Do not show documentation shown in both the echo area and in the `corfu-doc' popup
  (corfu-echo-documentation nil))

(use-package popon
  :straight (popon :type git
                   :repo "https://codeberg.org/akib/emacs-popon.git")
  :unless (display-graphic-p))

(use-package corfu-terminal
  :straight (corfu-terminal :type git
                            :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :unless (display-graphic-p)
  :hook (corfu-mode-hook . corfu-terminal-mode))

;; Here is a snippet to show how to support `company' backends with `cape'.
;; https://github.com/minad/cape/issues/20
;; (fset #'cape-path (cape-company-to-capf #'company-files))
;; (add-hook 'completion-at-point-functions #'cape-path)

;; https://kristofferbalintona.me/posts/cape/
(use-package cape
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
             cape-dabbrev)
  :custom
  (cape-dabbrev-min-length 3)
  :config
  ;; (dolist (backends '(cape-symbol cape-keyword cape-file cape-tex
  ;;                                 cape-dabbrev cape-ispell cape-dict cape-history))
  ;; (add-to-list 'completion-at-point-functions backends))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-super-capf #'cape-file
                                                 #'cape-symbol
                                                 #'cape-keyword
                                                 #'cape-history
                                                 #'cape-dict
                                                 #'cape-ispell
                                                 #'cape-dabbrev)))
              ;; (add-to-list 'completion-at-point-functions #'cape-file 'append)
              ;; (add-to-list 'completion-at-point-functions #'cape-symbol 'append)
              ;; (add-to-list 'completion-at-point-functions #'cape-keyword 'append)
              ;; (add-to-list 'completion-at-point-functions #'cape-history 'append)
              ;; (add-to-list 'completion-at-point-functions #'cape-dict 'append)
              ;; (add-to-list 'completion-at-point-functions #'cape-ispell 'append)
              ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append)
              ))
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-file 'append)
              (add-to-list 'completion-at-point-functions #'cape-keyword 'append)
              (add-to-list 'completion-at-point-functions #'cape-history 'append)
              (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append)
              (add-to-list 'completion-at-point-functions #'cape-dict 'append)
              (add-to-list 'completion-at-point-functions #'cape-ispell 'append)))

  ;; (add-hook 'text-mode-hook
  ;;           (lambda ()
  ;;             ;; (setq-local completion-at-point-functions
  ;;             ;;             (list (cape-super-capf #'cape-dabbrev #'cape-file #'cape-history #'cape-ispell #'cape-dict)))
  ;;             (setq-local completion-at-point-functions
  ;;                         (list (cape-capf-properties #'lsp-completion-at-point :exclusive 'no) t))

  ;;             (add-to-list 'completion-at-point-functions #'cape-file 'append)
  ;;             (add-to-list 'completion-at-point-functions #'cape-dict 'append)
  ;;             (add-to-list 'completion-at-point-functions #'cape-history 'append)
  ;;             (add-to-list 'completion-at-point-functions #'cape-ispell 'append)
  ;;             (add-to-list 'completion-at-point-functions #'cape-dabbrev 'append)))
  )

;; Provide icons for Corfu
(use-package kind-icon
  :after corfu
  :demand t
  :commands kind-icon-margin-formatter
  :if (display-graphic-p)
  :custom
  (kind-icon-face 'corfu-default)
  (kind-icon-default-face 'corfu-default) ; To compute blended backgrounds correctly
  (kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 0.8))
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'init-corfu)

;;; init-corfu.el ends here
